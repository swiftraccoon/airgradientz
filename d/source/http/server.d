module http.server;

import std.socket;
import std.string : startsWith, indexOf;
import std.file : read, exists, isFile, getSize;
import std.path : extension, buildPath;
import std.format : format;

import core.sys.linux.epoll;
import core.stdc.errno : errno, EAGAIN, EINTR, EWOULDBLOCK;
import core.sys.posix.signal : SIGPIPE, SIG_IGN;
import core.stdc.signal : signal;
import core.sys.posix.fcntl : fcntl, F_GETFL, F_SETFL, O_NONBLOCK;
import core.sys.posix.unistd : close;
import core.time : dur, MonoTime;

import http.request : HttpRequest, Method, urlDecode;
import http.response : HttpResponse;
import api;
import state : AppState;
import log : logf, logln;

private string contentTypeFor(string path) {
    switch (path.extension) {
        case ".html": return "text/html; charset=utf-8";
        case ".css":  return "text/css; charset=utf-8";
        case ".js":   return "application/javascript; charset=utf-8";
        case ".json": return "application/json; charset=utf-8";
        case ".png":  return "image/png";
        case ".jpg":  return "image/jpeg";
        case ".jpeg": return "image/jpeg";
        case ".svg":  return "image/svg+xml";
        case ".ico":  return "image/x-icon";
        default:      return "application/octet-stream";
    }
}

private HttpResponse serveStatic(string reqPath) {
    import core.sys.posix.stdlib : realpath;
    import core.stdc.stdlib : free;

    // URL-decode the path
    string decoded = urlDecode(reqPath);

    // Check for path traversal in decoded path
    if (indexOf(decoded, "..") >= 0)
        return HttpResponse.notFound();

    // Reject control characters (0x00-0x1F, 0x7F)
    foreach (ch; decoded) {
        if (ch < 0x20 || ch == 0x7F)
            return HttpResponse.notFound();
    }

    // Strip leading slash
    string relative = decoded;
    while (relative.length > 0 && relative[0] == '/')
        relative = relative[1 .. $];

    string filePath;
    if (relative.length == 0)
        filePath = "public/index.html";
    else
        filePath = buildPath("public", relative);

    // Resolve to absolute path and verify under public/
    import std.string : toStringz, fromStringz;
    auto resolvedPtr = realpath(filePath.toStringz, null);
    if (resolvedPtr is null)
        return HttpResponse.notFound();
    string resolved = resolvedPtr.fromStringz.idup;
    free(resolvedPtr);

    auto publicPtr = realpath("public".toStringz, null);
    if (publicPtr is null)
        return HttpResponse.notFound();
    string publicResolved = publicPtr.fromStringz.idup;
    free(publicPtr);

    if (!startsWith(resolved, publicResolved ~ "/") && resolved != publicResolved)
        return HttpResponse.notFound();

    if (!exists(resolved) || !isFile(resolved))
        return HttpResponse.notFound();

    try {
        auto fileSize = getSize(resolved);
        if (fileSize > 16 * 1024 * 1024)
            return HttpResponse.internalError("File too large");
    } catch (Exception) {
        return HttpResponse.notFound();
    }

    try {
        auto contents = cast(const(ubyte)[]) read(resolved);
        auto ct = contentTypeFor(resolved);
        return HttpResponse.okStatic(contents, ct);
    } catch (Exception) {
        return HttpResponse.notFound();
    }
}

private enum MAX_CONNECTIONS = 1024;
private enum CONN_TIMEOUT = dur!"seconds"(30);
private enum ConnPhase { reading, writing }

private struct ConnData {
    ConnPhase phase = ConnPhase.reading;
    MonoTime acceptedAt;
    ubyte[8192] readBuf;
    size_t readPos = 0;
    const(ubyte)[] response;
    size_t writePos = 0;
}

private void setNonblocking(int fd) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1) {
        logf("[server] fcntl F_GETFL failed: errno=%d", errno);
        return;
    }
    if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        logf("[server] fcntl F_SETFL failed: errno=%d", errno);
    }
}

private void closeAndCleanup(int fd, ref ConnData[int] conns, int epfd, AppState appState) {
    // Remove from epoll (best effort, fd may already be deregistered on close)
    epoll_event dummy;
    epoll_ctl(epfd, EPOLL_CTL_DEL, fd, &dummy);
    close(fd);
    conns.remove(fd);
    appState.decrementConnections();
}

private HttpResponse routeRequest(HttpRequest req, AppState appState) {
    if (req.method == Method.get) {
        switch (req.path) {
            case "/api/readings/count":   return handleReadingsCount(appState, req);
            case "/api/readings":        return handleReadings(appState, req);
            case "/api/readings/latest":  return handleReadingsLatest(appState);
            case "/api/devices":          return handleDevices(appState);
            case "/api/health":           return handleHealth(appState);
            case "/api/config":           return handleConfig(appState);
            case "/api/stats":            return handleStats(appState);
            default:                      return serveStatic(req.path);
        }
    } else {
        return HttpResponse.methodNotAllowed();
    }
}

void runServer(AppState appState) {
    // Ignore SIGPIPE so writes to closed sockets don't kill us
    signal(SIGPIPE, SIG_IGN);

    // Set up the listen socket using D's TcpSocket for convenience
    auto listener = new TcpSocket();
    listener.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, true);
    listener.bind(new InternetAddress(InternetAddress.ADDR_ANY, appState.config.port));
    listener.listen(128);

    int listenFd = listener.handle;
    setNonblocking(listenFd);

    logf("[server] Listening on http://localhost:%d", appState.config.port);

    // Create epoll instance
    int epfd = epoll_create1(EPOLL_CLOEXEC);
    if (epfd < 0) {
        logf("[server] epoll_create1 failed: errno=%d", errno);
        return;
    }
    scope(exit) close(epfd);

    // Register listen socket with epoll (edge-triggered)
    epoll_event listenEv;
    listenEv.events = EPOLLIN | EPOLLET;
    listenEv.data.fd = listenFd;
    if (epoll_ctl(epfd, EPOLL_CTL_ADD, listenFd, &listenEv) != 0) {
        logf("[server] epoll_ctl ADD listen failed: errno=%d", errno);
        return;
    }

    ConnData[int] conns;
    enum MAX_EVENTS = 64;
    epoll_event[MAX_EVENTS] events;

    while (!appState.isShutdown) {
        // 1-second timeout so we can check shutdown flag
        int nfds = epoll_wait(epfd, events.ptr, MAX_EVENTS, 1000);

        if (nfds < 0) {
            if (errno == EINTR)
                continue;
            logf("[server] epoll_wait error: errno=%d", errno);
            break;
        }

        for (int i = 0; i < nfds; i++) {
            int fd = events[i].data.fd;
            uint ev = events[i].events;

            if (fd == listenFd) {
                // Accept loop (edge-triggered: accept until EAGAIN)
                while (true) {
                    int clientFd = acceptRaw(listenFd);
                    if (clientFd < 0) {
                        auto err = errno;
                        if (err == EAGAIN || err == EWOULDBLOCK)
                            break;
                        logf("[server] accept error: errno=%d", err);
                        break;
                    }

                    if (conns.length >= MAX_CONNECTIONS) {
                        close(clientFd);
                        continue;
                    }

                    setNonblocking(clientFd);
                    appState.incrementConnections();

                    // Register client fd for reading (edge-triggered)
                    epoll_event clientEv;
                    clientEv.events = EPOLLIN | EPOLLET;
                    clientEv.data.fd = clientFd;
                    if (epoll_ctl(epfd, EPOLL_CTL_ADD, clientFd, &clientEv) != 0) {
                        logf("[server] epoll_ctl ADD client failed: errno=%d", errno);
                        close(clientFd);
                        appState.decrementConnections();
                        continue;
                    }

                    auto cd = ConnData();
                    cd.acceptedAt = MonoTime.currTime;
                    conns[clientFd] = cd;
                }
            } else {
                // Handle EPOLLERR / EPOLLHUP
                if (ev & (EPOLLERR | EPOLLHUP)) {
                    closeAndCleanup(fd, conns, epfd, appState);
                    continue;
                }

                auto pConn = fd in conns;
                if (pConn is null) {
                    // Unknown fd, close it
                    epoll_event dummy;
                    epoll_ctl(epfd, EPOLL_CTL_DEL, fd, &dummy);
                    close(fd);
                    continue;
                }

                if ((ev & EPOLLIN) && pConn.phase == ConnPhase.reading) {
                    handleRead(fd, pConn, conns, epfd, appState);
                }

                // Re-check pConn since handleRead may have removed the fd
                pConn = fd in conns;
                if (pConn is null)
                    continue;

                if ((ev & EPOLLOUT) && pConn.phase == ConnPhase.writing) {
                    handleWrite(fd, pConn, conns, epfd, appState);
                }
            }
        }

        // Sweep stale connections (Slowloris defense)
        auto now = MonoTime.currTime;
        int[] stale;
        foreach (fd, ref conn; conns) {
            if ((now - conn.acceptedAt) > CONN_TIMEOUT)
                stale ~= fd;
        }
        foreach (fd; stale)
            closeAndCleanup(fd, conns, epfd, appState);
    }

    // Shutdown: close all remaining connections
    foreach (fd; conns.keys) {
        close(fd);
        appState.decrementConnections();
    }
    conns.clear();
    listener.close();
    logln("[server] Shutdown complete");
}

private void handleRead(int fd, ConnData* conn, ref ConnData[int] conns, int epfd, AppState appState) {
    // Edge-triggered: read until EAGAIN
    while (true) {
        if (conn.readPos >= conn.readBuf.length) {
            // Buffer full without finding \r\n\r\n — request too large, close
            closeAndCleanup(fd, conns, epfd, appState);
            return;
        }

        auto n = rawRead(fd, conn.readBuf[conn.readPos .. $]);
        if (n < 0) {
            auto err = errno;
            if (err == EAGAIN || err == EWOULDBLOCK)
                break; // Done reading for now
            // Real error
            closeAndCleanup(fd, conns, epfd, appState);
            return;
        }
        if (n == 0) {
            // Client closed connection
            closeAndCleanup(fd, conns, epfd, appState);
            return;
        }
        conn.readPos += cast(size_t) n;

        // Check if we have the full header (\r\n\r\n)
        auto data = cast(string) conn.readBuf[0 .. conn.readPos];
        if (indexOf(data, "\r\n\r\n") >= 0) {
            // Full header received, parse and route
            HttpRequest req;
            try {
                req = HttpRequest.parseFromBuf(data);
            } catch (Exception e) {
                logf("[server] parse error: %s", e.msg);
                closeAndCleanup(fd, conns, epfd, appState);
                return;
            }

            appState.incrementRequests();

            HttpResponse response;
            try {
                response = routeRequest(req, appState);
            } catch (Exception e) {
                logf("[server] route error: %s", e.msg);
                response = HttpResponse.internalError("Internal server error");
            }

            conn.response = response.toBytes();
            conn.writePos = 0;
            conn.phase = ConnPhase.writing;

            // Switch epoll interest to EPOLLOUT (edge-triggered)
            epoll_event modEv;
            modEv.events = EPOLLOUT | EPOLLET;
            modEv.data.fd = fd;
            if (epoll_ctl(epfd, EPOLL_CTL_MOD, fd, &modEv) != 0) {
                logf("[server] epoll_ctl MOD to EPOLLOUT failed: errno=%d", errno);
                closeAndCleanup(fd, conns, epfd, appState);
                return;
            }

            // Try to write immediately since we just switched to EPOLLOUT
            handleWrite(fd, conn, conns, epfd, appState);
            return;
        }
    }
}

private void handleWrite(int fd, ConnData* conn, ref ConnData[int] conns, int epfd, AppState appState) {
    // Edge-triggered: write until EAGAIN or done
    while (conn.writePos < conn.response.length) {
        auto remaining = conn.response[conn.writePos .. $];
        auto n = rawWrite(fd, remaining);
        if (n < 0) {
            auto err = errno;
            if (err == EAGAIN || err == EWOULDBLOCK)
                return; // Will get EPOLLOUT again when writable
            // Real error
            closeAndCleanup(fd, conns, epfd, appState);
            return;
        }
        if (n == 0) {
            closeAndCleanup(fd, conns, epfd, appState);
            return;
        }
        conn.writePos += cast(size_t) n;
    }

    // All data written, close connection
    closeAndCleanup(fd, conns, epfd, appState);
}

// Raw POSIX accept wrapper - avoids D exception overhead
private int acceptRaw(int listenFd) {
    import core.sys.posix.sys.socket : accept, sockaddr, socklen_t;
    sockaddr addr;
    socklen_t addrlen = sockaddr.sizeof;
    return accept(listenFd, &addr, &addrlen);
}

// Raw POSIX read wrapper
private ptrdiff_t rawRead(int fd, ubyte[] buf) {
    import core.sys.posix.unistd : read;
    return read(fd, buf.ptr, buf.length);
}

// Raw POSIX write wrapper
private ptrdiff_t rawWrite(int fd, const(ubyte)[] buf) {
    import core.sys.posix.unistd : write;
    return write(fd, buf.ptr, buf.length);
}
