module http.server;

import std.socket;
import std.stdio : stderr;
import std.string : startsWith, indexOf;
import std.file : read, exists, isFile;
import std.path : extension, buildPath;
import std.format : format;
import core.thread : Thread;

import http.request : HttpRequest, Method;
import http.response : HttpResponse;
import api;
import state : AppState;

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
    if (indexOf(reqPath, "..") >= 0)
        return HttpResponse.notFound();

    // Strip leading slash
    string relative = reqPath;
    while (relative.length > 0 && relative[0] == '/')
        relative = relative[1 .. $];

    string filePath;
    if (relative.length == 0)
        filePath = "public/index.html";
    else
        filePath = buildPath("public", relative);

    if (!exists(filePath) || !isFile(filePath))
        return HttpResponse.notFound();

    try {
        auto contents = cast(const(ubyte)[]) read(filePath);
        auto ct = contentTypeFor(filePath);
        return HttpResponse.okStatic(contents, ct);
    } catch (Exception) {
        return HttpResponse.notFound();
    }
}

private void handleConnection(Socket clientSock, AppState appState) {
    scope(exit) clientSock.close();

    auto req = HttpRequest.parse(clientSock);

    HttpResponse response;
    if (req.method == Method.get) {
        switch (req.path) {
            case "/api/readings":        response = handleReadings(appState, req); break;
            case "/api/readings/latest":  response = handleReadingsLatest(appState); break;
            case "/api/devices":          response = handleDevices(appState); break;
            case "/api/health":           response = handleHealth(appState); break;
            case "/api/config":           response = handleConfig(appState); break;
            default:                      response = serveStatic(req.path); break;
        }
    } else {
        response = HttpResponse.methodNotAllowed();
    }

    response.writeTo(clientSock);
}

void runServer(AppState appState) {
    auto listener = new TcpSocket();
    listener.setOption(SocketOptionLevel.SOCKET, SocketOption.REUSEADDR, true);
    listener.bind(new InternetAddress(InternetAddress.ADDR_ANY, appState.config.port));
    listener.listen(128);

    stderr.writefln("[server] Listening on http://localhost:%d", appState.config.port);

    while (!appState.isShutdown) {
        auto clientSock = listener.accept();
        if (clientSock is null) continue;

        auto t = new Thread({
            try {
                handleConnection(clientSock, appState);
            } catch (Exception e) {
                stderr.writefln("[server] Connection error: %s", e.msg);
            }
        });
        t.isDaemon = true;
        t.start();
    }
}
