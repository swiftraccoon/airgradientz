#include "http_client.h"

#include <errno.h>
#include <netdb.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "strbuf.h"

static void set_err(char *errbuf, size_t size, const char *fmt, ...)
    __attribute__((format(printf, 3, 4)));

static void set_err(char *errbuf, size_t size, const char *fmt, ...)
{
    if (!errbuf || size == 0) return;
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errbuf, size, fmt, ap);
    va_end(ap);
}

char *http_get(const char *ip, const char *path, uint32_t timeout_ms,
               char *errbuf, size_t errbuf_size)
{
    /* Split "host:port" or bare "host" — AirGradient devices are IPv4 only */
    char host_buf[256];
    const char *host;
    const char *port;

    const char *colon = strchr(ip, ':');
    if (colon) {
        size_t host_len = (size_t)(colon - ip);
        if (host_len >= sizeof(host_buf)) {
            set_err(errbuf, errbuf_size, "hostname too long");
            return NULL;
        }
        memcpy(host_buf, ip, host_len);
        host_buf[host_len] = '\0';
        host = host_buf;
        port = colon + 1;
    } else {
        host = ip;
        port = "80";
    }

    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    struct addrinfo *res = NULL;
    int gai_rc = getaddrinfo(host, port, &hints, &res);
    if (gai_rc != 0) {
        set_err(errbuf, errbuf_size, "resolve failed: %s", gai_strerror(gai_rc));
        return NULL;
    }

    int fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (fd < 0) {
        set_err(errbuf, errbuf_size, "socket: %s", strerror(errno));
        freeaddrinfo(res);
        return NULL;
    }

    struct timeval tv;
    tv.tv_sec  = (time_t)(timeout_ms / 1000u);
    tv.tv_usec = (long)((timeout_ms % 1000u) * 1000u);
    setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));

    if (connect(fd, res->ai_addr, res->ai_addrlen) < 0) {
        set_err(errbuf, errbuf_size, "connect: %s", strerror(errno));
        freeaddrinfo(res);
        close(fd);
        return NULL;
    }
    freeaddrinfo(res);

    /* Send request */
    StrBuf req = strbuf_new();
    strbuf_appendf(&req, "GET %s HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n\r\n", path, host);

    const char *tosend = strbuf_cstr(&req);
    size_t total = req.len;
    size_t sent = 0;
    while (sent < total) {
        ssize_t n = write(fd, tosend + sent, total - sent);
        if (n <= 0) {
            set_err(errbuf, errbuf_size, "write: %s", strerror(errno));
            strbuf_free(&req);
            close(fd);
            return NULL;
        }
        sent += (size_t)n;
    }
    strbuf_free(&req);

    /* Read response (cap at 1 MB to prevent OOM from rogue devices) */
    StrBuf resp = strbuf_new();
    char buf[4096];
    for (;;) {
        ssize_t n = read(fd, buf, sizeof(buf));
        if (n < 0) {
            set_err(errbuf, errbuf_size, "read: %s", strerror(errno));
            strbuf_free(&resp);
            close(fd);
            return NULL;
        }
        if (n == 0) break;
        strbuf_append(&resp, buf, (size_t)n);
        if (resp.len > 1024 * 1024) {
            set_err(errbuf, errbuf_size, "response too large");
            strbuf_free(&resp);
            close(fd);
            return NULL;
        }
    }
    close(fd);

    /* Parse status line */
    const char *data = strbuf_cstr(&resp);
    const char *eol = strstr(data, "\r\n");
    if (!eol) {
        set_err(errbuf, errbuf_size, "no status line");
        strbuf_free(&resp);
        return NULL;
    }

    /* "HTTP/1.1 200 OK" — extract status code */
    const char *sp = strchr(data, ' ');
    if (!sp || sp >= eol) {
        set_err(errbuf, errbuf_size, "invalid status line");
        strbuf_free(&resp);
        return NULL;
    }
    int status = atoi(sp + 1);
    if (status < 200 || status >= 300) {
        set_err(errbuf, errbuf_size, "HTTP %d", status);
        strbuf_free(&resp);
        return NULL;
    }

    /* Find end of headers */
    const char *body_start = strstr(data, "\r\n\r\n");
    if (!body_start) {
        set_err(errbuf, errbuf_size, "no header terminator");
        strbuf_free(&resp);
        return NULL;
    }
    body_start += 4;

    char *body = strdup(body_start);
    strbuf_free(&resp);

    if (!body) {
        set_err(errbuf, errbuf_size, "out of memory");
        return NULL;
    }

    return body;
}
