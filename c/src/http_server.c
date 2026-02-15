#include "http_server.h"
#include "app.h"
#include "api.h"
#include "json.h"
#include "strbuf.h"

#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#define MAX_REQUEST_BYTES 8192

/* ---- content type ---- */

static const char *content_type_for(const char *path)
{
    const char *dot = strrchr(path, '.');
    if (!dot) return "application/octet-stream";
    if (strcmp(dot, ".html") == 0) return "text/html; charset=utf-8";
    if (strcmp(dot, ".css") == 0)  return "text/css; charset=utf-8";
    if (strcmp(dot, ".js") == 0)   return "application/javascript; charset=utf-8";
    if (strcmp(dot, ".json") == 0) return "application/json; charset=utf-8";
    if (strcmp(dot, ".png") == 0)  return "image/png";
    if (strcmp(dot, ".jpg") == 0 || strcmp(dot, ".jpeg") == 0) return "image/jpeg";
    if (strcmp(dot, ".svg") == 0)  return "image/svg+xml";
    if (strcmp(dot, ".ico") == 0)  return "image/x-icon";
    return "application/octet-stream";
}

/* ---- response writers ---- */

static void write_all(int fd, const char *buf, size_t len)
{
    size_t sent = 0;
    while (sent < len) {
        ssize_t n = write(fd, buf + sent, len - sent);
        if (n <= 0) return;
        sent += (size_t)n;
    }
}

static void send_response(int fd, int status, const char *status_text,
                           const char *content_type, const char *body, size_t body_len,
                           const char *extra_headers)
{
    StrBuf hdr = strbuf_new();
    strbuf_appendf(&hdr, "HTTP/1.1 %d %s\r\n", status, status_text);
    strbuf_append_cstr(&hdr, "Connection: close\r\n");
    strbuf_append_cstr(&hdr, "X-Content-Type-Options: nosniff\r\n");
    strbuf_append_cstr(&hdr, "X-Frame-Options: DENY\r\n");
    strbuf_appendf(&hdr, "Content-Type: %s\r\n", content_type);
    strbuf_appendf(&hdr, "Content-Length: %zu\r\n", body_len);
    if (extra_headers) {
        strbuf_append_cstr(&hdr, extra_headers);
    }
    strbuf_append_cstr(&hdr, "\r\n");

    write_all(fd, strbuf_cstr(&hdr), hdr.len);
    strbuf_free(&hdr);

    if (body_len > 0) {
        write_all(fd, body, body_len);
    }
}

static void send_json_response(int fd, int status, const char *status_text, JsonValue *json)
{
    StrBuf body = strbuf_new();
    json_serialize(json, &body);
    send_response(fd, status, status_text,
                  "application/json", strbuf_cstr(&body), body.len, NULL);
    strbuf_free(&body);
}

static void send_error(int fd, int status, const char *status_text, const char *msg)
{
    StrBuf body = strbuf_new();
    strbuf_appendf(&body, "{\"error\":\"%s\"}", msg);
    send_response(fd, status, status_text,
                  "application/json", strbuf_cstr(&body), body.len, NULL);
    strbuf_free(&body);
}

/* ---- static file serving ---- */

static void serve_static(int fd, const char *req_path)
{
    if (strstr(req_path, "..")) {
        send_error(fd, 404, "Not Found", "Not found");
        return;
    }

    /* Strip leading slash */
    const char *relative = req_path;
    while (*relative == '/') relative++;

    char filepath[512];
    if (relative[0] == '\0') {
        snprintf(filepath, sizeof(filepath), "public/index.html");
    } else {
        snprintf(filepath, sizeof(filepath), "public/%s", relative);
    }

    int file_fd = open(filepath, O_RDONLY);
    if (file_fd < 0) {
        send_error(fd, 404, "Not Found", "Not found");
        return;
    }

    struct stat st;
    if (fstat(file_fd, &st) < 0 || !S_ISREG(st.st_mode)) {
        close(file_fd);
        send_error(fd, 404, "Not Found", "Not found");
        return;
    }

    size_t file_size = (size_t)st.st_size;
    char *buf = malloc(file_size);
    if (!buf) {
        close(file_fd);
        send_error(fd, 500, "Internal Server Error", "Internal server error");
        return;
    }

    size_t total_read = 0;
    while (total_read < file_size) {
        ssize_t n = read(file_fd, buf + total_read, file_size - total_read);
        if (n <= 0) break;
        total_read += (size_t)n;
    }
    close(file_fd);

    const char *ct = content_type_for(filepath);
    send_response(fd, 200, "OK", ct, buf, total_read,
                  "Cache-Control: public, max-age=600\r\n");
    free(buf);
}

/* ---- request parsing ---- */

static int parse_request(int fd, HttpReq *req)
{
    memset(req, 0, sizeof(*req));

    /* Set read timeout */
    struct timeval tv;
    tv.tv_sec = 10;
    tv.tv_usec = 0;
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));

    char buf[MAX_REQUEST_BYTES];
    ssize_t n = read(fd, buf, sizeof(buf) - 1);
    if (n <= 0) return -1;
    buf[n] = '\0';

    /* Parse request line: METHOD /path?query HTTP/1.x */
    char *sp1 = strchr(buf, ' ');
    if (!sp1) return -1;

    size_t method_len = (size_t)(sp1 - buf);
    if (method_len >= sizeof(req->method)) method_len = sizeof(req->method) - 1;
    memcpy(req->method, buf, method_len);
    req->method[method_len] = '\0';

    char *uri_start = sp1 + 1;
    char *sp2 = strchr(uri_start, ' ');
    if (!sp2) return -1;

    size_t uri_len = (size_t)(sp2 - uri_start);
    char uri[2048];
    if (uri_len >= sizeof(uri)) uri_len = sizeof(uri) - 1;
    memcpy(uri, uri_start, uri_len);
    uri[uri_len] = '\0';

    /* Split path and query */
    char *qmark = strchr(uri, '?');
    if (qmark) {
        size_t path_len = (size_t)(qmark - uri);
        if (path_len >= sizeof(req->path)) path_len = sizeof(req->path) - 1;
        memcpy(req->path, uri, path_len);
        req->path[path_len] = '\0';

        const char *qs = qmark + 1;
        size_t qs_len = strlen(qs);
        if (qs_len >= sizeof(req->query)) qs_len = sizeof(req->query) - 1;
        memcpy(req->query, qs, qs_len);
        req->query[qs_len] = '\0';
    } else {
        size_t path_len = uri_len;
        if (path_len >= sizeof(req->path)) path_len = sizeof(req->path) - 1;
        memcpy(req->path, uri, path_len);
        req->path[path_len] = '\0';
    }

    return 0;
}

/* ---- connection handler ---- */

typedef struct {
    int       fd;
    AppState *state;
} ConnArg;

static void *handle_connection(void *arg)
{
    ConnArg *ca = (ConnArg *)arg;
    int fd = ca->fd;
    AppState *state = ca->state;
    free(ca);

    atomic_fetch_add(&state->active_connections, 1);

    HttpReq req;
    if (parse_request(fd, &req) < 0) {
        atomic_fetch_sub(&state->active_connections, 1);
        close(fd);
        return NULL;
    }

    atomic_fetch_add(&state->requests_served, 1);

    if (strcmp(req.method, "GET") != 0) {
        send_error(fd, 405, "Method Not Allowed", "Method not allowed");
        atomic_fetch_sub(&state->active_connections, 1);
        close(fd);
        return NULL;
    }

    /* Route */
    if (strcmp(req.path, "/api/readings") == 0) {
        int status;
        JsonValue *json = api_handle_readings(state, &req, &status);
        if (json) {
            send_json_response(fd, status, "OK", json);
            json_free(json);
        } else {
            send_error(fd, 500, "Internal Server Error", "Internal server error");
        }
    } else if (strcmp(req.path, "/api/readings/latest") == 0) {
        int status;
        JsonValue *json = api_handle_readings_latest(state, &status);
        if (json) {
            send_json_response(fd, status, "OK", json);
            json_free(json);
        } else {
            send_error(fd, 500, "Internal Server Error", "Internal server error");
        }
    } else if (strcmp(req.path, "/api/devices") == 0) {
        int status;
        JsonValue *json = api_handle_devices(state, &status);
        if (json) {
            send_json_response(fd, status, "OK", json);
            json_free(json);
        } else {
            send_error(fd, 500, "Internal Server Error", "Internal server error");
        }
    } else if (strcmp(req.path, "/api/health") == 0) {
        int status;
        JsonValue *json = api_handle_health(state, &status);
        send_json_response(fd, status, "OK", json);
        json_free(json);
    } else if (strcmp(req.path, "/api/config") == 0) {
        int status;
        JsonValue *json = api_handle_config(state, &status);
        send_json_response(fd, status, "OK", json);
        json_free(json);
    } else if (strcmp(req.path, "/api/stats") == 0) {
        int status;
        JsonValue *json = api_handle_stats(state, &status);
        send_json_response(fd, status, "OK", json);
        json_free(json);
    } else {
        serve_static(fd, req.path);
    }

    atomic_fetch_sub(&state->active_connections, 1);
    close(fd);
    return NULL;
}

/* ---- server main loop ---- */

int http_server_run(struct AppState *state)
{
    int listen_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (listen_fd < 0) {
        fprintf(stderr, "[server] socket: %s\n", strerror(errno));
        return -1;
    }

    int opt = 1;
    setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(state->config.port);

    if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        fprintf(stderr, "[server] bind: %s\n", strerror(errno));
        close(listen_fd);
        return -1;
    }

    if (listen(listen_fd, 128) < 0) {
        fprintf(stderr, "[server] listen: %s\n", strerror(errno));
        close(listen_fd);
        return -1;
    }

    fprintf(stderr, "[server] Listening on http://localhost:%u\n", state->config.port);

    for (;;) {
        if (atomic_load(&state->shutdown)) break;

        int client_fd = accept(listen_fd, NULL, NULL);
        if (client_fd < 0) {
            if (errno == EINTR) continue;
            fprintf(stderr, "[server] accept: %s\n", strerror(errno));
            continue;
        }

        ConnArg *ca = malloc(sizeof(*ca));
        if (!ca) {
            close(client_fd);
            continue;
        }
        ca->fd = client_fd;
        ca->state = (AppState *)state;

        pthread_t tid;
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        if (pthread_create(&tid, &attr, handle_connection, ca) != 0) {
            fprintf(stderr, "[server] pthread_create: %s\n", strerror(errno));
            close(client_fd);
            free(ca);
        }
        pthread_attr_destroy(&attr);
    }

    close(listen_fd);
    return 0;
}
