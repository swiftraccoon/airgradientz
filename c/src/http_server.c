#include "http_server.h"
#include "app.h"
#include "api.h"
#include "json.h"
#include "strbuf.h"

#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <unistd.h>

#define MAX_EVENTS    64
#define MAX_CONNS     4096
#define READ_BUF_SIZE 8192

typedef enum { CONN_EMPTY, CONN_READING, CONN_WRITING } ConnPhase;

typedef struct {
    ConnPhase  phase;
    char       read_buf[READ_BUF_SIZE];
    size_t     read_pos;
    char      *response;      /* heap-allocated: headers + body */
    size_t     response_len;
    size_t     write_pos;
} ConnData;

static ConnData conn_table[MAX_CONNS];

/* ---- helpers ---- */

static int set_nonblocking(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) return -1;
    return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

static void conn_reset(int fd)
{
    if (fd >= 0 && fd < MAX_CONNS) {
        free(conn_table[fd].response);
        memset(&conn_table[fd], 0, sizeof(ConnData));
    }
}

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

/* ---- response builders ---- */

static char *build_response(int status, const char *status_text,
                             const char *content_type, const char *body,
                             size_t body_len, const char *extra_headers,
                             size_t *out_len)
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

    size_t total = hdr.len + body_len;
    char *buf = malloc(total);
    if (!buf) {
        strbuf_free(&hdr);
        *out_len = 0;
        return NULL;
    }

    memcpy(buf, strbuf_cstr(&hdr), hdr.len);
    if (body_len > 0) {
        memcpy(buf + hdr.len, body, body_len);
    }
    strbuf_free(&hdr);

    *out_len = total;
    return buf;
}

static char *build_json_response(int status, const char *status_text,
                                  JsonValue *json, size_t *out_len)
{
    StrBuf body = strbuf_new();
    json_serialize(json, &body);
    char *resp = build_response(status, status_text,
                                "application/json", strbuf_cstr(&body),
                                body.len, NULL, out_len);
    strbuf_free(&body);
    return resp;
}

static char *build_error_response(int status, const char *status_text,
                                   const char *msg, size_t *out_len)
{
    StrBuf body = strbuf_new();
    strbuf_appendf(&body, "{\"error\":\"%s\"}", msg);
    char *resp = build_response(status, status_text,
                                "application/json", strbuf_cstr(&body),
                                body.len, NULL, out_len);
    strbuf_free(&body);
    return resp;
}

/* ---- static file serving ---- */

static char *build_static_response(const char *req_path, size_t *out_len)
{
    if (strstr(req_path, "..")) {
        return build_error_response(404, "Not Found", "Not found", out_len);
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
        return build_error_response(404, "Not Found", "Not found", out_len);
    }

    struct stat st;
    if (fstat(file_fd, &st) < 0 || !S_ISREG(st.st_mode)) {
        close(file_fd);
        return build_error_response(404, "Not Found", "Not found", out_len);
    }

    size_t file_size = (size_t)st.st_size;
    char *file_buf = malloc(file_size);
    if (!file_buf) {
        close(file_fd);
        return build_error_response(500, "Internal Server Error",
                                     "Internal server error", out_len);
    }

    size_t total_read = 0;
    while (total_read < file_size) {
        ssize_t n = read(file_fd, file_buf + total_read, file_size - total_read);
        if (n <= 0) break;
        total_read += (size_t)n;
    }
    close(file_fd);

    const char *ct = content_type_for(filepath);
    char *resp = build_response(200, "OK", ct, file_buf, total_read,
                                "Cache-Control: public, max-age=600\r\n", out_len);
    free(file_buf);
    return resp;
}

/* ---- request parsing ---- */

static int parse_request_buf(const char *buf, size_t len, HttpReq *req)
{
    memset(req, 0, sizeof(*req));

    /* Find end of request line (\r\n) */
    const char *line_end = NULL;
    for (size_t j = 0; j + 1 < len; j++) {
        if (buf[j] == '\r' && buf[j + 1] == '\n') {
            line_end = buf + j;
            break;
        }
    }
    if (!line_end) return -1;

    size_t line_len = (size_t)(line_end - buf);

    /* Parse request line: METHOD /path?query HTTP/1.x */
    const char *sp1 = memchr(buf, ' ', line_len);
    if (!sp1) return -1;

    size_t method_len = (size_t)(sp1 - buf);
    if (method_len >= sizeof(req->method)) method_len = sizeof(req->method) - 1;
    memcpy(req->method, buf, method_len);
    req->method[method_len] = '\0';

    const char *uri_start = sp1 + 1;
    size_t remaining = (size_t)(line_end - uri_start);
    const char *sp2 = memchr(uri_start, ' ', remaining);
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

/* ---- routing ---- */

static void route_request(AppState *state, HttpReq *req,
                           char **resp, size_t *resp_len)
{
    if (strcmp(req->method, "GET") != 0) {
        *resp = build_error_response(405, "Method Not Allowed",
                                      "Method not allowed", resp_len);
        return;
    }

    if (strcmp(req->path, "/api/readings") == 0) {
        int status;
        JsonValue *json = api_handle_readings(state, req, &status);
        if (json) {
            *resp = build_json_response(status, "OK", json, resp_len);
            json_free(json);
        } else {
            *resp = build_error_response(500, "Internal Server Error",
                                          "Internal server error", resp_len);
        }
    } else if (strcmp(req->path, "/api/readings/latest") == 0) {
        int status;
        JsonValue *json = api_handle_readings_latest(state, &status);
        if (json) {
            *resp = build_json_response(status, "OK", json, resp_len);
            json_free(json);
        } else {
            *resp = build_error_response(500, "Internal Server Error",
                                          "Internal server error", resp_len);
        }
    } else if (strcmp(req->path, "/api/devices") == 0) {
        int status;
        JsonValue *json = api_handle_devices(state, &status);
        if (json) {
            *resp = build_json_response(status, "OK", json, resp_len);
            json_free(json);
        } else {
            *resp = build_error_response(500, "Internal Server Error",
                                          "Internal server error", resp_len);
        }
    } else if (strcmp(req->path, "/api/health") == 0) {
        int status;
        JsonValue *json = api_handle_health(state, &status);
        *resp = build_json_response(status, "OK", json, resp_len);
        json_free(json);
    } else if (strcmp(req->path, "/api/config") == 0) {
        int status;
        JsonValue *json = api_handle_config(state, &status);
        *resp = build_json_response(status, "OK", json, resp_len);
        json_free(json);
    } else if (strcmp(req->path, "/api/stats") == 0) {
        int status;
        JsonValue *json = api_handle_stats(state, &status);
        *resp = build_json_response(status, "OK", json, resp_len);
        json_free(json);
    } else {
        *resp = build_static_response(req->path, resp_len);
    }
}

static int has_header_end(const char *buf, size_t len)
{
    for (size_t i = 0; i + 3 < len; i++) {
        if (buf[i] == '\r' && buf[i+1] == '\n' &&
            buf[i+2] == '\r' && buf[i+3] == '\n')
            return 1;
    }
    return 0;
}

/* ---- epoll event loop ---- */

static void close_conn(int epoll_fd, int fd, AppState *state)
{
    epoll_ctl(epoll_fd, EPOLL_CTL_DEL, fd, NULL);
    conn_reset(fd);
    close(fd);
    atomic_fetch_sub(&state->active_connections, 1);
}

int http_server_run(struct AppState *state)
{
    /* Ignore SIGPIPE */
    signal(SIGPIPE, SIG_IGN);

    /* Zero connection table */
    memset(conn_table, 0, sizeof(conn_table));

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

    if (set_nonblocking(listen_fd) < 0) {
        fprintf(stderr, "[server] set_nonblocking(listen_fd): %s\n", strerror(errno));
        close(listen_fd);
        return -1;
    }

    int epoll_fd = epoll_create1(EPOLL_CLOEXEC);
    if (epoll_fd < 0) {
        fprintf(stderr, "[server] epoll_create1: %s\n", strerror(errno));
        close(listen_fd);
        return -1;
    }

    struct epoll_event ev;
    ev.events = EPOLLIN | EPOLLET;
    ev.data.fd = listen_fd;
    if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, listen_fd, &ev) < 0) {
        fprintf(stderr, "[server] epoll_ctl(listen): %s\n", strerror(errno));
        close(epoll_fd);
        close(listen_fd);
        return -1;
    }

    fprintf(stderr, "[server] Listening on http://localhost:%u\n", state->config.port);

    struct epoll_event events[MAX_EVENTS];

    for (;;) {
        if (atomic_load(&state->shutdown)) break;

        int nfds = epoll_wait(epoll_fd, events, MAX_EVENTS, 1000);
        if (nfds < 0) {
            if (errno == EINTR) continue;
            fprintf(stderr, "[server] epoll_wait: %s\n", strerror(errno));
            break;
        }

        for (int i = 0; i < nfds; i++) {
            int fd = events[i].data.fd;
            uint32_t ev_flags = events[i].events;

            /* ---- listen socket: accept loop ---- */
            if (fd == listen_fd) {
                for (;;) {
                    int client_fd = accept(listen_fd, NULL, NULL);
                    if (client_fd < 0) {
                        if (errno == EAGAIN || errno == EWOULDBLOCK) break;
                        fprintf(stderr, "[server] accept: %s\n", strerror(errno));
                        break;
                    }

                    if (client_fd >= MAX_CONNS) {
                        fprintf(stderr, "[server] fd %d >= MAX_CONNS, rejecting\n",
                                client_fd);
                        close(client_fd);
                        continue;
                    }

                    if (set_nonblocking(client_fd) < 0) {
                        fprintf(stderr, "[server] set_nonblocking(client): %s\n",
                                strerror(errno));
                        close(client_fd);
                        continue;
                    }

                    conn_reset(client_fd);
                    conn_table[client_fd].phase = CONN_READING;

                    struct epoll_event cev;
                    cev.events = EPOLLIN | EPOLLET;
                    cev.data.fd = client_fd;
                    if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, client_fd, &cev) < 0) {
                        fprintf(stderr, "[server] epoll_ctl(add client): %s\n",
                                strerror(errno));
                        conn_reset(client_fd);
                        close(client_fd);
                        continue;
                    }

                    atomic_fetch_add(&state->active_connections, 1);
                }
                continue;
            }

            /* ---- error / hangup ---- */
            if (ev_flags & (EPOLLERR | EPOLLHUP)) {
                close_conn(epoll_fd, fd, state);
                continue;
            }

            /* ---- client readable (ET) ---- */
            if ((ev_flags & EPOLLIN) && fd < MAX_CONNS &&
                conn_table[fd].phase == CONN_READING)
            {
                ConnData *c = &conn_table[fd];
                int do_close = 0;

                for (;;) {
                    size_t space = READ_BUF_SIZE - c->read_pos;
                    if (space == 0) {
                        /* Request too large */
                        do_close = 1;
                        break;
                    }

                    ssize_t n = read(fd, c->read_buf + c->read_pos, space);
                    if (n > 0) {
                        c->read_pos += (size_t)n;
                        continue;
                    }
                    if (n == 0) {
                        /* EOF */
                        do_close = 1;
                        break;
                    }
                    /* n < 0 */
                    if (errno == EAGAIN || errno == EWOULDBLOCK) {
                        break;
                    }
                    /* Real error */
                    do_close = 1;
                    break;
                }

                if (do_close && c->read_pos == 0) {
                    close_conn(epoll_fd, fd, state);
                    continue;
                }

                /* Wait for full HTTP header before parsing */
                if (!has_header_end(c->read_buf, c->read_pos)) {
                    if (do_close) {
                        close_conn(epoll_fd, fd, state);
                    }
                    continue;
                }

                /* Try to parse the request */
                HttpReq req;
                if (parse_request_buf(c->read_buf, c->read_pos, &req) == 0) {
                    atomic_fetch_add(&state->requests_served, 1);

                    route_request((AppState *)state, &req,
                                  &c->response, &c->response_len);
                    if (!c->response) {
                        close_conn(epoll_fd, fd, state);
                        continue;
                    }
                    c->write_pos = 0;
                    c->phase = CONN_WRITING;

                    struct epoll_event wev;
                    wev.events = EPOLLOUT | EPOLLET;
                    wev.data.fd = fd;
                    if (epoll_ctl(epoll_fd, EPOLL_CTL_MOD, fd, &wev) < 0) {
                        close_conn(epoll_fd, fd, state);
                        continue;
                    }
                } else if (do_close) {
                    /* Got EOF/error but couldn't parse — drop connection */
                    close_conn(epoll_fd, fd, state);
                }
                /* else: incomplete request, keep reading */
                continue;
            }

            /* ---- client writable (ET) ---- */
            if ((ev_flags & EPOLLOUT) && fd < MAX_CONNS &&
                conn_table[fd].phase == CONN_WRITING)
            {
                ConnData *c = &conn_table[fd];

                for (;;) {
                    size_t remaining = c->response_len - c->write_pos;
                    if (remaining == 0) {
                        /* All written */
                        close_conn(epoll_fd, fd, state);
                        break;
                    }

                    ssize_t n = write(fd, c->response + c->write_pos, remaining);
                    if (n > 0) {
                        c->write_pos += (size_t)n;
                        continue;
                    }
                    if (n < 0) {
                        if (errno == EAGAIN || errno == EWOULDBLOCK) {
                            break; /* Wait for next EPOLLOUT */
                        }
                        /* Write error */
                        close_conn(epoll_fd, fd, state);
                        break;
                    }
                    /* n == 0: unusual for write, treat as done */
                    break;
                }
                continue;
            }
        }
    }

    /* Cleanup: close all active connections */
    for (int fd = 0; fd < MAX_CONNS; fd++) {
        if (conn_table[fd].phase != CONN_EMPTY) {
            close(fd);
            conn_reset(fd);
        }
    }

    close(epoll_fd);
    close(listen_fd);
    return 0;
}
