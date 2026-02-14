#ifndef HTTP_SERVER_H
#define HTTP_SERVER_H

struct AppState;

/* Blocks forever, accepting connections. Returns -1 on fatal bind/listen error. */
int http_server_run(struct AppState *state);

#endif /* HTTP_SERVER_H */
