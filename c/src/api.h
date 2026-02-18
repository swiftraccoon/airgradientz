#ifndef API_H
#define API_H

#include "json.h"

struct AppState;

/* Query string parameter map (simple) */
typedef struct {
    char method[8];
    char path[256];
    char query[1024];
} HttpReq;

/* Each returns a JsonValue* that the caller frees, or NULL on error.
   On error, *status is set to 500; otherwise 200. */

JsonValue *api_handle_readings(struct AppState *state, const HttpReq *req, int *status);
JsonValue *api_handle_readings_count(struct AppState *state, const HttpReq *req, int *status);
JsonValue *api_handle_readings_latest(struct AppState *state, int *status);
JsonValue *api_handle_devices(struct AppState *state, int *status);
JsonValue *api_handle_health(struct AppState *state, int *status);
JsonValue *api_handle_config(const struct AppState *state, int *status);
JsonValue *api_handle_stats(struct AppState *state, int *status);

/* URL-decode a string. Returns heap-allocated result or NULL. Rejects null bytes. */
char *url_decode(const char *s, size_t len);

/* Parse a query param from raw query string. Returns heap-allocated value or NULL. */
char *query_param(const char *query, const char *name);

#endif /* API_H */
