#ifndef POLLER_H
#define POLLER_H

#include <stdatomic.h>
#include <stdint.h>

#include "config.h"
#include "json.h"

typedef enum {
    HEALTH_UNKNOWN,
    HEALTH_OK,
    HEALTH_ERROR
} HealthStatus;

typedef struct {
    const char   *ip;
    const char   *label;
    HealthStatus  status;
    int64_t       last_success;
    int64_t       last_error;
    char          last_error_message[256];
    uint32_t      consecutive_failures;
} DeviceHealth;

/* Forward decl — defined in app.h */
struct AppState;

/* Background polling thread entry point */
void *poller_run(void *arg);   /* arg = AppState* */

/* Get health as JSON array (caller frees). Reads under health rdlock. */
JsonValue *poller_get_health_json(struct AppState *state);

/* Serialize health directly to StrBuf (no JSON DOM). Reads under health rdlock. */
void poller_serialize_health_to(struct AppState *state, StrBuf *out);

/* Get poll success/failure counters. */
void poller_get_counters(uint64_t *successes, uint64_t *failures);

#endif /* POLLER_H */
