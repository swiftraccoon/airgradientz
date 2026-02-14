#ifndef POLLER_H
#define POLLER_H

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

#endif /* POLLER_H */
