#ifndef LOG_H
#define LOG_H

#include <stdio.h>
#include <time.h>

/* Write "[YYYY-MM-DD HH:MM:SS] " timestamp prefix to stderr. */
static inline void log_timestamp(void)
{
    time_t now = time(NULL);
    struct tm tm_buf;
    localtime_r(&now, &tm_buf);
    char buf[32];
    strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", &tm_buf);
    fprintf(stderr, "[%s] ", buf);
}

#endif /* LOG_H */
