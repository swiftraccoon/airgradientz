#ifndef HTTP_CLIENT_H
#define HTTP_CLIENT_H

#include <stdint.h>
#include <stddef.h>

/* HTTP GET to ip:80/path. Returns heap-allocated body on success, NULL on error.
   errbuf receives a human-readable error message on failure. */
char *http_get(const char *ip, const char *path, uint32_t timeout_ms,
               char *errbuf, size_t errbuf_size);

#endif /* HTTP_CLIENT_H */
