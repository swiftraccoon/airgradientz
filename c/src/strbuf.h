#ifndef STRBUF_H
#define STRBUF_H

#include <stddef.h>

typedef struct {
    char  *data;
    size_t len;
    size_t cap;
} StrBuf;

StrBuf      strbuf_new(void);
void        strbuf_free(StrBuf *sb);
void        strbuf_append(StrBuf *sb, const char *s, size_t n);
void        strbuf_append_cstr(StrBuf *sb, const char *s);
void        strbuf_append_char(StrBuf *sb, char c);
void        strbuf_appendf(StrBuf *sb, const char *fmt, ...)
                __attribute__((format(printf, 2, 3)));
void        strbuf_clear(StrBuf *sb);
const char *strbuf_cstr(const StrBuf *sb);

#endif /* STRBUF_H */
