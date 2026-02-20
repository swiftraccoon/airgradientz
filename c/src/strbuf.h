#ifndef STRBUF_H
#define STRBUF_H

#include <stddef.h>

typedef struct {
    char  *data;
    size_t len;
    size_t cap;
} StrBuf;

StrBuf      strbuf_new(void);
StrBuf      strbuf_new_with_cap(size_t cap);
void        strbuf_free(StrBuf *sb);
void        strbuf_append(StrBuf *sb, const char *s, size_t n);
void        strbuf_append_cstr(StrBuf *sb, const char *s);
void        strbuf_appendf(StrBuf *sb, const char *fmt, ...)
                __attribute__((format(printf, 2, 3)));
void        strbuf_clear(StrBuf *sb);
const char *strbuf_cstr(const StrBuf *sb);

/* Grow buffer to fit at least `needed` more bytes. Called by inline append_char. */
void        strbuf_grow(StrBuf *sb, size_t needed);

/* Inline single-char append — avoids function call overhead for delimiters. */
static inline void strbuf_append_char(StrBuf *sb, char c)
{
    if (sb->len + 2 > sb->cap) {
        strbuf_grow(sb, 1);
    }
    sb->data[sb->len] = c;
    sb->len++;
    sb->data[sb->len] = '\0';
}

#endif /* STRBUF_H */
