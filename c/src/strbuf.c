#include "strbuf.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void strbuf_grow(StrBuf *sb, size_t needed)
{
    if (sb->len + needed + 1 <= sb->cap) {
        return;
    }
    size_t new_cap = sb->cap ? sb->cap : 64;
    while (new_cap < sb->len + needed + 1) {
        new_cap *= 2;
    }
    char *p = realloc(sb->data, new_cap);
    if (!p) {
        fprintf(stderr, "strbuf: out of memory\n");
        abort();
    }
    sb->data = p;
    sb->cap = new_cap;
}

StrBuf strbuf_new(void)
{
    StrBuf sb;
    sb.data = NULL;
    sb.len = 0;
    sb.cap = 0;
    return sb;
}

StrBuf strbuf_new_with_cap(size_t cap)
{
    StrBuf sb;
    if (cap > 0) {
        sb.data = malloc(cap);
        if (!sb.data) {
            fprintf(stderr, "strbuf: out of memory\n");
            abort();
        }
        sb.data[0] = '\0';
        sb.cap = cap;
    } else {
        sb.data = NULL;
        sb.cap = 0;
    }
    sb.len = 0;
    return sb;
}

void strbuf_free(StrBuf *sb)
{
    free(sb->data);
    sb->data = NULL;
    sb->len = 0;
    sb->cap = 0;
}

void strbuf_append(StrBuf *sb, const char *s, size_t n)
{
    if (n == 0) return;
    strbuf_grow(sb, n);
    memcpy(sb->data + sb->len, s, n);
    sb->len += n;
    sb->data[sb->len] = '\0';
}

void strbuf_append_cstr(StrBuf *sb, const char *s)
{
    strbuf_append(sb, s, strlen(s));
}

void strbuf_appendf(StrBuf *sb, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    va_list ap2;
    va_copy(ap2, ap);
    int n = vsnprintf(NULL, 0, fmt, ap);
    va_end(ap);

    if (n < 0) {
        va_end(ap2);
        return;
    }

    strbuf_grow(sb, (size_t)n);
    vsnprintf(sb->data + sb->len, (size_t)n + 1, fmt, ap2);
    sb->len += (size_t)n;
    va_end(ap2);
}

void strbuf_clear(StrBuf *sb)
{
    sb->len = 0;
    if (sb->data) {
        sb->data[0] = '\0';
    }
}

const char *strbuf_cstr(const StrBuf *sb)
{
    return sb->data ? sb->data : "";
}
