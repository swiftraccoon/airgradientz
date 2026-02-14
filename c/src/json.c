#include "json.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- helpers ---- */

static void *xmalloc(size_t n)
{
    void *p = malloc(n);
    if (!p) { fprintf(stderr, "json: out of memory\n"); abort(); }
    return p;
}

static char *xstrdup(const char *s)
{
    size_t n = strlen(s) + 1;
    char *p = xmalloc(n);
    memcpy(p, s, n);
    return p;
}

/* ---- constructors ---- */

JsonValue *json_null(void)
{
    JsonValue *v = xmalloc(sizeof(*v));
    v->type = JSON_NULL;
    memset(&v->u, 0, sizeof(v->u));
    return v;
}

JsonValue *json_bool(bool b)
{
    JsonValue *v = xmalloc(sizeof(*v));
    v->type = JSON_BOOL;
    memset(&v->u, 0, sizeof(v->u));
    v->u.boolean = b;
    return v;
}

JsonValue *json_number(double n)
{
    JsonValue *v = xmalloc(sizeof(*v));
    v->type = JSON_NUMBER;
    memset(&v->u, 0, sizeof(v->u));
    v->u.number = n;
    return v;
}

JsonValue *json_from_i64(int64_t n)
{
    return json_number((double)n);
}

JsonValue *json_string(const char *s)
{
    JsonValue *v = xmalloc(sizeof(*v));
    v->type = JSON_STRING;
    memset(&v->u, 0, sizeof(v->u));
    v->u.string = xstrdup(s);
    return v;
}

JsonValue *json_array_new(void)
{
    JsonValue *v = xmalloc(sizeof(*v));
    v->type = JSON_ARRAY;
    memset(&v->u, 0, sizeof(v->u));
    return v;
}

JsonValue *json_object_new(void)
{
    JsonValue *v = xmalloc(sizeof(*v));
    v->type = JSON_OBJECT;
    memset(&v->u, 0, sizeof(v->u));
    return v;
}

void json_free(JsonValue *v)
{
    if (!v) return;
    switch (v->type) {
    case JSON_STRING:
        free(v->u.string);
        break;
    case JSON_ARRAY:
        for (size_t i = 0; i < v->u.array.count; i++) {
            json_free(v->u.array.items[i]);
        }
        free(v->u.array.items);
        break;
    case JSON_OBJECT:
        for (size_t i = 0; i < v->u.object.count; i++) {
            free(v->u.object.pairs[i].key);
            json_free(v->u.object.pairs[i].value);
        }
        free(v->u.object.pairs);
        break;
    default:
        break;
    }
    free(v);
}

/* ---- builders ---- */

void json_array_push(JsonValue *arr, JsonValue *item)
{
    if (arr->type != JSON_ARRAY) return;
    if (arr->u.array.count >= arr->u.array.cap) {
        size_t new_cap = arr->u.array.cap ? arr->u.array.cap * 2 : 8;
        JsonValue **p = realloc(arr->u.array.items, new_cap * sizeof(*p));
        if (!p) { fprintf(stderr, "json: out of memory\n"); abort(); }
        arr->u.array.items = p;
        arr->u.array.cap = new_cap;
    }
    arr->u.array.items[arr->u.array.count++] = item;
}

void json_object_set(JsonValue *obj, const char *key, JsonValue *val)
{
    if (obj->type != JSON_OBJECT) return;
    /* overwrite existing key */
    for (size_t i = 0; i < obj->u.object.count; i++) {
        if (strcmp(obj->u.object.pairs[i].key, key) == 0) {
            json_free(obj->u.object.pairs[i].value);
            obj->u.object.pairs[i].value = val;
            return;
        }
    }
    if (obj->u.object.count >= obj->u.object.cap) {
        size_t new_cap = obj->u.object.cap ? obj->u.object.cap * 2 : 8;
        JsonPair *p = realloc(obj->u.object.pairs, new_cap * sizeof(*p));
        if (!p) { fprintf(stderr, "json: out of memory\n"); abort(); }
        obj->u.object.pairs = p;
        obj->u.object.cap = new_cap;
    }
    JsonPair *pair = &obj->u.object.pairs[obj->u.object.count++];
    pair->key = xstrdup(key);
    pair->value = val;
}

/* ---- accessors ---- */

const JsonValue *json_get(const JsonValue *obj, const char *key)
{
    if (!obj || obj->type != JSON_OBJECT) return NULL;
    for (size_t i = 0; i < obj->u.object.count; i++) {
        if (strcmp(obj->u.object.pairs[i].key, key) == 0) {
            return obj->u.object.pairs[i].value;
        }
    }
    return NULL;
}

double json_as_f64(const JsonValue *v, bool *ok)
{
    if (v && v->type == JSON_NUMBER) {
        if (ok) *ok = true;
        return v->u.number;
    }
    if (ok) *ok = false;
    return 0.0;
}

int64_t json_as_i64(const JsonValue *v, bool *ok)
{
    if (v && v->type == JSON_NUMBER) {
        if (ok) *ok = true;
        return (int64_t)v->u.number;
    }
    if (ok) *ok = false;
    return 0;
}

const char *json_as_str(const JsonValue *v)
{
    if (v && v->type == JSON_STRING) return v->u.string;
    return NULL;
}

bool json_is_null(const JsonValue *v)
{
    return v && v->type == JSON_NULL;
}

bool json_is_object(const JsonValue *v)
{
    return v && v->type == JSON_OBJECT;
}

/* ---- serializer ---- */

static void serialize_string(const char *s, StrBuf *out)
{
    strbuf_append_char(out, '"');
    for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
        switch (*p) {
        case '"':  strbuf_append_cstr(out, "\\\""); break;
        case '\\': strbuf_append_cstr(out, "\\\\"); break;
        case '\n': strbuf_append_cstr(out, "\\n");  break;
        case '\r': strbuf_append_cstr(out, "\\r");  break;
        case '\t': strbuf_append_cstr(out, "\\t");  break;
        default:
            if (*p < 0x20) {
                strbuf_appendf(out, "\\u%04x", (unsigned)*p);
            } else {
                strbuf_append_char(out, (char)*p);
            }
            break;
        }
    }
    strbuf_append_char(out, '"');
}

void json_serialize(const JsonValue *v, StrBuf *out)
{
    if (!v) { strbuf_append_cstr(out, "null"); return; }

    switch (v->type) {
    case JSON_NULL:
        strbuf_append_cstr(out, "null");
        break;
    case JSON_BOOL:
        strbuf_append_cstr(out, v->u.boolean ? "true" : "false");
        break;
    case JSON_NUMBER: {
        double n = v->u.number;
        double intpart;
        if (modf(n, &intpart) == 0.0 && fabs(n) < 9.007199254740992e15) {
            strbuf_appendf(out, "%lld", (long long)(int64_t)n);
        } else {
            /* Shortest representation that roundtrips */
            char buf[64];
            char *endptr;
            int prec;
            for (prec = 1; prec <= 17; prec++) {
                snprintf(buf, sizeof(buf), "%.*g", prec, n);
                double check = strtod(buf, &endptr);
                if (check == n) break;
            }
            strbuf_append_cstr(out, buf);
        }
        break;
    }
    case JSON_STRING:
        serialize_string(v->u.string, out);
        break;
    case JSON_ARRAY:
        strbuf_append_char(out, '[');
        for (size_t i = 0; i < v->u.array.count; i++) {
            if (i > 0) strbuf_append_char(out, ',');
            json_serialize(v->u.array.items[i], out);
        }
        strbuf_append_char(out, ']');
        break;
    case JSON_OBJECT:
        strbuf_append_char(out, '{');
        for (size_t i = 0; i < v->u.object.count; i++) {
            if (i > 0) strbuf_append_char(out, ',');
            serialize_string(v->u.object.pairs[i].key, out);
            strbuf_append_char(out, ':');
            json_serialize(v->u.object.pairs[i].value, out);
        }
        strbuf_append_char(out, '}');
        break;
    }
}

/* ---- parser ---- */

typedef struct {
    const char *input;
    size_t      len;
    size_t      pos;
} Parser;

static void skip_ws(Parser *p)
{
    while (p->pos < p->len) {
        char c = p->input[p->pos];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            p->pos++;
        } else {
            break;
        }
    }
}

static JsonValue *parse_value(Parser *p, JsonError *err);

static bool peek(Parser *p, char *out)
{
    if (p->pos >= p->len) return false;
    *out = p->input[p->pos];
    return true;
}

static void set_error(JsonError *err, int kind, size_t pos)
{
    if (err) { err->kind = kind; err->pos = pos; }
}

static unsigned hex_digit(char c)
{
    if (c >= '0' && c <= '9') return (unsigned)(c - '0');
    if (c >= 'a' && c <= 'f') return (unsigned)(c - 'a' + 10);
    if (c >= 'A' && c <= 'F') return (unsigned)(c - 'A' + 10);
    return 0xFFu;
}

static bool parse_hex4(Parser *p, uint32_t *out, JsonError *err)
{
    if (p->pos + 4 > p->len) { set_error(err, 1, p->pos); return false; }
    uint32_t val = 0;
    for (int i = 0; i < 4; i++) {
        unsigned d = hex_digit(p->input[p->pos + (size_t)i]);
        if (d > 15) { set_error(err, 3, p->pos); return false; }
        val = (val << 4) | d;
    }
    p->pos += 4;
    *out = val;
    return true;
}

/* Encode a Unicode codepoint as UTF-8 into a StrBuf */
static void encode_utf8(StrBuf *sb, uint32_t cp)
{
    if (cp < 0x80) {
        strbuf_append_char(sb, (char)cp);
    } else if (cp < 0x800) {
        strbuf_append_char(sb, (char)(0xC0 | (cp >> 6)));
        strbuf_append_char(sb, (char)(0x80 | (cp & 0x3F)));
    } else if (cp < 0x10000) {
        strbuf_append_char(sb, (char)(0xE0 | (cp >> 12)));
        strbuf_append_char(sb, (char)(0x80 | ((cp >> 6) & 0x3F)));
        strbuf_append_char(sb, (char)(0x80 | (cp & 0x3F)));
    } else {
        strbuf_append_char(sb, (char)(0xF0 | (cp >> 18)));
        strbuf_append_char(sb, (char)(0x80 | ((cp >> 12) & 0x3F)));
        strbuf_append_char(sb, (char)(0x80 | ((cp >> 6) & 0x3F)));
        strbuf_append_char(sb, (char)(0x80 | (cp & 0x3F)));
    }
}

static size_t utf8_char_len(unsigned char b)
{
    if (b < 0x80) return 1;
    if (b < 0xE0) return 2;
    if (b < 0xF0) return 3;
    return 4;
}

static JsonValue *parse_string_value(Parser *p, JsonError *err)
{
    p->pos++; /* skip opening '"' */
    StrBuf sb = strbuf_new();

    for (;;) {
        if (p->pos >= p->len) {
            strbuf_free(&sb);
            set_error(err, 1, p->pos);
            return NULL;
        }
        unsigned char b = (unsigned char)p->input[p->pos];
        if (b == '"') {
            p->pos++;
            JsonValue *v = json_string(strbuf_cstr(&sb));
            strbuf_free(&sb);
            return v;
        }
        if (b == '\\') {
            p->pos++;
            if (p->pos >= p->len) {
                strbuf_free(&sb);
                set_error(err, 1, p->pos);
                return NULL;
            }
            char esc = p->input[p->pos];
            switch (esc) {
            case '"':  strbuf_append_char(&sb, '"');  p->pos++; break;
            case '\\': strbuf_append_char(&sb, '\\'); p->pos++; break;
            case '/':  strbuf_append_char(&sb, '/');  p->pos++; break;
            case 'n':  strbuf_append_char(&sb, '\n'); p->pos++; break;
            case 'r':  strbuf_append_char(&sb, '\r'); p->pos++; break;
            case 't':  strbuf_append_char(&sb, '\t'); p->pos++; break;
            case 'b':  strbuf_append_char(&sb, '\x08'); p->pos++; break;
            case 'f':  strbuf_append_char(&sb, '\x0C'); p->pos++; break;
            case 'u': {
                p->pos++;
                uint32_t cp;
                if (!parse_hex4(p, &cp, err)) { strbuf_free(&sb); return NULL; }
                if (cp >= 0xD800 && cp <= 0xDBFF) {
                    /* high surrogate, expect \uXXXX low surrogate */
                    if (p->pos + 1 < p->len &&
                        p->input[p->pos] == '\\' &&
                        p->input[p->pos + 1] == 'u') {
                        p->pos += 2;
                        uint32_t low;
                        if (!parse_hex4(p, &low, err)) { strbuf_free(&sb); return NULL; }
                        if (low >= 0xDC00 && low <= 0xDFFF) {
                            uint32_t combined = 0x10000 + ((cp - 0xD800) << 10) + (low - 0xDC00);
                            encode_utf8(&sb, combined);
                        } else {
                            strbuf_free(&sb);
                            set_error(err, 3, p->pos);
                            return NULL;
                        }
                    } else {
                        strbuf_free(&sb);
                        set_error(err, 3, p->pos);
                        return NULL;
                    }
                } else {
                    encode_utf8(&sb, cp);
                }
                break;
            }
            default:
                strbuf_free(&sb);
                set_error(err, 3, p->pos);
                return NULL;
            }
        } else {
            /* regular byte or multi-byte UTF-8 */
            size_t clen = utf8_char_len(b);
            if (p->pos + clen > p->len) {
                strbuf_free(&sb);
                set_error(err, 1, p->pos);
                return NULL;
            }
            strbuf_append(&sb, &p->input[p->pos], clen);
            p->pos += clen;
        }
    }
}

/* Parse a raw string (for object keys). Returns heap-allocated string or NULL. */
static char *parse_string_raw(Parser *p, JsonError *err)
{
    JsonValue *v = parse_string_value(p, err);
    if (!v) return NULL;
    char *s = v->u.string;
    v->u.string = NULL;
    free(v);
    return s;
}

static JsonValue *parse_number(Parser *p, JsonError *err)
{
    size_t start = p->pos;

    if (p->pos < p->len && p->input[p->pos] == '-') p->pos++;

    if (p->pos < p->len && p->input[p->pos] == '0') {
        p->pos++;
    } else if (p->pos < p->len && p->input[p->pos] >= '1' && p->input[p->pos] <= '9') {
        while (p->pos < p->len && p->input[p->pos] >= '0' && p->input[p->pos] <= '9') {
            p->pos++;
        }
    } else {
        set_error(err, 4, start);
        return NULL;
    }

    if (p->pos < p->len && p->input[p->pos] == '.') {
        p->pos++;
        if (p->pos >= p->len || p->input[p->pos] < '0' || p->input[p->pos] > '9') {
            set_error(err, 4, start);
            return NULL;
        }
        while (p->pos < p->len && p->input[p->pos] >= '0' && p->input[p->pos] <= '9') {
            p->pos++;
        }
    }

    if (p->pos < p->len && (p->input[p->pos] == 'e' || p->input[p->pos] == 'E')) {
        p->pos++;
        if (p->pos < p->len && (p->input[p->pos] == '+' || p->input[p->pos] == '-')) {
            p->pos++;
        }
        if (p->pos >= p->len || p->input[p->pos] < '0' || p->input[p->pos] > '9') {
            set_error(err, 4, start);
            return NULL;
        }
        while (p->pos < p->len && p->input[p->pos] >= '0' && p->input[p->pos] <= '9') {
            p->pos++;
        }
    }

    /* NUL-terminate the number substring for strtod */
    size_t nlen = p->pos - start;
    char buf[64];
    if (nlen >= sizeof(buf)) {
        set_error(err, 4, start);
        return NULL;
    }
    memcpy(buf, &p->input[start], nlen);
    buf[nlen] = '\0';

    char *endptr;
    double val = strtod(buf, &endptr);
    if (endptr != buf + nlen) {
        set_error(err, 4, start);
        return NULL;
    }

    return json_number(val);
}

static JsonValue *parse_object(Parser *p, JsonError *err)
{
    p->pos++; /* skip '{' */
    skip_ws(p);

    JsonValue *obj = json_object_new();

    char c;
    if (peek(p, &c) && c == '}') {
        p->pos++;
        return obj;
    }

    for (;;) {
        skip_ws(p);
        if (!peek(p, &c) || c != '"') {
            json_free(obj);
            set_error(err, p->pos < p->len ? 2 : 1, p->pos);
            return NULL;
        }

        char *key = parse_string_raw(p, err);
        if (!key) { json_free(obj); return NULL; }

        skip_ws(p);
        if (!peek(p, &c) || c != ':') {
            free(key);
            json_free(obj);
            set_error(err, p->pos < p->len ? 2 : 1, p->pos);
            return NULL;
        }
        p->pos++; /* skip ':' */

        JsonValue *val = parse_value(p, err);
        if (!val) { free(key); json_free(obj); return NULL; }

        /* Directly insert (key is already heap-allocated) */
        if (obj->u.object.count >= obj->u.object.cap) {
            size_t new_cap = obj->u.object.cap ? obj->u.object.cap * 2 : 8;
            JsonPair *pairs = realloc(obj->u.object.pairs, new_cap * sizeof(*pairs));
            if (!pairs) { free(key); json_free(val); json_free(obj); abort(); }
            obj->u.object.pairs = pairs;
            obj->u.object.cap = new_cap;
        }
        JsonPair *pair = &obj->u.object.pairs[obj->u.object.count++];
        pair->key = key;
        pair->value = val;

        skip_ws(p);
        if (!peek(p, &c)) {
            json_free(obj);
            set_error(err, 1, p->pos);
            return NULL;
        }
        if (c == ',') { p->pos++; continue; }
        if (c == '}') { p->pos++; return obj; }

        json_free(obj);
        set_error(err, 2, p->pos);
        return NULL;
    }
}

static JsonValue *parse_array(Parser *p, JsonError *err)
{
    p->pos++; /* skip '[' */
    skip_ws(p);

    JsonValue *arr = json_array_new();

    char c;
    if (peek(p, &c) && c == ']') {
        p->pos++;
        return arr;
    }

    for (;;) {
        JsonValue *item = parse_value(p, err);
        if (!item) { json_free(arr); return NULL; }
        json_array_push(arr, item);

        skip_ws(p);
        if (!peek(p, &c)) {
            json_free(arr);
            set_error(err, 1, p->pos);
            return NULL;
        }
        if (c == ',') { p->pos++; continue; }
        if (c == ']') { p->pos++; return arr; }

        json_free(arr);
        set_error(err, 2, p->pos);
        return NULL;
    }
}

static JsonValue *parse_value(Parser *p, JsonError *err)
{
    skip_ws(p);
    char c;
    if (!peek(p, &c)) {
        set_error(err, 1, p->pos);
        return NULL;
    }

    switch (c) {
    case '"': return parse_string_value(p, err);
    case '{': return parse_object(p, err);
    case '[': return parse_array(p, err);
    case 't':
        if (p->pos + 4 <= p->len && memcmp(&p->input[p->pos], "true", 4) == 0) {
            p->pos += 4;
            return json_bool(true);
        }
        set_error(err, 2, p->pos);
        return NULL;
    case 'f':
        if (p->pos + 5 <= p->len && memcmp(&p->input[p->pos], "false", 5) == 0) {
            p->pos += 5;
            return json_bool(false);
        }
        set_error(err, 2, p->pos);
        return NULL;
    case 'n':
        if (p->pos + 4 <= p->len && memcmp(&p->input[p->pos], "null", 4) == 0) {
            p->pos += 4;
            return json_null();
        }
        set_error(err, 2, p->pos);
        return NULL;
    case '-':
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        return parse_number(p, err);
    default:
        set_error(err, 2, p->pos);
        return NULL;
    }
}

JsonValue *json_parse(const char *input, size_t len, JsonError *err)
{
    JsonError local_err = {0, 0};
    if (!err) err = &local_err;
    err->kind = 0;
    err->pos = 0;

    Parser p = { input, len, 0 };
    JsonValue *v = parse_value(&p, err);
    if (!v) return NULL;

    skip_ws(&p);
    if (p.pos < p.len) {
        json_free(v);
        set_error(err, 5, p.pos);
        return NULL;
    }

    return v;
}
