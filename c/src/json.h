#ifndef JSON_H
#define JSON_H

#include <stddef.h>
#include <stdint.h>

#include "strbuf.h"

typedef enum {
    JSON_NULL,
    JSON_BOOL,
    JSON_NUMBER,
    JSON_STRING,
    JSON_ARRAY,
    JSON_OBJECT
} JsonType;

typedef struct JsonValue JsonValue;
typedef struct {
    char      *key;
    JsonValue *value;
} JsonPair;

struct JsonValue {
    JsonType type;
    union {
        bool   boolean;
        double number;
        char  *string;
        struct { JsonValue **items; size_t count; size_t cap; } array;
        struct { JsonPair   *pairs; size_t count; size_t cap; } object;
    } u;
};

/* Constructors */
JsonValue *json_null(void);
JsonValue *json_number(double v);
JsonValue *json_from_i64(int64_t n);
JsonValue *json_string(const char *s);
JsonValue *json_array_new(void);
JsonValue *json_object_new(void);
void       json_free(JsonValue *v);

/* Builders */
void json_array_push(JsonValue *arr, JsonValue *item);
void json_object_set(JsonValue *obj, const char *key, JsonValue *val);

/* Accessors (borrow, don't free) */
const JsonValue *json_get(const JsonValue *obj, const char *key);
double           json_as_f64(const JsonValue *v, bool *ok);
int64_t          json_as_i64(const JsonValue *v, bool *ok);
const char      *json_as_str(const JsonValue *v);
bool             json_is_null(const JsonValue *v);
bool             json_is_object(const JsonValue *v);

/* Parser */
typedef struct {
    int    kind;   /* 0=ok, 1=eof, 2=unexpected_char, 3=invalid_escape, 4=invalid_number, 5=trailing */
    size_t pos;
} JsonError;

JsonValue *json_parse(const char *input, size_t len, JsonError *err);

/* Serializer */
void json_serialize(const JsonValue *v, StrBuf *out);

#endif /* JSON_H */
