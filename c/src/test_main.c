#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqlite3.h"
#include "strbuf.h"
#include "json.h"
#include "db.h"
#include "api.h"

/* ================================================================
 * Test framework
 * ================================================================ */

static int g_tests_run;
static int g_tests_passed;
static int g_tests_failed;
static int g_current_failed;

#define RUN_TEST(fn)                                                     \
    do {                                                                 \
        g_tests_run++;                                                   \
        g_current_failed = 0;                                            \
        fn();                                                            \
        if (g_current_failed) {                                          \
            g_tests_failed++;                                            \
            fprintf(stderr, "  FAIL  %s\n", #fn);                       \
        } else {                                                         \
            g_tests_passed++;                                            \
            fprintf(stderr, "  pass  %s\n", #fn);                       \
        }                                                                \
    } while (0)

#define ASSERT(cond)                                                     \
    do {                                                                 \
        if (!(cond)) {                                                   \
            fprintf(stderr, "    ASSERT FAILED: %s:%d: %s\n",           \
                    __FILE__, __LINE__, #cond);                          \
            g_current_failed = 1;                                        \
            return;                                                      \
        }                                                                \
    } while (0)

#define ASSERT_STR_EQ(a, b)                                              \
    do {                                                                 \
        const char *_a = (a);                                            \
        const char *_b = (b);                                            \
        if (!_a || !_b || strcmp(_a, _b) != 0) {                         \
            fprintf(stderr, "    ASSERT_STR_EQ FAILED: %s:%d\n"         \
                    "      got:      \"%s\"\n"                           \
                    "      expected: \"%s\"\n",                          \
                    __FILE__, __LINE__,                                  \
                    _a ? _a : "(null)", _b ? _b : "(null)");             \
            g_current_failed = 1;                                        \
            return;                                                      \
        }                                                                \
    } while (0)

#define ASSERT_INT_EQ(a, b)                                              \
    do {                                                                 \
        long long _a = (long long)(a);                                   \
        long long _b = (long long)(b);                                   \
        if (_a != _b) {                                                  \
            fprintf(stderr, "    ASSERT_INT_EQ FAILED: %s:%d\n"         \
                    "      got:      %lld\n"                             \
                    "      expected: %lld\n",                            \
                    __FILE__, __LINE__, _a, _b);                         \
            g_current_failed = 1;                                        \
            return;                                                      \
        }                                                                \
    } while (0)

#define ASSERT_DBL_EQ(a, b)                                              \
    do {                                                                 \
        double _a = (double)(a);                                         \
        double _b = (double)(b);                                         \
        if (_a != _b) {                                                  \
            fprintf(stderr, "    ASSERT_DBL_EQ FAILED: %s:%d\n"         \
                    "      got:      %.17g\n"                            \
                    "      expected: %.17g\n",                           \
                    __FILE__, __LINE__, _a, _b);                         \
            g_current_failed = 1;                                        \
            return;                                                      \
        }                                                                \
    } while (0)

#define ASSERT_NULL(p)                                                   \
    do {                                                                 \
        if ((p) != NULL) {                                               \
            fprintf(stderr, "    ASSERT_NULL FAILED: %s:%d: %s\n",      \
                    __FILE__, __LINE__, #p);                             \
            g_current_failed = 1;                                        \
            return;                                                      \
        }                                                                \
    } while (0)

#define ASSERT_NOT_NULL(p)                                               \
    do {                                                                 \
        if ((p) == NULL) {                                               \
            fprintf(stderr, "    ASSERT_NOT_NULL FAILED: %s:%d: %s\n",  \
                    __FILE__, __LINE__, #p);                             \
            g_current_failed = 1;                                        \
            return;                                                      \
        }                                                                \
    } while (0)

/* Helper: parse JSON from string literal, abort test on failure */
static JsonValue *must_parse(const char *input)
{
    JsonError err;
    JsonValue *v = json_parse(input, strlen(input), &err);
    if (!v) {
        fprintf(stderr, "    json_parse failed at pos %zu (kind %d) for: %s\n",
                err.pos, err.kind, input);
    }
    return v;
}

/* Helper: serialize a JsonValue to a heap-allocated string */
static char *serialize(const JsonValue *v)
{
    StrBuf sb = strbuf_new();
    json_serialize(v, &sb);
    char *s = strdup(strbuf_cstr(&sb));
    strbuf_free(&sb);
    return s;
}

/* Helper: read an entire file into a heap-allocated string */
static char *read_file(const char *path)
{
    FILE *f = fopen(path, "r");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    if (len <= 0) { fclose(f); return NULL; }
    fseek(f, 0, SEEK_SET);
    char *buf = (char *)malloc((size_t)len + 1);
    if (!buf) { fclose(f); return NULL; }
    size_t n = fread(buf, 1, (size_t)len, f);
    fclose(f);
    buf[n] = '\0';
    return buf;
}

/* Helper: load a named fixture from ../test-fixtures.json */
static JsonValue *load_fixture(const char *name)
{
    char *content = read_file("../test-fixtures.json");
    if (!content) return NULL;
    JsonError err;
    JsonValue *root = json_parse(content, strlen(content), &err);
    free(content);
    if (!root) return NULL;
    const JsonValue *fixture = json_get(root, name);
    if (!fixture) { json_free(root); return NULL; }
    /* Serialize and re-parse to get an independent copy */
    StrBuf sb = strbuf_new();
    json_serialize(fixture, &sb);
    JsonValue *result = json_parse(strbuf_cstr(&sb), sb.len, &err);
    strbuf_free(&sb);
    json_free(root);
    return result;
}

/* ================================================================
 * StrBuf tests
 * ================================================================ */

static void test_strbuf_new_empty(void)
{
    StrBuf sb = strbuf_new();
    ASSERT_STR_EQ(strbuf_cstr(&sb), "");
    ASSERT_INT_EQ(sb.len, 0);
    strbuf_free(&sb);
}

static void test_strbuf_append(void)
{
    StrBuf sb = strbuf_new();
    strbuf_append_cstr(&sb, "hello");
    ASSERT_STR_EQ(strbuf_cstr(&sb), "hello");
    ASSERT_INT_EQ(sb.len, 5);

    strbuf_append_cstr(&sb, " world");
    ASSERT_STR_EQ(strbuf_cstr(&sb), "hello world");
    ASSERT_INT_EQ(sb.len, 11);
    strbuf_free(&sb);
}

static void test_strbuf_append_char(void)
{
    StrBuf sb = strbuf_new();
    strbuf_append_char(&sb, 'a');
    strbuf_append_char(&sb, 'b');
    strbuf_append_char(&sb, 'c');
    ASSERT_STR_EQ(strbuf_cstr(&sb), "abc");
    ASSERT_INT_EQ(sb.len, 3);
    strbuf_free(&sb);
}

static void test_strbuf_appendf(void)
{
    StrBuf sb = strbuf_new();
    strbuf_appendf(&sb, "%d + %d = %d", 1, 2, 3);
    ASSERT_STR_EQ(strbuf_cstr(&sb), "1 + 2 = 3");
    strbuf_free(&sb);
}

static void test_strbuf_clear(void)
{
    StrBuf sb = strbuf_new();
    strbuf_append_cstr(&sb, "hello");
    strbuf_clear(&sb);
    ASSERT_STR_EQ(strbuf_cstr(&sb), "");
    ASSERT_INT_EQ(sb.len, 0);

    strbuf_append_cstr(&sb, "world");
    ASSERT_STR_EQ(strbuf_cstr(&sb), "world");
    strbuf_free(&sb);
}

static void test_strbuf_append_binary(void)
{
    StrBuf sb = strbuf_new();
    strbuf_append(&sb, "he\0lo", 5);
    ASSERT_INT_EQ(sb.len, 5);
    ASSERT(memcmp(sb.data, "he\0lo", 5) == 0);
    strbuf_free(&sb);
}

static void test_strbuf_large(void)
{
    StrBuf sb = strbuf_new();
    for (int i = 0; i < 10000; i++) {
        strbuf_append_char(&sb, 'x');
    }
    ASSERT_INT_EQ(sb.len, 10000);
    ASSERT(sb.data[0] == 'x');
    ASSERT(sb.data[9999] == 'x');
    strbuf_free(&sb);
}

/* ================================================================
 * JSON parser tests
 * ================================================================ */

static void test_parse_null(void)
{
    JsonValue *v = must_parse("null");
    ASSERT_NOT_NULL(v);
    ASSERT(json_is_null(v));
    json_free(v);
}

static void test_parse_bools(void)
{
    JsonValue *t = must_parse("true");
    ASSERT_NOT_NULL(t);
    ASSERT(t->type == JSON_BOOL);
    ASSERT(t->u.boolean == true);
    json_free(t);

    JsonValue *f = must_parse("false");
    ASSERT_NOT_NULL(f);
    ASSERT(f->type == JSON_BOOL);
    ASSERT(f->u.boolean == false);
    json_free(f);
}

static void test_parse_numbers(void)
{
    struct { const char *input; double expected; } cases[] = {
        {"42",     42.0},
        {"-7",     -7.0},
        {"3.14",   3.14},
        {"1e10",   1e10},
        {"2.5E-3", 2.5e-3},
        {"0",      0.0},
        {"-0.5",   -0.5},
    };
    for (size_t i = 0; i < sizeof(cases)/sizeof(cases[0]); i++) {
        JsonValue *v = must_parse(cases[i].input);
        ASSERT_NOT_NULL(v);
        ASSERT(v->type == JSON_NUMBER);
        ASSERT_DBL_EQ(v->u.number, cases[i].expected);
        json_free(v);
    }
}

static void test_parse_strings(void)
{
    JsonValue *v;

    v = must_parse("\"hello\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "hello");
    json_free(v);

    v = must_parse("\"with \\\"quotes\\\"\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "with \"quotes\"");
    json_free(v);

    v = must_parse("\"line\\nbreak\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "line\nbreak");
    json_free(v);

    v = must_parse("\"tab\\there\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "tab\there");
    json_free(v);

    v = must_parse("\"\\u0041\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "A");
    json_free(v);
}

static void test_parse_string_escapes_all(void)
{
    JsonValue *v;

    v = must_parse("\"slash\\\\/\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "slash\\/");
    json_free(v);

    v = must_parse("\"bs\\b\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "bs\x08");
    json_free(v);

    v = must_parse("\"ff\\f\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "ff\x0C");
    json_free(v);

    v = must_parse("\"cr\\r\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "cr\r");
    json_free(v);
}

static void test_parse_unicode_escape(void)
{
    /* Euro sign U+20AC */
    JsonValue *v = must_parse("\"\\u20AC\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "\xe2\x82\xac");
    json_free(v);
}

static void test_parse_surrogate_pair(void)
{
    /* U+1F600 (grinning face) = \uD83D\uDE00 */
    JsonValue *v = must_parse("\"\\uD83D\\uDE00\"");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(json_as_str(v), "\xf0\x9f\x98\x80");
    json_free(v);
}

static void test_parse_empty_array(void)
{
    JsonValue *v = must_parse("[]");
    ASSERT_NOT_NULL(v);
    ASSERT(v->type == JSON_ARRAY);
    ASSERT_INT_EQ(v->u.array.count, 0);
    json_free(v);
}

static void test_parse_array(void)
{
    JsonValue *v = must_parse("[1, 2, 3]");
    ASSERT_NOT_NULL(v);
    ASSERT(v->type == JSON_ARRAY);
    ASSERT_INT_EQ(v->u.array.count, 3);
    ASSERT_DBL_EQ(v->u.array.items[0]->u.number, 1.0);
    ASSERT_DBL_EQ(v->u.array.items[2]->u.number, 3.0);
    json_free(v);
}

static void test_parse_mixed_array(void)
{
    JsonValue *v = must_parse("[1, \"two\", true, null, 3.14]");
    ASSERT_NOT_NULL(v);
    ASSERT(v->type == JSON_ARRAY);
    ASSERT_INT_EQ(v->u.array.count, 5);
    ASSERT(v->u.array.items[0]->type == JSON_NUMBER);
    ASSERT(v->u.array.items[1]->type == JSON_STRING);
    ASSERT(v->u.array.items[2]->type == JSON_BOOL);
    ASSERT(v->u.array.items[3]->type == JSON_NULL);
    ASSERT(v->u.array.items[4]->type == JSON_NUMBER);
    json_free(v);
}

static void test_parse_empty_object(void)
{
    JsonValue *v = must_parse("{}");
    ASSERT_NOT_NULL(v);
    ASSERT(json_is_object(v));
    ASSERT_INT_EQ(v->u.object.count, 0);
    json_free(v);
}

static void test_parse_object(void)
{
    JsonValue *v = must_parse("{\"a\": 1, \"b\": \"two\", \"c\": null}");
    ASSERT_NOT_NULL(v);

    bool ok;
    ASSERT_DBL_EQ(json_as_f64(json_get(v, "a"), &ok), 1.0);
    ASSERT(ok);
    ASSERT_STR_EQ(json_as_str(json_get(v, "b")), "two");
    ASSERT(json_is_null(json_get(v, "c")));
    ASSERT_NULL(json_get(v, "d"));
    json_free(v);
}

static void test_parse_nested(void)
{
    JsonValue *v = must_parse("{\"arr\": [1, {\"x\": true}], \"obj\": {\"n\": null}}");
    ASSERT_NOT_NULL(v);

    const JsonValue *arr = json_get(v, "arr");
    ASSERT_NOT_NULL(arr);
    ASSERT(arr->type == JSON_ARRAY);
    ASSERT_INT_EQ(arr->u.array.count, 2);

    const JsonValue *inner = arr->u.array.items[1];
    ASSERT(json_is_object(inner));
    const JsonValue *x = json_get(inner, "x");
    ASSERT_NOT_NULL(x);
    ASSERT(x->type == JSON_BOOL);
    ASSERT(x->u.boolean == true);

    const JsonValue *obj = json_get(v, "obj");
    ASSERT(json_is_null(json_get(obj, "n")));
    json_free(v);
}

static void test_parse_deeply_nested(void)
{
    JsonValue *v = must_parse("[[[[1]]]]");
    ASSERT_NOT_NULL(v);
    ASSERT(v->type == JSON_ARRAY);
    const JsonValue *inner = v->u.array.items[0]->u.array.items[0]->u.array.items[0];
    ASSERT(inner->type == JSON_ARRAY);
    ASSERT_DBL_EQ(inner->u.array.items[0]->u.number, 1.0);
    json_free(v);
}

static void test_whitespace_handling(void)
{
    JsonValue *v = must_parse("  {  \"a\"  :  1  ,  \"b\"  :  2  }  ");
    ASSERT_NOT_NULL(v);
    bool ok;
    ASSERT_DBL_EQ(json_as_f64(json_get(v, "a"), &ok), 1.0);
    ASSERT_DBL_EQ(json_as_f64(json_get(v, "b"), &ok), 2.0);
    json_free(v);
}

/* ---- JSON error cases ---- */

static void test_parse_trailing_data_error(void)
{
    JsonError err;
    const JsonValue *v = json_parse("42 extra", 8, &err);
    ASSERT_NULL(v);
    ASSERT_INT_EQ(err.kind, 5); /* trailing data */
}

static void test_parse_unexpected_eof(void)
{
    JsonError err;

    const JsonValue *v = json_parse("{\"a\":", 5, &err);
    ASSERT_NULL(v);

    v = json_parse("[1,", 3, &err);
    ASSERT_NULL(v);

    v = json_parse("\"unterminated", 13, &err);
    ASSERT_NULL(v);
}

static void test_parse_invalid_input(void)
{
    JsonError err;

    const JsonValue *v = json_parse("xyz", 3, &err);
    ASSERT_NULL(v);
    ASSERT_INT_EQ(err.kind, 2); /* unexpected char */

    v = json_parse("", 0, &err);
    ASSERT_NULL(v);
    ASSERT_INT_EQ(err.kind, 1); /* eof */
}

static void test_parse_invalid_number(void)
{
    JsonError err;
    const JsonValue *v = json_parse("1.", 2, &err);
    ASSERT_NULL(v);
    ASSERT_INT_EQ(err.kind, 4); /* invalid number */
}

static void test_parse_invalid_escape(void)
{
    JsonError err;
    const JsonValue *v = json_parse("\"\\q\"", 4, &err);
    ASSERT_NULL(v);
    ASSERT_INT_EQ(err.kind, 3); /* invalid escape */
}

static void test_parse_lone_surrogate(void)
{
    JsonError err;
    /* High surrogate without low surrogate */
    const JsonValue *v = json_parse("\"\\uD800\"", 8, &err);
    ASSERT_NULL(v);
}

/* ---- JSON serializer tests ---- */

static void test_serialize_roundtrip(void)
{
    const char *cases[] = {
        "null", "true", "false", "42", "-7",
        "\"hello\"", "[]", "[1,2,3]", "{}", "{\"a\":1,\"b\":\"two\"}",
    };
    for (size_t i = 0; i < sizeof(cases)/sizeof(cases[0]); i++) {
        JsonValue *v1 = must_parse(cases[i]);
        ASSERT_NOT_NULL(v1);

        char *s1 = serialize(v1);
        JsonValue *v2 = must_parse(s1);
        ASSERT_NOT_NULL(v2);

        char *s2 = serialize(v2);
        ASSERT_STR_EQ(s1, s2);

        free(s1);
        free(s2);
        json_free(v1);
        json_free(v2);
    }
}

static void test_serialize_escapes(void)
{
    JsonValue *v = json_string("quote\"back\\slash");
    char *s = serialize(v);
    ASSERT_STR_EQ(s, "\"quote\\\"back\\\\slash\"");
    free(s);
    json_free(v);
}

static void test_serialize_control_chars(void)
{
    JsonValue *v = json_string("a\nb\tc\rd");
    char *s = serialize(v);
    ASSERT_STR_EQ(s, "\"a\\nb\\tc\\rd\"");
    free(s);
    json_free(v);
}

static void test_serialize_number_integer(void)
{
    char *s;

    s = serialize(json_number(42.0));
    ASSERT_STR_EQ(s, "42");
    free(s);

    s = serialize(json_number(-7.0));
    ASSERT_STR_EQ(s, "-7");
    free(s);

    s = serialize(json_number(0.0));
    ASSERT_STR_EQ(s, "0");
    free(s);

    /* json_number doesn't free — we need to allocate+free properly */
    JsonValue *v = json_number(42.0);
    s = serialize(v);
    ASSERT_STR_EQ(s, "42");
    free(s);
    json_free(v);
}

static void test_serialize_number_float(void)
{
    JsonValue *v = json_number(3.14);
    char *s = serialize(v);
    ASSERT_STR_EQ(s, "3.14");
    free(s);
    json_free(v);
}

static void test_serialize_large_integer(void)
{
    JsonValue *v = json_from_i64(1234567890123LL);
    char *s = serialize(v);
    ASSERT_STR_EQ(s, "1234567890123");
    free(s);
    json_free(v);
}

/* ---- JSON builder tests ---- */

static void test_json_object_builder(void)
{
    JsonValue *obj = json_object_new();
    json_object_set(obj, "name", json_string("test"));
    json_object_set(obj, "count", json_number(5.0));

    ASSERT_STR_EQ(json_as_str(json_get(obj, "name")), "test");
    bool ok;
    ASSERT_DBL_EQ(json_as_f64(json_get(obj, "count"), &ok), 5.0);
    ASSERT(ok);

    json_free(obj);
}

static void test_json_object_overwrite(void)
{
    JsonValue *obj = json_object_new();
    json_object_set(obj, "key", json_number(1.0));
    json_object_set(obj, "key", json_number(2.0));

    bool ok;
    ASSERT_DBL_EQ(json_as_f64(json_get(obj, "key"), &ok), 2.0);
    ASSERT_INT_EQ(obj->u.object.count, 1);
    json_free(obj);
}

static void test_json_array_builder(void)
{
    JsonValue *arr = json_array_new();
    json_array_push(arr, json_number(1.0));
    json_array_push(arr, json_string("two"));
    json_array_push(arr, json_null());

    ASSERT_INT_EQ(arr->u.array.count, 3);
    ASSERT(arr->u.array.items[0]->type == JSON_NUMBER);
    ASSERT(arr->u.array.items[1]->type == JSON_STRING);
    ASSERT(arr->u.array.items[2]->type == JSON_NULL);
    json_free(arr);
}

static void test_json_from_i64(void)
{
    JsonValue *v = json_from_i64(-51);
    ASSERT(v->type == JSON_NUMBER);
    bool ok;
    ASSERT_INT_EQ(json_as_i64(v, &ok), -51);
    ASSERT(ok);
    json_free(v);
}

static void test_json_accessors_wrong_type(void)
{
    JsonValue *s = json_string("hello");
    bool ok;

    json_as_f64(s, &ok);
    ASSERT(!ok);
    json_as_i64(s, &ok);
    ASSERT(!ok);
    ASSERT(!json_is_null(s));
    ASSERT(!json_is_object(s));

    JsonValue *n = json_number(42.0);
    ASSERT_NULL(json_as_str(n));
    ASSERT_NULL(json_get(n, "key"));

    json_free(s);
    json_free(n);
}

/* ---- AirGradient fixture tests ---- */

static void test_airgradient_indoor_fixture(void)
{
    JsonValue *v = load_fixture("indoorFull");
    ASSERT_NOT_NULL(v);

    bool ok;
    ASSERT_INT_EQ(json_as_i64(json_get(v, "wifi"), &ok), -51);
    ASSERT_STR_EQ(json_as_str(json_get(v, "serialno")), "84fce602549c");
    ASSERT_INT_EQ(json_as_i64(json_get(v, "rco2"), &ok), 489);
    ASSERT_DBL_EQ(json_as_f64(json_get(v, "pm02"), &ok), 41.67);
    ASSERT_DBL_EQ(json_as_f64(json_get(v, "pm02Compensated"), &ok), 31.18);
    ASSERT_STR_EQ(json_as_str(json_get(v, "model")), "I-9PSL");
    ASSERT_STR_EQ(json_as_str(json_get(v, "firmware")), "3.6.0");
    json_free(v);
}

static void test_airgradient_null_fields(void)
{
    JsonValue *v = load_fixture("afterBoot");
    ASSERT_NOT_NULL(v);

    ASSERT(json_is_null(json_get(v, "rco2")));
    ASSERT(json_is_null(json_get(v, "pm01")));
    ASSERT(json_is_null(json_get(v, "pm02")));
    ASSERT(json_is_null(json_get(v, "pm10")));
    ASSERT(json_is_null(json_get(v, "atmp")));
    ASSERT(json_is_null(json_get(v, "atmpCompensated")));
    ASSERT(json_is_null(json_get(v, "rhum")));
    ASSERT(json_is_null(json_get(v, "rhumCompensated")));
    ASSERT(json_is_null(json_get(v, "tvocIndex")));
    ASSERT(json_is_null(json_get(v, "noxIndex")));
    bool ok;
    ASSERT_INT_EQ(json_as_i64(json_get(v, "wifi"), &ok), -59);
    ASSERT(ok);
    json_free(v);
}

static void test_airgradient_zero_compensated(void)
{
    JsonValue *v = load_fixture("zeroCompensated");
    ASSERT_NOT_NULL(v);

    bool ok;
    ASSERT_DBL_EQ(json_as_f64(json_get(v, "pm02Compensated"), &ok), 0.0);
    ASSERT(ok);
    ASSERT_DBL_EQ(json_as_f64(json_get(v, "atmpCompensated"), &ok), 0.0);
    ASSERT_DBL_EQ(json_as_f64(json_get(v, "rhumCompensated"), &ok), 0.0);
    ASSERT_INT_EQ(json_as_i64(json_get(v, "rco2"), &ok), 400);
    ASSERT_INT_EQ(json_as_i64(json_get(v, "pm01"), &ok), 5);
    ASSERT_INT_EQ(json_as_i64(json_get(v, "pm02"), &ok), 10);
    json_free(v);
}

/* ================================================================
 * Database tests
 * ================================================================ */

static sqlite3 *setup_test_db(void)
{
    sqlite3 *db;
    int rc = sqlite3_open(":memory:", &db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "    Failed to open in-memory DB\n");
        return NULL;
    }
    if (db_initialize(db) != 0) {
        fprintf(stderr, "    Failed to initialize DB\n");
        sqlite3_close(db);
        return NULL;
    }
    return db;
}

static JsonValue *indoor_fixture(void)
{
    return load_fixture("indoorFull");
}

static JsonValue *outdoor_fixture(void)
{
    return load_fixture("outdoorFull");
}

static void test_db_insert_and_query(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *data = indoor_fixture();
    ASSERT_NOT_NULL(data);
    ASSERT_INT_EQ(db_insert_reading(db, "192.168.1.1", data), 0);
    json_free(data);

    ReadingQuery q = { .device = NULL, .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 1);
    ASSERT_STR_EQ(rl.items[0].device_id, "84fce602549c");
    ASSERT_STR_EQ(rl.items[0].device_type, "indoor");
    ASSERT_STR_EQ(rl.items[0].device_ip, "192.168.1.1");
    ASSERT(rl.items[0].has_pm02);
    ASSERT_DBL_EQ(rl.items[0].pm02, 41.67);
    ASSERT(rl.items[0].has_rco2);
    ASSERT_INT_EQ(rl.items[0].rco2, 489);
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_device_type_classification(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *d1 = indoor_fixture();
    JsonValue *d2 = outdoor_fixture();
    ASSERT_NOT_NULL(d1);
    ASSERT_NOT_NULL(d2);
    db_insert_reading(db, "192.168.1.1", d1);
    db_insert_reading(db, "192.168.1.2", d2);
    json_free(d1);
    json_free(d2);

    DeviceSummaryList dl;
    ASSERT_INT_EQ(db_get_devices(db, &dl), 0);
    ASSERT_INT_EQ(dl.count, 2);

    /* Find indoor and outdoor */
    const DeviceSummary *indoor_dev = NULL;
    const DeviceSummary *outdoor_dev = NULL;
    for (size_t i = 0; i < dl.count; i++) {
        if (strcmp(dl.items[i].device_type, "indoor") == 0) indoor_dev = &dl.items[i];
        if (strcmp(dl.items[i].device_type, "outdoor") == 0) outdoor_dev = &dl.items[i];
    }

    ASSERT_NOT_NULL(indoor_dev);
    ASSERT_STR_EQ(indoor_dev->device_id, "84fce602549c");

    ASSERT_NOT_NULL(outdoor_dev);
    ASSERT_STR_EQ(outdoor_dev->device_id, "ecda3b1d09d8");

    device_summary_list_free(&dl);
    sqlite3_close(db);
}

static void test_db_null_fields(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *data = load_fixture("afterBoot");
    ASSERT_NOT_NULL(data);
    db_insert_reading(db, "192.168.1.1", data);
    json_free(data);

    ReadingQuery q = { .device = NULL, .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 1);
    ASSERT(!rl.items[0].has_rco2);
    ASSERT(!rl.items[0].has_pm01);
    ASSERT(!rl.items[0].has_pm02);
    ASSERT(!rl.items[0].has_pm10);
    ASSERT(!rl.items[0].has_atmp);
    ASSERT(!rl.items[0].has_atmp_compensated);
    ASSERT(!rl.items[0].has_rhum);
    ASSERT(!rl.items[0].has_rhum_compensated);
    ASSERT(!rl.items[0].has_tvoc_index);
    ASSERT(!rl.items[0].has_nox_index);
    ASSERT(rl.items[0].has_wifi);
    ASSERT_INT_EQ(rl.items[0].wifi, -59);
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_zero_compensated_values(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *data = load_fixture("zeroCompensated");
    ASSERT_NOT_NULL(data);
    db_insert_reading(db, "192.168.1.1", data);
    json_free(data);

    ReadingQuery q = { .device = NULL, .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 1);
    ASSERT(rl.items[0].has_pm02_compensated);
    ASSERT_DBL_EQ(rl.items[0].pm02_compensated, 0.0);
    ASSERT(rl.items[0].has_atmp_compensated);
    ASSERT_DBL_EQ(rl.items[0].atmp_compensated, 0.0);
    ASSERT(rl.items[0].has_rhum_compensated);
    ASSERT_DBL_EQ(rl.items[0].rhum_compensated, 0.0);
    ASSERT(rl.items[0].has_rco2);
    ASSERT_INT_EQ(rl.items[0].rco2, 400);
    ASSERT(rl.items[0].has_pm02);
    ASSERT_DBL_EQ(rl.items[0].pm02, 10.0);
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_query_with_device_filter(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *d1 = indoor_fixture();
    JsonValue *d2 = outdoor_fixture();
    db_insert_reading(db, "192.168.1.1", d1);
    db_insert_reading(db, "192.168.1.2", d2);
    json_free(d1);
    json_free(d2);

    ReadingQuery q = { .device = "84fce602549c", .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 1);
    ASSERT_STR_EQ(rl.items[0].device_id, "84fce602549c");
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_query_with_all_device(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *d1 = indoor_fixture();
    JsonValue *d2 = outdoor_fixture();
    db_insert_reading(db, "192.168.1.1", d1);
    db_insert_reading(db, "192.168.1.2", d2);
    json_free(d1);
    json_free(d2);

    /* "all" should return everything, same as NULL */
    ReadingQuery q = { .device = "all", .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 2);
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_query_with_limit(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *data = indoor_fixture();
    for (int i = 0; i < 5; i++) {
        db_insert_reading(db, "192.168.1.1", data);
    }
    json_free(data);

    ReadingQuery q = { .device = NULL, .from = 0, .to = INT64_MAX, .limit = 3 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 3);
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_query_with_device_and_limit(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *d1 = indoor_fixture();
    JsonValue *d2 = outdoor_fixture();
    for (int i = 0; i < 5; i++) {
        db_insert_reading(db, "192.168.1.1", d1);
        db_insert_reading(db, "192.168.1.2", d2);
    }
    json_free(d1);
    json_free(d2);

    ReadingQuery q = { .device = "84fce602549c", .from = 0, .to = INT64_MAX, .limit = 2 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 2);
    ASSERT_STR_EQ(rl.items[0].device_id, "84fce602549c");
    ASSERT_STR_EQ(rl.items[1].device_id, "84fce602549c");
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_get_latest_readings(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *d1 = indoor_fixture();
    JsonValue *d2 = outdoor_fixture();
    db_insert_reading(db, "192.168.1.1", d1);
    db_insert_reading(db, "192.168.1.1", d1);
    db_insert_reading(db, "192.168.1.2", d2);
    json_free(d1);
    json_free(d2);

    ReadingList rl;
    ASSERT_INT_EQ(db_get_latest_readings(db, &rl), 0);
    ASSERT_INT_EQ(rl.count, 2);
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_get_devices(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *d1 = indoor_fixture();
    JsonValue *d2 = outdoor_fixture();
    db_insert_reading(db, "192.168.1.1", d1);
    db_insert_reading(db, "192.168.1.1", d1);
    db_insert_reading(db, "192.168.1.2", d2);
    json_free(d1);
    json_free(d2);

    DeviceSummaryList dl;
    ASSERT_INT_EQ(db_get_devices(db, &dl), 0);
    ASSERT_INT_EQ(dl.count, 2);

    const DeviceSummary *indoor_dev = NULL;
    for (size_t i = 0; i < dl.count; i++) {
        if (strcmp(dl.items[i].device_type, "indoor") == 0)
            indoor_dev = &dl.items[i];
    }
    ASSERT_NOT_NULL(indoor_dev);
    ASSERT_INT_EQ(indoor_dev->reading_count, 2);

    device_summary_list_free(&dl);
    sqlite3_close(db);
}

static void test_db_missing_serialno(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *data = must_parse("{\"wifi\":-45,\"model\":\"I-9PSL\"}");
    ASSERT_NOT_NULL(data);
    db_insert_reading(db, "192.168.1.1", data);
    json_free(data);

    ReadingQuery q = { .device = NULL, .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 1);
    ASSERT_STR_EQ(rl.items[0].device_id, "unknown");
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_reading_to_json(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *data = indoor_fixture();
    db_insert_reading(db, "192.168.1.1", data);
    json_free(data);

    ReadingQuery q = { .device = NULL, .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 1);

    JsonValue *json = reading_to_json(&rl.items[0]);
    ASSERT_NOT_NULL(json);
    ASSERT_STR_EQ(json_as_str(json_get(json, "device_id")), "84fce602549c");
    ASSERT_STR_EQ(json_as_str(json_get(json, "device_type")), "indoor");
    bool ok;
    ASSERT_DBL_EQ(json_as_f64(json_get(json, "pm02"), &ok), 41.67);
    ASSERT(ok);

    json_free(json);
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_reading_to_json_nulls(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *data = must_parse(
        "{\"wifi\":-59,\"serialno\":\"abc\",\"rco2\":null,\"pm02\":null,\"model\":\"I-9PSL\"}"
    );
    db_insert_reading(db, "192.168.1.1", data);
    json_free(data);

    ReadingQuery q = { .device = NULL, .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    db_query_readings(db, &q, &rl);

    JsonValue *json = reading_to_json(&rl.items[0]);
    ASSERT(json_is_null(json_get(json, "rco2")));
    ASSERT(json_is_null(json_get(json, "pm02")));
    bool ok;
    ASSERT_INT_EQ(json_as_i64(json_get(json, "wifi"), &ok), -59);
    ASSERT(ok);

    json_free(json);
    reading_list_free(&rl);
    sqlite3_close(db);
}

static void test_db_device_summary_to_json(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    JsonValue *data = indoor_fixture();
    db_insert_reading(db, "192.168.1.1", data);
    db_insert_reading(db, "192.168.1.1", data);
    json_free(data);

    DeviceSummaryList dl;
    db_get_devices(db, &dl);
    ASSERT_INT_EQ(dl.count, 1);

    JsonValue *json = device_summary_to_json(&dl.items[0]);
    ASSERT_STR_EQ(json_as_str(json_get(json, "device_id")), "84fce602549c");
    ASSERT_STR_EQ(json_as_str(json_get(json, "device_type")), "indoor");
    bool ok;
    ASSERT_INT_EQ(json_as_i64(json_get(json, "reading_count"), &ok), 2);

    json_free(json);
    device_summary_list_free(&dl);
    sqlite3_close(db);
}

static void test_db_empty_query(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);

    ReadingQuery q = { .device = NULL, .from = 0, .to = INT64_MAX, .limit = 0 };
    ReadingList rl;
    ASSERT_INT_EQ(db_query_readings(db, &q, &rl), 0);
    ASSERT_INT_EQ(rl.count, 0);
    reading_list_free(&rl);

    ReadingList latest;
    ASSERT_INT_EQ(db_get_latest_readings(db, &latest), 0);
    ASSERT_INT_EQ(latest.count, 0);
    reading_list_free(&latest);

    DeviceSummaryList dl;
    ASSERT_INT_EQ(db_get_devices(db, &dl), 0);
    ASSERT_INT_EQ(dl.count, 0);
    device_summary_list_free(&dl);

    sqlite3_close(db);
}

static void test_db_checkpoint(void)
{
    sqlite3 *db = setup_test_db();
    ASSERT_NOT_NULL(db);
    /* Checkpoint on in-memory DB should not crash */
    ASSERT_INT_EQ(db_checkpoint(db), 0);
    sqlite3_close(db);
}

static void test_db_now_millis(void)
{
    int64_t t1 = db_now_millis();
    ASSERT(t1 > 1700000000000LL); /* sanity: after 2023 */
    int64_t t2 = db_now_millis();
    ASSERT(t2 >= t1);
    ASSERT(t2 - t1 < 1000); /* should be < 1s apart */
}

/* ================================================================
 * API query_param tests
 * ================================================================ */

static void test_query_param_basic(void)
{
    char *v;

    v = query_param("from=100&to=200", "from");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(v, "100");
    free(v);

    v = query_param("from=100&to=200", "to");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(v, "200");
    free(v);

    v = query_param("from=100&to=200", "missing");
    ASSERT_NULL(v);
}

static void test_query_param_url_decode(void)
{
    char *v;

    v = query_param("name=hello%20world", "name");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(v, "hello world");
    free(v);

    v = query_param("q=a+b", "q");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(v, "a b");
    free(v);

    v = query_param("hex=%41%42%43", "hex");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(v, "ABC");
    free(v);
}

static void test_query_param_empty(void)
{
    ASSERT_NULL(query_param("", "key"));
    ASSERT_NULL(query_param(NULL, "key"));
}

static void test_query_param_no_value(void)
{
    char *v = query_param("flag&other=1", "flag");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(v, "");
    free(v);
}

static void test_query_param_first_and_last(void)
{
    char *v;

    v = query_param("a=1&b=2&c=3", "a");
    ASSERT_STR_EQ(v, "1");
    free(v);

    v = query_param("a=1&b=2&c=3", "c");
    ASSERT_STR_EQ(v, "3");
    free(v);
}

static void test_query_param_single(void)
{
    char *v = query_param("device=abc123", "device");
    ASSERT_NOT_NULL(v);
    ASSERT_STR_EQ(v, "abc123");
    free(v);
}

/* ================================================================
 * Main
 * ================================================================ */

int main(void)
{
    fprintf(stderr, "\n=== StrBuf tests ===\n");
    RUN_TEST(test_strbuf_new_empty);
    RUN_TEST(test_strbuf_append);
    RUN_TEST(test_strbuf_append_char);
    RUN_TEST(test_strbuf_appendf);
    RUN_TEST(test_strbuf_clear);
    RUN_TEST(test_strbuf_append_binary);
    RUN_TEST(test_strbuf_large);

    fprintf(stderr, "\n=== JSON parser tests ===\n");
    RUN_TEST(test_parse_null);
    RUN_TEST(test_parse_bools);
    RUN_TEST(test_parse_numbers);
    RUN_TEST(test_parse_strings);
    RUN_TEST(test_parse_string_escapes_all);
    RUN_TEST(test_parse_unicode_escape);
    RUN_TEST(test_parse_surrogate_pair);
    RUN_TEST(test_parse_empty_array);
    RUN_TEST(test_parse_array);
    RUN_TEST(test_parse_mixed_array);
    RUN_TEST(test_parse_empty_object);
    RUN_TEST(test_parse_object);
    RUN_TEST(test_parse_nested);
    RUN_TEST(test_parse_deeply_nested);
    RUN_TEST(test_whitespace_handling);

    fprintf(stderr, "\n=== JSON error tests ===\n");
    RUN_TEST(test_parse_trailing_data_error);
    RUN_TEST(test_parse_unexpected_eof);
    RUN_TEST(test_parse_invalid_input);
    RUN_TEST(test_parse_invalid_number);
    RUN_TEST(test_parse_invalid_escape);
    RUN_TEST(test_parse_lone_surrogate);

    fprintf(stderr, "\n=== JSON serializer tests ===\n");
    RUN_TEST(test_serialize_roundtrip);
    RUN_TEST(test_serialize_escapes);
    RUN_TEST(test_serialize_control_chars);
    RUN_TEST(test_serialize_number_integer);
    RUN_TEST(test_serialize_number_float);
    RUN_TEST(test_serialize_large_integer);

    fprintf(stderr, "\n=== JSON builder tests ===\n");
    RUN_TEST(test_json_object_builder);
    RUN_TEST(test_json_object_overwrite);
    RUN_TEST(test_json_array_builder);
    RUN_TEST(test_json_from_i64);
    RUN_TEST(test_json_accessors_wrong_type);

    fprintf(stderr, "\n=== JSON AirGradient fixture tests ===\n");
    RUN_TEST(test_airgradient_indoor_fixture);
    RUN_TEST(test_airgradient_null_fields);
    RUN_TEST(test_airgradient_zero_compensated);

    fprintf(stderr, "\n=== DB tests ===\n");
    RUN_TEST(test_db_insert_and_query);
    RUN_TEST(test_db_device_type_classification);
    RUN_TEST(test_db_null_fields);
    RUN_TEST(test_db_zero_compensated_values);
    RUN_TEST(test_db_query_with_device_filter);
    RUN_TEST(test_db_query_with_all_device);
    RUN_TEST(test_db_query_with_limit);
    RUN_TEST(test_db_query_with_device_and_limit);
    RUN_TEST(test_db_get_latest_readings);
    RUN_TEST(test_db_get_devices);
    RUN_TEST(test_db_missing_serialno);
    RUN_TEST(test_db_reading_to_json);
    RUN_TEST(test_db_reading_to_json_nulls);
    RUN_TEST(test_db_device_summary_to_json);
    RUN_TEST(test_db_empty_query);
    RUN_TEST(test_db_checkpoint);
    RUN_TEST(test_db_now_millis);

    fprintf(stderr, "\n=== API query_param tests ===\n");
    RUN_TEST(test_query_param_basic);
    RUN_TEST(test_query_param_url_decode);
    RUN_TEST(test_query_param_empty);
    RUN_TEST(test_query_param_no_value);
    RUN_TEST(test_query_param_first_and_last);
    RUN_TEST(test_query_param_single);

    fprintf(stderr, "\n========================================\n");
    fprintf(stderr, "  %d passed, %d failed, %d total\n",
            g_tests_passed, g_tests_failed, g_tests_run);
    fprintf(stderr, "========================================\n\n");

    return g_tests_failed > 0 ? 1 : 0;
}
