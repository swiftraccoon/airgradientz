CC       = gcc
CFLAGS   = -std=c23 -D_POSIX_C_SOURCE=200809L \
           -Wall -Wextra -Werror -Wpedantic \
           -Wshadow -Wstrict-prototypes -Wmissing-prototypes \
           -Wconversion -Wsign-conversion -Wformat=2 \
           -Wdouble-promotion -Wnull-dereference -Wwrite-strings \
           -Wcast-align -Wstrict-overflow=5 -Wundef \
           -fstack-protector-strong
SQLITE_CFLAGS = -std=c11 -D_POSIX_C_SOURCE=200809L \
                -DSQLITE_THREADSAFE=2 -DSQLITE_DQS=0 \
                -DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1 \
                -DSQLITE_OMIT_DEPRECATED -w
LDFLAGS  = -lpthread -ldl -lm

SRCS     = src/main.c src/strbuf.c src/json.c src/db.c \
           src/http_client.c src/http_server.c src/api.c src/poller.c
OBJS     = $(SRCS:.c=.o)
SQLITE_OBJ = sqlite3.o
TARGET   = airgradientz

# Test: same sources minus main.c, plus test_main.c
TEST_SRCS = src/strbuf.c src/json.c src/db.c src/api.c src/poller.c src/http_client.c
TEST_OBJS = $(TEST_SRCS:.c=.o) src/test_main.o
TEST_TARGET = test_runner

.PHONY: all clean debug test

all: CFLAGS += -O2 -DNDEBUG
all: $(TARGET)

debug: CFLAGS += -g -O0 -fsanitize=address,undefined -fno-omit-frame-pointer
debug: LDFLAGS += -fsanitize=address,undefined
debug: $(TARGET)

test: CFLAGS += -g -O0
test: $(TEST_TARGET)
	./$(TEST_TARGET)

$(TARGET): $(OBJS) $(SQLITE_OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)

$(TEST_TARGET): $(TEST_OBJS) $(SQLITE_OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)

$(SQLITE_OBJ): sqlite3.c
	$(CC) $(SQLITE_CFLAGS) -c -o $@ $<

src/%.o: src/%.c
	$(CC) $(CFLAGS) -I. -c -o $@ $<

clean:
	rm -f $(OBJS) $(SQLITE_OBJ) $(TARGET) $(TEST_OBJS) $(TEST_TARGET)
