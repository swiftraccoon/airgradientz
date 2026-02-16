NASM      := nasm
CC        := gcc
NASMFLAGS := -f elf64 -g -F dwarf
CFLAGS    := -c -DSQLITE_THREADSAFE=2 -DSQLITE_DQS=0 -DSQLITE_OMIT_DEPRECATED -w
LDFLAGS   := -lpthread -ldl -lm -no-pie

ASM_SRCS  := $(wildcard src/*.asm)
ASM_OBJS  := $(ASM_SRCS:.asm=.o)
SQLITE_OBJ := sqlite3.o
TARGET    := airgradientz

.PHONY: all clean test

all: $(TARGET)

$(TARGET): $(ASM_OBJS) $(SQLITE_OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)

src/%.o: src/%.asm
	$(NASM) $(NASMFLAGS) -o $@ $<

$(SQLITE_OBJ): ../c/sqlite3.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(ASM_OBJS) $(SQLITE_OBJ) $(TARGET)

test: $(TARGET)
	bash tests/run_tests.sh
