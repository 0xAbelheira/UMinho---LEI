CC=gcc
CFLAGS=-c -g -Wall -Iinclude
LDFLAGS=

SOURCES=$(wildcard src/*.c)
OBJECTS=$(patsubst src/%.c, obj/%.o, $(SOURCES))

deque: $(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@

obj/%.o: src/%.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f obj/%.o deque
