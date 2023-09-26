CC=gcc
CFLAGS=-c -g -Wall
LDFLAGS=

deque: src/deque.o src/node.o src/main.o 
	$(CC) $(LDFLAGS) $^ -o deque 

src/deque.o: src/deque.c

src/node.o: src/node.c

src/main.o: src/main.c

clean: 
	rm src/deque.o src/node.o src/main.o
