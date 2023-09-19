#ifndef DEQUE
#define DEQUE

#include "node.h"
#include <stdbool.h>

typedef struct {
    Node* start;
    Node* end;
    int size;
    int boolean;
} Deque;

Deque* create();

void push(Deque* deque, void* data);

void pushFront(Deque* deque, void* data);

void* pop(Deque* deque);

void* popFront(Deque* deque);

int size(Deque* deque);

bool isEmpty(Deque* deque);

void reverse1(Deque* deque);

void reverse2(Deque* deque);

void printDeque(Deque* deque, void(*printFunc)(void*));

void destroy(Deque* deque);

#endif