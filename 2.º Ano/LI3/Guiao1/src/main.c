#include "deque.h"
#include "node.h"
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

typedef enum dequeOp{
    PUSH,
    PUSH_FRONT,
    POP,
    POP_FRONT,
    REVERSE,
    SIZE,
    IS_EMPTY
} DequeOp;

DequeOp randomOp(){
    return rand() % 7;
}

int *randomInt(){
    int *data = malloc(sizeof(int));
    *data = rand() % 100;
    return data;
}

void printInt(void *i){
    int *in = i;
    printf("%d", *in);
}

void testDeque(int nOps){
    Deque *deque = create();
    push(deque, randomInt());
    push(deque, randomInt());
    push(deque, randomInt());

    printf("Initializing Deque...\n");
    printDeque(deque, &printInt);
    printf("\n");

    for(int i = 0; i < nOps; i++){
        DequeOp op = randomOp();
        int *data = NULL;

        switch(op){
        case PUSH:
            data = randomInt();
            printf("PUSH(%d) = ", *data);
            push(deque, data);
            break;

        case PUSH_FRONT:
            data = randomInt();
            printf("PUSH_FRONT(%d) = ", *data);
            pushFront(deque, data);
            break;

        case POP:
            printf("POP = ");
            data = pop(deque);
            if(data) free(data);
            break;

        case POP_FRONT:
            printf("POP_FRONT = ");
            data = popFront(deque);
            if(data) free(data);
            break;

        case REVERSE:
            printf("REVERSE = ");
            reverse(deque);

        case SIZE:
            printf("SIZE: %d ", size(deque));
            break;

        case IS_EMPTY:
            printf("IS_EMPTY = %d ", isEmpty(deque));
            break;
        }

        printDeque(deque, &printInt);
    }

    int *data = pop(deque);
    while(data){
        free(data);
        data = pop(deque);
    }

    destroy(deque);
}

int main(int argc, char** argv){
    srand(time(NULL));
    puts("Testing Deque...\n");
    testDeque(20);

    return 0;
}