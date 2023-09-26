#include "deque.h"
#include "node.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#


Deque* create(){
    Deque *d = malloc(sizeof(Deque));
    d->start = NULL;
    d->end = NULL;
    d->size = 0;
    return d;
}

void push(Deque* deque, void* data){
    Node *n = nodeCreate(data);
    n->prev = deque->end;
    deque->end = n;
    if(deque->size == 0){
        deque->start = n;
    }
    else{
        n->prev->next = n;
    }
    deque->size++;
}


void pushFront(Deque* deque, void* data){
    Node *n = nodeCreate(data);
    n->next = deque->start;
    deque->start = n;
    if(deque->size == 0){
        deque->end = n;
    }
    else n->next->prev = n;
    deque->size++;
}

void* pop(Deque* deque){
    if(deque->size == 0) return NULL;
    Node *n = deque->end;
    void *data = n->data;
    deque->end = deque->end->prev;
    if(deque->size == 1){
        deque->start = NULL;
    } else{
        deque->end->next = NULL;
    }
    free(n);
    deque->size--;
    return data;
}

void* popFront(Deque* deque){
    if(deque->size == 0) return NULL;
    Node *n = deque->start;
    void *data = n->data;
    deque->start = n->next;
    if(deque->size == 1){
        deque->end = NULL;
    } else{
        deque->start->prev = NULL;
    }
    free(n);
    deque->size--;
    return data;
}

int size(Deque* deque){
    return deque->size;
}

bool isEmpty(Deque* deque){
    return !deque->size;
}

void reverse(Deque* deque){
    Node *current = deque->start;
    Node *prev = NULL;
    Node *next = NULL;

    // Acertar a ordem da lista
    while(current != NULL){
        next = current->next;
        current->next = prev;
        current->prev = next;
        prev = current;
        current = next;
    }

    // Acertar inicio e fim da lista
    Node* temp = deque->start;
    deque->start = deque->end;
    deque->end = temp;
}

void printDeque(Deque* deque, void(*printFunc)(void*)){
    Node* current = deque->start;

    printf("[");

    while(current != NULL){
        printFunc(current->data);
        current = current->next;
        if (current) printf("->");
    }

    printf("]\n");
}

void destroy(Deque* deque){
    Node* current = deque->start;
    Node *temp;

    while(current != NULL){
        temp = current;
        current = current->next;
        free(temp);
    }
    free(deque);
}