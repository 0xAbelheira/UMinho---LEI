#include "node.h"
#include <stdio.h>
#include <stdlib.h>

Node* nodeCreate(void* data){
    Node* new = malloc(sizeof(Node));
    new->data = data;
    new->next = NULL;
    new->prev = NULL;
    return new;
}