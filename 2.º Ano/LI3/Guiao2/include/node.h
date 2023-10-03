//node.h
#ifndef NODE
#define NODE

typedef struct node {
    void* data;
    struct node* next;
    struct node* prev;
} Node;

// Cria um novo Node
Node* nodeCreate(void* data);

#endif