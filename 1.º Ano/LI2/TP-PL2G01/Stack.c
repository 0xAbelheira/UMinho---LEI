/**
 * @file stack.c 
 * @brief Este ficheiro contém as funções que manipulam a stack
 */
#include <stdio.h>
#include <stdlib.h>
#include "Stack.h"
#include <assert.h>
#include <string.h>

/** 
 * \brief Imprime qualquer tipo dentro dos DADOS para formato string
 * @param d Dados a imprimir 
 */
void PRINT_DADOS(DADOS d){
    switch (d.tipo){
            case LONG : printf ("%ld", d.DATA.LONG); break;
            case DOUBLE : printf ("%g", d.DATA.DOUBLE); break;
            case CHAR : printf ("%c", d.DATA.CHAR); break; 
            case STRING : if(d.DATA.STRING[0] != '\n') {printf("%s",d.DATA.STRING); break;} else break;
            case POINTER : PRINT(d.DATA.sAp); break;
    }
}
/**
 * @brief Coloca o apontador para o início da stack (lista-ligada) em NULL
 * @param s apontador para a stack
 */
void INIT (STACK *s){ s->top = NULL; s->beg = NULL; } //topo e início da Stack não está definido ainda


/**
 * @brief Imprime a Stack
 * Percorre a lista ligada até encontrar NULL e vai imprimindo a data em cada nodo
 * @param s Apontador para a Stack
 */
void PRINT (STACK *s){
    NODE *p = s->beg;

    while (p != NULL){ // enquanto a stack não estiver vazia
        PRINT_DADOS(p->data); //  imprime o valor demonstrando no terminal o seu tipo de dados
        //PRINT_TIPO(p->data); // imprime o valor, não sendo visível (no terminal) qual o seu tipo de dados
        p = p->next;
    }
}

/** 
 * \brief Imprime o tipo de dados correto de cada dado
 * @param d 
 * @return vazio
 */
void PRINT_TIPO(DADOS d){   
    switch (d.tipo){
            case LONG : printf("Tipo : LONG | Valor = %ld", d.DATA.LONG); break;
            case DOUBLE : printf ("Tipo : DOUBLE | Valor = %g", d.DATA.DOUBLE); break;
            case CHAR : printf ("Tipo : CHAR | Valor = %c", d.DATA.CHAR); break;
            case STRING : printf ("Tipo : STRING | Valor = %s", d.DATA.STRING); break;
            case POINTER : printf ("Tipo : POINTER | Valor = %p", d.DATA.sAp); break; //printf ("Tipo : POINTER | Valor = %p", d.sAp); break; // PRINT(d.sAp); break; //printf ("Tipo : POINTER | Valor = %p", d.sAp); break; // PRINT(d.sAp);
    }
    putchar('\n');
}

/**
 * \brief Coloca um elemento no topo da stack
 * Aloca espaço em memória no topo da pilha (lista-ligada).
 * Coloca o último elemento da lista a apontar para o novo nodo
 * @param val Valor a colocar
 * @param s Apontador para a Stack
 */
void PUSH (DADOS val, STACK *s){
    NODE *newNode = (NODE *)malloc(16+sizeof(val)); // aloca memória para o novo nodo

    (newNode->data) = val; // Define o tipo de dados do nodo
    newNode->next = NULL;

    if (s->top == NULL){ // No caso em que a stack está vazia
        newNode->before = NULL;
        s->top = newNode;
        s->beg = newNode;
        return;
    }
    ligaNodo(newNode, s->top);
    s->top = newNode;
}

/**
 * @brief Liga dois nodos 
 * @param n nodo a ligar 
 * @param b nodo a ligar 
 */
void ligaNodo(NODE *n, NODE *b){ //next e before
    n->before = b;
    b->next = n;
}


/**
 * \brief Retira um elemento do topo da pilha
 * Liberta (em memória) o último nodo da lista ligada
 * @param s Apontador para a Stack
 * @return int O valor no topo da pilha
 */
DADOS POPsemFREE(STACK *s){
    DADOS r;

    assert (s->top != NULL); // Se a stack estiver vazia não faz sentido fazer POP
    r = s->top->data; // r é o valor que se encontra no topo da stack
    if (s->top->before == NULL){
        INIT(s);
    } else {
        s->top->before->next = NULL;
        s->top = s->top->before;
    }
    return r;
}

/**
 * \brief Retira um elemento do topo da pilha
 * Liberta (em memória) o último nodo da lista ligada
 * @param s Apontador para a Stack
 * @return int O valor no topo da pilha
 */
DADOS POP(STACK *s){
    DADOS r;

    assert (s->top != NULL); // Se a stack estiver vazia não faz sentido fazer POP
    r = s->top->data;
    if (s->top->before == NULL){
        free(s->top);
        INIT(s);
    } else {
        s->top->before->next = NULL;
        free(s->top);
        s->top = s->top->before;
    }
    return r;
}

/**
 * @brief Função que apaga a stack e liberta memória 
 * @param s Apontador para a Stack
 */
void popStack(STACK *s){
    NODE *p = s->top;

    while (p!=NULL){
        p = p->before;
        POP(s);
    }
    INIT(s);
    free(s);
}

/**
 * @brief Função que apaga (2) a stack e liberta memória 
 * @param s Apontador para a Stack
 */
void freeStack2(STACK *s){
    NODE *p = s->top;

    while (p->before != NULL){
        p = p->before;
        freeNode(p->next);
    }
    freeNode(p);
    free(s);
}

/**
 * @brief Função que apaga a informação
 * @param s Apontador para o nodo
 */
void freeNode(NODE *p){
    TIPO t = p->data.tipo;

    if(t == STRING){
        free(p->data.DATA.STRING);
    }
    else if(t == POINTER){
        freeStack2(p->data.DATA.sAp);
    }
}

/**
 * \brief Duplica o valor do topo da Stack
 * @param s Apontador para a Stack
 */
void DUPLICATE (STACK *s){
    if(s->top == NULL) return;
    DADOS X = s->top->data;
    PUSH (X,s);
}

/** 
 * \brief Esta função recebe um índice e copia o elemento do indice seguinte para o endereço do indice especificado.
 * Índice incrementado a cada interação
 * Quando for encontrado o indice, copia o valor para o topo da stack
 * @param s Apontador para a Stack  
 * @param n Indice do elemento a ser copiado
 * @param cnt Contador que controla se uma operação já foi realizada
 */ 
void COPYnELEMENT (STACK *s, int *cnt) { 
    if(s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG) return;
    DADOS n = POP(s);
    NODE *p = s->top;
    int indice = 0; 
    while (indice != n.DATA.LONG) { 
        indice ++; p = p->before;
    } 
    PUSH (p->data,s); 
    *cnt = 0;
}

/**
 * \brief Esta função roda 3 elementos no topo da stack
 * Usamos uma variável temporária para guardar o primeiro valor a ser trocado
 * Coloca o elemento a apontar para o elemento que se encontra em cima na stack, repetindo-se para os próximos 2 elementos
 * @param s Apontador para a Stack
 */
void ROTATE (STACK *s){
    if(s->top == NULL || s->top->before == NULL || s->top->before->before == NULL) return;

    DADOS d1 = s->top->data, d2 = s->top->before->data, d3 = s->top->before->before->data;
    s->top->before->before->data = d2;
    s->top->before->data = d1;
    s->top->data = d3;
}

/**
 * \brief Esta função troca 2 elementos no topo da Stack
 * Usamos uma variável temporária para guardar o primeiro valor a ser trocado
 * Coloca o elemento a apontar para o valor que se encontra em cima na Stack
 * @param s Apontador para a Stack
 */
void SWAP (STACK *s){
    if(s->top == NULL || s->top->before == NULL) return;
    DADOS d1 = s->top->data, d2 = s->top->before->data;
    s->top->data = d2;
    s->top->before->data = d1;
}

/**
 * @brief Imprime a Stack
 * Percorre a lista ligada até encontrar NULL e vai imprimindo a data em cada nodo
 * @param s Apontador para a Stack
 * @param arr Apontador para a array
 */
/*
void DEBUGGER (STACK *s){
    NODE *p = s->top;
    goToBegin(&p);
    while (p != NULL){ 
        PRINT_TIPO(p->data);
        p = p->next; 
    } 
    printf ("Topo = "); PRINT_TIPO (s->top->data);
    putchar('\n');
}*/

/**
 * @brief Função que retorna o tamanho da stack 
 * @param s Apontador para a Stack
 */
int tamanho(STACK *s){
    int i=1;
    NODE *p = s->beg;

    while (p != NULL){
        i++;
        p = p -> next;
    }
    return i;
}

/**
 * @brief Função que retorna o tamanho da stack 
 * @param s Apontador para a Stack
 * @param n Nodo a ser removido 
 */
void removeNode(STACK *s, NODE *n){
    NODE *p = s->beg;

    if (n == s->beg){
        s->beg = n->next;
        free(n);
    } else if (n == s->top){
        s->top = n->before;
        free(n);
    } else {
        while (p != n) p = p->next;
        ligaNodo(p->next, p->before);
        free(p);
    }
}

/**
 * @brief Função que copia a stack 
 * @param orig Apontador para a Stack original 
 */
STACK* copyStack(STACK *orig){
    NODE *p=orig->beg;
    if (orig != NULL){
        STACK *dest;
        dest = malloc(sizeof(STACK));
        INIT(dest);

        while(p != NULL){
            PUSH(p->data, dest);
            p = p->next;
        }
        return dest;
    }
    else return NULL;
}

/**
 * @brief Função que converte string para array 
 * @param string Apontador para a string
 * 
 * @return Array que contém a string
 */
STACK *stringToArr(char *string){
    STACK *array = malloc(sizeof(STACK));
    DADOS d;
    int i = 0;

    d.tipo = CHAR; // Tratamos uma lista de carateres como uma lista de inteiros neste caso

    while(string[i] != '\0'){
        d.DATA.CHAR = string[i];
        PUSH(d, array);
        i++;
    }
    return array;
}

/**
 * @brief Função que converte array para string
 * @param array Apontador para o array
 * 
 * @return String que contém o array
 */
char *arrToString(STACK *array){
    NODE *p = array->beg;
    char *newString = (char *) malloc(tamanho(array)*sizeof(char));
    int i = 0;

    while(p != NULL){
        newString[i] = (p->data.DATA.CHAR);
        i++;
        p = p->next;
    }
    return newString;
}
