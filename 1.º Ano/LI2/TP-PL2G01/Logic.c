/** 
 * @file arraysParser.c
 * @brief Funções de auxílio ao parser relacionadas com os arrays
 */
#include "Logic.h"
#include "Conversions.h"
#include "macros.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "Arrays.h"


/**
 * @brief Esta função inicia o array que contém todas as variáveis.
 * @param varArr apontador para o array
 */
void INIT_VAR(DADOS varArr[]){ // Indices : 'A' = 0 ; 'B' = 1 ; 'C' = 2 ; 'D' = 3 ; 'E' = 4 ; 'F' = 5 ; 'N' = 6 ; 'S' = 7 ; 'X' = 8 ; 'Y' = 9 ; 'Z' = 10
    int i;
    for (i = 0; i <= 5; i++){
        varArr[i].tipo = LONG;
        varArr[i].DATA.LONG = 10+i;
    }

    varArr[13].tipo = CHAR; varArr[13].DATA.CHAR = '\n';
    varArr[18].tipo = CHAR; varArr[18].DATA.CHAR = ' ';

    for (i = 0; i < 3; i++){
        varArr[i+23].tipo = LONG;
        varArr[i+23].DATA.LONG = i;
    }
}

/**
 * @brief Esta função copia o topo da stack para o respetivo indice no array
 * @param c token que vamos verificar
 * @param arr array de DADOS que contém as variáveis
 * @param s apontador para a stack
 */
void VAR_COPY(char c, DADOS arr[], STACK *s){ // 'A' = 65
    arr[c-65] = s->top->data; // 65 é 'A' em ASCII
}

/**
 * @brief Operação Not. Lê o valor do tipo e troca para 1 se for 0 e para 0 se for diferente de 0.
 * 
 * @param s apontador para a stack
 */
void NOT (STACK *s){
    switch (s->beg->data.tipo){
        case LONG : s->beg->data.DATA.LONG = !s->beg->data.DATA.LONG; break; // !valor do inteiro
        case DOUBLE : s->beg->data.DATA.LONG = !s->beg->data.DATA.DOUBLE; s->beg->data.tipo = LONG; break; // !valor do double
        case CHAR : s->beg->data.DATA.LONG = !s->beg->data.DATA.CHAR; s->beg->data.tipo = LONG; break; // !valor do char
        case STRING : s->beg->data.DATA.LONG = !s->beg->data.DATA.STRING; s->beg->data.tipo = LONG; break; // !valor da string
        default : break; // caso seja outro tipo de dados
    }
}

/**
 * @brief Função que faz o if then else lógico.
 * 
 * @param s apontador para a stack
 * 
 * @return Vazio
 */
void IfThenElse (STACK *s){
    DADOS X = POP(s), Y = POP(s), Z = POP(s); // faz primeiros 3 valores do topo da stack - de forma consecutiva
    switch(Z.tipo){
        case LONG :  (Z.DATA.LONG) ? PUSH(Y,s) : PUSH(X,s); break;
        case DOUBLE : (Z.DATA.LONG) ? PUSH(Y,s) : PUSH(X,s); break;
        case CHAR : (Z.DATA.LONG) ? PUSH(Y,s) : PUSH(X,s); break;
        case STRING : (Z.DATA.LONG) ? PUSH(Y,s) : PUSH(X,s); break;
        default : break;
    }
}

/**
 * @brief Compara os dois valores de cima da stack
 * @param s apontador para o topo da stack
 * @return 0 se forem iguais, <0 se X for menor que Y e >0 se Y for menor que X  
 */
double compareTop (STACK *s){
    DADOS Y = POP(s), X = POP(s);
    return compare(X, Y);
}

/**
 * @brief Compara dois valores
 * @param X apontador para o primeiro valor
 * @param Y apontador para o segundo valor
 * @return 0 se forem iguais, <0 se X for menor que Y e >0 se Y for menor que X
 */
double compare(DADOS X, DADOS Y){ // devolve a diferença entre o valor de X e o valor de Y
    TIPO t = convertToLargest (&X,&Y); 
    double r = 1;

    switch (t){
        case LONG : r = X.DATA.LONG - Y.DATA.LONG; break; 
        case DOUBLE : r = X.DATA.DOUBLE - Y.DATA.DOUBLE; break;
        case CHAR : r = X.DATA.CHAR - Y.DATA.CHAR; break;
        case STRING : r = strcmp(X.DATA.STRING, Y.DATA.STRING); break; //Não está definido para strings
        case POINTER : r = compareStacks(X.DATA.sAp, Y.DATA.sAp); break;
    }
    return r;
}

/**
 * @brief Compara duas stack
 * @param s1 apontador para a primeira stack
 * @param s2 apontador para a segunda stack
 * @return 0 se forem iguais, <0 se s1 for menor que s2 e >0 se s2 for menor que s1
 */
double compareStacks(STACK *s1, STACK *s2){
    NODE *p1 = s1->beg; // p1 aponta para o início da stack s1
    NODE *p2 = s2->beg; // p2 aponta para o início da stack s2
    double cmp;

    if (p1 == NULL && p2 != NULL) return -1;  
    if (p1 != NULL && p2 == NULL) return 1;

    while(p1 != NULL || p2 != NULL){ // enquanto as stacks não forem vazias
        cmp = compare((p1->data), (p2->data));
        if (cmp != 0) return cmp;
        p1 = p1->next;
        p2 = p2->next;
    }

    return 0;
}

/**
 * @brief Função que devolve o valor lógico da condição verificada
 * @param D tipo de dados a ser avaliado
 * @return 0 se a condição for verdadeira retorna 1, caso contrário retorna 0
 */
int logVal(DADOS D){
    switch(D.tipo){
        case LONG: return D.DATA.LONG != 0; break;
        case DOUBLE: return D.DATA.DOUBLE != (double) 0; break;
        case CHAR: return D.DATA.CHAR != (char)0; break;
        case POINTER: return arrayLength(D.DATA.sAp)!=0; break; // For now
        case STRING: return strlen(D.DATA.STRING)!=0; break;
    }
    return 0;
}

/**
 * @brief Função que auxilia à operação do if-then-else lógico
 * @param s apontador para a stack
 */
void questionOperator(STACK *s){
    if (s->top == NULL || s->top->before == NULL || s->top->before->before == NULL) return; // se a stack estiver vazia
    DADOS Z = POP(s), Y = POP(s), X = POP(s);

    if (logVal(X) != 0) PUSH(Y, s); 
    else PUSH(Z, s);
}

/**
 * @brief Função que devolve o maior elemento entre dois valores da stack
 * @param s apontador para a stack
 */
void selectGreater(STACK *s){
    if (s->top->data.tipo == POINTER && s->top->before->data.tipo == POINTER){
    // se o valor que está no topo da stack for menor que o valor que vem a seguir na stack, devolve o valor que vem a seguir 
        if (compare(s->top->before->data, s->top->data) > 0) { popStack(s->top->data.DATA.sAp); POP(s); } 
        else { popStack(s->top->before->data.DATA.sAp); removeNode(s, s->top->before); }
    }
    else {
        DADOS Y = POP(s), X = POP(s);
        RELATIONSHORTCUT(X, Y, >);
    }
}

/**
 * @brief Função que devolve o menor elemento entre dois valores da stack
 * @param s apontador para a stack
 */
void selectLesser(STACK *s){
    if (s->top->data.tipo == POINTER && s->top->before->data.tipo == POINTER){
    // se o valor que está no topo da stack for maior que o valor que vem a seguir na stack, devolve o valor que vem a seguir 
        if (compare(s->top->before->data, s->top->data) < 0) { popStack(s->top->data.DATA.sAp); POP(s); }
        else { popStack(s->top->before->data.DATA.sAp); removeNode(s, s->top->before); }
    }
    else {
        DADOS Y = POP(s), X = POP(s);
        RELATIONSHORTCUT(X, Y, <);
    }
}
