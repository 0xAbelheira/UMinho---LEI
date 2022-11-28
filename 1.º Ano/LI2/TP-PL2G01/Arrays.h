#ifndef _ARRAYSH_
#define _ARRAYSH_

#include "Stack.h" 

/**
 * \brief Esta função recebe uma linha e seleciona o array que se encontra a seguir
 * @param line Linha que foi lida
 * @param rest Apontador para a string que contém o resto (que ainda não foi lido)
 * @return O array sob a forma de string
 */
char *get_delimited(char **rest, char *delims);
char *get_delimitedS(char *token, char **rest, char *delims);

/**
 * @brief Cria uma stack com o array range
 * @param tam Tamanho do array range
 * @return O apontador para a stack criada
 */
STACK *createRange(long tam);
int arrayLength(STACK *s);

/**
 * @brief Esta função implementa o operador ,
 * Tanto pode trabalhar com arrays como com inteiros
 * @param s apontador para a stack
 */
void commaOperator(STACK *s, int *cnt);

void freeStack(STACK *s);
void arrayToStack (STACK *s, int *cnt);
void pushStack (STACK *s1, STACK *s2);
void concat (STACK *s, int *cnt);
void multArray (STACK *s, int *cnt);

/**
 * @brief Procura uma subtring na string.
 * Pega nas duas últimas strings e procura a primeira ocorrência da segunda string na primeira
 * @param s apontador para a stack
 */
void procSubstr(STACK *s, int *cnt);

/**
 * @brief Remove o primeiro valor do último array/string e coloca-o na stack
 * @param s Apontador para a stack do programa
 */
void removePrimeiro(STACK *s, int *cnt);

/**
 * @brief Remove o último valor do último array/string e coloca-o na stack
 * @param s Apontador para a stack do programa
 */
void removeUltimo(STACK *s, int *cnt);

void byIndex(STACK *s, int *cnt);

void getFirstElems(STACK *s, int *cnt);

void getLastElems(STACK *s, int *cnt);

DADOS * linkedListToArray (STACK *s);

void swap(DADOS *X, DADOS *Y);

#endif
