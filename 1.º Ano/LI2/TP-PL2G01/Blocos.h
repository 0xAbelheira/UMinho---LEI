/**
 * @file blocos.h
 * @brief Este ficheiro contém as funções que operam sobre blocos
 */
#include "Stack.h"


/**
 * @brief Executa um bloco
 * @param s apontador para a Stack
 */
void execute(STACK *s, DADOS* varArray);

/**
 * @brief Aplica um bloco a um array/string
 * @param s apontador para a Stack
 */
void map(STACK *s, DADOS* varArray);

DADOS applyBlock(DADOS d, DADOS bloco, STACK *s, DADOS *varArray);

/**
 * @brief Faz fold de um array utilizando a operação especificada no bloco
 * @param s apontador para a Stack
 */
void fold(STACK *s, DADOS* varArray);

void filterA(STACK *s, DADOS* varArray);

void filterS(STACK *s, DADOS *varArray);

void filter(STACK *s, DADOS *varArray);

void sort(STACK *s, DADOS *varArray);

void truthyExecute (STACK *s, DADOS* varArray);

