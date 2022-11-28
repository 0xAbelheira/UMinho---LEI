/** 
 * @file arraysParser.h
 * @brief Funções de auxílio ao parser relacionadas com os arrays
 */
#ifndef _LOGICh_
#define _LOGICh_

#include "Stack.h"

/**
 * @brief Esta função inicia o array que contém todas as variáveis.
 * @param s apontador para o array
 * @return Vazio
 */
void INIT_VAR (DADOS varArr[]);


/**
 * @brief Esta função copia o topo da stack para o respetivo indice no array
 * @param c token que vamos verificar
 * @param arr array de DADOS que contém as variáveis
 * @param s apontador para a stack
 * @return Vazio
 */
void VAR_COPY (char c, DADOS arr[], STACK *s);

/**
 * @brief Operação Not. Lê o valor do tipo e troca para 1 se for 0 e para 0 se for diferente de 0.
 * @param s apontador para a stack
 */

void NOT (STACK *s);

/**
 * @brief Função que faz o if then else lógico.
 * @param s apontador para a stack
 */
void IfThenElse (STACK *s);

/**
 * @brief Compara os dois valores de cima da stack
 * @param s apontador para o topo da stack
 * @return 0 se forem iguais, <0 se X for menor que Y e >0 se Y for menor que X  
 */
double compareTop (STACK *s);

/**
 * @brief Compara dois valores
 * @param X apontador para o primeiro valor
 * @param Y apontador para o segundo valor
 * @return 0 se forem iguais, <0 se X for menor que Y e >0 se Y for menor que X
 */
double compare(DADOS X, DADOS Y);

/**
 * @brief Compara duas stack
 * @param s1 apontador para a primeira stack
 * @param s2 apontador para a segunda stack
 * @return 0 se forem iguais, <0 se s1 for menor que s2 e >0 se s2 for menor que s1
 */
double compareStacks(STACK *s1, STACK *s2);

int logVal(DADOS D);

void questionOperator(STACK *s);

void selectGreater(STACK *s);

void selectLesser(STACK *s);


#endif
