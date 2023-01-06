 /**
  * @file parser.h 
  * @brief Este ficheiro contém as funções do parser.c
  */

#ifndef _PARSERH_
#define _PARSERH_

#include "Stack.h"

/**
 * \brief Esta é a função que faz o parse de uma linha
 * Cria uma stack caso a stack atual seja NULL
 * @param line Linha lida que vai ser interpretada e colocada na stack
 * @param init_stack Apontador para a stack inicial (pode ser NULL)
 * @return O apontador para a stack depois das operações todas
 */
STACK *eval(char *line, STACK *init_stack, DADOS varArray[]);

/**
 * \brief Esta é a função que vai lidar com os comandos relacionados com os arrays
 * @param token A palavra que está a ser lida
 * @param s Apontador para a stack do programa
 */
void handle_arrays(char *token, STACK* s, int *cnt);

void handle_strings(char *token, STACK *s, int *cnt);

/**
 * \brief Esta é a função que vai lidar com as conversões
 * @param token O comando que indica a conversão (i,f,c,s)
 * @param s Apontador para a stack do programa
 */
void handle_conversoes(char token, STACK *s, int *cnt);


/**
 * \brief Esta é a função que vai lidar com a manipulação da stack
 * @param op Operador que indica a operação de manipulação a realizar (_, ;, \, @ ou $)
 * @param s Apontador para a stack do programa
 */
void handle_stackmanipulation (char op, STACK *s, int *cnt);
void handleEop(STACK *s, char *op);
 /**
  * @brief Função que lida com as operações lógica
  * @param s Apontador para a Stack do programa
  * @param op apontador para o token
  */
void handle_Logic(char *op, STACK *s, int *cnt);

 /**
  * @brief Função que lida com as operações unárias
  * @param op Operação a ser realizada
  * @param s Apontador para a Stack do programa
  */
void handle_uni(char op, STACK *s, int *cnt);

void simpleOperations(char op, STACK *s);
void division(STACK *s);
void power(STACK *s);

/**
* \brief Esta é a função que lida com as operações aritméticas 
* @param op Apontador para o token
* @param s Apontador para a Stack do programa
*/
void handle_arithmetic (char *op, STACK *s, int *cnt);

/**
* \brief Esta é a função que coloca na stack os valores (dos vários tipos: inteiro, double, etc)
* @param token Valor a ser colocado eventualmente na stack
* @param s Apontador para a Stack do programa
*/
void autoPush(char *token, STACK *s, int *cnt);

/**
 * @brief Lê uma linha e coloca o seu valor na stack
 * @param op Carater do primeiro elemento do token
 * @param s Apontador para um apontador para a stack (necessário para passar por referência)
 */
void leLinha(char op, STACK *s, DADOS* varArray, int *cnt);
void handle_readline(char op, STACK *s, int *cnt);

/**
 * \brief Esta função recebe a string que ainda não foi lida e decidi onde colocar os valores
 * @param mode Pode ser 0, se quiseremos mandar para a stack ou 1, se quisermos mandar para um array
 * @param s Apontador para a stack do programa
 * @param token Palavra atual
 * @param line Linha que vem depois da palavra atual
 * @param resto Apontador para a string que fica depois destas operações serem feitas
 * @return O array sob a forma de string
 */
void push_where(STACK *s, char* token, char **resto, DADOS* varArray, int *cnt);

void handle_vars(STACK *s, char *token, DADOS* varArray, int *cnt);

void handle_blocks(char *token, STACK *s, int *cnt, DADOS* varArray);

#endif
