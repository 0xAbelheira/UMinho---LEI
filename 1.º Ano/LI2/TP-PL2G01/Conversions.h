#ifndef _CONVERSIONSH_
#define _CONVERSIONSH_

#include "Stack.h"
/** 
 * \brief Converte qualquer tipo dentro dos DADOS para o tipo Double
 * @param X variável com um tipo dentro de DADOS que será convertida para Double
 */ 
void convertToDouble (DADOS *X);

/** 
 * \brief Converte qualquer tipo dentro dos DADOS para o tipo Long
 * @param X variável com um tipo dentro de DADOS que será convertida para Long
 */ 
void convertToLong (DADOS *X);

/** 
 * \brief Converte qualquer tipo dentro dos DADOS para o tipo string
 * @param X variável com um tipo dentro de DADOS que será convertida para String
 */ 
void convertToString (DADOS *X);

/** 
 * \brief Converte qualquer tipo dentro dos DADOS para o tipo Char
 * @param X variável com um tipo dentro de DADOS que será convertida para Char
 */ 
void convertToChar (DADOS *X);

/**
 * \brief Função que converte duas variáveis para o maior tipo entre as duas
 * @param X variável que é recebida para conversão do maior tipo entre elas
 * @param Y variável que é recebida para conversão do maior tipo entre elas
 * @return retorna uma variável r, com o tipo correspondente ao maior entre as variáveis X e Y
 */
TIPO convertToLargest (DADOS *X, DADOS *Y);

#endif
