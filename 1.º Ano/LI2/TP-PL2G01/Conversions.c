/** 
 * @file auxParser.c
 * @brief Funções de auxílio ao parser
 */
#include "Conversions.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/** 
 * \brief Converte qualquer tipo dentro dos DADOS para o tipo Double
 * @param X variável com um tipo dentro de DADOS que será convertida para Double
 */ 
void convertToDouble (DADOS *X){
    switch (X->tipo){
        case LONG : X->DATA.DOUBLE = X->DATA.LONG; break;
        case CHAR : X->DATA.DOUBLE = X->DATA.CHAR; break;
        case STRING : X->DATA.DOUBLE = atof(X->DATA.STRING); break;
        default : break;
    }
    X->tipo = DOUBLE;
}

/**
 * \brief Converte qualquer tipo dentro dos DADOS para o tipo Long
 * @param X variável com um tipo dentro de DADOS que será convertida para Long
 */
void convertToLong (DADOS *X){
    switch (X->tipo){
        case LONG : break;
        case DOUBLE : X->DATA.LONG = X->DATA.DOUBLE; break;
        case CHAR :   X->DATA.LONG = X->DATA.CHAR; break;
        case STRING : X->DATA.LONG = atoi(X->DATA.STRING); break;
        default : break;
    }
    X->tipo = LONG;
}

/** 
 * \brief Converte qualquer tipo dentro dos DADOS para o tipo string
 * @param X variável com um tipo dentro de DADOS que será convertida para String
 */ 
void convertToString (DADOS *X){
    char str[50];                                                                                                       
    switch ((*X).tipo){                                                                                                 
            case LONG : sprintf (str,"%ld", (*X).DATA.LONG); (*X).tipo = STRING; (*X).DATA.STRING = strdup (str); break;          
            case DOUBLE : sprintf (str, "%g", (*X).DATA.DOUBLE); (*X).tipo = STRING; (*X).DATA.STRING = strdup (str); break;      
            case CHAR : sprintf (str,"%c", (*X).DATA.CHAR); (*X).tipo = STRING; (*X).DATA.STRING = strdup (str); break;           
            case STRING: sprintf (str,"%s", (*X).DATA.STRING); (*X).tipo = STRING; (*X).DATA.STRING = strdup (str); break;  
            default : break;      
    }   
} 

/** 
 * \brief Converte qualquer tipo dentro dos DADOS para o tipo Char
 * @param X variável com um tipo dentro de DADOS que será convertida para Char
 */ 
void convertToChar (DADOS *X){
    switch ((*X).tipo){
        case LONG : X->DATA.CHAR = X->DATA.LONG; break;
        case DOUBLE : X->DATA.CHAR = X->DATA.DOUBLE; break;
        default : break;
    }
    X->tipo = CHAR;
}

/**
 * \brief Função que converte duas variáveis para o maior tipo entre as duas
 * @param X variável que é recebida para conversão do maior tipo entre elas
 * @param Y variável que é recebida para conversão do maior tipo entre elas
 * @return retorna uma variável r, com o tipo correspondente ao maior entre as variáveis X e Y
 */
TIPO convertToLargest (DADOS *X, DADOS *Y){ // falta caso das strings
    TIPO r;
    if((*X).tipo == DOUBLE || (*Y).tipo == DOUBLE){ /*o tipo double é o maior entre os que definimos*/
        convertToDouble(X); convertToDouble(Y);
        r = DOUBLE;
        return r;
    }
    else if ((*X).tipo == LONG || (*Y).tipo == LONG){ /* a seguir ao double, o maior tipo é o long integer*/
        convertToLong(X); convertToLong(Y);
        r = LONG;
        return r;
    }
    else if ((*X).tipo == POINTER && (*Y).tipo == POINTER) return POINTER;
    else if ((*X).tipo == STRING && (*Y).tipo == STRING) return STRING;
    else { convertToChar(X); convertToChar(Y);r = CHAR; return r;} /* caso nenhuma das variáveis seja do tipo double ou long o resultado será apresentado em char*/   
}




