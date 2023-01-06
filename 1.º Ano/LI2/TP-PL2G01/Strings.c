/**
 * @file strings.c 
 * @brief Este ficheiro contém as funções que operam sobre strings
 */
#include "Stack.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/**
 * @brief Esta função concatena strings
 * @param s apontador para a stack
 */
void concatStringsSS(STACK *s){
    char *string1 = s->top->before->data.DATA.STRING, *string2 = s->top->data.DATA.STRING;
    int len1 = strlen(string1), len2 = strlen(string2);
    char *result = (char *) malloc((len1+len2)*sizeof(char));
    
    strcpy(result, string1);
    strcat(result, string2);
    POP(s);
    //free(string1); free(string2);
    s->top->data.DATA.STRING = result;
}

/**
 * @brief Esta função concatena uma string e um carater
 * @param s apontador para a stack
 */
void concatStringsSC(STACK *s){
    char *string = s->top->before->data.DATA.STRING;
    int len = strlen(string);
    char *result = (char *) malloc((len+1)*sizeof(char));

    strcpy(result, string);
    result[len] = s->top->data.DATA.CHAR;
    free(string);
    s->top->before->data.DATA.STRING = result;
    POP(s);
}

/**
 * @brief Esta função concatena um carater e uma string
 * @param s apontador para a stack
 */
void concatStringsCS(STACK *s){
    char *string = s->top->data.DATA.STRING;

    POP(s);

    int len = strlen(string);
    char *result = (char *) malloc((len+1)*sizeof(char));

    result[0] = s->top->data.DATA.CHAR;
    result[1] = '\0';
    strcat(result, string);
    free(string);
    s->top->data.tipo = STRING;
    s->top->data.DATA.STRING = result;
}

/**
 * @brief Esta função concatena strings
 * Cobre o caso onde concatenamos uma string com outra string, uma string com um char e ainda um char com uma string
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void concatStrings(STACK *s, int *cnt){
    if(*cnt == 0) return;
    if(s->top == NULL || s->top->before == NULL || (s->top->data.tipo != STRING && s->top->before->data.tipo != STRING)) return;
    if(s->top->data.tipo == STRING && s->top->before->data.tipo == STRING){
        concatStringsSS(s);
        *cnt = 0;
    }
    else if(s->top->before->data.tipo == STRING && s->top->data.tipo == CHAR){
        concatStringsSC(s);
        *cnt = 0;
    }
    else if(s->top->before->data.tipo == CHAR && s->top->data.tipo == STRING){
        concatStringsCS(s);
        *cnt = 0;
    }
}

/**
 * @brief Esta função separa uma string pelos delimitadores especificados
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 * @param delims apontador para a string dos delimitadores 
 */
 void separateByDelims (STACK *s, int *cnt, char *delims) { // N/ operator - s é a stack original
    if (s->top == NULL) return;
    char *string = (char *) malloc(strlen(s->top->data.DATA.STRING)*sizeof(char));
    strcpy(string, s->top->data.DATA.STRING); // aponta para a string a ser separada
    char *resto;
    STACK *newArray = malloc(sizeof(STACK));
    INIT(newArray);
    DADOS P;

    P.tipo = POINTER;
    P.DATA.sAp = newArray;
    POP(s);

    for (char *token = strtok_r(string, delims, &resto); token != NULL; token = strtok_r(NULL, delims, &resto)){
        DADOS Z;
        Z.tipo = STRING;
        Z.DATA.STRING = token;
        PUSH (Z, newArray);
    }
    PUSH(P,s);
    *cnt = 0;
}	

/**
 * @brief Esta função separa uma string pelas strings especificadas 
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void separateByString(STACK *s, int *cnt){
    if(s->top == NULL || s->top->before == NULL) return;

    DADOS d1 = POP(s), d2 = POP(s), newPointer;

    newPointer.tipo = POINTER;
    newPointer.DATA.sAp = malloc(sizeof(STACK));
    INIT(newPointer.DATA.sAp);

    char *string = d2.DATA.STRING; // aponta para a string a ser separada
    char *delimiter = d1.DATA.STRING;
    char *pointer = strtok (string,delimiter);

    while (pointer != NULL){
        char *result = strdup(pointer);
        DADOS Z;
        Z.tipo = STRING;
        Z.DATA.STRING = result;
        PUSH (Z, newPointer.DATA.sAp);
        pointer = strtok (NULL, delimiter);
    } 

    PUSH(newPointer, s);

    *cnt = 0;    
}

/**
 * @brief Esta função auxilia à concatenação de uma string ou arrays várias vezes
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void multString(STACK *s, int *cnt){
    if(*cnt == 0) return;
    if(s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG || s->top->before->data.tipo != STRING) return;

    DADOS d = POP(s);
    char *string = s->top->data.DATA.STRING;
    int i, len = strlen(string);
    char *result = (char *) malloc((len*d.DATA.LONG)*sizeof(char));

    for(i=0; i<d.DATA.LONG; i++){
        strcat(result, string);
    }

    s->top->data.DATA.STRING = result;

    *cnt = 0;
}

/**
 * @brief Esta função determina o tamanho ou range 
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void commaOperatorString(STACK *s, int *cnt){
    if(*cnt == 0) return;
    if(s->top == NULL || s->top->data.tipo != STRING) return;

    int len = strlen(s->top->data.DATA.STRING);

    s->top->data.tipo = LONG;
    s->top->data.DATA.LONG = len;

    *cnt = 0;
}

/**
 * @brief Esta função vai buscar um valor por índice
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void byIndexString(STACK *s, int *cnt){
    if(*cnt == 0) return;
    if(s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG || s->top->before->data.tipo != STRING) return;

    DADOS d1 = POP(s), d2 = POP(s);

    d1.tipo = CHAR;
    d1.DATA.CHAR = d2.DATA.STRING[d1.DATA.LONG];

    PUSH(d1,s);

    *cnt = 0;
}
/**
 * @brief Esta função executa  operador < para strings
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void getFirstChars(STACK *s, int *cnt){
    char *string, *result;
    int i, ln;
    DADOS d;

    if(*cnt == 0) return;
    if(s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG || s->top->before->data.tipo != STRING) return;

    ln = POP(s).DATA.LONG;                                       // Comprimento que pretendemos
    string = POP(s).DATA.STRING;                                 // String onde procuramos
    result = (char *) malloc((ln+1)*sizeof(char));          // String resultado

    for(i = 0; i<ln; i++) result[i] = string[i];            // Até chegar ao índice pretendido vai passando para a string resultado
    result[i] = '\0';                                       // Para terminar a string resultado

    free(string);                                           // Liberta a string antiga

    d.tipo = STRING;
    d.DATA.STRING = result;                                      // Cria uma nova string e coloca na stack
    PUSH(d, s);

    *cnt = 0;
}

/**
 * @brief Esta função executa  operador > para strings
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void getLastChars(STACK *s, int *cnt){
    DADOS d;
    char *string, *result;
    int i, N;

    if(*cnt == 0) return;
    if(s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG || s->top->before->data.tipo != STRING) return;

    d = POP(s);                                                     // Valor que contém a quantidade de carateres a ir buscar
    string = POP(s).DATA.STRING;                                         // String onde vamos buscar os carateres
    N = strlen(string);                                             // Calcular o tamanho da string
    i = N-d.DATA.LONG - 1;                                               // Posição onde vamos começar a buscar carateres
    result = (char *) malloc((d.DATA.LONG+1) * sizeof(char));                     // Ver se não houve erros na alocação de memória

    while(i <= N){                                                  // Até chegar ao fim da string
        result[i-(N-d.DATA.LONG)] = string[i];                           // Copia da string original para a nova e incrementa o i
        i++;
    }    

    free(string);                                                   // Liberta a string antiga

    d.tipo = STRING;
    d.DATA.STRING = result;                                              // Cria uma nova string e coloca na stack
    PUSH(d, s);

    *cnt = 0;
}

/**
 * @brief Remove o primeiro caracter da string no topo da stack 
 * 
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void removeFirstString(STACK *s, int *cnt){
    if(*cnt == 0) return;
    if(s->top==NULL || s->top->data.tipo != STRING) return;

    DADOS d;
    char *string = s->top->data.DATA.STRING;
    int len = strlen(string);
    char *result = (char *) malloc((len-1)*sizeof(char));
    
    strcpy(result, string+1);

    s->top->data.DATA.STRING = result;
    d.tipo = CHAR;
    d.DATA.CHAR = string[0];

    PUSH(d,s);
    *cnt = 0; 
}

/**
 * @brief Remove o último caracter da string no topo da stack 
 * 
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void removeLastString(STACK *s, int *cnt){
    if(*cnt == 0) return;
    if(s->top==NULL || s->top->data.tipo != STRING) return;

    DADOS d;
    char *string = s->top->data.DATA.STRING;
    int len = strlen(string);

    d.tipo = CHAR;
    d.DATA.CHAR = string[len-1];
    string[len-1] = '\0';

    PUSH(d,s);
    *cnt = 0;
}

/**
 * @brief Procura uma subtring na string.
 * Pega nas duas últimas strings e procura a primeira ocorrência da segunda string na primeira
 * 
 * @param s apontador para a stack
 */
void procSubstr(STACK *s, int *cnt){
    DADOS X, Y, R;
    char* res;
    if (*cnt == 0 || s->top == NULL || s->top->before == NULL || (s->top->data.tipo != STRING && s->top->data.tipo != CHAR) || s->top->before->data.tipo != STRING) return;

    Y = POP(s);
    X = POP(s);
    
    R.tipo = LONG;

    res = (Y.tipo == CHAR) ? strchr(X.DATA.STRING, Y.DATA.CHAR) : strstr(X.DATA.STRING, Y.DATA.STRING);
    R.DATA.LONG = (res == NULL) ? (-1) : (res-X.DATA.STRING);

    free(X.DATA.STRING);
    if (Y.tipo == STRING) free(Y.DATA.STRING);
    PUSH(R, s);
    *cnt = 0;
}
