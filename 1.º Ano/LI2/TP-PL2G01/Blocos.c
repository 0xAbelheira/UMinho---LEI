/**
 * @file blocos.c 
 * @brief Este ficheiro contém as funções que operam blocos
 */

#include "Stack.h"
#include "Parser.h"
#include "Arrays.h"
#include "Blocos.h"
#include "Logic.h"
#include "Strings.h"
#include "macros.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/**
 * @brief Tira as chavetas de um bloco
 * @param string Apontador para a string que contém o bloco com chavetas
 * @return Apontador para a string que contém o bloco sem as chavetas
 */
char* retiraChaveta(char *string){
    int len = strlen(string);
    char *result = (char *) malloc((len-1)*sizeof(char));

    strcpy(result, string+1);
    result[len-2] = '\0';

    return result;
}

/**
 * @brief Executa um bloco
 * @param s apontador para a Stack
 * @param varArray Array com as variáveis
 */
void execute(STACK *s, DADOS* varArray){
    DADOS blocoDados = POP(s);
    char *bloco = retiraChaveta(blocoDados.DATA.STRING);

    eval(bloco, s, varArray);    
}

/**
 * @brief Aplica um bloco a um array
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
void mapA(STACK *s, DADOS* varArray){
    DADOS blocoDados = POP(s);
    STACK *array = POP(s).DATA.sAp;
    NODE *p = array->beg;
    NODE *prev;
    char *bloco = retiraChaveta(blocoDados.DATA.STRING);
    char *blocoModificado = (char *) malloc(strlen(bloco)*sizeof(char)); // Evitar usar execute e evitamos muitos mallocs

    STACK *newStack = malloc(sizeof(STACK)); // Alocar memória para a nova stack
    INIT(newStack);

    while(p!=NULL){                         // Aplicar o ciclo até p ser NULL
        strcpy(blocoModificado, bloco);
        PUSH(p->data, newStack);
        eval(blocoModificado, newStack, varArray);
        prev = p;
        p=p->next;
        free(prev);
    }

    DADOS push;             // Criar novo DADO para dar push na stack original
    push.tipo = POINTER;
    push.DATA.sAp = newStack;

    PUSH(push, s);
}

/**
 * @brief Aplica um bloco a uma string
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
void mapS(STACK *s, DADOS *varArray){
    DADOS blocoDados = POP(s);
    char *string = POP(s).DATA.STRING;
    STACK *arrayChars = stringToArr(string);

    free(string);

    DADOS newArray;
    newArray.tipo = POINTER;
    newArray.DATA.sAp = arrayChars;

    PUSH(newArray, s);
    PUSH(blocoDados, s);

    mapA(s, varArray);

    char *newString = arrToString(s->top->data.DATA.sAp);
    freeStack(s->top->data.DATA.sAp);
    //freeNode(s->top); //Apaga a informaç
    POP(s);

    DADOS push;
    push.tipo = STRING;
    push.DATA.STRING = newString;

    PUSH(push, s);

}

/**
 * @brief Aplica um bloco a um array/string
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
void map(STACK *s, DADOS* varArray){
    if (s->top->before->data.tipo == POINTER){ // Caso estejamos perante o map
        mapA(s, varArray);
    } 
    else if (s->top->before->data.tipo == STRING) { // Se não estivermos perante um caso de map (não é um array)
        mapS(s, varArray);
    }
}

/**
 * @brief Aplica um bloco a um array, dois a dois elementos
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
void fold(STACK *s, DADOS *varArray){
    DADOS blocoDados = POP(s), newArr;
    STACK *array = POP(s).DATA.sAp;
    NODE *p = array->beg;
    STACK *aux = malloc(sizeof(STACK));

    PUSH(p->data, aux);
    p = p->next;

    while(p != NULL){
        PUSH(p->data,aux);
        PUSH(blocoDados, aux);
        execute(aux, varArray);
        p = p->next;
    }

    freeStack(array);
    newArr.tipo = POINTER;
    newArr.DATA.sAp = aux;
    PUSH(newArr, s);

}

/**
 * @brief Filtra um array utilizando um bloco
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
void filterA(STACK *s, DADOS *varArray){
    DADOS blocoDados = POP(s);
    STACK *array = POP(s).DATA.sAp;
    NODE *p = array->beg;

    STACK *newStack = malloc(sizeof(STACK)); // Alocar memória para a nova stack
    INIT(newStack);

    while(p!=NULL){
        PUSH(p->data, newStack);
        PUSH(blocoDados, newStack);        
        execute(newStack, varArray);
        DADOS temp = POP(newStack);
        if(logVal(temp)){ // Se o valor logico for diferente de zero meto o valor na nova stack;
            PUSH(p->data, newStack);
        }
        p=p->next;
    }

    DADOS push;             // Criar novo DADO para dar push na stack original
    push.tipo = POINTER;
    push.DATA.sAp = newStack;

    PUSH(push, s);
}

/**
 * @brief Filtra uma string utilizando um bloco
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
void filterS(STACK *s, DADOS *varArray){
    s->top->before->data.tipo = POINTER;
    s->top->before->data.DATA.sAp = stringToArr(s->top->before->data.DATA.STRING);

    filterA(s, varArray);

    DADOS temp = POP(s);
    char *newString = arrToString(temp.DATA.sAp);
    freeStack(temp.DATA.sAp);

    DADOS push;
    push.tipo = STRING;
    push.DATA.STRING = newString;

    PUSH(push, s);
}

/**
 * @brief Decide se o filter é com arrays ou com strings
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
void filter(STACK *s, DADOS *varArray){
    if (s->top->before->data.tipo == POINTER){ // Caso estejamos perante o map
        filterA(s, varArray);
    } 
    else if (s->top->before->data.tipo == STRING) { // Se não estivermos perante um caso de map (não é um array)
        filterS(s, varArray);
    }
}

/**
 * @brief Aplica um bloco a um DADO
 * @param d Dado onde vai ser aplicado o bloco
 * @param bloco Dado que contém a string do bloco
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
DADOS applyBlock(DADOS d, DADOS bloco, STACK *s, DADOS *varArray){
    PUSH(d, s);
    PUSH(bloco, s);
    execute(s, varArray);
    return(POP(s));
}

/**
 * @brief Ordena utilizando um bloco
 * @param s Apontador para a stack
 * @param varArray Array com as variáveis
 * @return Vazio
 */
void sort(STACK *s, DADOS *varArray){
    DADOS blocoDADOS = POP(s);
    DADOS array = POP(s);
    int len = arrayLength(array.DATA.sAp);
    DADOS *arr = linkedListToArray(array.DATA.sAp);

    DADOS newStack;
    MAKE_STACK(newStack);
    INIT(newStack.DATA.sAp);

    /*int i, j, m;
    for(j=0; j<len-1; j++){
        m = j;
        for(i=j+1; i<len; i++){
            DADOS d1 = applyBlock(arr[i], blocoDADOS, newStack.DATA.sAp, varArray);
            DADOS d2 = applyBlock(arr[m], blocoDADOS, newStack.DATA.sAp, varArray);
            if(compare(d1,d2)<0) m=i;
        }
        swap(&arr[m], &arr[j]);
    } // array ordenado*/
    int i, j;
    for (i = 0; i < len-1; i++)
        for (j = 0; j < len-i-1; j++){
            DADOS d1 = applyBlock(arr[j], blocoDADOS, newStack.DATA.sAp, varArray);
            DADOS d2 = applyBlock(arr[j+1], blocoDADOS, newStack.DATA.sAp, varArray);
            if (compare(d1,d2) > 0) swap(&arr[j], &arr[j+1]);
        }

    for(int i=0; i<len; i++) PUSH(arr[i], newStack.DATA.sAp);
    PUSH(newStack, s);    
}

/**
 * @brief Função que executa um bloco enquanto o topo da stack for um truthy
 * @param s Apontador para a Stack do programa
 * @param varArray Array com as variáveis
 */ 
void truthyExecute (STACK *s, DADOS *varArray){
    NODE *p = s->top;
    NODE *temp;
    DADOS block = p ->data;
    DADOS aux;
    do{
        execute (s, varArray);
        PUSH(block, s);
        temp = s->top->before;
        aux = s->top->before->data;
        //p = p->before;
        ligaNodo(temp->next, temp->before);
        free (temp); // liberta o nodo que foi sujeito à condição
    } while (logVal (aux) != 0); // enquanto devollver truthy
    POP(s);
}

