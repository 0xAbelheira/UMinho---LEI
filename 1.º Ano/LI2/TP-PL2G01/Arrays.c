#include "Conversions.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "macros.h"

/**
 * \brief Esta função recebe uma linha e seleciona o array que se encontra a seguir
 * @param delims Apontador para a string dos delimitadores  
 * @param rest Apontador para a string que contém o resto (que ainda não foi lido)
 * @return a string 
 */
char *get_delimited(char **rest, char *delims)
{
    int n, i, cnt = 1;
    char *total;

    for (n = 0; cnt >= 1; n++)
    {
        if ((*rest)[n] == delims[0])
            cnt++;
        if ((*rest)[n] == delims[1])
            cnt--;
    }

    total = (char *)malloc(n * sizeof(char) + 1);
    total[n] = '\0';
    total[0] = ' ';

    for (i = 1; i < n; i++)
        total[i] = (*rest)[i - 1];

    *rest += i + 1;
    return total;
}

/**
 * \brief Esta função recebe uma linha e seleciona a string que se encontra a seguir
 * @param token Símbolo que representa a operação a ser realizada
 * @param rest Apontador para a string que contém o resto (que ainda não foi lido)
 * @param delims Apontador para a string dos delimitadores  
 * @return O array sob a forma de string
 */
char *get_delimitedS(char *token, char **rest, char *delims)
{
    int i, ln = strlen(token);
    char *total;
    if (strchr(delims, token[ln - 1]) && ln != 1)
    {
        token[ln - 1] = '\0';
        total = (char *) malloc((ln - 2) * sizeof(char));
        strcpy(total, token+1);
    }
    else
    {
        for (i = 0; !strchr(delims, (*rest)[i]); i++);
        total = (char *) malloc((ln + i + 3)*sizeof(char));
        strcpy(total, token+1);
        total[ln - 1] = ' ';
        for (i = 0; !strchr(delims, (*rest)[i]); i++)
            total[i+ln] = (*rest)[i];

        total[ln + i] = '\0';
        *rest = *rest + i + 1;
    }
    return total;
}

/**
 * @brief Cria uma stack com o array range
 * @param tam Tamanho do array range
 * @return O apontador para a stack criada
 */
STACK *createRange(long tam)
{
    STACK *s;
    DADOS d;
    int i;
    s = malloc(sizeof(STACK));
    d.tipo = LONG;
    INIT(s);

    for (i = 0; i < tam; i++)
    {
        d.DATA.LONG = i;
        PUSH(d, s);
    }
    return s;
}

/**
 * @brief Função que retorna o comprimento do array
 * @param s apontador para a stack
 */
int arrayLength(STACK *s)
{
    int r = 0;
    if (s->beg == NULL)
        return 0;
    NODE *p = s->beg;
    //if (s->beg->data.tipo == STRING && s->top->data.tipo == STRING) r = strlen(s->beg->data.STRING) -1 /*porquê?*/;

    while (p != NULL)
    {
        r++;
        p = p->next;
    }
    return r;
}

/**
 * @brief Esta função implementa o operador ,
 * Tanto pode trabalhar com arrays como com inteiros
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void commaOperator(STACK *s, int *cnt)
{
    if (*cnt == 0)
        return;
    if (s->top == NULL || s->top->data.tipo == CHAR || s->top->data.tipo == STRING || s->top->data.tipo == DOUBLE)
        return;
    NODE *top = s->top;
    DADOS d;
    //printf("AQUI");
    if (top->data.tipo == LONG)
    {
        DADOS t = POP(s);
        d.tipo = POINTER;
        d.DATA.sAp = (void *)createRange(t.DATA.LONG);
    }
    else if (top->data.tipo == POINTER)
    {
        d.tipo = LONG;
        d.DATA.LONG = arrayLength(top->data.DATA.sAp);
        POP(s);
    }
    PUSH(d, s);
    *cnt = 0;
}

/**
 * @brief Função que liberta memóra alocada na Stack
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void freeStack(STACK *s)
{
    NODE *temp, *p = s->beg;
    while (p != NULL)
    {
        temp = p;
        p = p->next;
        free(temp);
    }
}

/**
 * @brief Função que coloca um array na stack
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void arrayToStack(STACK *s, int *cnt)
{
    if (*cnt == 0)
        return;
    if (s->top->data.tipo != POINTER)
        return;
    NODE *top = s->top;
    STACK *s1 = s->top->data.DATA.sAp;
    NODE *p = s1->beg;
    POPsemFREE(s);
    if (top->data.tipo == POINTER)
    {
        while (p != NULL)
        {
            PUSH(p->data, s);
            p = p->next;
        }
    }
    freeStack(s1);
    *cnt = 0;
}

/**
 * @brief Função que coloca a Stack2 em cima da Stack1
 * @param s1 Apontador para a Stack1
 * @param s2 Apontador para a Stack2 
 */
void pushStack(STACK *s1, STACK *s2)
{
    NODE *p2 = s2->beg;
    while (p2 != NULL)
    {
        PUSH(p2->data, s1);
        p2 = p2->next;
    }
}

/**
 * @brief Função que concatena dois arrays 
 * @param s apontador para a stack
 */
void concatAA(STACK *s){
    STACK *s1 = s->top->before->data.DATA.sAp;
    STACK *s2 = s->top->data.DATA.sAp;

    DADOS newArray;
    newArray.tipo = POINTER;
    newArray.DATA.sAp = malloc(sizeof(STACK));

    pushStack(newArray.DATA.sAp, s1);
    pushStack(newArray.DATA.sAp, s2);
    POPsemFREE(s);
    POPsemFREE(s);

    PUSH(newArray, s);

    //freeStack(s2);
}

/**
 * @brief Função que concatena um array e um valor
 * @param s apontador para a stack
 */
void concatAV(STACK *s){
    DADOS temp = POP(s);
    DADOS array = POP(s);

    DADOS newArray;
    newArray.tipo = POINTER;
    newArray.DATA.sAp = malloc(sizeof(STACK));

    pushStack(newArray.DATA.sAp, array.DATA.sAp);
    PUSH(temp, newArray.DATA.sAp);

    PUSH(newArray, s);
}

/**
 * @brief Função que concatena dois arrays 
 * @param s apontador para a stack
 */
void concatVA(STACK *s){
    DADOS array = POP(s);
    DADOS temp = POP(s);

    DADOS newArray;
    newArray.tipo = POINTER;
    newArray.DATA.sAp = malloc(sizeof(STACK));

    PUSH(temp, newArray.DATA.sAp);

    pushStack(newArray.DATA.sAp, array.DATA.sAp);

    PUSH(newArray, s);
}

/**
 * @brief Função que concatena dois arrays e/ou strings 
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void concat(STACK *s, int *cnt)
{
    if (*cnt == 0 || s->top == NULL || s->top->before == NULL || (s->top->data.tipo != POINTER && s->top->before->data.tipo != POINTER))
        return;
    if (s->top->data.tipo == POINTER && s->top->before->data.tipo == POINTER)
    {
        concatAA(s);
        *cnt = 0;
    }
    else if (s->top->data.tipo == POINTER)
    { 
        concatVA(s);
        *cnt = 0;
    }
    else
    {
        concatAV(s);
        *cnt = 0;
    }
}

/**
 * @brief Função que concatena várias vezes dois arrays
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void multArray(STACK *s, int *cnt)
{
    if (*cnt == 0 || s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG || s->top->before->data.tipo != POINTER)
        return;

    int num = POP(s).DATA.LONG;
    STACK *s1 = POP(s).DATA.sAp;
    int length = arrayLength(s1) * num;
    DADOS newArray;
    NODE *p = s1->beg;
    
    newArray.tipo = POINTER;
    newArray.DATA.sAp = malloc(sizeof(STACK));

    while(length > 0)
    {
        PUSH(p->data, newArray.DATA.sAp);
        if(p->next == NULL) p = s1->beg;
        else p = p->next;
        length--; 
    }
    PUSH(newArray, s);
    *cnt = 0;
}

/**
 * \brief Remove o primeiro valor do último array/string e coloca-o na stack
 * @param s Apontador para a stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void removePrimeiro(STACK *s, int *cnt)
{
    if (*cnt == 0)
        return;
    if (s->top == NULL || s->top->data.tipo != POINTER)
        return;

    STACK *array = s->top->data.DATA.sAp;
    DADOS d = array->beg->data;
    NODE *beg = array->beg;

    array->beg = array->beg->next;
    free(beg);

    PUSH(d, s);
    *cnt = 0;
}

/**
 * \brief Remove o último valor do último array/string e coloca-o na stack
 * @param s Apontador para a stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void removeUltimo(STACK *s, int *cnt)
{
    if (*cnt == 0)
        return;
    if (s->top == NULL || s->top->data.tipo != POINTER)
        return;
    STACK *s1 = s->top->data.DATA.sAp;
    DADOS Z = POP(s1);

    PUSH(Z, s);
    *cnt = 0;
}

/**
 * @brief Função que indica um valor na stack através do seu índice
 * @param s Apontador para a stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void byIndex(STACK *s, int *cnt)
{
    int i;
    DADOS Z;
    DADOS P;
    if (*cnt == 0)
        return;
    if (s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG || s->top->before->data.tipo != POINTER)
        return;

    Z = POP(s);
    P = POP(s);
    STACK *array = P.DATA.sAp;
    NODE *n = array->beg;
    for (i = 0; i < Z.DATA.LONG; i++)
        n = n->next;
    PUSH(n->data, s);
    *cnt = 0;
    // Libertar a stack
}

/**
 * @brief Função que indica os primeiros elementos de um array ou de uma string
 * @param s Apontador para a stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void getFirstElems(STACK *s, int *cnt)
{
    if (*cnt == 0)
        return;
    if (s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG || s->top->before->data.tipo != POINTER)
        return;
    DADOS D = POP(s);
    STACK *s1 = s->top->data.DATA.sAp;
    int length = arrayLength(s1), i;
    for (i = length - D.DATA.LONG; i > 0; i--)
        POP(s1);
    *cnt = 0;
}

/**
 * @brief Função que indica os últimos elementos de um array ou de uma string
 * @param s Apontador para a stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void getLastElems(STACK *s, int *cnt)
{
    if (*cnt == 0)
        return;
    if (s->top == NULL || s->top->before == NULL || s->top->data.tipo != LONG || s->top->before->data.tipo != POINTER)
        return;
    DADOS D1 = POP(s), D2 = POP(s);
    STACK *s1 = D2.DATA.sAp, *s2 = malloc(sizeof(STACK));
    NODE *p1 = s1->beg, *p2;
    INIT(s2);
    int i, length = arrayLength(s1);
    for (i = 0; i < length - D1.DATA.LONG; i++)
        p1 = p1->next;
    p2 = p1;
    for (i = D1.DATA.LONG; i > 0; i--)
    {
        PUSH(p2->data, s2);
        p2 = p2->next;
    }
    D2.DATA.sAp = s2;
    PUSH(D2, s);
    *cnt = 0;
}

/**
 * @brief Função que transforma uma estrutura de listas ligadas num array
 * @param s Apontador para a stack 
 */
DADOS *linkedListToArray(STACK *s)
{
    int len = arrayLength(s);
    DADOS *r = malloc(len * sizeof(DADOS));
    int i;
    NODE *p = s->beg;
    for (i = 0; p != NULL; i++)
    {
        r[i] = p->data;
        p = p->next;
    }
    return r;
}

/**
 * @brief Função que troca dois valores de dados 
 * @param Apontador para o primeiro valor 
 * @param Apontador para o segundo valor 
 */
void swap(DADOS *X, DADOS *Y)
{
    DADOS temp;
    temp = *X;
    *X = *Y;
    *Y = temp;
}
