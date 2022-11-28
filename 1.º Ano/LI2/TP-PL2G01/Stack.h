/**
 * @file stack.h 
 * @brief Este ficheiro contém a struck do programa
 */

#ifndef _STACKH_
#define _STACKH_

/**
 * @brief Enum para os diferentes tipos de dados
 */
typedef enum { /** Estrutura do tipo TIPO */
    LONG, /** Dados do tipo inteiro */
    DOUBLE, /** Dados em vírgula flutuante (precisão dupla) */
    CHAR, /** Dados do tipo carater */
    STRING, /** Dados do tipo string */
    POINTER /** Dados do tipo apontador */
} TIPO; 

/**
 * \brief Estrutra do tipo DATA
 */
typedef union Data{ /** Estrutura designada para armazenar o tipo dados */
        long LONG; /** Valor do tipo long guardado na struct */
        double DOUBLE; /** Valor do tipo double guardado na struct */
        char CHAR; /** Valor do tipo char guardado na struct */
        char* STRING; /** Valor do tipo string guardado na struct */
        void* sAp; /** Valor do tipo apontador guardado na struct */
} DATA; 

/**
 * \brief Estrutura que guarda dados e informação relativa ao tipo de dados armazenada 
 */
typedef struct dados{ /** Estrutura do tipo DADOS */
    TIPO tipo; /** Tipo de dados guardado pela struct */
    DATA DATA; /** DATA union */
} DADOS;

/**
 * \brief Nodo da lista, guarda informação sobre o dado a guarda e o valor seguinte
 */
typedef struct node { /** Estrutura do tipo NODE */
	DADOS data; /** Variável do tipo DADOS que guarda o conteúdo do nodo */
	struct node *next; /** Apontador para o nodo seguinte */
    struct node *before; /** Apontador para o nodo anterior */
} NODE;

/**
 * \brief Stack - Apontador para o topo da pilha
 */
typedef struct stack { /** Estrutura do tipo STACK */
    NODE *top; /** Aponta para o topo da stack */
    NODE *beg; /** Aponta para o topo da stack */
} STACK;

/**
 * @brief Faz a ligação entre dois nodos.
 * @param n apontador para o nodo que fica depois (next)
 * @param b apontador para o nodo que antes (before) 
 */
void ligaNodo(NODE *n, NODE *b);
void freeNode(NODE *p);
void freeStack2(STACK *s);

/**
 * \brief Coloca um elemento no topo da stack
 * Aloca espaço em memória no topo da pilha (lista-ligada).
 * Coloca o último elemento da lista a apontar para o novo nodo
 * @param data Valor a colocar
 * @param s Apontador para a Stack
 */
void PUSH (DADOS data, STACK *s);

/**
 * @brief Coloca o apontador para o início da stack (lista-ligada) em NULL
 * @param s apontador para a stack
 */
void INIT (STACK *s);

/**
 * @brief Imprime a Stack
 * Percorre a lista ligada até encontrar NULL e vai imprimindo a data em cada nodo
 * @param s Apontador para a Stack
 * @param arr Apontador para a array
 */
void PRINT (STACK *s);

/**
 * \brief Duplica o valor do topo da Stack
 * @param s Apontador para a Stack
 */
void DUPLICATE (STACK *s);

/**
 * @brief Volta ao início da Stack e devolve o índice respetivo
 * 
 * Apontador p vai percorrendo a struct do topo para a base.
 * P vai tomando sucessivamente o nodo imediatamente anterior
 * @param p apontador para o apontador para um nodo da stack (não tem de ser o topo)
 * @return int o indice relativamente ao início da contagem
 */
int goToBegin (NODE **p);

/**
 * \brief Esta função recebe um indice e copia o elemento do indice seguinte para o endereço do indice especificado
 * Indice incrementado a cada interação
 * Quando for encontrado o indice, copia o valor para p
 * @param s Apontador para a Stack
 * @param n Indice a selecionar
 */
void COPYnELEMENT (STACK *s, int *cnt);

/**
 * \brief Devolve o comprimento da Stack
 * Percorre a lista até encontrar NULL
 * Incrementa o contador (i) a cada iteração
 * @param s Apontador para a Stack
 */
int length (STACK *s);

/**
 * \brief Retira um elemento do topo da pilha
 * Liberta (em memória) o último nodo da lista ligada
 * @param s Apontador para a Stack
 * @return int O valor no topo da pilha
 */
DADOS POPsemFREE(STACK *s);

/**
 * \brief Retira um elemento do topo da pilha
 * Liberta (em memória) o último nodo da lista ligada
 * @param s Apontador para a Stack
 * @return int O valor no topo da pilha
 */
DADOS POP (STACK *s);

void popStack(STACK *s);

/**
 * \brief Esta função roda 3 elementos no topo da stack
 * Usamos uma variável temporária para guardar o primeiro valor a ser trocado
 * Coloca o elemento a apontar para o elemento que se encontra em cima na stack, repetindo-se para os próximos 2 elementos
 * @param s Apontador para a Stack
 */
void ROTATE (STACK *s);

/**
 * \brief Esta função troca 2 elementos no topo da Stack
 * Usamos uma variável temporária para guardar o primeiro valor a ser trocado
 * Coloca o elemento a apontar para o valor que se encontra em cima na Stack
 *
 * @param s Apontador para a Stack
 */
void SWAP (STACK *s);

/** 
 * \brief Imprime qualquer tipo dentro dos DADOS para formato string
 * @param d Dados a imprimir
 */
void PRINT_DADOS(DADOS d);

/** 
 * \brief Imprime o tipo de dados correto de cada dado
 * @param d 
 * @return vazio
 */
void PRINT_TIPO(DADOS d);

/**
 * @brief Imprime a Stack
 * Percorre a lista ligada até encontrar NULL e vai imprimindo a data em cada nodo
 * @param s Apontador para a Stack
 * @param arr Apontador para a array
 */
void DEBUGGER (STACK *s);

int tamanho(STACK *s);

void removeNode(STACK *s, NODE *n);

STACK* copyStack(STACK *orig);

STACK *stringToArr(char *string);

char *arrToString(STACK *array);

#endif
