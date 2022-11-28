 /**
  * @file projeto.c 
  * @brief Ficheiro que contém a função principal do programa
  */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "Parser.h"
#include "Logic.h"
#include "Arrays.h"

/**
 * \brief Esta é a função principal do programa
 * A função lê uma linha e faz o parse da mesma, só volta ao main quando todas as linhas forem lidas e interpretadas
 * @return 0 (não houve erros)
 */
int main () {
    DADOS* varArray = malloc(26*sizeof(DADOS)); INIT_VAR(varArray);
    STACK *final;

    char line [10000]; 
    assert ( fgets (line, 10000, stdin) != NULL );
    assert (line [strlen (line) -1] == '\n');
    
    final = eval (line, NULL, varArray); //PRINT(&s2);
    PRINT(final);
    //freeStack(final);
    free(final);
    
    putchar('\n');
    return 0; 
}
  
