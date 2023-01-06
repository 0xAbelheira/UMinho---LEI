 /**
  * @file parser.c 
  * @brief Contém também várias macros para ajuda na conversão entre tipos.
  */
#include "Stack.h" 
#include "macros.h" 
#include "Parser.h"
#include "Conversions.h"
#include "Arrays.h"
#include "Logic.h"
#include "Blocos.h"
#include "Strings.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <ctype.h>
 
/**
 * \brief Esta é a função que faz o parse de uma linha
 * Cria uma stack (na heap) caso a stack atual seja NULL
 * @param line Linha lida que vai ser interpretada e colocada na stack
 * @param init_stack Apontador para a stack inicial (pode ser NULL)
 * @param varArray Array que contém as variáveis
 * @return O apontador para a stack depois das operações todas
 */
STACK *eval(char *line, STACK *init_stack, DADOS* varArray){
    char *delims = " \t\n", *resto;
    int cnt;

    if(init_stack == NULL){
        init_stack = malloc(sizeof(STACK));
        INIT(init_stack);
    }

    for (char *token = strtok_r(line, delims, &resto); token != NULL; token = strtok_r(NULL, delims, &resto)){
        cnt = 1;
        handle_arithmetic(token, init_stack, &cnt);
        handle_uni(*token, init_stack, &cnt);
        handle_Logic(token, init_stack, &cnt);
        handle_stackmanipulation(*token, init_stack, &cnt);
        handle_conversoes(*token, init_stack, &cnt);
        handle_arrays(token, init_stack, &cnt);
        handle_strings(token, init_stack, &cnt);
        handle_blocks(token, init_stack, &cnt, varArray);
        handle_readline(*token, init_stack, &cnt); 
        push_where(init_stack, token, &resto, varArray, &cnt);
        handle_vars(init_stack, token, varArray, &cnt);     
    }
    return init_stack;
}

/**
 * \brief Esta função recebe a string com o Array e coloca-o na Stack
 * @param s Apontador para a stack do programa
 * @param resto Apontador para a string que fica depois destas operações serem feitas
 * @param varArray Array que contém as variáveis
 */
void getArray(STACK *s, char **resto, DADOS *varArray){
    DADOS pointerToArray;
    char *array = get_delimited(resto, "[]");

    pointerToArray.tipo = POINTER;
    pointerToArray.DATA.sAp = (void*) eval(array, NULL, varArray);

    PUSH(pointerToArray, s);    
}

/**
 * \brief Esta função recebe a string com o bloco e coloca-o na Stack em forma de string
 * @param s Apontador para a stack do programa
 * @param resto Apontador para a string que fica depois destas operações serem feitas
 */
void getBlock(STACK *s, char **resto){
    DADOS Z;
    char* newBlock, *block;
    int ln;

    block = get_delimited(resto, "{}");

    ln = strlen(block); //tamanho do bloco (sem chavetas)

    newBlock = (char *) malloc((ln+2)*sizeof(char));
    strcpy(newBlock+1, block);
    newBlock[0] = '{';
    newBlock[ln+1] = '}';
    free(block);

    Z.tipo = STRING;
    Z.DATA.STRING = newBlock;
    PUSH(Z, s);
}

/**
 * \brief Esta função recebe a string que ainda não foi lida e decidi onde colocar os valores
 * @param s Apontador para a stack do programa
 * @param token Palavra atual
 * @param line Linha que vem depois da palavra atual
 * @param resto Apontador para a string que fica depois destas operações serem feitas
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void push_where(STACK *s, char* token, char **resto, DADOS* varArray, int *cnt){
    //printf("%s\n%s\n", token, line);
    if(*cnt ==0) return;
    if(*token == '['){            
            getArray(s, resto, varArray);
            *cnt = 0;
    } else if (*token == '\"'){
            DADOS dString;
            dString.DATA.STRING = get_delimitedS(token, resto, "\"");
            dString.tipo = STRING;
            PUSH(dString, s);
            *cnt = 0;
    }
    else if (*token == '{'){
            getBlock(s, resto);
            *cnt = 0;
    }  
    else {
        autoPush(token , s, cnt);
    }
}

/**
 * @brief Esta função separa uma string por substrings
 * @param token A palavra que está a ser lida
 * @param s apontador para a stack
 * @param cnt Contador que controla se uma operação já foi realizada
 * @param varArray Array que contém as variáveis
 */
//void substring (STACK *s) { /// the / operator

void handle_blocks(char *token, STACK *s, int *cnt, DADOS* varArray){
    char *ops = "~%*,$w";
    
    if(*cnt == 0) return;
    if (strchr(ops, *token) == NULL) return;
    if(s->top == NULL || s->top->before == NULL || s->top->data.tipo != STRING) return;
    switch(*token){
        case '~': execute(s, varArray); break;
        case '%': map(s, varArray); break;
        case '*': fold(s, varArray); break;
        case ',': filter(s, varArray); break;
        case '$': sort(s, varArray); break;
        case 'w': truthyExecute (s,varArray); break;
    }
    *cnt = 0;
}


/**
 * \brief Esta é a função que vai lidar com os comandos relacionados com os arrays
 * @param token A palavra que está a ser lida
 * @param s Apontador para a stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void handle_arrays(char *token, STACK* s, int *cnt){
    char *ops = "~+*,=<>()#/";

    if(*cnt == 0) return;
    if(strchr(ops, *token) == NULL) return;
    if (s->top->data.tipo == STRING) return;

        switch(*token){
        case '~' : arrayToStack(s, cnt); break;
        case ',' : commaOperator(s, cnt); break;
        case '+' : concat(s, cnt); break;
        case '*' : multArray(s, cnt); break;
        case '(' : removePrimeiro(s, cnt); break;
        case ')' : removeUltimo(s, cnt); break;
        case '=' : byIndex(s, cnt); break;
        case '<' : getFirstElems(s, cnt); break;
        case '>' : getLastElems(s, cnt); break;
    }

}

/**
 * \brief Esta é a função que vai lidar com as conversões
 * @param token O comando que indica a conversão (i,f,c,s)
 * @param s Apontador para a stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void handle_conversoes(char token, STACK *s, int *cnt){
    char *ops = "ifcs";
    if (s->top != NULL && strchr(ops, token) != NULL && *cnt != 0){
        if (token == 'i') convertToLong(&(s->top->data));
        if (token == 'f') convertToDouble(&(s->top->data));
        if (token == 'c') convertToChar(&(s->top->data));
        if (token == 's') convertToString(&(s->top->data));
        *cnt = 0;
    }
}

/**
 * \brief Esta é a função que vai lidar com a manipulação da stack
 * @param op Operador que indica a operação de manipulação a realizar (_, ;, \, @ ou $)
 * @param s Apontador para a stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void handle_stackmanipulation (char op, STACK *s, int *cnt){
    char *ops = "_;\\@$";
    if(*cnt == 0 || s->top == NULL || strchr(ops,op) == NULL) return;
    switch(op){
        case '_' : DUPLICATE(s); *cnt = 0; break;
        case ';' : POP(s); *cnt = 0; break;
        case '\\' : SWAP(s); *cnt = 0; break;
        case '@' :  ROTATE(s); *cnt = 0; break;
        case '$' : COPYnELEMENT(s, cnt); break;
    }
}

/**
 * @brief Função que lida com as operações lógica
 * @param s Apontador para a Stack do programa
 * @param op apontador para o token
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void handle_Logic(char *op, STACK *s, int *cnt){
    char *ops = "?>=<e";
    double dif;
    DADOS Z;
    if(*cnt==0 || strchr(ops,*op) == NULL || s->top == NULL || s->top->before == NULL ||
    ((*op == '=' || *op == '<' || *op == '>') && s->top->data.tipo == LONG && (s->top->before->data.tipo == POINTER || s->top->before->data.tipo == STRING))) 
        return;

    Z.tipo = LONG;

    switch(*op){
        case '>' : dif = compareTop(s); Z.DATA.LONG = dif > 0 ? 1 : 0; PUSH(Z, s);  break;
        case '=' : dif = compareTop(s); Z.DATA.LONG = dif == 0 ? 1 : 0; PUSH(Z, s);  break;
        case '<' : dif = compareTop(s); Z.DATA.LONG = dif < 0 ? 1 : 0; PUSH(Z, s);  break;
        case '?' : questionOperator(s);  break;
        case 'e' : handleEop(s, op); break;
    }
    *cnt=0;
}

/**
 * @brief Operações lógicas com e
 * @param s Apontador para a stack
 * @param op Apontador para o token
 * @return Vazio
 */
void handleEop(STACK *s, char *op){
    DADOS X, Y;
    TIPO t;
    switch(*(op+1)){
        case '>' : selectGreater(s);  break;
        case '<' : selectLesser(s);  break;
        case '&' : X = POP(s); Y = POP(s); ANDORSHORTCUT(X,Y);  break;
        case '|' : X = POP(s); Y = POP(s); ANDORSHORTCUT(Y,X);  break;
    }
}

/**
 * @brief Função que lida com as operações unárias
 * @param op Operação a ser realizada
 * @param s Apontador para a Stack do programa
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void handle_uni(char op, STACK *s, int *cnt){
    char *ops = "()~!";
    DADOS* top = &s->top->data;

    if(*cnt == 0 || strchr(ops, op) == NULL) return;
    if (s->top == NULL || top->tipo == POINTER || top->tipo == STRING) return;
    
    switch(op){
        case '(' : UNARY(top,-1+); break;
        case ')' : UNARY(top,1+); break;
        case '~' : UNARY(top,~); break;
        case '!' : top->DATA.LONG = !logVal(*top); top->tipo = LONG;  break;
    }
    *cnt = 0;
}

/**
 * @brief Verifica os tipos do topo da stack
 * @param s Apontador para a stack
 * @return Valor lógico
 */
int topsArray(STACK *s){
    TIPO X = s->top->data.tipo, Y = s->top->before->data.tipo;
    return X == POINTER || X == STRING || Y == POINTER || Y == STRING;
}

/**
 * @brief Faz as operações +, - e *
 * @param op Operação a efetuar
 * @param s Apontador para a stack
 */
void simpleOperations(char op, STACK *s){
    DADOS Y = POP(s), X = POP(s), Z;
    TIPO t = convertToLargest(&Y, &X);
    Z.tipo = t;

    switch (op){
        case '+': OPERATION(Z, X, Y, +);  break;
        case '-': OPERATION(Z, X, Y, -);  break;
        case '*': OPERATION(Z, X, Y, *);  break;
        default : break;
    }

    PUSH(Z, s);
}

/**
 * @brief Faz a operação /
 * @param s Apontador para a stack
 */
void division(STACK *s){
    DADOS Y = POP(s), X = POP(s), Z;
    TIPO t = convertToLargest(&Y, &X);
    Z.tipo = t;

    switch(t){
        case LONG : Z.DATA.LONG = X.DATA.LONG/Y.DATA.LONG; break;
        case DOUBLE : Z.DATA.DOUBLE = X.DATA.DOUBLE/Y.DATA.DOUBLE; break;
        case CHAR : Z.DATA.CHAR = X.DATA.CHAR/Y.DATA.CHAR; break;
        default : break;
    }
    PUSH(Z, s);
}

/**
 * @brief Faz a operação /
 * @param s Apontador para a stack
 */
void power(STACK *s){
    DADOS Y = POP(s), X = POP(s), Z;
    TIPO t = convertToLargest(&Y, &X);
    Z.tipo = t;

    switch(t){
        case LONG : Z.DATA.LONG = pow(X.DATA.LONG, Y.DATA.LONG); break;
        case DOUBLE : Z.DATA.DOUBLE = pow(X.DATA.DOUBLE,Y.DATA.DOUBLE); break;
        case CHAR : Z.DATA.CHAR = pow(X.DATA.CHAR,Y.DATA.CHAR); break;
        default : break;
    }
    PUSH(Z, s);
}

/**
 * @brief Faz a operação /
 * @param s Apontador para a stack
 */
void mod(STACK *s){
    DADOS Y = POP(s), X = POP(s), Z;
    TIPO t = convertToLargest(&Y, &X);
    Z.tipo = t;

    switch(t){
        case LONG : Z.DATA.LONG = X.DATA.LONG % Y.DATA.LONG; break;
        case CHAR : Z.DATA.CHAR = X.DATA.CHAR % Y.DATA.CHAR; break;
        default: break;
    }
    PUSH(Z, s);
}

/**
 * @brief Faz as operações bit a bit
 * @param op Operação a efetuar
 * @param s Apontador para a stack
 */
void bitOperations(char op, STACK *s){
    DADOS Y = POP(s), X = POP(s), Z;
    TIPO t = convertToLargest(&Y, &X);
    Z.tipo = t;

    switch(op){
        case '|': OPERATION_INT(Z, X, Y, |); break;
        case '&': OPERATION_INT(Z, X, Y, &); break;
        case '^': OPERATION_INT(Z, X, Y, ^); break;
        default : break;
    }
    PUSH(Z,s);
}

/**
* \brief Esta é a função que lida com as operações aritméticas 
* @param op Apontador para o token
* @param s Apontador para a Stack do programa
* @param cnt Contador que controla se uma operação já foi realizada
*/
void handle_arithmetic (char *op, STACK *s, int *cnt) {
    char *ops = "+-*/#|&%^";

    if(*cnt == 0 || strchr(ops, *op) == NULL || s->top == NULL || s->top->before == NULL || (*op == '-' && isdigit(*(op+1))) || topsArray(s)) return;
    
    switch(*op){
        case '+': simpleOperations(*op, s); break;
        case '-': simpleOperations(*op, s); break;
        case '*': simpleOperations(*op, s); break;
        case '/': division(s); break;
        case '#': power(s); break;
        case '%': mod(s); break; 
        default : bitOperations(*op,s); break;
    }
    *cnt = 0;
}

/**
* \brief Esta é a função que coloca na stack os valores (dos vários tipos: inteiro, double, etc)
* @param token Valor a ser colocado eventualmente na stack
* @param s Apontador para a Stack do programa
* @param cnt Contador que controla se uma operação já foi realizada
*/
void autoPush(char *token, STACK *s, int *cnt){
    DADOS Z;
    char *sobral, *sobrad;
    long val_i = strtol(token, &sobral, 10);
    double val_d = strtod(token, &sobrad);

    if (sobral[0] == '\0'){
        MAKE_DADOS(Z, LONG, val_i);
        PUSH(Z, s);
        *cnt = 0;
    } else if (sobrad[0] == '\0'){
        MAKE_DADOS(Z, DOUBLE, val_d);
        PUSH(Z, s);
        *cnt = 0;
    } 
}

/**
 * @brief Lê o input de múltiplas linhas 
 * @param s Apontador para a stack
 */ 
void leInput(STACK *s){
    DADOS X;
    char linha[1024];
    char tLinhas [10240] = "";

    assert(fgets(linha, 1024, stdin) != NULL);
    strcpy(tLinhas, linha);

    while(fgets(linha, 1024, stdin) != NULL) { // lê o input todo
        strcat(tLinhas, linha);
    }
    
    X.tipo = STRING; 
    X.DATA.STRING = (char *) malloc(strlen(tLinhas)*sizeof(char));
    strcpy(X.DATA.STRING, tLinhas);
    PUSH(X, s); // poe o input lido na stack 
}

/**
 * @brief Lê uma linha e coloca o seu valor na stack
 * @param op Carater do primeiro elemento do token
 * @param s Apontador para a stack
 * @param varArray Array que contém as variáveis
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void leLinha(char op, STACK *s, DADOS* varArray, int *cnt);


/**
 * @brief Função que lida com as operações de leitura de linhas
 * @param s Apontador para a stack
 * @param op Valor a ser colocado eventualmente na stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void handle_readline(char op, STACK *s, int *cnt){
    char *ops = "ltp";
    if (*cnt == 0 || strchr(ops,op) == NULL) return;
    if (op == 'l'){
        DADOS Z;
        char newline[10240];
        assert (fgets (newline, 10240, stdin) != NULL );
        assert (newline [strlen (newline) -1] == '\n'); // Verificar se a leitura correu bmem
        int len = strlen(newline);

        newline[len-1] = '\0'; // Tirar o '\n' da linha

        Z.tipo = STRING;
        Z.DATA.STRING = (char *) malloc(len*sizeof(char)); // Guardar espaço para a string lida
        strcpy(Z.DATA.STRING, newline); // Guardar na string o valor lido
        PUSH(Z,s); // Colocar a string na stack
    }
    else if (op == 't' ) {
        leInput (s);
    }
    else if(op == 'p'){
        PRINT_DADOS(s->top->data);
        putchar('\n');
    }
    *cnt = 0;
}

/**
 * @brief Função que lida com as operações entre variáveis
 * @param s Apontador para a stack
 * @param token Valor a ser colocado eventualmente na stack
 * @param varArray Array que contém as variáveis
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void handle_vars(STACK *s, char *token, DADOS* varArray, int *cnt){
    DADOS D;
    DADOS *topo;

    if(*cnt == 0) return;

    if (*token >= 65 && *token <= 90) 
        PUSH(varArray[*token-65], s);
    else if (*token == ':' && *(token+1) >= 65 && *(token+1) <= 90) {
        topo = &s->top->data;
        D.tipo = topo->tipo;
        if(topo->tipo == STRING){
            D.DATA.STRING = strdup(topo->DATA.STRING);
        }
        else if (topo->tipo == POINTER){
            D.DATA.STRING = strdup(topo->DATA.STRING);
            D.DATA.sAp = copyStack(topo->DATA.sAp);
        }
        else D = *topo;

        varArray[*(token+1)-65] = D;
    }
}

/**
 * @brief Função que lida com as operações entre strings
 * @param s Apontador para a stack
 * @param token Valor a ser colocado eventualmente na stack
 * @param cnt Contador que controla se uma operação já foi realizada
 */
void handle_strings(char *token, STACK *s, int *cnt){
    char *ops = "+*,=<>()#";
    char *op2 = "S/";
    char *op3 = "N/"; 

    if(*cnt == 0) return;
    if(strchr(ops, *token) == NULL && strstr(op2, token) == NULL && strstr(op3, token) == NULL) return; // || s->top->data.STRING[0] == '{' segmentation faults
    if (*token == ',' && s->top->data.tipo == STRING && s->top->data.DATA.STRING[0] == '{') return;
    if (strcmp (token ,"N/") == 0) separateByDelims (s, cnt, "\n"); 
    if (strcmp (token ,"S/") == 0) separateByDelims (s, cnt, "\n\t "); 
    
    switch(*token){
        case '+' : concatStrings(s, cnt); break;
        case '*' : multString(s, cnt); break;
        case ',' : commaOperatorString(s, cnt); break;
        case '=' : byIndexString(s, cnt); break;
        case '<' : getFirstChars(s, cnt); break;
        case '>' : getLastChars(s, cnt); break;
        case '(' : removeFirstString(s, cnt); break;
        case ')' : removeLastString(s, cnt); break;
        case '#' : procSubstr(s, cnt); break;
        case '/' : separateByString(s, cnt); break;
    }
}
