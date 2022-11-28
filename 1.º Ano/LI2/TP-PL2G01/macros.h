 /**
  * @file macros.h
  * @brief 
  */

/**
 * \brief Esta Macro inicializa um tipo Dados com o tipo e o valor que este irá guardar -  Macro que atribui um novo tipo a uma variável 
 */
#define MAKE_DADOS(var, TIPO, valor)        \
        var.DATA.TIPO = valor;                   \
        var.tipo = TIPO;                    \

/**
 * \brief Esta Macro inicializa uma STACK
 */
#define MAKE_STACK(var) \
        var.tipo = POINTER; \
        var.DATA.sAp = malloc(sizeof(STACK)); \
        
/**
 * \brief Macro que define o tipo operação entre variáveis do mesmo tipo 
 */
#define OPERATION(out,in1,in2,operacao)                                                                 \
        switch(t){ \
                case LONG : out.DATA.LONG = in1.DATA.LONG operacao in2.DATA.LONG; break;                                                       \
                case DOUBLE : out.DATA.DOUBLE = in1.DATA.DOUBLE operacao in2.DATA.DOUBLE; break;                                     \
                case CHAR : out.DATA.CHAR = in1.DATA.CHAR operacao in2.DATA.CHAR; break;                                                         \
                default : break; \
        } \

/**
 * \brief Macro para as variáveis unárias
 */
#define UNARY(in,op) \
        switch(in->tipo){ \
            case LONG : in->DATA.LONG = op in->DATA.LONG; break; \
            case CHAR : in->DATA.CHAR = op in->DATA.LONG; break; \
            default: break; \
        } \


/**
 * @brief Para operações apenas válidas entre inteiros
 */
#define OPERATION_INT(out,in1,in2,operacao)                                                             \
        switch(t){ \
                case LONG : out.DATA.LONG = in1.DATA.LONG operacao in2.DATA.LONG; break;                  \
                case CHAR : out.DATA.CHAR = in1.DATA.CHAR operacao in2.DATA.CHAR;  break;                 \
                default : break; \
        } \

/**
 * @brief Macro que faz as operaçãos lógicas com shortcuts
 */ 
#define ANDORSHORTCUT(A,B)                                                                              \
        t = convertToLargest(&A, &B);                                                                   \
        switch (t){                                                                                     \
            case LONG : (Y.DATA.LONG) ? PUSH(A,s) : PUSH(B,s); break;                                        \
            case DOUBLE : (Y.DATA.DOUBLE) ? PUSH(A,s) : PUSH(B,s); break;                                    \
            case CHAR : (Y.DATA.CHAR) ? PUSH(A,s) : PUSH(B,s); break;                                        \
            case STRING : (Y.DATA.STRING) ? PUSH(A,s) : PUSH(B,s); break;                                    \
            default : break;                                                                            \
        }                                                                                               \

/**
 * @brief Macro que faz as operaçãos lógicas com shortcuts AND e OR
 */ 
#define LOGICRELATION(operacao)                                                                                                               \
        X = POP(s); Y = POP(s); (compare(&Y,&X) operacao 0) ? (Z.tipo = LONG, Z.LONG = 1) : (Z.tipo = LONG, Z.LONG = 0) ; PUSH (Z,s);          \

/**
 * @brief Macro que faz as operações lógicas com shortcuts de maior e menor
 */
#define RELATIONSHORTCUT(A,B,operacao)                             \
        (compare(B,A) operacao 0) ? PUSH(B,s) : PUSH(A,s);         \


