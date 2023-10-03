#ifndef COMMAND_PARSER
#define COMMAND_PARSER

#include "deque.h"

typedef struct cmd {
    char* command;
    int* args; // NULL se não houver
    int nargs; // número de argumentos
} Cmd;

void processCommand(Deque* deque, Cmd* cmd);

Cmd* parseLine(char* line);


#endif
