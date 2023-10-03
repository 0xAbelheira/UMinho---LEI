#include "deque.h"
#include "node.h"
#include "command_parser.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void printInt(void *i){
    int *in = i;
    printf("%d", *in);
}

void processCommand(Deque* deque, Cmd* cmd){

        if(strcmp(cmd->command,"PUSH") == 0){
            for(int i = 0; i < cmd->nargs; i++){
                void* data = malloc(sizeof(int));
                *(int*) data = cmd->args[i];
                push(deque, data);
            }
        }
                    
        else if(strcmp(cmd->command,"PUSH_FRONT") == 0){
            for(int i = 0; i < cmd->nargs; i++){
                void* data = malloc(sizeof(int));
                *(int*) data = cmd->args[i];
                pushFront(deque, data);
            }
        }
                    
        else if(strcmp(cmd->command,"POP") == 0){
            int* data = (int*)pop(deque);
            if(data == NULL) printf("EMPTY\n");
            else{
                printf("%d\n", *data);
                free(data);
            }
        }
                    
        else if(strcmp(cmd->command,"POP_FRONT") == 0){
            int* data = (int*)popFront(deque);
            if(data == NULL) printf("EMPTY\n");
            else{ 
                printf("%d\n", *data);
                free(data);
            }
        }
                  
        else if(strcmp(cmd->command,"SIZE") == 0){
            int data = size(deque);
            printf("%d\n", data);
        }
                    
        else if(strcmp(cmd->command,"REVERSE") == 0){
            reverse(deque);
        }
                    
        else if(strcmp(cmd->command,"PRINT") == 0){
            printDeque(deque,&printInt);
        }
        else{
            printf("Unknown Command\n");
        }
}

Cmd* parseLine(char* line) {
  int j = 0; 
  int i = 0; // flag de controlo (para assegurar que não reescreve por cima do comando)
  char command[12];
  int* args = NULL; // j é o índice deste array
  int nargs = 0;
  char temp[256];
  strcpy(temp, line);

  char* token = strsep(&line, " ");

  while(token != NULL){
    if(i == 0) strcpy(command, token);
    if(strcmp(command,temp) == 0){
        args = NULL;
        nargs = 0;
        break;
    }
    else{   
        char* endptr;
        long n = strtol(token, &endptr, 10);
        args = (int*)realloc(args, (nargs+1)*sizeof(int));
        args[j] = (int)n;
        j++;
        nargs++;
    }
    token = strsep(&line, " ");
    i = 1;
  }

    Cmd* cmd = (Cmd*)malloc(sizeof(Cmd));
    cmd->command = (char*)malloc(strlen(command)+1);
    strcpy(cmd->command, command);
    if(nargs != 0){
        cmd->args = (int*)malloc(nargs * sizeof(int));

        for(int k = 0; k < nargs; k++){
            cmd->args[k] = args[k];
        }
    }
    else cmd->args = NULL;
    cmd->nargs = nargs;
    if(args) free(args);

    return cmd;
}




