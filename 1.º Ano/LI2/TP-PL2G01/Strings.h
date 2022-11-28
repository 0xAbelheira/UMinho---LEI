/**
 * @file strings.h
 * @brief Este ficheiro contém as funções relativas às strings do programa
 */
#include "Stack.h"

void concatStrings(STACK *s, int *cnt);

void multString(STACK *s, int *cnt);

void commaOperatorString(STACK *s, int *cnt);

void byIndexString(STACK *s, int *cnt);

void getFirstChars(STACK *s, int *cnt);

void getLastChars(STACK *s, int *cnt);

void removeFirstString(STACK *s, int *cnt);

void removeLastString(STACK *s, int *cnt);

void separateByDelims (STACK *s, int *cnt, char *delims);

void separateByString(STACK *s, int *cnt);

void procSubstr(STACK *s, int *cnt);
