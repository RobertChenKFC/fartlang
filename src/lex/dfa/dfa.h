#ifndef DFA_H
#define DFA_H

#include "lex/fa/fa.h"
#include <stdbool.h>
#include <stdio.h>

// Initialize all constant DFAs
void DFAInit();
// Construct an FA from "nfa"
FA *DFAFromNFA(FA *nfa);
// Minimizes "dfa"
FA *DFAMinimize(FA *dfa);

#endif // DFA_H
