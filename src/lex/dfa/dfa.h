#ifndef DFA_H
#define DFA_H

#include "lex/fa/fa.h"
#include "util/vector/vector.h"
#include <stdbool.h>
#include <stdio.h>

// Initialize all constant DFAs
void DFAInit();
// Construct an FA from "nfa"
FA *DFAFromNFA(FA *nfa);
// Construct an FA from a vector of multiple "nfa"s
FA *DFAFromNFAs(Vector *nfas);
// Minimizes "dfa"
FA *DFAMinimize(FA *dfa);

#endif // DFA_H
