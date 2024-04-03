#ifndef DFA_H
#define DFA_H

#include "lex/fa/fa.h"
#include "util/vector/vector.h"
#include <stdbool.h>
#include <stdio.h>

// Initialize all constant DFAs
void DFAInit(void);
// Construct an FA from "nfa"
FA *DFAFromNFA(FA *nfa);
// Construct an FA from a vector of multiple "nfa"s. Note that if multiple
// NFAs accept the same string, then it is considered to be accepted by the
// first NFA in "nfas"
FA *DFAFromNFAs(Vector *nfas);
// Minimizes "dfa"
FA *DFAMinimize(FA *dfa);

#endif // DFA_H
