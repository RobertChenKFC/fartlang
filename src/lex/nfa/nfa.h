#ifndef NFA_H
#define NFA_H

#include "lex/regex/regex.h"
#include "lex/fa/fa.h"
#include <stdbool.h>
#include <stdio.h>

// Construct an NFA from "regex"
FA *NFAFromRegex(Regex *regex);

#endif // NFA_H
