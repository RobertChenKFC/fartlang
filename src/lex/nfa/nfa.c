#include "lex/nfa/nfa.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// Constants
// Check if a character is an ASCII control character
#define IS_CONTROL_CHARACTER(a) (((a) >= 0 && (a) <= 31) || (a) == 127)

// Helper functions
// Check if all subregexes of "list" are letter regexes
bool NFAAllLetterRegexes(RegexList *list);
// Create NFA from "regexUnion", which is the union of all letter regexes
FA *NFAFromUnionLetterRegexes(RegexUnion *regexUnion);
// Creates NFA from "regexConcat", which is the concat of all letter regexes
FA *NFAFromConcatLetterRegexes(RegexConcat *regexConcat);

bool NFAAllLetterRegexes(RegexList *list) {
  for (RegexListNode *node = list->head; node; node = node->next) {
    Regex *regex = node->cur;
    if (regex->type != REGEX_LETTER)
      return false;
  }
  return true;
}

FA *NFAFromUnionLetterRegexes(RegexUnion *regexUnion) {
  FA *nfa = FANew();
  FAState *init = FAStateNew(FA_ACCEPT_NONE);
  FAState *accept = FAStateNew(0);
  FAAddState(nfa, init);
  FAAddState(nfa, accept);
  for (RegexListNode *node = regexUnion->list.head; node;
       node = node->next) {
    Regex *subRegex = node->cur;
    unsigned char a = subRegex->regexLetter.a;
    FAStateAddTransition(init, a, accept);
  }
  return nfa;
}

FA *NFAFromConcatLetterRegexes(RegexConcat *regexConcat) {
  FA *nfa = FANew();
  FAState *prev = FAStateNew(FA_ACCEPT_NONE);
  FAAddState(nfa, prev);
  for (RegexListNode *node = regexConcat->list.head; node;
       node = node->next) {
    Regex *subRegex = node->cur;
    unsigned char a = subRegex->regexLetter.a;
    FAState *state = FAStateNew(FA_ACCEPT_NONE);
    FAAddState(nfa, state);
    FAStateAddTransition(prev, a, state);
    prev = state;
  }
  prev->accepting = 0;
  return nfa;
}

FA *NFAFromRegex(Regex *regex) {
  FA *nfa;
  switch (regex->type) {
    case REGEX_NULL: {
      nfa = FANew();
      FAState *init = FAStateNew(FA_ACCEPT_NONE);
      FAAddState(nfa, init);
      break;
    } case REGEX_LETTER: {
      unsigned char a = regex->regexLetter.a;
      nfa = FANew();
      FAState *init = FAStateNew(FA_ACCEPT_NONE);
      FAState *accept = FAStateNew(0);
      // An NFA accepting a single letter "a" can be constructed by having
      // two states: the initiali state "init" and the accepting state "accept",
      // connecting them with a transition "a"
      FAStateAddTransition(init, a, accept);
      FAAddState(nfa, init);
      FAAddState(nfa, accept);
      break;
    } case REGEX_UNION: {
      // Specifically optimize union of all letter regexes to avoid adding too
      // many states
      if (NFAAllLetterRegexes(&regex->regexUnion.list)) {
        nfa = NFAFromUnionLetterRegexes(&regex->regexUnion);
      } else {
        nfa = FANew();
        FAState *init = FAStateNew(FA_ACCEPT_NONE);
        FAAddState(nfa, init);
        // An NFA accepting a union of sub-regexes can be constructed by having
        // an initial state "init", and connecting "init" to all the initial
        // states of the sub-NFAs, which are constructed from the sub-regexes,
        // via epsilon transition
        for (RegexListNode *node = regex->regexUnion.list.head; node;
             node = node->next) {
          Regex *subRegex = node->cur;
          FA *subNFA = NFAFromRegex(subRegex);
          FAAddStates(nfa, subNFA);
          FAStateAddTransition(init, FA_EPSILON, subNFA->init);
          free(subNFA);
        }
      }
      break;
    } case REGEX_CONCAT: {
      // Specifically optimize concatenation of all letter regexes to avoid
      // adding too many states
      if (NFAAllLetterRegexes(&regex->regexUnion.list)) {
        nfa = NFAFromConcatLetterRegexes(&regex->regexConcat);
      } else {
        nfa = FANew();
        FAState *prev = NULL;
        // An NFA accepting a concatenation of sub-regexes can be constructed by
        // connecting all accepting states of the i-th sub-NFA, which is
        // constructed from the i-th sub-regex, to the initial state of the
        // (i+1)-th sub-NFA, then transforming all accepting states of the i-th
        // sub-NFA into non-accepting states
        for (RegexListNode *node = regex->regexUnion.list.head; node;
             node = node->next) {
          Regex *subRegex = node->cur;
          FA *subNFA = NFAFromRegex(subRegex);
          if (prev) {
            for (FAState *state = prev; state; state = state->next) {
              if (state->accepting != FA_ACCEPT_NONE) {
                FAStateAddTransition(state, FA_EPSILON, subNFA->init);
                state->accepting = FA_ACCEPT_NONE;
              }
            }
          }
          FAAddStates(nfa, subNFA);
          prev = subNFA->init;
          free(subNFA);
        }
      }
      break;
    } case REGEX_STAR: {
      // An NFA accepting zero or more occrurrences of a sub-regex can be
      // constructed by first having an initial accepting state "init",
      // then construct the sub-NFA from the sub-regex, connect "init" to the
      // initial state of the sub-NFA via epsilon transitions, and finally
      // connect all final states of the sub-NFA to "init" via epsilon
      // transitions
      nfa = FANew();
      FAState *init = FAStateNew(0);
      FAAddState(nfa, init);
      FA *subNFA = NFAFromRegex(regex->regexStar.regex);
      FAAddStates(nfa, subNFA);
      FAStateAddTransition(init, FA_EPSILON, subNFA->init);
      for (FAState *state = subNFA->init; state; state = state->next) {
        if (state->accepting != FA_ACCEPT_NONE)
          FAStateAddTransition(state, FA_EPSILON, init);
      }
      free(subNFA);
      break;
    }
  }

  nfa->name = regex->name;
  return nfa;
}
