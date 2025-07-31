#include "lex/lexer/lexer.h"
#include "lex/nfa/nfa.h"
#include "lex/dfa/dfa.h"
#include "util/hashtable/hashtable.h"
#include <stdlib.h>
#include <limits.h>
#include <assert.h>

// Constants
LexerToken LEXER_TOKEN_EOF_INSTANCE;
LexerToken *LEXER_TOKEN_EOF = &LEXER_TOKEN_EOF_INSTANCE;
// Check if a character is a ASCII control character
#define IS_CONTROL_CHARACTER(a) (((a) >= 0 && (a) <= 31) || ((a) == 127))

LexerConfig *LexerConfigNew() {
  LexerConfig *config = malloc(sizeof(LexerConfig));
  config->nfas = VectorNew();
  config->ignoreNfa = NULL;
  return config;
}

void LexerConfigDelete(LexerConfig *config) {
  Vector *nfas = config->nfas;
  int numNfas = nfas->size;
  FA *ignoreNfa = config->ignoreNfa;
  for (int i = 0; i < numNfas; ++i) {
    FA *nfa = nfas->arr[i];
    if (nfa != ignoreNfa)
      FADelete(nfa);
  }
  if (ignoreNfa)
    FADelete(ignoreNfa);
  VectorDelete(config->nfas);
  free(config);
}

int LexerConfigAddRegexImpl(
    LexerConfig *config, Regex *regex, char *regexName) {
  Vector *nfas = config->nfas;
  int id = nfas->size;
  FA *nfa = NFAFromRegex(regex);
  nfa->name = regexName;
  VectorAdd(config->nfas, nfa);
  return id;
}

Lexer *LexerFromConfig(LexerConfig *config) {
  // Obtain minimal DFA from regexes
  Vector *nfas = config->nfas;
  FA *ignoreNfa = config->ignoreNfa;
  if (ignoreNfa)
    VectorAdd(nfas, ignoreNfa);
  int numNfas = nfas->size;
  FA *dfa = DFAFromNFAs(nfas);
  FA *minDFA = DFAMinimize(dfa);
  FADelete(dfa);
  dfa = minDFA;

  // Assign state numbers to each FAState
  HashTable *toStateNo = HashTableNew(
      FAStatePtrHash, FAStatePtrEqual, NULL, NULL);
  int numStates = 0;
  for (FAState *state = dfa->init; state; state = state->next)
    HashTableEntryAdd(toStateNo, state, (void*)(long long)numStates++);

  // Compressing all the transitions into one big array, so that the goto state
  // can be accessed via "transitionStateTos[transitionOffsets[from] + a]"; note
  // that the transition may be from another state, and we must check if the
  // transition indeed is from "state" by checking if
  // "transitionStateFroms[transitionOffsets[from] + a]" is equal to "from"
  //
  // The way this is done is that going through each state "from", we loop
  // through all possible characters "a", find the transition
  // "from" --"a"--> "to" (note that "to" may not exist, then we let
  // "to == LEXER_TRANSITION_STATE_NONE", or NONE for short), and add the
  // "to" states one by one into the array, then if NONE occupies an array
  // entry, it can be used to store "to" states of other "from" state's
  // transitions, because we can distinguish them via "transitionStateFroms",
  // which stores the "from" states that each "to" state comes from
  Vector *transitionStateFroms = VectorNew();
  Vector *transitionStateTos = VectorNew();
  int *transitionOffsets = malloc(sizeof(int) * numStates);
  int *stateTokenIDs = malloc(sizeof(int) * numStates);
  int from = 0;
  // "freePos" represents the first position in "transitionStateTos" where
  // it either contains a NULL "to" state, or is out of bounds; in other words,
  // it the left-most position that can possibly be filled with a new "to"
  // state
  int freePos = 0;
  for (FAState *state = dfa->init; state; state = state->next, ++from) {
    int numTransitions = 0;
    unsigned char minA = UCHAR_MAX;
    for (FATransition *transition = state->transition; transition;
         transition = transition->next) {
      if (transition->a < minA)
        minA = transition->a;
      ++numTransitions;
    }
    // We can skip an entire state if it contains no transitions
    if (numTransitions == 0) {
      transitionOffsets[from] = 0;
      stateTokenIDs[from] = (state->accepting == FA_ACCEPT_NONE) ?
                            LEXER_TOKEN_ID_NONE :
                            state->accepting;
      continue;
    }

    int curFreePos = freePos - minA;
    if (curFreePos < 0)
      curFreePos = 0;
    for (; true; ++curFreePos) {
      bool posIsFree = true;
      // A position is considered free all the non-NONE "to" states will occupy
      // entires in "transitionStateTos" that aren't already filled with
      // non-NONE "to" states
      for (FATransition *transition = state->transition; transition;
           transition = transition->next) {
        int pos = curFreePos + transition->a;
        if (pos < transitionStateTos->size &&
            ((int)(long long)transitionStateTos->arr[pos] !=
             LEXER_TRANSITION_STATE_NONE)) {
          posIsFree = false;
          break;
        }
      }
      if (posIsFree)
        break;
    }

    // The free position is already determined, then start filling
    // "transitionStateTos" with corresponding non-NONE "to" states
    for (FATransition *transition = state->transition; transition;
         transition = transition->next) {
      int pos = curFreePos + transition->a;
      while (pos >= transitionStateTos->size) {
        VectorAdd(transitionStateFroms,
                  (void*)(long long)LEXER_TRANSITION_STATE_NONE);
        VectorAdd(transitionStateTos,
                  (void*)(long long)LEXER_TRANSITION_STATE_NONE);
      }
      HashTableEntry *entry = HashTableEntryRetrieve(
          toStateNo, transition->state);
      assert(entry);
      int to = (int)(long long)entry->value;
      transitionStateFroms->arr[pos] = (void*)(long long)from;
      transitionStateTos->arr[pos] = (void*)(long long)to;
    }
    transitionOffsets[from] = curFreePos;
    stateTokenIDs[from] = (state->accepting == FA_ACCEPT_NONE) ?
                          LEXER_TOKEN_ID_NONE :
                          state->accepting;
    while (freePos < transitionStateTos->size &&
           ((int)(long long)transitionStateTos->arr[freePos] !=
            LEXER_TRANSITION_STATE_NONE))
      ++freePos;
  }
  FADelete(dfa);
  HashTableDelete(toStateNo);

  Lexer *lexer = malloc(sizeof(Lexer));
  int numTransitions = transitionStateFroms->size;
  lexer->numTransitions = numTransitions;
  lexer->transitionStateFroms = malloc(sizeof(int) * numTransitions);
  lexer->transitionStateTos = malloc(sizeof(int) * numTransitions);
  for (int i = 0; i < numTransitions; ++i) {
    lexer->transitionStateFroms[i] =
        (int)(long long)transitionStateFroms->arr[i];
    lexer->transitionStateTos[i] =
        (int)(long long)transitionStateTos->arr[i];
  }
  VectorDelete(transitionStateFroms);
  VectorDelete(transitionStateTos);
  lexer->numStates = numStates;
  lexer->transitionOffsets = transitionOffsets;
  lexer->stateTokenIDs = stateTokenIDs;
  lexer->ignoreTokenID = config->ignoreNfa ? numNfas - 1
                                           : LEXER_TOKEN_ID_NONE;
  lexer->filename = NULL;
  lexer->source = NULL;
  lexer->point.lineNo = 0;
  lexer->point.charNo = 0;
  return lexer;
}

void LexerToFile(Lexer *lexer, FILE *file) {
  // TODO: transform to binary format
  fprintf(file, "%d\n", lexer->numTransitions);
  for (int i = 0; i < lexer->numTransitions; ++i)
    fprintf(file, "%d ", lexer->transitionStateFroms[i]);
  fprintf(file, "\n");
  for (int i = 0; i < lexer->numTransitions; ++i)
    fprintf(file, "%d ", lexer->transitionStateTos[i]);
  fprintf(file, "\n%d\n", lexer->numStates);
  for (int i = 0; i < lexer->numStates; ++i)
    fprintf(file, "%d ", lexer->transitionOffsets[i]);
  fprintf(file, "\n");
  for (int i = 0; i < lexer->numStates; ++i)
    fprintf(file, "%d ", lexer->stateTokenIDs[i]);
  fprintf(file, "\n%d\n", lexer->ignoreTokenID);
}

Lexer *LexerFromFile(FILE *file) {
  Lexer *lexer = malloc(sizeof(Lexer));
  fscanf(file, "%d", &lexer->numTransitions);
  lexer->transitionStateFroms = malloc(sizeof(int) * lexer->numTransitions);
  for (int i = 0; i < lexer->numTransitions; ++i)
    fscanf(file, "%d", &lexer->transitionStateFroms[i]);
  lexer->transitionStateTos = malloc(sizeof(int) * lexer->numTransitions);
  for (int i = 0; i < lexer->numTransitions; ++i)
    fscanf(file, "%d", &lexer->transitionStateTos[i]);
  fscanf(file, "%d", &lexer->numStates);
  lexer->transitionOffsets = malloc(sizeof(int) * lexer->numStates);
  for (int i = 0; i < lexer->numStates; ++i)
    fscanf(file, "%d", &lexer->transitionOffsets[i]);
  lexer->stateTokenIDs = malloc(sizeof(int) * lexer->numStates);
  for (int i = 0; i < lexer->numStates; ++i)
    fscanf(file, "%d ", &lexer->stateTokenIDs[i]);
  fscanf(file, "%d", &lexer->ignoreTokenID);
  lexer->filename = NULL;
  lexer->source = NULL;
  return lexer;
}

void LexerDelete(Lexer *lexer) {
  free(lexer->transitionStateFroms);
  free(lexer->transitionStateTos);
  free(lexer->transitionOffsets);
  free(lexer->stateTokenIDs);
  if (lexer->source)
    SourceDelete(lexer->source);
  free(lexer);
}

void LexerSetInputFile(Lexer *lexer, FILE *file, const char *filename) {
  if (lexer->source)
    SourceDelete(lexer->source);
  lexer->filename = filename;
  lexer->source = SourceFromFile(file);
  lexer->point.lineNo = lexer->point.charNo = 0;
}

LexerToken *LexerNextToken(Lexer *lexer) {
  Source *source = lexer->source;
  SourcePoint *point = &lexer->point;
  int lastLine = source->lines->size - 1;
  int ignoreTokenID = lexer->ignoreTokenID;
  int *transitionStateFroms = lexer->transitionStateFroms;
  int *transitionStateTos = lexer->transitionStateTos;
  int *transitionOffsets = lexer->transitionOffsets;
  int *stateTokenIDs = lexer->stateTokenIDs;

  while (true) {
    if (point->lineNo == lastLine)
      return LEXER_TOKEN_EOF;
    LexerToken *token = malloc(sizeof(LexerToken));
    token->loc.from = *point;

    int state = 0;
    SourcePoint lastAcceptedPoint;
    int lastAcceptedTokenId = LEXER_TOKEN_ID_NONE;
    if (stateTokenIDs[state] != LEXER_TOKEN_ID_NONE) {
      lastAcceptedTokenId = stateTokenIDs[state];
      lastAcceptedPoint = *point;
    }
    while (point->lineNo != lastLine) {
      unsigned char a = SourceGetFromPoint(source, point);
      int pos = transitionOffsets[state] + a;
      if (pos >= lexer->numTransitions ||
          transitionStateFroms[pos] != state ||
          transitionStateTos[pos] == LEXER_TRANSITION_STATE_NONE)
        break;
      state = transitionStateTos[pos];
      if (stateTokenIDs[state] != LEXER_TOKEN_ID_NONE) {
        lastAcceptedTokenId = stateTokenIDs[state];
        lastAcceptedPoint = *point;
      }
      SourcePointIncrement(source, point);
    }

    if (lastAcceptedTokenId == LEXER_TOKEN_ID_NONE) {
      fprintf(stderr, SOURCE_COLOR_RED"[Error]"SOURCE_COLOR_RESET" %s:%d: ",
              lexer->filename, point->lineNo + 1);
      if (point->lineNo != lastLine) {
        fprintf(stderr, "Unexpected character '"SOURCE_COLOR_RED);
        unsigned char a = SourceGetFromPoint(source, point);
        switch (a) {
          case '\r': fprintf(stderr, "\\r"); break;
          case '\t': fprintf(stderr, "\\t"); break;
          case '\n': fprintf(stderr, "\\n"); break;
          default:
            if (IS_CONTROL_CHARACTER(a))
              fprintf(stderr, "\\x%02x", (unsigned)a);
            else
              fputc(a, stderr);
            break;
        }
        fprintf(stderr, SOURCE_COLOR_RESET"'.\n");
        SourcePoint midPoint = *point;
        SourcePointDecrement(source, &midPoint);
        token->loc.to = midPoint;
        SourceLocation location2;
        location2.from = location2.to = *point;
        SourceLocationPrint(source, 2, SOURCE_COLOR_GREEN, &token->loc,
                                       SOURCE_COLOR_RED, &location2);
      } else {
        SourcePointDecrement(source, point);
        fprintf(stderr, "Unexpected end of file.\n");
        token->loc.to = *point;
        SourceLocationPrint(source, 1, SOURCE_COLOR_GREEN, &token->loc);
      }
      free(token);
      return NULL;
    }
    SourcePointDecrement(source, point);
    *point = lastAcceptedPoint;
    token->loc.to = *point;
    token->str = source->file +
        (int)(long long)source->lines->arr[token->loc.from.lineNo] +
        token->loc.from.charNo;
    token->length = (int)(long long)source->lines->arr[token->loc.to.lineNo] +
        token->loc.to.charNo -
        (int)(long long)source->lines->arr[token->loc.from.lineNo] -
        token->loc.from.charNo + 1;
    token->tokenID = lastAcceptedTokenId;
    SourcePointIncrement(source, point);

    if (token->tokenID != ignoreTokenID)
      return token;
    LexerTokenDelete(token);
  }
}

void LexerTokenDelete(LexerToken *token) {
  free(token);
}
