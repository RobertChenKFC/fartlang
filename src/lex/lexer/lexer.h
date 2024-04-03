#ifndef LEXER_H
#define LEXER_H

#include "lex/regex/regex.h"
#include "lex/fa/fa.h"
#include "lex/nfa/nfa.h"
#include "util/source/source.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

// Forward declarations
typedef struct LexerConfig LexerConfig;
typedef struct LexerTransition LexerTransition;
typedef struct LexerToken LexerToken;
typedef struct Lexer Lexer;

// Constants
extern LexerToken *LEXER_TOKEN_EOF;

// A struct that contains a vector of NFAs, which corresponds to each token,
// and one NFA that corresponds to patterns to ignore.
struct LexerConfig {
  Vector *nfas;
  FA *ignoreNfa;
};

// A token type that stores the starting pointer of the token, the length of
// the token, the token ID, and the source location it was lexed from
struct LexerToken {
  char *str;
  int length;
  int tokenID;
  SourceLocation loc;
};

// A struct that packs the compressed DFA, the current state, a stack of
// traversed token IDs, the source it is reading from, and the current
// point it has read to
#define LEXER_TRANSITION_STATE_NONE -1
#define LEXER_TOKEN_ID_NONE -1
struct Lexer {
  int numTransitions;
  int *transitionStateFroms;
  int *transitionStateTos;
  int numStates;
  int *transitionOffsets;
  int *stateTokenIDs;
  int ignoreTokenID;
  Vector *traversedTokenIDs;
  Vector *traversedPoints;
  const char *filename;
  Source *source;
  SourcePoint point;
};

// Add "regex" to "config", and returns the token ID corresponding to "regex".
// The returned token ID is expected to be the number of times
// LexerConfigAddRegex has been called on this particular "config". In other
// words, the first call returns 0, second call returns 1, third call returns
// 2, ... etc
#define LexerConfigAddRegex(config, regex) \
  LexerConfigAddRegexImpl(config, regex, #regex)
// Set ignore regex of "config" to "regex"
#define LexerConfigSetIgnoreRegex(config, regex) \
  do { \
    FA *nfa = NFAFromRegex(regex); \
    nfa->name = #regex; \
    config->ignoreNfa = nfa; \
  } while (0);

// Constructs a lexer config with no regexes and no ignore regex
LexerConfig *LexerConfigNew(void);
// Delete a "config" created with LexerConfigNew
void LexerConfigDelete(LexerConfig *config);
// Implementation of LexerConfigAddRegex with extra parameter "regexName"
// inferred from the macro argument "regex"
int LexerConfigAddRegexImpl(
    LexerConfig *config, Regex *regex, char *regexName);
// Construct a lexer from "config"
Lexer *LexerFromConfig(LexerConfig *config);
// Store the contents of "lexer" into "file"
void LexerToFile(Lexer *lexer, FILE *file);
// Read the stored contents of a lexer from "file"
Lexer *LexerFromFile(FILE *file);
// Delete "lexer" created with either LexerFromConfig or LexerFromFile
void LexerDelete(Lexer *lexer);
// Set the input file for "lexer" to lex from, as well as its "filename" for
// reporting errors; note that it also resets the source point to point to
// the start of the file
void LexerSetInputFile(Lexer *lexer, FILE *file, const char *filename);
// Lex and return the next token from "lexer"; it returns NULL if a lexer error
// is encountered, or a LEXER_TOKEN_EOF when the end of file is reached and
// there is no error
LexerToken *LexerNextToken(Lexer *lexer);
// Delete "token" created from LexerNextToken
void LexerTokenDelete(LexerToken *token);

#endif // LEXER_H
