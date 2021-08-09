#ifndef LEXER_H
#define LEXER_H

#include "lex/regex/regex.h"
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
LexerToken *LEXER_TOKEN_EOF;

// A struct that contains a vector of regexes, which corresponds to each token,
// and one regex that corresponds to patterns to ignore
struct LexerConfig {
  Vector *regexes;
  Regex *ignoreRegex;
};

// A token type that stores the starting pointer of the token, the length of
// the token, the token ID, and the source location it was lexed from
struct LexerToken {
  unsigned char *str;
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

// Constructs a lexer config with no regexes and no ignore regex
LexerConfig *LexerConfigNew();
// Delete a "config" created with LexerConfigNew
void LexerConfigDelete(LexerConfig *config);
// Add "regex" to "config", and returns the token ID corresponding to "regex"
int LexerConfigAddRegex(LexerConfig *config, Regex *regex);
// Set ignore regex of "config" to "regex"
void LexerConfigSetIgnoreRegex(LexerConfig *config, Regex *regex);
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
