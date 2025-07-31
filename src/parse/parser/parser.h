#ifndef PARSER_H
#define PARSER_H

#include "lex/lexer/lexer.h"
#include "parse/cfg/cfg.h"
#include "parse/lr1/lr1.h"
#include "util/hashtable/hashtable.h"
#include "string.h"

// Forward declarations
typedef struct ParserObject ParserObject;
typedef struct ParserConfig ParserConfig;
typedef struct Parser Parser;
typedef struct ParserRHS ParserRHS;

// Constants
extern void *PARSER_OBJECT_FAILURE;
// A hash table that maps a handler function's name to the handler function
// pointer. This table is populated when declaring a handler function using
// 
extern HashTable *PARSER_HANDLER_TABLE;

// A struct that can either be a LexerToken or a custom user object, which is
// stored on the parse stack and given to the user via their provided handlers
typedef enum {
  PARSER_OBJECT_TOKEN,
  PARSER_OBJECT_OBJECT
} ParserObjectType;
struct ParserObject {
  union {
    LexerToken *token;
    void *object;
  };
  ParserObjectType type;
};

// A function type that handles taking a "rhs". More info on "rhs" can be read
// in the description of ParserRHS. Note that if any of the objects in
// "rhs->arr" is not used (will not be contained in the returned new custom
// object and deleted later), it must be deleted
typedef void *(*ParserHandler)(ParserRHS *rhs);

// A function type that deletes the a user created custom "object"
typedef void (*ParserObjectDestructor)(void *object);

// A struct containing all the stuff needed to construct a parser, including
// a "lexer", a "cfg", a vector of "handlers" for each rule, a "useLALR1"
// variable to indicate whether to consturct a LR(1) table ("useLALR1 = false")
// or a LALR(1) table ("useLALR1 = true"), a destructor to delete user-created
// custom objects (for use when an error occurs; since the parser stack needs
// to be cleared without passing objects to the users handlers, the objects
// still on the stack are deleted using the destructor), vectors of the LHS and
// RHS of each CFG rule in string form for printing relavent information when
// conflict occurs during the construction of the parser, and a vector
// "conflictResolution" of integers, where the i-th integer corresponds to the
// action to choose for the i-th conflict. An additional "htmlFilePath" can be
// provided for the parser to write the LR(1)/LALR(1) state graph in a human
// readable format
struct ParserConfig {
  Lexer *lexer;
  CFG *cfg;
  Vector *handlers;
  Vector *handlerNames;
  bool useLALR1;
  ParserObjectDestructor destructor;
  Vector *lhsStrings;
  Vector *rhsStrings;
  Vector *conflictResolution;
  const char *htmlFilePath;
};

// A struct containing a compressed table of a LR(1) or LALR(1) transition
// graph, a list of the number of RHS in each rule, a lexer from which the
// tokens are read, and a list of handlers
#define PARSER_TRANSITION_STATE_NONE 0
struct Parser {
  int numTransitions;
  int *transitionStateFroms;
  int *transitionStateTos;
  int numStates;
  int *transitionOffsets;
  int maxTokenID;
  int initState;
  int acceptingState;
  int numRules;
  int *lhses;
  int *numRHSes;
  Lexer *lexer;
  Vector *handlers;
  Vector *handlerNames;
  ParserObjectDestructor destructor;
};

// A struct containing all the things that may be used by a parser handler,
// including the "arr" of RHS terminals (LexerToken*) and non-terminals
// (custom objects), the number of RHS stored in "size", and the "parser"
// object itself just in case a warning/error needs to be reported
struct ParserRHS {
  void **arr;
  int size;
  Parser *parser;
};

// A wrapper to CFGAddRule, and includes extra arguments "config" and
// "handler" so that the "handler" can be added to "config" at the same time
// CFGAddRule is called, to ensure that the handlers are added in the right
// order
#define ParserAddRuleAndHandler(config, handler, lhs, n, ...) \
    do { \
      void *handlerVoidPtr; \
      *((ParserHandler*)(&handlerVoidPtr)) = handler; \
      VectorAdd((config)->handlers, handlerVoidPtr); \
      VectorAdd((config)->handlerNames, strdup(#handler)); \
      CFGAddRule((config)->cfg, lhs, n, ##__VA_ARGS__); \
      VectorAdd((config)->lhsStrings, #lhs); \
      VectorAdd((config)->rhsStrings, #__VA_ARGS__); \
    } while (0)

// Construct a new parser config from "lexer", "cfg", "useLALR1" and custom
// object "destructor" (see more detail of the usage of these arguments in the
// documentation of ParserConfig)
ParserConfig *ParserConfigNew(
    Lexer *lexer, CFG *cfg, bool useLALR1, ParserObjectDestructor destructor);
// Add a conflict resolution rule to "config", by specifying which "option"
// to choose for the i-th config, where i = "conflictNum"
void ParserConfigAddResolution(
    ParserConfig *config, int conflictNum, int option);
// Construct a parser configuration "config". Note that unlike LexerFromConfig,
// this function actually deletes "config" before returning
Parser *ParserFromConfig(ParserConfig *config);
// Store the contents of a "parser" to "file"
void ParserToFile(Parser *parser, FILE *file);
// The rest of the implementation for the ParserFromFile function (see
// documentation of this function for more detail). It is not recommended to use
// this function directly. You should most likely use ParserFromFile directly
Parser *ParserFromFileImpl(
    FILE *file, Lexer *lexer, HashTable *handlerTable,
    ParserObjectDestructor destructor);
// A hash function for character string "key". For use to lookup handler table
// using handler name
uint64_t ParserStringHash(void *key);
// An equal function for two character strings "string1" and "string2"
bool ParserStringEqual(void *key1, void *key2);
// A function to create a parser from (1) "file" created with ParserToFile,
// (2) "lexer" and (3) a custom object "destructor". Note that in order to use
// this function, the macro PARSER_HANDLER_FILE_NAME must be defined to be
// the header file that contains all the declarations of the handlers used by
// this parser (declared using ParserDeclareHandler). It is recommended to
// define this macro before all includes, so that you can make sure that the
// macro is defined before this header is included
#ifdef PARSER_HANDLER_FILE_NAME
#include PARSER_HANDLER_FILE_NAME

Parser *ParserFromFile(
    FILE *file, Lexer *lexer, ParserObjectDestructor destructor) {
  HashTable *handlerTable = HashTableNew(
      ParserStringHash, ParserStringEqual, NULL, NULL);

#define PARSER_ADD_HANDLERS_TO_TABLE
#include PARSER_HANDLER_FILE_NAME
#undef PARSER_ADD_HANDLERS_TO_TABLE
  Parser *parser = ParserFromFileImpl(file, lexer, handlerTable, destructor);
  HashTableDelete(handlerTable);
  return parser;
}
#include "parse/parser/handler.h"
#endif
// Delete a "parser" created with ParserConfigNew or ParserFromFile
void ParserDelete(Parser *parser);
// Run the parsing algorithm using "parser"; returns the final created custom
// user object if the parsing completes succesfully; otherwise returns
// PARSER_OBJECT_FAILURE
void *ParserParse(Parser *parser);

#endif // PARSER_H
