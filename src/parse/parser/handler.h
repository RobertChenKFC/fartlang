// This file is specially designed to contain no include guards, thus the
// following code will be replicated in every include, so that each replication
// can have a different function for the macro ParseDeclareHandler defined
// below. Note that for all header files that include this file, they must not
// contain include guards, either, so that their code can also be replicated for
// different purposes.

#include "util/vector/vector.h"

// Clear the previous function of this macro if it is already defined
#ifdef ParserDeclareHandler
#undef ParserDeclareHandler
#endif

// A macro to declare a handler function with "name" and argument "rhs". The
// function of this macro changes depending on whether the macro
// PARSER_ADD_HANDLERS_TO_TABLE is defined or not: if it is defined, then this
// macro generates code to add handler to table. Otherwise, this macro declares
// a handler function.
#ifdef PARSER_ADD_HANDLERS_TO_TABLE
#define ParserDeclareHandler(handler, rhs) \
  do { \
    void *handlerVoidPtr; \
    *((ParserHandler*)&handlerVoidPtr) = handler; \
    HashTableEntryAdd(handlerTable, #handler, handlerVoidPtr); \
  } while (0)
#else
#define ParserDeclareHandler(handler, rhs) \
  void *handler(Vector *rhs)
#endif
