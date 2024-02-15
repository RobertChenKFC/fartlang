#ifndef SYNTAX_H
#define SYNTAX_H

#include "util/source/source.h"
#include <stdio.h>

// Forward declarations
typedef struct SyntaxAST SyntaxAST;

// A structure to store the AST of a fartlang program. This structs stores
// any node in the AST. A node has pointers "firstChild" and "lastChild" that
// points to the first and last children of the node, respectively, and a
// pointer "sibling" that points to the next node that shares the same parent as
// the current node. A node also stores a "kind", which is a label of what kind
// of AST node it is, and depending on the kind, there may be additional data
// stored in the union. Finally, the node stores a "loc", which is a source
// location pointing to the part of code that parsed into this AST node
typedef enum {
  SYNTAX_AST_KIND_MODULE,
  SYNTAX_AST_KIND_IMPORT_DECLS,
  SYNTAX_AST_KIND_IMPORT_DECL,
  SYNTAX_AST_KIND_IMPORT_MODULE_PATH,
  SYNTAX_AST_KIND_IDENTIFIER,
} SyntaxASTKind;
struct SyntaxAST {
  SyntaxAST *firstChild;
  SyntaxAST *lastChild;
  SyntaxAST *sibling;
  int kind;
  SourceLocation loc;
  // TODO: fill this in later
  union {
    char *string; // SYNTAX_AST_KIND_IDENTIFIER
    struct {
      char *alias;
      bool isWildcard;
    } import;
  };
};

// Parse a fartlang source "file" with name "filename" into an AST
SyntaxAST *SyntaxParseFile(FILE *file, const char *filename);
// Deletes an entire AST (the "node" and all its successors) created from
// SyntaxParseFile
void SyntaxASTDelete(SyntaxAST *node);

#endif // SYNTAX_H
