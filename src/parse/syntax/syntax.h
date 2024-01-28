#ifndef SYNTAX_H
#define SYNTAX_H

// Forward declarations
typedef struct SyntaxAST SyntaxAST;

// A structure to store the AST of a fartlang program. This structs stores
// any node in the AST. A node has a pointer "child" that points to the first
// children of the node, and a pointer "sibling" that points to the next node
// that shares the same parent as the current node. A node also stores a "kind",
// which is a label of what kind of AST node it is, and depending on the kind,
// there may be additional data stored in the union.
enum SyntaxASTKind {
}
struct SyntaxAST {
	SyntaxAST *child;
	SyntaxAST *sibling;
	SyntaxASTKind kind;
	union {}
};

// Parse a fartlang source "file" with name "filename" into an AST
SyntaxAST *SyntaxParseFile(FILE *file, const char *filename);

#endif // SYNTAX_H
