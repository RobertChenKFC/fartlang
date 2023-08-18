#include "parse/parser/handler.h"

ParserDeclareHandler(NullHandler, rhs);
ParserDeclareHandler(AddHandler, rhs);
ParserDeclareHandler(SubHandler, rhs);
ParserDeclareHandler(MulHandler, rhs);
ParserDeclareHandler(DivHandler, rhs);
ParserDeclareHandler(MoveHandler, rhs);
ParserDeclareHandler(ParenHandler, rhs);
