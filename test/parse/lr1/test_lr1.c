#include "parse/lr1/lr1.h"
#include <assert.h>

int main() {
  // 1. Create LR(1) graph of simple CFG (1)
  CFG *cfg = CFGNew();
  CFGSetNumTokens(cfg, 2);
  int tokA = 0;
  int tokB = 1;
  int varS = CFGAddVariable(cfg);
  int varA = CFGAddVariable(cfg);
  CFGAddRule(cfg, varS, 2, varA, varA);
  CFGAddRule(cfg, varA, 2, tokA, varA);
  CFGAddRule(cfg, varA, 1, tokB);
  LR1StateGraph *graph = LR1StateGraphFromCFG(cfg);
  Vector *tokens = VectorNew();
  VectorAdd(tokens, "a");
  VectorAdd(tokens, "b");
  Vector *variables = VectorNew();
  VectorAdd(variables, "S");
  VectorAdd(variables, "A");
  FILE *file = fopen("1.out", "w");
  LR1StateGraphPrint(cfg, graph, tokens, variables, file);
  fclose(file);
  VectorDelete(tokens);
  VectorDelete(variables);
  CFGDelete(cfg);
  LR1StateGraphDelete(graph);

  // 2. Create LR(1) graph of simple CFG (2)
  cfg = CFGNew();
  CFGSetNumTokens(cfg, 3);
  tokA = 0;
  tokB = 1;
  int tokD = 2;
  varS = CFGAddVariable(cfg);
  varA = CFGAddVariable(cfg);
  int varB = CFGAddVariable(cfg);
  CFGAddRule(cfg, varS, 2, varA, varB);
  CFGAddRule(cfg, varA, 3, tokA, varA, tokB);
  CFGAddRule(cfg, varA, 1, tokA);
  CFGAddRule(cfg, varB, 1, tokD);
  graph = LR1StateGraphFromCFG(cfg);
  tokens = VectorNew();
  VectorAdd(tokens, "a");
  VectorAdd(tokens, "b");
  VectorAdd(tokens, "d");
  variables = VectorNew();
  VectorAdd(variables, "S");
  VectorAdd(variables, "A");
  VectorAdd(variables, "B");
  file = fopen("2.out", "w");
  LR1StateGraphPrint(cfg, graph, tokens, variables, file);
  fclose(file);
  VectorDelete(tokens);
  VectorDelete(variables);
  CFGDelete(cfg);
  LR1StateGraphDelete(graph);

  // 3. Create LR(1) graph of simple CFG (3)
  cfg = CFGNew();
  CFGSetNumTokens(cfg, 5);
  int add = 0;
  int mul = 1;
  int lParen = 2;
  int rParen = 3;
  int identifier = 4;
  int varE = CFGAddVariable(cfg);
  int varT = CFGAddVariable(cfg);
  int varF = CFGAddVariable(cfg);
  CFGAddRule(cfg, varE, 3, varE, add, varT);
  CFGAddRule(cfg, varE, 1, varT);
  CFGAddRule(cfg, varT, 3, varT, mul, varF);
  CFGAddRule(cfg, varT, 1, varF);
  CFGAddRule(cfg, varF, 1, identifier);
  CFGAddRule(cfg, varF, 3, lParen, varE, rParen);
  graph = LR1StateGraphFromCFG(cfg);
  tokens = VectorNew();
  VectorAdd(tokens, "+");
  VectorAdd(tokens, "*");
  VectorAdd(tokens, "(");
  VectorAdd(tokens, ")");
  VectorAdd(tokens, "identifier");
  variables = VectorNew();
  VectorAdd(variables, "E");
  VectorAdd(variables, "T");
  VectorAdd(variables, "F");
  file = fopen("3.out", "w");
  LR1StateGraphPrint(cfg, graph, tokens, variables, file);
  fclose(file);
  VectorDelete(tokens);
  VectorDelete(variables);
  CFGDelete(cfg);
  LR1StateGraphDelete(graph);

  // 4. Create LR(1) graph of simple CFG with empty productions
  cfg = CFGNew();
  CFGSetNumTokens(cfg, 2);
  tokA = 0;
  tokB = 1;
  varS = CFGAddVariable(cfg);
  varA = CFGAddVariable(cfg);
  varB = CFGAddVariable(cfg);
  CFGAddRule(cfg, varS, 2, varA, varB);
  CFGAddRule(cfg, varA, 0);
  CFGAddRule(cfg, varA, 2, tokA, varA);
  CFGAddRule(cfg, varB, 0);
  CFGAddRule(cfg, varB, 2, tokB, varB);
  graph = LR1StateGraphFromCFG(cfg);
  tokens = VectorNew();
  VectorAdd(tokens, "a");
  VectorAdd(tokens, "b");
  variables = VectorNew();
  VectorAdd(variables, "S");
  VectorAdd(variables, "A");
  VectorAdd(variables, "B");
  file = fopen("4.out", "w");
  LR1StateGraphPrint(cfg, graph, tokens, variables, file);
  fclose(file);
  VectorDelete(tokens);
  VectorDelete(variables);
  CFGDelete(cfg);
  LR1StateGraphDelete(graph);

  // 5. Create LR(1) graph of complex CFG (1)
  cfg = CFGNew();
  CFGSetNumTokens(cfg, 18);
  lParen = 0;
  rParen = 1;
  int lBrack = 2;
  int rBrack = 3;
  int lBrace = 4;
  int rBrace = 5;
  int eq = 6;
  add = 7;
  int sub = 8;
  int comma = 9;
  int semicol = 10;
  int constant = 11;
  int intKeyword = 12;
  int floatKeyword = 13;
  identifier = 14;
  int ifKeyword = 15;
  int elseKeyword = 16;
  int whileKeyword = 17;
  int function = CFGAddVariable(cfg);
  int paramList = CFGAddVariable(cfg);
  int param = CFGAddVariable(cfg);
  int type = CFGAddVariable(cfg);
  int block = CFGAddVariable(cfg);
  int stmtList = CFGAddVariable(cfg);
  int stmt = CFGAddVariable(cfg);
  int test = CFGAddVariable(cfg);
  int var = CFGAddVariable(cfg);
  int expr = CFGAddVariable(cfg);
  int term = CFGAddVariable(cfg);
  CFGAddRule(cfg, function, 8,
      type, identifier, lParen, paramList, rParen, lBrace, block, rBrace);
  CFGAddRule(cfg, paramList, 3, paramList, comma, param);
  CFGAddRule(cfg, paramList, 1, param);
  CFGAddRule(cfg, param, 2, type, identifier);
  CFGAddRule(cfg, type, 1, intKeyword);
  CFGAddRule(cfg, type, 1, floatKeyword);
  CFGAddRule(cfg, block, 1, stmtList);
  CFGAddRule(cfg, stmtList, 3, stmtList, semicol, stmt);
  CFGAddRule(cfg, stmtList, 1, stmt);
  CFGAddRule(cfg, stmt, 3, lBrace, block, rBrace);
  CFGAddRule(cfg, stmt, 4, var, eq, expr, semicol);
  CFGAddRule(cfg, stmt, 5, whileKeyword, lParen, test, rParen, stmt);
  CFGAddRule(cfg, stmt, 5, ifKeyword, lParen, test, rParen, stmt);
  CFGAddRule(cfg, stmt, 7,
      ifKeyword, lParen, test, rParen, stmt, elseKeyword, stmt);
  CFGAddRule(cfg, test, 1, expr);
  CFGAddRule(cfg, test, 3, var, eq, expr);
  CFGAddRule(cfg, var, 1, identifier);
  CFGAddRule(cfg, var, 4, identifier, lBrack, expr, rBrack);
  CFGAddRule(cfg, expr, 3, expr, add, term);
  CFGAddRule(cfg, expr, 3, expr, sub, term);
  CFGAddRule(cfg, expr, 1, term);
  CFGAddRule(cfg, term, 1, var);
  CFGAddRule(cfg, term, 3, lParen, expr, rParen);
  CFGAddRule(cfg, term, 1, constant);
  graph = LR1StateGraphFromCFG(cfg);
  tokens = VectorNew();
  VectorAdd(tokens, "'('");
  VectorAdd(tokens, "')'");
  VectorAdd(tokens, "'['");
  VectorAdd(tokens, "']'");
  VectorAdd(tokens, "'{'");
  VectorAdd(tokens, "'}'");
  VectorAdd(tokens, "'='");
  VectorAdd(tokens, "'+'");
  VectorAdd(tokens, "'-'");
  VectorAdd(tokens, "','");
  VectorAdd(tokens, "';'");
  VectorAdd(tokens, "constant");
  VectorAdd(tokens, "int");
  VectorAdd(tokens, "float");
  VectorAdd(tokens, "identifier");
  VectorAdd(tokens, "if");
  VectorAdd(tokens, "else");
  VectorAdd(tokens, "while");
  variables = VectorNew();
  VectorAdd(variables, "Function");
  VectorAdd(variables, "ParamList");
  VectorAdd(variables, "Param");
  VectorAdd(variables, "Type");
  VectorAdd(variables, "Block");
  VectorAdd(variables, "StmtList");
  VectorAdd(variables, "Stmt");
  VectorAdd(variables, "Test");
  VectorAdd(variables, "Var");
  VectorAdd(variables, "Expr");
  VectorAdd(variables, "Term");
  file = fopen("5.out", "w");
  LR1StateGraphPrint(cfg, graph, tokens, variables, file);
  fclose(file);
  VectorDelete(tokens);
  VectorDelete(variables);
  CFGDelete(cfg);
  LR1StateGraphDelete(graph);

  // 6. Create LR(1) graph of complex CFG (2)
  cfg = CFGNew();
  CFGSetNumTokens(cfg, 97);
  int IDENTIFIER = 0;
  int I_CONSTANT = 1;
  int F_CONSTANT = 2;
  int STRING_LITERAL = 3;
  int FUNC_NAME = 4;
  int SIZEOF = 5;
  int PTR_OP = 6;
  int INC_OP = 7;
  int DEC_OP = 8;
  int LEFT_OP = 9;
  int RIGHT_OP = 10;
  int LE_OP = 11;
  int GE_OP = 12;
  int EQ_OP = 13;
  int NE_OP = 14;
  int AND_OP = 15;
  int OR_OP = 16;
  int MUL_ASSIGN = 17;
  int DIV_ASSIGN = 18;
  int MOD_ASSIGN = 19;
  int ADD_ASSIGN = 20;
  int SUB_ASSIGN = 21;
  int LEFT_ASSIGN = 22;
  int RIGHT_ASSIGN = 23;
  int AND_ASSIGN = 24;
  int XOR_ASSIGN = 25;
  int OR_ASSIGN = 26;
  int TYPEDEF_NAME = 27;
  int ENUMERATION_CONSTANT = 28;
  int TYPEDEF = 29;
  int EXTERN = 30;
  int STATIC = 31;
  int AUTO = 32;
  int REGISTER = 33;
  int INLINE = 34;
  int CONST = 35;
  int RESTRICT = 36;
  int VOLATILE = 37;
  int BOOL = 38;
  int CHAR = 39;
  int SHORT = 40;
  int INT = 41;
  int LONG = 42;
  int SIGNED = 43;
  int UNSIGNED = 44;
  int FLOAT = 45;
  int DOUBLE = 46;
  int VOID = 47;
  int COMPLEX = 48;
  int IMAGINARY = 49;
  int STRUCT = 50;
  int UNION = 51;
  int ENUM = 52;
  int ELLIPSIS = 53;
  int CASE = 54;
  int DEFAULT = 55;
  int IF = 56;
  int ELSE = 57;
  int SWITCH = 58;
  int WHILE = 59;
  int DO = 60;
  int FOR = 61;
  int GOTO = 62;
  int CONTINUE = 63;
  int BREAK = 64;
  int RETURN = 65;
  int ALIGNAS = 66;
  int ALIGNOF = 67;
  int ATOMIC = 68;
  int GENERIC = 69;
  int NORETURN = 70;
  int STATIC_ASSERT = 71;
  int THREAD_LOCAL = 72;
  int LPAREN = 73;
  int RPAREN = 74;
  int LBRACK = 75;
  int RBRACK = 76;
  int LBRACE = 77;
  int RBRACE = 78;
  int COMMA = 79;
  int COLON = 80;
  int SEMICOLON = 81;
  int DOT = 82;
  int ADD = 83;
  int SUB = 84;
  int MUL = 85;
  int DIV = 86;
  int MOD = 87;
  int LT = 88;
  int GT = 89;
  int ASSIGN = 90;
  int NOT_OP = 91;
  int QUESTION = 92;
  int BITWISE_AND = 93;
  int BITWISE_OR = 94;
  int BITWISE_NOT = 95;
  int BITWISE_XOR = 96;

  // DEBUG
  int translation_unit = CFGAddVariable(cfg);
  // int statement = CFGAddVariable(cfg);
  // int primary_expression = CFGAddVariable(cfg);
  // int postfix_expression = CFGAddVariable(cfg);
  // int unary_expression = CFGAddVariable(cfg);
  // int cast_expression = CFGAddVariable(cfg);
  // int multiplicative_expression = CFGAddVariable(cfg);
  // int additive_expression = CFGAddVariable(cfg);
  // int shift_expression = CFGAddVariable(cfg);
  // int relational_expression = CFGAddVariable(cfg);
  // int equality_expression = CFGAddVariable(cfg);
  // int exclusive_or_expression = CFGAddVariable(cfg);
  // int logical_and_expression = CFGAddVariable(cfg);
  // int logical_or_expression = CFGAddVariable(cfg);
  // int conditional_expression = CFGAddVariable(cfg);
  // int assignment_expression = CFGAddVariable(cfg);
  // int expression = CFGAddVariable(cfg);

  // DEBUG
  int primary_expression = CFGAddVariable(cfg);
  constant = CFGAddVariable(cfg);
  int enumeration_constant = CFGAddVariable(cfg);
  int string = CFGAddVariable(cfg);
  int generic_selection = CFGAddVariable(cfg);
  int generic_assoc_list = CFGAddVariable(cfg);
  int generic_association = CFGAddVariable(cfg);
  // DEBUG
  int postfix_expression = CFGAddVariable(cfg);
  int argument_expression_list = CFGAddVariable(cfg);
  // DEBUG
  int unary_expression = CFGAddVariable(cfg);
  int unary_operator = CFGAddVariable(cfg);
  // DEBUG
  int cast_expression = CFGAddVariable(cfg);
  // DEBUG
  int multiplicative_expression = CFGAddVariable(cfg);
  // DEBUG
  int additive_expression = CFGAddVariable(cfg);
  // DEBUG
  int shift_expression = CFGAddVariable(cfg);
  // DEBUG
  int relational_expression = CFGAddVariable(cfg);
  // DEBUG
  int equality_expression = CFGAddVariable(cfg);
  int and_expression = CFGAddVariable(cfg);
  // DEBUG
  int exclusive_or_expression = CFGAddVariable(cfg);
  int inclusive_or_expression = CFGAddVariable(cfg);
  // DEBUG
  int logical_and_expression = CFGAddVariable(cfg);
  // DEBUG
  int logical_or_expression = CFGAddVariable(cfg);
  // DEBUG
  int conditional_expression = CFGAddVariable(cfg);
  // DEBUG
  int assignment_expression = CFGAddVariable(cfg);
  int assignment_operator = CFGAddVariable(cfg);
  // DEBUG
  int expression = CFGAddVariable(cfg);
  int constant_expression = CFGAddVariable(cfg);
  int declaration = CFGAddVariable(cfg);
  int declaration_specifiers = CFGAddVariable(cfg);
  int init_declarator_list = CFGAddVariable(cfg);
  int init_declarator = CFGAddVariable(cfg);
  int storage_class_specifier = CFGAddVariable(cfg);
  int type_specifier = CFGAddVariable(cfg);
  int optional_identifier = CFGAddVariable(cfg);
  int struct_or_union_specifier = CFGAddVariable(cfg);
  int struct_or_union = CFGAddVariable(cfg);
  int struct_declaration_list = CFGAddVariable(cfg);
  int struct_declaration = CFGAddVariable(cfg);
  int specifier_qualifier_list = CFGAddVariable(cfg);
  int struct_declarator_list = CFGAddVariable(cfg);
  int struct_declarator = CFGAddVariable(cfg);
  int optional_comma = CFGAddVariable(cfg);
  int enum_specifier = CFGAddVariable(cfg);
  int enumerator_list = CFGAddVariable(cfg);
  int enumerator = CFGAddVariable(cfg);
  int atomic_type_specifier = CFGAddVariable(cfg);
  int type_qualifier = CFGAddVariable(cfg);
  int function_specifier = CFGAddVariable(cfg);
  int alignment_specifier = CFGAddVariable(cfg);
  int declarator = CFGAddVariable(cfg);
  int direct_declarator = CFGAddVariable(cfg);
  int pointer = CFGAddVariable(cfg);
  int type_qualifier_list = CFGAddVariable(cfg);
  int parameter_type_list = CFGAddVariable(cfg);
  int parameter_list = CFGAddVariable(cfg);
  int parameter_declaration = CFGAddVariable(cfg);
  int identifier_list = CFGAddVariable(cfg);
  int type_name = CFGAddVariable(cfg);
  int abstract_declarator = CFGAddVariable(cfg);
  int optional_mul = CFGAddVariable(cfg);
  int direct_abstract_declarator = CFGAddVariable(cfg);
  int initializer = CFGAddVariable(cfg);
  int initializer_list = CFGAddVariable(cfg);
  int designation = CFGAddVariable(cfg);
  int designator_list = CFGAddVariable(cfg);
  int designator = CFGAddVariable(cfg);
  int static_assert_declaration = CFGAddVariable(cfg);
  // DEBUG
  int statement = CFGAddVariable(cfg);
  int labeled_statement = CFGAddVariable(cfg);
  int compound_statement = CFGAddVariable(cfg);
  int block_item_list = CFGAddVariable(cfg);
  int block_item = CFGAddVariable(cfg);
  int expression_statement = CFGAddVariable(cfg);
  int selection_statement = CFGAddVariable(cfg);
  int optional_expression = CFGAddVariable(cfg);
  int iteration_statement = CFGAddVariable(cfg);
  int jump_statement = CFGAddVariable(cfg);
  // DEBUG
  // int translation_unit = CFGAddVariable(cfg);
  int external_declaration = CFGAddVariable(cfg);
  int function_definition = CFGAddVariable(cfg);
  int declaration_list = CFGAddVariable(cfg);

	CFGAddRule(cfg, primary_expression, 1, IDENTIFIER);
	CFGAddRule(cfg, primary_expression, 1, constant);
	CFGAddRule(cfg, primary_expression, 1, string);
  // DEBUG
	CFGAddRule(cfg, primary_expression, 3, LPAREN, expression, RPAREN);
	CFGAddRule(cfg, primary_expression, 1, generic_selection);

	CFGAddRule(cfg, constant, 1, I_CONSTANT);
	CFGAddRule(cfg, constant, 1, F_CONSTANT);
	CFGAddRule(cfg, constant, 1, ENUMERATION_CONSTANT);

	CFGAddRule(cfg, enumeration_constant, 1, IDENTIFIER);

	CFGAddRule(cfg, string, 1, STRING_LITERAL);
	CFGAddRule(cfg, string, 1, FUNC_NAME);

	CFGAddRule(cfg, generic_selection, 6,
      GENERIC, LPAREN, assignment_expression, COMMA,
      generic_assoc_list, RPAREN);

	CFGAddRule(cfg, generic_assoc_list, 1, generic_association);
	CFGAddRule(cfg, generic_assoc_list, 3,
      generic_assoc_list, COMMA, generic_association);

	CFGAddRule(cfg, generic_association, 3,
      type_name, COLON, assignment_expression);
	CFGAddRule(cfg, generic_association, 3,
      DEFAULT, COLON, assignment_expression);

	CFGAddRule(cfg, postfix_expression, 1,
      primary_expression);
  // DEBUG
	CFGAddRule(cfg, postfix_expression, 4,
      postfix_expression, LBRACK, expression, RBRACK);
	CFGAddRule(cfg, postfix_expression, 4,
      postfix_expression, LPAREN, argument_expression_list, RPAREN);
	CFGAddRule(cfg, postfix_expression, 3,
      postfix_expression, DOT, IDENTIFIER);
	CFGAddRule(cfg, postfix_expression, 3,
      postfix_expression, PTR_OP, IDENTIFIER);
	CFGAddRule(cfg, postfix_expression, 2,
      postfix_expression, INC_OP);
	CFGAddRule(cfg, postfix_expression, 2,
      postfix_expression, DEC_OP);
  // DEBUG
	CFGAddRule(cfg, postfix_expression, 6,
      LPAREN, type_name, RPAREN, LBRACE, initializer_list, RBRACE);
	CFGAddRule(cfg, postfix_expression, 7,
      LPAREN, type_name, RPAREN, LBRACE, initializer_list, COMMA, RBRACE);

	CFGAddRule(cfg, argument_expression_list, 1, assignment_expression);
	CFGAddRule(cfg, argument_expression_list, 3,
      argument_expression_list, COMMA, assignment_expression);
  CFGAddRule(cfg, argument_expression_list, 0);

	CFGAddRule(cfg, unary_expression, 1, postfix_expression);
	CFGAddRule(cfg, unary_expression, 2, INC_OP, unary_expression);
	CFGAddRule(cfg, unary_expression, 2, DEC_OP, unary_expression);
  // DEBUG
	CFGAddRule(cfg, unary_expression, 2, unary_operator, cast_expression);
	CFGAddRule(cfg, unary_expression, 2, unary_operator, unary_expression);
	CFGAddRule(cfg, unary_expression, 2, SIZEOF, unary_expression);
  // DEBUG
	CFGAddRule(cfg, unary_expression, 4, SIZEOF, LPAREN, type_name, RPAREN);
	CFGAddRule(cfg, unary_expression, 4, ALIGNOF, LPAREN, type_name, RPAREN);

	CFGAddRule(cfg, unary_operator, 1, BITWISE_AND);
	CFGAddRule(cfg, unary_operator, 1, MUL);
	CFGAddRule(cfg, unary_operator, 1, ADD);
	CFGAddRule(cfg, unary_operator, 1, SUB);
	CFGAddRule(cfg, unary_operator, 1, BITWISE_NOT);
	CFGAddRule(cfg, unary_operator, 1, NOT_OP);

	CFGAddRule(cfg, cast_expression, 1, unary_expression);
  // DEBUG
	CFGAddRule(cfg, cast_expression, 4,
      LPAREN, type_name, RPAREN, cast_expression);

	CFGAddRule(cfg, multiplicative_expression, 1,
      cast_expression);
	CFGAddRule(cfg, multiplicative_expression, 3,
      multiplicative_expression, MUL, cast_expression);
	CFGAddRule(cfg, multiplicative_expression, 3,
      multiplicative_expression, DIV, cast_expression);
	CFGAddRule(cfg, multiplicative_expression, 3,
      multiplicative_expression, MOD, cast_expression);

	CFGAddRule(cfg, additive_expression, 1,
      multiplicative_expression);
	CFGAddRule(cfg, additive_expression, 3,
      additive_expression, ADD, multiplicative_expression);
	CFGAddRule(cfg, additive_expression, 3,
      additive_expression, SUB, multiplicative_expression);

	CFGAddRule(cfg, shift_expression, 1,
      additive_expression);
	CFGAddRule(cfg, shift_expression, 3,
      shift_expression, LEFT_OP, additive_expression);
	CFGAddRule(cfg, shift_expression, 3,
      shift_expression, RIGHT_OP, additive_expression);

	CFGAddRule(cfg, relational_expression, 1,
      shift_expression);
	CFGAddRule(cfg, relational_expression, 3,
      relational_expression, LT, shift_expression);
	CFGAddRule(cfg, relational_expression, 3,
      relational_expression, GT, shift_expression);
	CFGAddRule(cfg, relational_expression, 3,
      relational_expression, LE_OP, shift_expression);
	CFGAddRule(cfg, relational_expression, 3,
      relational_expression, GE_OP, shift_expression);

	CFGAddRule(cfg, equality_expression, 1,
      relational_expression);
	CFGAddRule(cfg, equality_expression, 3,
      equality_expression, EQ_OP, relational_expression);
	CFGAddRule(cfg, equality_expression, 3,
      equality_expression, NE_OP, relational_expression);

	CFGAddRule(cfg, and_expression, 1,
      equality_expression);
	CFGAddRule(cfg, and_expression, 3,
      and_expression, BITWISE_AND, equality_expression);

	CFGAddRule(cfg, exclusive_or_expression, 1,
      and_expression);
	CFGAddRule(cfg, exclusive_or_expression, 3,
      exclusive_or_expression, BITWISE_XOR, and_expression);

	CFGAddRule(cfg, inclusive_or_expression, 1,
      exclusive_or_expression);
	CFGAddRule(cfg, inclusive_or_expression, 3,
      inclusive_or_expression, BITWISE_OR, exclusive_or_expression);

	CFGAddRule(cfg, logical_and_expression, 1,
      inclusive_or_expression);
	CFGAddRule(cfg, logical_and_expression, 3,
      logical_and_expression, AND_OP, inclusive_or_expression);

	CFGAddRule(cfg, logical_or_expression, 1,
      logical_and_expression);
	CFGAddRule(cfg, logical_or_expression, 3,
      logical_or_expression, OR_OP, logical_and_expression);

	CFGAddRule(cfg, conditional_expression, 1,
      logical_or_expression);
  // DEBUG
	CFGAddRule(cfg, conditional_expression, 5,
      logical_or_expression, QUESTION, expression,
      COLON, conditional_expression);

	CFGAddRule(cfg, assignment_expression, 1,
      conditional_expression);
  // DEBUG
	CFGAddRule(cfg, assignment_expression, 3,
      unary_expression, assignment_operator, assignment_expression);

	CFGAddRule(cfg, assignment_operator, 1, ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, MUL_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, DIV_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, MOD_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, ADD_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, SUB_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, LEFT_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, RIGHT_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, AND_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, XOR_ASSIGN);
	CFGAddRule(cfg, assignment_operator, 1, OR_ASSIGN);

	CFGAddRule(cfg, expression, 1, assignment_expression);
	CFGAddRule(cfg, expression, 3, expression, COMMA, assignment_expression);

	CFGAddRule(cfg, constant_expression, 1, conditional_expression);

	CFGAddRule(cfg, declaration, 2, declaration_specifiers, SEMICOLON);
	CFGAddRule(cfg, declaration, 3,
      declaration_specifiers, init_declarator_list, SEMICOLON);
	CFGAddRule(cfg, declaration, 1, static_assert_declaration);

	CFGAddRule(cfg, declaration_specifiers, 2,
      storage_class_specifier, declaration_specifiers);
	CFGAddRule(cfg, declaration_specifiers, 1,
      storage_class_specifier);
	CFGAddRule(cfg, declaration_specifiers, 2,
      type_specifier, declaration_specifiers);
	CFGAddRule(cfg, declaration_specifiers, 1,
      type_specifier);
	CFGAddRule(cfg, declaration_specifiers, 2,
      type_qualifier, declaration_specifiers);
	CFGAddRule(cfg, declaration_specifiers, 1,
      type_qualifier);
	CFGAddRule(cfg, declaration_specifiers, 2,
      function_specifier, declaration_specifiers);
	CFGAddRule(cfg, declaration_specifiers, 1,
      function_specifier);
	CFGAddRule(cfg, declaration_specifiers, 2,
      alignment_specifier, declaration_specifiers);
	CFGAddRule(cfg, declaration_specifiers, 1,
      alignment_specifier);

	CFGAddRule(cfg, init_declarator_list, 1, init_declarator);
	CFGAddRule(cfg, init_declarator_list, 3,
      init_declarator_list, COMMA, init_declarator);

	CFGAddRule(cfg, init_declarator, 3, declarator, ASSIGN, initializer);
	CFGAddRule(cfg, init_declarator, 1, declarator);

	CFGAddRule(cfg, storage_class_specifier, 1, TYPEDEF);
	CFGAddRule(cfg, storage_class_specifier, 1, EXTERN);
	CFGAddRule(cfg, storage_class_specifier, 1, STATIC);
	CFGAddRule(cfg, storage_class_specifier, 1, THREAD_LOCAL);
	CFGAddRule(cfg, storage_class_specifier, 1, AUTO);
	CFGAddRule(cfg, storage_class_specifier, 1, REGISTER);

	CFGAddRule(cfg, type_specifier, 1, VOID);
	CFGAddRule(cfg, type_specifier, 1, CHAR);
	CFGAddRule(cfg, type_specifier, 1, SHORT);
	CFGAddRule(cfg, type_specifier, 1, INT);
	CFGAddRule(cfg, type_specifier, 1, LONG);
	CFGAddRule(cfg, type_specifier, 1, FLOAT);
	CFGAddRule(cfg, type_specifier, 1, DOUBLE);
	CFGAddRule(cfg, type_specifier, 1, SIGNED);
	CFGAddRule(cfg, type_specifier, 1, UNSIGNED);
	CFGAddRule(cfg, type_specifier, 1, BOOL);
	CFGAddRule(cfg, type_specifier, 1, COMPLEX);
	CFGAddRule(cfg, type_specifier, 1, IMAGINARY);
	CFGAddRule(cfg, type_specifier, 1, atomic_type_specifier);
	CFGAddRule(cfg, type_specifier, 1, struct_or_union_specifier);
	CFGAddRule(cfg, type_specifier, 1, enum_specifier);
	CFGAddRule(cfg, type_specifier, 1, TYPEDEF_NAME);

  CFGAddRule(cfg, optional_identifier, 1, IDENTIFIER);
  CFGAddRule(cfg, optional_identifier, 0);

	CFGAddRule(cfg, struct_or_union_specifier, 5,
      struct_or_union, optional_identifier,
      LBRACE, struct_declaration_list, RBRACE);
	CFGAddRule(cfg, struct_or_union_specifier, 2,
      struct_or_union, IDENTIFIER);

	CFGAddRule(cfg, struct_or_union, 1, STRUCT);
	CFGAddRule(cfg, struct_or_union, 1, UNION);

	CFGAddRule(cfg, struct_declaration_list, 1,
      struct_declaration);
	CFGAddRule(cfg, struct_declaration_list, 2,
      struct_declaration_list, struct_declaration);

	CFGAddRule(cfg, struct_declaration, 2,
      specifier_qualifier_list, SEMICOLON);
	CFGAddRule(cfg, struct_declaration, 3,
      specifier_qualifier_list, struct_declarator_list, SEMICOLON);
	CFGAddRule(cfg, struct_declaration, 1,
      static_assert_declaration);

	CFGAddRule(cfg, specifier_qualifier_list, 2,
      type_specifier, specifier_qualifier_list);
	CFGAddRule(cfg, specifier_qualifier_list, 1,
      type_specifier);
	CFGAddRule(cfg, specifier_qualifier_list, 2,
      type_qualifier, specifier_qualifier_list);
	CFGAddRule(cfg, specifier_qualifier_list, 1,
      type_qualifier);

	CFGAddRule(cfg, struct_declarator_list, 1,
      struct_declarator);
	CFGAddRule(cfg, struct_declarator_list, 3,
      struct_declarator_list, COMMA, struct_declarator);

	CFGAddRule(cfg, struct_declarator, 2,
      COLON, constant_expression);
	CFGAddRule(cfg, struct_declarator, 3,
      declarator, COLON, constant_expression);
	CFGAddRule(cfg, struct_declarator, 1,
      declarator);

  CFGAddRule(cfg, optional_comma, 1, COMMA);
  CFGAddRule(cfg, optional_comma, 0);

	CFGAddRule(cfg, enum_specifier, 5,
      ENUM, LBRACE, enumerator_list, optional_comma, RBRACE);
	CFGAddRule(cfg, enum_specifier, 6,
      ENUM, IDENTIFIER, LBRACE, enumerator_list, optional_comma, RBRACE);
	CFGAddRule(cfg, enum_specifier, 2,
    ENUM, IDENTIFIER);

	CFGAddRule(cfg, enumerator_list, 1, enumerator);
	CFGAddRule(cfg, enumerator_list, 3,
      enumerator_list, COMMA, enumerator);

	CFGAddRule(cfg, enumerator, 3,
      enumeration_constant, ASSIGN, constant_expression);
	CFGAddRule(cfg, enumerator, 1,
      enumeration_constant);

	CFGAddRule(cfg, atomic_type_specifier, 4, ATOMIC, LPAREN, type_name, RPAREN);

	CFGAddRule(cfg, type_qualifier, 1, CONST);
	CFGAddRule(cfg, type_qualifier, 1, RESTRICT);
	CFGAddRule(cfg, type_qualifier, 1, VOLATILE);
	CFGAddRule(cfg, type_qualifier, 1, ATOMIC);

	CFGAddRule(cfg, function_specifier, 1, INLINE);
	CFGAddRule(cfg, function_specifier, 1, NORETURN);

	CFGAddRule(cfg, alignment_specifier, 4,
      ALIGNAS, LPAREN, type_name, RPAREN);
	CFGAddRule(cfg, alignment_specifier, 4,
      ALIGNAS, LPAREN, constant_expression, RPAREN);

	CFGAddRule(cfg, declarator, 2, pointer, direct_declarator);
	CFGAddRule(cfg, declarator, 1, direct_declarator);

	CFGAddRule(cfg, direct_declarator, 1,
      IDENTIFIER);
	CFGAddRule(cfg, direct_declarator, 3,
      LPAREN, declarator, RPAREN);
	CFGAddRule(cfg, direct_declarator, 3,
      direct_declarator, LBRACK, RBRACK);
	CFGAddRule(cfg, direct_declarator, 4,
      direct_declarator, LBRACK, MUL, RBRACK);
	CFGAddRule(cfg, direct_declarator, 6,
      direct_declarator, LBRACK, STATIC,
      type_qualifier_list, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_declarator, 5,
      direct_declarator, LBRACK, STATIC, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_declarator, 5,
      direct_declarator, LBRACK, type_qualifier_list, MUL, RBRACK);
	CFGAddRule(cfg, direct_declarator, 6,
      direct_declarator, LBRACK, type_qualifier_list, STATIC,
      assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_declarator, 5,
      direct_declarator, LBRACK, type_qualifier_list,
      assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_declarator, 4,
      direct_declarator, LBRACK, type_qualifier_list, RBRACK);
	CFGAddRule(cfg, direct_declarator, 4,
      direct_declarator, LBRACK, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_declarator, 4,
      direct_declarator, LPAREN, parameter_type_list, RPAREN);
	CFGAddRule(cfg, direct_declarator, 4,
      direct_declarator, LPAREN, identifier_list, RPAREN);

	CFGAddRule(cfg, pointer, 3, MUL, type_qualifier_list, pointer);
	CFGAddRule(cfg, pointer, 2, MUL, type_qualifier_list);
	CFGAddRule(cfg, pointer, 2, MUL, pointer);
	CFGAddRule(cfg, pointer, 1, MUL);

	CFGAddRule(cfg, type_qualifier_list, 1, type_qualifier);
	CFGAddRule(cfg, type_qualifier_list, 2, type_qualifier_list, type_qualifier);

	CFGAddRule(cfg, parameter_type_list, 3, parameter_list, COMMA, ELLIPSIS);
	CFGAddRule(cfg, parameter_type_list, 1, parameter_list);
  CFGAddRule(cfg, parameter_type_list, 0);

	CFGAddRule(cfg, parameter_list, 1,
      parameter_declaration);
	CFGAddRule(cfg, parameter_list, 3,
      parameter_list, COMMA, parameter_declaration);

	CFGAddRule(cfg, parameter_declaration, 2,
      declaration_specifiers, declarator);
	CFGAddRule(cfg, parameter_declaration, 2,
      declaration_specifiers, abstract_declarator);
	CFGAddRule(cfg, parameter_declaration, 1,
      declaration_specifiers);

	CFGAddRule(cfg, identifier_list, 1, IDENTIFIER);
	CFGAddRule(cfg, identifier_list, 3, identifier_list, COMMA, IDENTIFIER);

	CFGAddRule(cfg, type_name, 2,
      specifier_qualifier_list, abstract_declarator);
	CFGAddRule(cfg, type_name, 1,
      specifier_qualifier_list);

	CFGAddRule(cfg, abstract_declarator, 2, pointer, direct_abstract_declarator);
	CFGAddRule(cfg, abstract_declarator, 1, pointer);
	CFGAddRule(cfg, abstract_declarator, 1, direct_abstract_declarator);

  CFGAddRule(cfg, optional_mul, 1, MUL);
  CFGAddRule(cfg, optional_mul, 0);

	CFGAddRule(cfg, direct_abstract_declarator, 3,
      LPAREN, abstract_declarator, RPAREN);
	CFGAddRule(cfg, direct_abstract_declarator, 3,
      LBRACK, optional_mul, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 5,
      LBRACK, STATIC, type_qualifier_list, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 4,
      LBRACK, STATIC, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 5,
      LBRACK, type_qualifier_list, STATIC, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 4,
      LBRACK, type_qualifier_list, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 3,
      LBRACK, type_qualifier_list, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 3,
      LBRACK, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 4,
      direct_abstract_declarator, LBRACK, optional_mul, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 6,
      direct_abstract_declarator, LBRACK, STATIC, type_qualifier_list,
      assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 5,
      direct_abstract_declarator, LBRACK, STATIC, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 5,
      direct_abstract_declarator, LBRACK, type_qualifier_list,
      assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 6,
      direct_abstract_declarator, LBRACK, type_qualifier_list, STATIC,
      assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 4,
      direct_abstract_declarator, LBRACK, type_qualifier_list, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 4,
      direct_abstract_declarator, LBRACK, assignment_expression, RBRACK);
	CFGAddRule(cfg, direct_abstract_declarator, 3,
      LPAREN, parameter_type_list, RPAREN);
	CFGAddRule(cfg, direct_abstract_declarator, 4,
      direct_abstract_declarator, LPAREN, parameter_type_list, RPAREN);

	CFGAddRule(cfg, initializer, 3, LBRACE, initializer_list, RBRACE);
	CFGAddRule(cfg, initializer, 4, LBRACE, initializer_list, COMMA, RBRACE);
	CFGAddRule(cfg, initializer, 1, assignment_expression);

	CFGAddRule(cfg, initializer_list, 2,
      designation, initializer);
	CFGAddRule(cfg, initializer_list, 1,
      initializer);
	CFGAddRule(cfg, initializer_list, 4,
      initializer_list, COMMA, designation, initializer);
	CFGAddRule(cfg, initializer_list, 3,
      initializer_list, COMMA, initializer);

	CFGAddRule(cfg, designation, 2, designator_list, ASSIGN);

	CFGAddRule(cfg, designator_list, 1, designator);
	CFGAddRule(cfg, designator_list, 2, designator_list, designator);

	CFGAddRule(cfg, designator, 3, LBRACK, constant_expression, RBRACK);
	CFGAddRule(cfg, designator, 2, DOT, IDENTIFIER);

	CFGAddRule(cfg, static_assert_declaration, 7,
      STATIC_ASSERT, LPAREN, constant_expression, COMMA,
      STRING_LITERAL, RPAREN, SEMICOLON);

	CFGAddRule(cfg, statement, 1, labeled_statement);
	CFGAddRule(cfg, statement, 1, compound_statement);
	CFGAddRule(cfg, statement, 1, expression_statement);
	CFGAddRule(cfg, statement, 1, selection_statement);
	CFGAddRule(cfg, statement, 1, iteration_statement);
	CFGAddRule(cfg, statement, 1, jump_statement);

	CFGAddRule(cfg, labeled_statement, 3,
      IDENTIFIER, COLON, statement);
	CFGAddRule(cfg, labeled_statement, 4,
      CASE, constant_expression, COLON, statement);
	CFGAddRule(cfg, labeled_statement, 3,
      DEFAULT, COLON, statement);

	CFGAddRule(cfg, compound_statement, 3, LBRACE, block_item_list, RBRACE);

	CFGAddRule(cfg, block_item_list, 1, block_item_list, block_item);
  CFGAddRule(cfg, block_item_list, 0);

	CFGAddRule(cfg, block_item, 1, declaration);
	CFGAddRule(cfg, block_item, 1, statement);

	CFGAddRule(cfg, expression_statement, 1, SEMICOLON);
	CFGAddRule(cfg, expression_statement, 2, expression, SEMICOLON);

	CFGAddRule(cfg, selection_statement, 7,
      IF, LPAREN, expression, RPAREN, statement, ELSE, statement);
	CFGAddRule(cfg, selection_statement, 5,
      IF, LPAREN, expression, RPAREN, statement);
	CFGAddRule(cfg, selection_statement, 5,
      SWITCH, LPAREN, expression, RPAREN, statement);

  CFGAddRule(cfg, optional_expression, 1, expression);
  CFGAddRule(cfg, optional_expression, 0);

	CFGAddRule(cfg, iteration_statement, 5,
      WHILE, LPAREN, expression, RPAREN, statement);
	CFGAddRule(cfg, iteration_statement, 7,
      DO, statement, WHILE, LPAREN, expression, RPAREN, SEMICOLON);
	CFGAddRule(cfg, iteration_statement, 7,
      FOR, LPAREN, expression_statement, expression_statement,
      optional_expression, RPAREN, statement);
	CFGAddRule(cfg, iteration_statement, 7,
      FOR, LPAREN, declaration, expression_statement,
      optional_expression, RPAREN, statement);

	CFGAddRule(cfg, jump_statement, 3, GOTO, IDENTIFIER, SEMICOLON);
	CFGAddRule(cfg, jump_statement, 2, CONTINUE, SEMICOLON);
	CFGAddRule(cfg, jump_statement, 2, BREAK, SEMICOLON);
	CFGAddRule(cfg, jump_statement, 2, RETURN, SEMICOLON);
	CFGAddRule(cfg, jump_statement, 3, RETURN, expression, SEMICOLON);

	CFGAddRule(cfg, translation_unit, 1, external_declaration);
	CFGAddRule(cfg, translation_unit, 2, translation_unit, external_declaration);

	CFGAddRule(cfg, external_declaration, 1, function_definition);
	CFGAddRule(cfg, external_declaration, 1, declaration);

	CFGAddRule(cfg, function_definition, 4,
      declaration_specifiers, declarator, declaration_list, compound_statement);

	CFGAddRule(cfg, declaration_list, 2, declaration_list, declaration);
  CFGAddRule(cfg, declaration_list, 0);

  graph = LR1StateGraphFromCFG(cfg);

  tokens = VectorNew();
  VectorAdd(tokens, "IDENTIFIER");
  VectorAdd(tokens, "I_CONSTANT");
  VectorAdd(tokens, "F_CONSTANT");
  VectorAdd(tokens, "STRING_LITERAL");
  VectorAdd(tokens, "FUNC_NAME");
  VectorAdd(tokens, "SIZEOF");
  VectorAdd(tokens, "PTR_OP");
  VectorAdd(tokens, "INC_OP");
  VectorAdd(tokens, "DEC_OP");
  VectorAdd(tokens, "LEFT_OP");
  VectorAdd(tokens, "RIGHT_OP");
  VectorAdd(tokens, "LE_OP");
  VectorAdd(tokens, "GE_OP");
  VectorAdd(tokens, "EQ_OP");
  VectorAdd(tokens, "NE_OP");
  VectorAdd(tokens, "AND_OP");
  VectorAdd(tokens, "OR_OP");
  VectorAdd(tokens, "MUL_ASSIGN");
  VectorAdd(tokens, "DIV_ASSIGN");
  VectorAdd(tokens, "MOD_ASSIGN");
  VectorAdd(tokens, "ADD_ASSIGN");
  VectorAdd(tokens, "SUB_ASSIGN");
  VectorAdd(tokens, "LEFT_ASSIGN");
  VectorAdd(tokens, "RIGHT_ASSIGN");
  VectorAdd(tokens, "AND_ASSIGN");
  VectorAdd(tokens, "XOR_ASSIGN");
  VectorAdd(tokens, "OR_ASSIGN");
  VectorAdd(tokens, "TYPEDEF_NAME");
  VectorAdd(tokens, "ENUMERATION_CONSTANT");
  VectorAdd(tokens, "TYPEDEF");
  VectorAdd(tokens, "EXTERN");
  VectorAdd(tokens, "STATIC");
  VectorAdd(tokens, "AUTO");
  VectorAdd(tokens, "REGISTER");
  VectorAdd(tokens, "INLINE");
  VectorAdd(tokens, "CONST");
  VectorAdd(tokens, "RESTRICT");
  VectorAdd(tokens, "VOLATILE");
  VectorAdd(tokens, "BOOL");
  VectorAdd(tokens, "CHAR");
  VectorAdd(tokens, "SHORT");
  VectorAdd(tokens, "INT");
  VectorAdd(tokens, "LONG");
  VectorAdd(tokens, "SIGNED");
  VectorAdd(tokens, "UNSIGNED");
  VectorAdd(tokens, "FLOAT");
  VectorAdd(tokens, "DOUBLE");
  VectorAdd(tokens, "VOID");
  VectorAdd(tokens, "COMPLEX");
  VectorAdd(tokens, "IMAGINARY");
  VectorAdd(tokens, "STRUCT");
  VectorAdd(tokens, "UNION");
  VectorAdd(tokens, "ENUM");
  VectorAdd(tokens, "ELLIPSIS");
  VectorAdd(tokens, "CASE");
  VectorAdd(tokens, "DEFAULT");
  VectorAdd(tokens, "IF");
  VectorAdd(tokens, "ELSE");
  VectorAdd(tokens, "SWITCH");
  VectorAdd(tokens, "WHILE");
  VectorAdd(tokens, "DO");
  VectorAdd(tokens, "FOR");
  VectorAdd(tokens, "GOTO");
  VectorAdd(tokens, "CONTINUE");
  VectorAdd(tokens, "BREAK");
  VectorAdd(tokens, "RETURN");
  VectorAdd(tokens, "ALIGNAS");
  VectorAdd(tokens, "ALIGNOF");
  VectorAdd(tokens, "ATOMIC");
  VectorAdd(tokens, "GENERIC");
  VectorAdd(tokens, "NORETURN");
  VectorAdd(tokens, "STATIC_ASSERT");
  VectorAdd(tokens, "THREAD_LOCAL");
  VectorAdd(tokens, "LPAREN");
  VectorAdd(tokens, "RPAREN");
  VectorAdd(tokens, "LBRACK");
  VectorAdd(tokens, "RBRACK");
  VectorAdd(tokens, "LBRACE");
  VectorAdd(tokens, "RBRACE");
  VectorAdd(tokens, "COMMA");
  VectorAdd(tokens, "COLON");
  VectorAdd(tokens, "SEMICOLON");
  VectorAdd(tokens, "DOT");
  VectorAdd(tokens, "ADD");
  VectorAdd(tokens, "SUB");
  VectorAdd(tokens, "MUL");
  VectorAdd(tokens, "DIV");
  VectorAdd(tokens, "MOD");
  VectorAdd(tokens, "LT");
  VectorAdd(tokens, "GT");
  VectorAdd(tokens, "ASSIGN");
  VectorAdd(tokens, "NOT_OP");
  VectorAdd(tokens, "QUESTION");
  VectorAdd(tokens, "BITWISE_AND");
  VectorAdd(tokens, "BITWISE_OR");
  VectorAdd(tokens, "BITWISE_NOT");
  VectorAdd(tokens, "BITWISE_XOR");

  variables = VectorNew();

  // DEBUG
  // VectorAdd(variables, "unary_expression");
  // VectorAdd(variables, "multiplicative_expression");
  // VectorAdd(variables, "expression");

  VectorAdd(variables, "primary_expression");
  VectorAdd(variables, "constant");
  VectorAdd(variables, "enumeration_constant");
  VectorAdd(variables, "string");
  VectorAdd(variables, "generic_selection");
  VectorAdd(variables, "generic_assoc_list");
  VectorAdd(variables, "generic_association");
  VectorAdd(variables, "postfix_expression");
  VectorAdd(variables, "argument_expression_list");
  // DEBUG
  VectorAdd(variables, "unary_expression");
  VectorAdd(variables, "unary_operator");
  VectorAdd(variables, "cast_expression");
  // DEBUG
  VectorAdd(variables, "multiplicative_expression");
  VectorAdd(variables, "additive_expression");
  VectorAdd(variables, "shift_expression");
  VectorAdd(variables, "relational_expression");
  VectorAdd(variables, "equality_expression");
  VectorAdd(variables, "and_expression");
  VectorAdd(variables, "exclusive_or_expression");
  VectorAdd(variables, "inclusive_or_expression");
  VectorAdd(variables, "logical_and_expression");
  VectorAdd(variables, "logical_or_expression");
  VectorAdd(variables, "conditional_expression");
  VectorAdd(variables, "assignment_expression");
  VectorAdd(variables, "assignment_operator");
  // DEBUG
  VectorAdd(variables, "expression");
  VectorAdd(variables, "constant_expression");
  VectorAdd(variables, "declaration");
  VectorAdd(variables, "declaration_specifiers");
  VectorAdd(variables, "init_declarator_list");
  VectorAdd(variables, "init_declarator");
  VectorAdd(variables, "storage_class_specifier");
  VectorAdd(variables, "type_specifier");
  VectorAdd(variables, "optional_identifier");
  VectorAdd(variables, "struct_or_union_specifier");
  VectorAdd(variables, "struct_or_union");
  VectorAdd(variables, "struct_declaration_list");
  VectorAdd(variables, "struct_declaration");
  VectorAdd(variables, "specifier_qualifier_list");
  VectorAdd(variables, "struct_declarator_list");
  VectorAdd(variables, "struct_declarator");
  VectorAdd(variables, "optional_comma");
  VectorAdd(variables, "enum_specifier");
  VectorAdd(variables, "enumerator_list");
  VectorAdd(variables, "enumerator");
  VectorAdd(variables, "atomic_type_specifier");
  VectorAdd(variables, "type_qualifier");
  VectorAdd(variables, "function_specifier");
  VectorAdd(variables, "alignment_specifier");
  VectorAdd(variables, "declarator");
  VectorAdd(variables, "direct_declarator");
  VectorAdd(variables, "pointer");
  VectorAdd(variables, "type_qualifier_list");
  VectorAdd(variables, "parameter_type_list");
  VectorAdd(variables, "parameter_list");
  VectorAdd(variables, "parameter_declaration");
  VectorAdd(variables, "identifier_list");
  VectorAdd(variables, "type_name");
  VectorAdd(variables, "abstract_declarator");
  VectorAdd(variables, "optional_mul");
  VectorAdd(variables, "direct_abstract_declarator");
  VectorAdd(variables, "initializer");
  VectorAdd(variables, "initializer_list");
  VectorAdd(variables, "designation");
  VectorAdd(variables, "designator_list");
  VectorAdd(variables, "designator");
  VectorAdd(variables, "static_assert_declaration");
  VectorAdd(variables, "statement");
  VectorAdd(variables, "labeled_statement");
  VectorAdd(variables, "compound_statement");
  VectorAdd(variables, "block_item_list");
  VectorAdd(variables, "block_item");
  VectorAdd(variables, "expression_statement");
  VectorAdd(variables, "selection_statement");
  VectorAdd(variables, "optional_expression");
  VectorAdd(variables, "iteration_statement");
  VectorAdd(variables, "jump_statement");
  VectorAdd(variables, "translation_unit");
  VectorAdd(variables, "external_declaration");
  VectorAdd(variables, "function_definition");
  VectorAdd(variables, "declaration_list");

  // DEBUG
  printf("%lu states\n", graph->states->size);

  file = fopen("6.out", "w");
  LR1StateGraphPrint(cfg, graph, tokens, variables, file);
  fclose(file);
  VectorDelete(tokens);
  VectorDelete(variables);
  CFGDelete(cfg);
  LR1StateGraphDelete(graph);
}
