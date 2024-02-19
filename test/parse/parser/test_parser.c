#define PARSER_HANDLER_FILE_NAME "parse/parser/test_handler.h"
#include "lex/regex/regex.h"
#include "lex/dfa/dfa.h"
#include "lex/lexer/lexer.h"
#include "parse/cfg/cfg.h"
#include "parse/parser/parser.h"
#include "util/vector/vector.h"
#include <assert.h>
#include <stdlib.h>

void NullDestructor(void *obj) {
}

int ParserObjToInt(ParserObject *obj) {
  LexerToken *token;
  char c;
  int ret;
  switch (obj->type) {
    case PARSER_OBJECT_TOKEN:
      token = obj->token;
      c = token->str[token->length];
      token->str[token->length] = '\0';
      ret = atoi((const char*)token->str);
      token->str[token->length] = c;
      LexerTokenDelete(token);
      break;
    case PARSER_OBJECT_OBJECT:
      ret = (int)(long long)obj->object;
      break;
  }
  return ret;
}

ParserDeclareHandler(NullHandler, rhs) {
  int numRHS = rhs->size;
  for (int i = 0; i < numRHS; ++i) {
    LexerToken *token = rhs->arr[i];
    if (token)
      LexerTokenDelete(token);
  }
  return NULL;
}

ParserDeclareHandler(AddHandler, rhs) {
  assert(rhs->size == 3);
  LexerTokenDelete(rhs->arr[1]);
  return (void*)(
      (long long)rhs->arr[0] + (long long)rhs->arr[2]);
}

ParserDeclareHandler(SubHandler, rhs) {
  assert(rhs->size == 3);
  LexerTokenDelete(rhs->arr[1]);
  return (void*)(
      (long long)rhs->arr[0] - (long long)rhs->arr[2]);
}

ParserDeclareHandler(MulHandler, rhs) {
  assert(rhs->size == 3);
  LexerTokenDelete(rhs->arr[1]);
  return (void*)(
      (long long)rhs->arr[0] * (long long)rhs->arr[2]);
}

ParserDeclareHandler(DivHandler, rhs) {
  assert(rhs->size == 3);
  LexerTokenDelete(rhs->arr[1]);
  return (void*)(
      (long long)rhs->arr[0] / (long long)rhs->arr[2]);
}

ParserDeclareHandler(MoveHandler, rhs) {
  assert(rhs->size == 1);
  return rhs->arr[0];
}

ParserDeclareHandler(ParenHandler, rhs) {
  assert(rhs->size == 3);
  LexerTokenDelete(rhs->arr[0]);
  LexerTokenDelete(rhs->arr[2]);
  return rhs->arr[1];
}

ParserDeclareHandler(StrToIntHandler, rhs) {
  assert(rhs->size == 1);
  LexerToken *token = rhs->arr[0];
  char c = token->str[token->length];
  token->str[token->length] = '\0';
  void *ret = (void*)atoll((const char*)token->str);
  token->str[token->length] = c;
  LexerTokenDelete(token);
  return ret;
}

void test1() {
  // Setup lexer
  LexerConfig *lexerConfig = LexerConfigNew();
  Regex *regA = RegexFromLetter('a');
  Regex *regB = RegexFromLetter('b');
  Regex *regWhitespace = RegexFromUnion(4,
    RegexFromLetter(' '),
    RegexFromLetter('\n'),
    RegexFromLetter('\t'),
    RegexFromLetter('\r'));
  int tokA = LexerConfigAddRegex(lexerConfig, regA);
  int tokB = LexerConfigAddRegex(lexerConfig, regB);
  LexerConfigSetIgnoreRegex(lexerConfig, regWhitespace);
  Lexer *lexer = LexerFromConfig(lexerConfig);

  // Setup parser
  CFG *cfg = CFGNew();
  CFGSetNumTokens(cfg, 2);
  int varS = CFGAddVariable(cfg);
  int varA = CFGAddVariable(cfg);
  ParserConfig *parserConfig = ParserConfigNew(
      lexer, cfg, false, NullDestructor);
  ParserAddRuleAndHandler(parserConfig, NullHandler, varS, 2, varA, varA);
  ParserAddRuleAndHandler(parserConfig, NullHandler, varA, 2, tokA, varA);
  ParserAddRuleAndHandler(parserConfig, NullHandler, varA, 1, tokB);
  Parser *parser = ParserFromConfig(parserConfig);

  // 1. Parse file without error
  FILE *file = fopen("1.in", "r");
  LexerSetInputFile(lexer, file, "1.in");
  assert(ParserParse(parser) == NULL);
  fclose(file);

  // 2. Parse file with error
  file = fopen("2.in", "r");
  LexerSetInputFile(lexer, file, "2.in");
  assert(ParserParse(parser) == PARSER_OBJECT_FAILURE);
  fclose(file);

  // Cleanup
  RegexDelete(regA);
  RegexDelete(regB);
  RegexDelete(regWhitespace);
  LexerConfigDelete(lexerConfig);
  LexerDelete(lexer);
  CFGDelete(cfg);
  ParserDelete(parser);
}

void test2() {
  // Setup lexer
  Regex *regAdd = RegexFromLetter('+');
  Regex *regSub = RegexFromLetter('-');
  Regex *regMul = RegexFromLetter('*');
  Regex *regDiv = RegexFromLetter('/');
  Regex *regLParen = RegexFromLetter('(');
  Regex *regRParen = RegexFromLetter(')');
  Regex *regNum = RegexOneOrMore(REGEX_DIGITS);
  Regex *regWhitespace = RegexFromUnion(4,
    RegexFromLetter(' '),
    RegexFromLetter('\n'),
    RegexFromLetter('\t'),
    RegexFromLetter('\r'));
  LexerConfig *lexerConfig = LexerConfigNew();
  int tokAdd = LexerConfigAddRegex(lexerConfig, regAdd);
  int tokSub = LexerConfigAddRegex(lexerConfig, regSub);
  int tokMul = LexerConfigAddRegex(lexerConfig, regMul);
  int tokDiv = LexerConfigAddRegex(lexerConfig, regDiv);
  int tokLParen = LexerConfigAddRegex(lexerConfig, regLParen);
  int tokRParen = LexerConfigAddRegex(lexerConfig, regRParen);
  int tokNum = LexerConfigAddRegex(lexerConfig, regNum);
  LexerConfigSetIgnoreRegex(lexerConfig, regWhitespace);
  Lexer *lexer = LexerFromConfig(lexerConfig);

  // Setup parser
  CFG *cfg = CFGNew();
  CFGSetNumTokens(cfg, 7);
  int addExpr = CFGAddVariable(cfg);
  int mulExpr = CFGAddVariable(cfg);
  int term = CFGAddVariable(cfg);
  ParserConfig *parserConfig = ParserConfigNew(
      lexer, cfg, false, NullDestructor);
  ParserAddRuleAndHandler(
      parserConfig, AddHandler, addExpr, 3, addExpr, tokAdd, mulExpr);
  ParserAddRuleAndHandler(
      parserConfig, SubHandler, addExpr, 3, addExpr, tokSub, mulExpr);
  ParserAddRuleAndHandler(
      parserConfig, MoveHandler, addExpr, 1, mulExpr);
  ParserAddRuleAndHandler(
      parserConfig, MulHandler, mulExpr, 3, mulExpr, tokMul, term);
  ParserAddRuleAndHandler(
      parserConfig, DivHandler, mulExpr, 3, mulExpr, tokDiv, term);
  ParserAddRuleAndHandler(
      parserConfig, MoveHandler, mulExpr, 1, term);
  ParserAddRuleAndHandler(
      parserConfig, ParenHandler, term, 3, tokLParen, addExpr, tokRParen);
  ParserAddRuleAndHandler(
      parserConfig, StrToIntHandler, term, 1, tokNum);
  Parser *parser = ParserFromConfig(parserConfig);

  // 3. Parse file without error
  FILE *file = fopen("3.in", "r");
  LexerSetInputFile(lexer, file, "3.in");
  void *result = ParserParse(parser);
  fclose(file);
  file = fopen("3.out", "w");
  fprintf(file, "%d\n", (int)(long long)result);
  fclose(file);

  // 4. Save and load LR(1) parser and parse file with error (unexpected token)
  FILE *parserFile = fopen("parser2.txt", "w");
  ParserToFile(parser, parserFile);
  ParserDelete(parser);
  fclose(parserFile);
  parserFile = fopen("parser2.txt", "r");
  parser = ParserFromFile(parserFile, lexer, NullDestructor);
  file = fopen("4.in", "r");
  LexerSetInputFile(lexer, file, "4.in");
  assert(ParserParse(parser) == PARSER_OBJECT_FAILURE);
  fclose(file);

  // 5. Parse file with error (unexpected token)
  file = fopen("5.in", "r");
  LexerSetInputFile(lexer, file, "5.in");
  assert(ParserParse(parser) == PARSER_OBJECT_FAILURE);
  fclose(file);

  // 6. Use same LR(1) parser to parse file with error (unexpected end of file)
  file = fopen("6.in", "r");
  LexerSetInputFile(lexer, file, "6.in");
  assert(ParserParse(parser) == PARSER_OBJECT_FAILURE);
  fclose(file);

  // Cleanup
  RegexDelete(regAdd);
  RegexDelete(regSub);
  RegexDelete(regMul);
  RegexDelete(regDiv);
  RegexDelete(regLParen);
  RegexDelete(regRParen);
  RegexDelete(regNum);
  RegexDelete(regWhitespace);
  LexerConfigDelete(lexerConfig);
  LexerDelete(lexer);
  CFGDelete(cfg);
  ParserDelete(parser);
}

#define test3GenerateParser(parser, useLALR1) \
  do { \
    CFG *cfg = CFGNew(); \
    CFGSetNumTokens(cfg, numTokens); \
    int functionList = CFGAddVariable(cfg); \
    int function = CFGAddVariable(cfg); \
    int block = CFGAddVariable(cfg); \
    int paramList = CFGAddVariable(cfg); \
    int nonEmptyParamList = CFGAddVariable(cfg); \
    int param = CFGAddVariable(cfg); \
    int type = CFGAddVariable(cfg); \
    int stmtList = CFGAddVariable(cfg); \
    int stmt = CFGAddVariable(cfg); \
    int elseIfBlocks = CFGAddVariable(cfg); \
    int var = CFGAddVariable(cfg); \
    int relOp = CFGAddVariable(cfg); \
    int addOp = CFGAddVariable(cfg); \
    int mulOp = CFGAddVariable(cfg); \
    int orExpr = CFGAddVariable(cfg); \
    int andExpr = CFGAddVariable(cfg); \
    int relExpr = CFGAddVariable(cfg); \
    int addExpr = CFGAddVariable(cfg); \
    int mulExpr = CFGAddVariable(cfg); \
    int term = CFGAddVariable(cfg); \
    ParserConfig *parserConfig = ParserConfigNew( \
        lexer, cfg, useLALR1, NullDestructor); \
    /* functions */ \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, functionList, 2, functionList, function); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, functionList, 0); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, function, 6, \
        type, identifier, lParen, paramList, rParen, block); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, block, 3, lBrace, stmtList, rBrace); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, paramList, 1, nonEmptyParamList); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, paramList, 0); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, nonEmptyParamList, 3, \
        nonEmptyParamList, comma, param); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, nonEmptyParamList, 1, param); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, param, 2, type, identifier); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, type, 1, intKeyword); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, type, 1, floatKeyword); \
    /* statements */ \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, stmtList, 2, stmtList, stmt); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, stmtList, 0); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, stmt, 4, var, assign, orExpr, semicol); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, stmt, 5, \
        whileKeyword, lParen, orExpr, rParen, block); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, stmt, 6, \
        ifKeyword, lParen, orExpr, rParen, block, elseIfBlocks); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, stmt, 8, \
        ifKeyword, lParen, orExpr, rParen, block, elseIfBlocks, \
        elseKeyword, block); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, elseIfBlocks, 7, \
        elseIfBlocks, elseKeyword, ifKeyword, lParen, orExpr, rParen, block); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, elseIfBlocks, 0); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, stmt, 3, \
        returnKeyword, orExpr, semicol); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, stmt, 2, \
        returnKeyword, semicol); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, stmt, 1, block); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, var, 1, identifier); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, var, 4, identifier, lBrack, orExpr, rBrack); \
    /* operators */ \
    ParserAddRuleAndHandler(parserConfig, NullHandler, relOp, 1, lt); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, relOp, 1, le); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, relOp, 1, gt); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, relOp, 1, ge); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, relOp, 1, eq); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, relOp, 1, ne); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, addOp, 1, add); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, addOp, 1, sub); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, mulOp, 1, mul); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, mulOp, 1, div); \
    /* expressions */ \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, orExpr, 3, orExpr, or, andExpr); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, orExpr, 1, andExpr); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, andExpr, 3, andExpr, and, relExpr); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, andExpr, 1, relExpr); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, relExpr, 3, addExpr, relOp, addExpr); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, relExpr, 1, addExpr); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, addExpr, 3, addExpr, addOp, mulExpr); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, addExpr, 1, mulExpr); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, mulExpr, 3, mulExpr, mulOp, term); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, mulExpr, 1, term); \
    /* terms */ \
    ParserAddRuleAndHandler(parserConfig, NullHandler, term, 1, var); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, term, 1, num); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, term, 1, floatNum); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, term, 2, not, term); \
    ParserAddRuleAndHandler( \
        parserConfig, NullHandler, term, 3, lParen, orExpr, rParen); \
    parser = ParserFromConfig(parserConfig); \
    /* Cleanup */ \
    CFGDelete(cfg); \
  } while (0)

#define test3ParseFiles(parserFilename) \
  do { \
    /* Save parser to file and load from file */ \
    FILE *parserFile = fopen(parserFilename, "w"); \
    ParserToFile(parser, parserFile); \
    ParserDelete(parser); \
    fclose(parserFile); \
    parserFile = fopen(parserFilename, "r"); \
    parser = ParserFromFile(parserFile, lexer, NullDestructor); \
    /* 7. Parse file without error */ \
    FILE *file = fopen("7.in", "r"); \
    LexerSetInputFile(lexer, file, "7.in"); \
    assert(ParserParse(parser) == NULL); \
    fclose(file); \
    /* 8. Parse file with error (unexpected token) */ \
    file = fopen("8.in", "r"); \
    LexerSetInputFile(lexer, file, "8.in"); \
    assert(ParserParse(parser) == PARSER_OBJECT_FAILURE); \
    fclose(file); \
    /* 9. Parse file with error (unexpected end of file) */ \
    file = fopen("9.in", "r"); \
    LexerSetInputFile(lexer, file, "9.in"); \
    assert(ParserParse(parser) == PARSER_OBJECT_FAILURE); \
    fclose(file); \
  } while (0)
  

void test3() {
  // Setup lexer
  Regex *regLParen = RegexFromLetter('(');
  Regex *regRParen = RegexFromLetter(')');
  Regex *regLBrack = RegexFromLetter('[');
  Regex *regRBrack = RegexFromLetter(']');
  Regex *regLBrace = RegexFromLetter('{');
  Regex *regRBrace = RegexFromLetter('}');
  Regex *regAdd = RegexFromLetter('+');
  Regex *regSub = RegexFromLetter('-');
  Regex *regMul = RegexFromLetter('*');
  Regex *regDiv = RegexFromLetter('/');
  Regex *regLe = RegexFromString("<=");
  Regex *regLt = RegexFromLetter('<');
  Regex *regGe = RegexFromString(">=");
  Regex *regGt = RegexFromLetter('>');
  Regex *regEq = RegexFromString("==");
  Regex *regNe = RegexFromString("!=");
  Regex *regAnd = RegexFromString("&&");
  Regex *regOr = RegexFromString("||");
  Regex *regNot = RegexFromLetter('!');
  Regex *regAssign = RegexFromLetter('=');
  Regex *regComma = RegexFromLetter(',');
  Regex *regSemicol = RegexFromLetter(';');
  Regex *regNum = RegexOneOrMore(REGEX_DIGITS);
  Regex *regFloatNum = RegexFromConcat(3, regNum, RegexFromLetter('.'), regNum);
  Regex *regIntKeyword = RegexFromString("int");
  Regex *regFloatKeyword = RegexFromString("float");
  Regex *regIfKeyword = RegexFromString("if");
  Regex *regElseKeyword = RegexFromString("else");
  Regex *regWhileKeyword = RegexFromString("while");
  Regex *regReturnKeyword = RegexFromString("return");
  Regex *regUnderscoreOrLetter = RegexFromUnion(
      2, RegexFromLetter('_'), REGEX_LETTERS);
  Regex *regIdentifier = RegexFromConcat(
      2, regUnderscoreOrLetter,
      RegexZeroOrMore(RegexFromUnion(2, regUnderscoreOrLetter, REGEX_DIGITS)));
  Regex *regWhitespace = RegexFromUnion(4,
      RegexFromLetter(' '),
      RegexFromLetter('\n'),
      RegexFromLetter('\t'),
      RegexFromLetter('\r'));
  LexerConfig *lexerConfig = LexerConfigNew();
  int add = LexerConfigAddRegex(lexerConfig, regAdd);
  int sub = LexerConfigAddRegex(lexerConfig, regSub);
  int mul = LexerConfigAddRegex(lexerConfig, regMul);
  int div = LexerConfigAddRegex(lexerConfig, regDiv);
  int lParen = LexerConfigAddRegex(lexerConfig, regLParen);
  int rParen = LexerConfigAddRegex(lexerConfig, regRParen);
  int lBrack = LexerConfigAddRegex(lexerConfig, regLBrack);
  int rBrack = LexerConfigAddRegex(lexerConfig, regRBrack);
  int lBrace = LexerConfigAddRegex(lexerConfig, regLBrace);
  int rBrace = LexerConfigAddRegex(lexerConfig, regRBrace);
  int le = LexerConfigAddRegex(lexerConfig, regLe);
  int lt = LexerConfigAddRegex(lexerConfig, regLt);
  int ge = LexerConfigAddRegex(lexerConfig, regGe);
  int gt = LexerConfigAddRegex(lexerConfig, regGt);
  int eq = LexerConfigAddRegex(lexerConfig, regEq);
  int ne = LexerConfigAddRegex(lexerConfig, regNe);
  int and = LexerConfigAddRegex(lexerConfig, regAnd);
  int or = LexerConfigAddRegex(lexerConfig, regOr);
  int not = LexerConfigAddRegex(lexerConfig, regNot);
  int assign = LexerConfigAddRegex(lexerConfig, regAssign);
  int comma = LexerConfigAddRegex(lexerConfig, regComma);
  int semicol = LexerConfigAddRegex(lexerConfig, regSemicol);
  int num = LexerConfigAddRegex(lexerConfig, regNum);
  int floatNum = LexerConfigAddRegex(lexerConfig, regFloatNum);
  int intKeyword = LexerConfigAddRegex(lexerConfig, regIntKeyword);
  int floatKeyword = LexerConfigAddRegex(lexerConfig, regFloatKeyword);
  int ifKeyword = LexerConfigAddRegex(lexerConfig, regIfKeyword);
  int elseKeyword = LexerConfigAddRegex(lexerConfig, regElseKeyword);
  int whileKeyword = LexerConfigAddRegex(lexerConfig, regWhileKeyword);
  int returnKeyword = LexerConfigAddRegex(lexerConfig, regReturnKeyword);
  int identifier = LexerConfigAddRegex(lexerConfig, regIdentifier);
  int numTokens = lexerConfig->nfas->size;
  LexerConfigSetIgnoreRegex(lexerConfig, regWhitespace);
  Lexer *lexer = LexerFromConfig(lexerConfig);

  // Setup LR(1) parser
  Parser *parser;
  test3GenerateParser(parser, false);
  FILE *numStatesFile = fopen("7-canonical-lr.out", "w");
  fprintf(numStatesFile, "%d\n", parser->numStates);
  fclose(numStatesFile);

  // Parse files
  test3ParseFiles("parser3-canonical-lr.txt");
  
  // Setup LALR(1) parser
  ParserDelete(parser);
  test3GenerateParser(parser, true);
  numStatesFile = fopen("7-lalr.out", "w");
  fprintf(numStatesFile, "%d\n", parser->numStates);
  fclose(numStatesFile);
  
  // Parse files
  test3ParseFiles("parser3-lalr.txt");

  // Cleanup
  RegexDelete(regAdd);
  RegexDelete(regSub);
  RegexDelete(regMul);
  RegexDelete(regDiv);
  RegexDelete(regLParen);
  RegexDelete(regRParen);
  RegexDelete(regLBrack);
  RegexDelete(regRBrack);
  RegexDelete(regLBrace);
  RegexDelete(regRBrace);
  RegexDelete(regLe);
  RegexDelete(regLt);
  RegexDelete(regGe);
  RegexDelete(regGt);
  RegexDelete(regEq);
  RegexDelete(regNe);
  RegexDelete(regAnd);
  RegexDelete(regOr);
  RegexDelete(regNot);
  RegexDelete(regAssign);
  RegexDelete(regComma);
  RegexDelete(regSemicol);
  RegexDelete(regFloatNum);
  RegexDelete(regIntKeyword);
  RegexDelete(regFloatKeyword);
  RegexDelete(regIfKeyword);
  RegexDelete(regElseKeyword);
  RegexDelete(regWhileKeyword);
  RegexDelete(regReturnKeyword);
  RegexDelete(regIdentifier);
  RegexDelete(regWhitespace);
  LexerConfigDelete(lexerConfig);
  LexerDelete(lexer);
  ParserDelete(parser);
}

#define test4GenerateParser(parser, useLALR1, useConflictResolution) \
	do { \
    CFG *cfg = CFGNew(); \
    CFGSetNumTokens(cfg, lexerConfig->nfas->size); \
    int s = CFGAddVariable(cfg); \
    int t = CFGAddVariable(cfg); \
    int x = CFGAddVariable(cfg); \
    int y = CFGAddVariable(cfg); \
    ParserConfig *parserConfig = ParserConfigNew( \
        lexer, cfg, useLALR1, NullDestructor); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, s, 4, t, t, t, t); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, t, 3, a, x, a); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, t, 3, b, x, b); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, t, 3, a, y, b); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, t, 3, b, y, a); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, x, 1, c); \
    ParserAddRuleAndHandler(parserConfig, NullHandler, y, 1, c); \
    if (useConflictResolution)  { \
      ParserConfigAddResolution(parserConfig, 0, 1); \
      ParserConfigAddResolution(parserConfig, 1, 0); \
    } \
    parser = ParserFromConfig(parserConfig); \
    CFGDelete(cfg); \
  } while (0)

void test4() {
  // Setup lexer
  Regex *regA = RegexFromLetter('a');
  Regex *regB = RegexFromLetter('b');
  Regex *regC = RegexFromLetter('c');
  Regex *regWhitespace = RegexFromUnion(4,
      RegexFromLetter(' '),
      RegexFromLetter('\t'),
      RegexFromLetter('\r'),
      RegexFromLetter('\n'));
  LexerConfig *lexerConfig = LexerConfigNew();
  int a = LexerConfigAddRegex(lexerConfig, regA);
  int b = LexerConfigAddRegex(lexerConfig, regB);
  int c = LexerConfigAddRegex(lexerConfig, regC);
  LexerConfigSetIgnoreRegex(lexerConfig, regWhitespace)
  Lexer *lexer = LexerFromConfig(lexerConfig);

  // Setup LR(1) parser
  Parser *parser;
  test4GenerateParser(parser, false, false);

  // 10. Parse file without error
  FILE *file = fopen("10.in", "r");
  LexerSetInputFile(lexer, file, "10.in");
  assert(ParserParse(parser) == NULL);

  // Setup LALR(1) parser without conflict resolution
  ParserDelete(parser);
  test4GenerateParser(parser, true, false);
  assert(parser == NULL);

  // Setup LALR(1) parser with conflict resolution
  test4GenerateParser(parser, true, true);

  // 11. Parse file with error
  file = fopen("10.in", "r");
  LexerSetInputFile(lexer, file, "10.in");
  assert(ParserParse(parser) == PARSER_OBJECT_FAILURE);

  // Cleanup
  RegexDelete(regA);
  RegexDelete(regB);
  RegexDelete(regC);
  RegexDelete(regWhitespace);
  LexerConfigDelete(lexerConfig);
  LexerDelete(lexer);
  ParserDelete(parser);
}

int main() {
  RegexInit();
  DFAInit();

  // Create LR(1) parser from simple CFG and parse file
  test1();

  // Create LR(1) parser from CFG to calculate arithmetic expressions
  test2();

  // Create LR(1) and LALR(1) parsers from complex CFG to parse C-like grammar
  test3();

  // Create LR(1) and LALR(1) parsers for LR(1) CFG that is not LALR(1), and
  // test conflict resolution
  test4();
}
