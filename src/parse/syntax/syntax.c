#define PARSER_HANDLER_FILE_NAME "parse/syntax/handlers.h"
#include "lex/regex/regex.h"
#include "lex/dfa/dfa.h"
#include "lex/lexer/lexer.h"
#include "parse/cfg/cfg.h"
#include "parse/parser/parser.h"
#include "parse/syntax/syntax.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <limits.h>

// Constants
const char *SYNTAX_LEXER_FILENAME = "lexer.txt";
const char *SYNTAX_PARSER_FILENAME = "parser.txt";
// Token ID's
enum {
  ADD_ADD,
  SUB_SUB,
  ADD_EQ,
  SUB_EQ,
  MUL_EQ,
  DIV_EQ,
  MOD_EQ,
  LSHIFT_EQ,
  RSHIFT_EQ,
  BIT_AND_EQ,
  BIT_XOR_EQ,
  BIT_OR_EQ,
  IMPORT,
  AS,
  DOT,
  CLASS,
  LBRACE,
  RBRACE,
  STATIC,
  SEMICOL,
  CONST,
  VAR,
  LBRACK,
  RBRACK,
  LPAREN,
  RPAREN,
  I64,
  U64,
  I32,
  U32,
  I16,
  U16,
  I8,
  U8,
  F64,
  F32,
  BOOL,
  ANY,
  VOID,
  COMMA,
  EQ,
  IF,
  ELSE,
  OR,
  AND,
  BIT_OR,
  BIT_XOR,
  BIT_AND,
  EQEQ,
  NEQ,
  LT,
  LEQ,
  GT,
  GEQ,
  LSHIFT,
  RSHIFT,
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  NOT,
  BIT_NOT,
  INT_LITERAL,
  FLOAT_LITERAL,
  STRING_LITERAL,
  CHAR_LITERAL,
  SWITCH,
  CASE,
  DEFAULT,
  TRUE,
  FALSE,
  THIS,
  FOR,
  IDENTIFIER,
  NUM_TOKENS
};
// CFG Variables
enum {
  MODULE,
  IMPORT_DECLS,
  IMPORT_DECL,
  MODULE_PATH,
  MODULE_PATH_EXT,
  CLASS_DECLS,
  CLASS_DECL,
  CLASS_VAR_DECL_STMTS,
  CLASS_VAR_DECL_STMT,
  CLASS_VAR_DECL_MODIFIERS,
  VAR_DECL_STMT,
  VAR_DECL,
  VAR_DECL_MODIFIERS,
  TYPE_OR_VAR,
  TYPE,
  PRIMITIVE_TYPES,
  TYPE_OR_VOID,
  TYPE_LIST,
  TYPE_LIST_NONEMPTY,
  VAR_INIT_LIST,
  VAR_INIT,
  EXPR,
  EXPR_TERNARY,
  EXPR_LOGIC_OR,
  EXPR_LOGIC_AND,
  EXPR_BIT_OR,
  EXPR_BIT_XOR,
  EXPR_BIT_AND,
  EXPR_REL,
  OP_REL,
  EXPR_SHIFT,
  OP_SHIFT,
  EXPR_ADD,
  OP_ADD,
  EXPR_MUL,
  OP_MUL,
  EXPR_UNARY_OP,
  UNARY_OP,
  CAST_OP,
  EXPR_ACCESS,
  EXPR_LIST,
  EXPR_LIST_NONEMPTY,
  TERM,
  METHOD_DECLS,
  METHOD_DECL,
  PARAM_LIST,
  PARAM_LIST_NONEMPTY,
  STMTS,
  STMT,
  EXPR_STMT,
  ASSIGN_STMT,
  ASSIGN,
  ASSIGN_EQ,
  OP_ASSIGN,
  IF_STMT,
  ELSE_IF_BODIES,
  ELSE_BODY,
  SWITCH_STMT,
  CASES,
  CASE_COND,
  FOR_STMT,
  FOR_INIT,
  FOR_COND,
  FOR_ITER,
  WHILE_STMT,
  BREAK_STMT,
  RETURN_STMT,
  METHOD_DECL_MODIFIERS
};
// Create a new regex named "chain" for use in ADD_REGEX_CHAIN
#define INIT_REGEX_CHAIN(chain) \
  Regex *chain = REGEX_EMPTY_STRING;
// Defines a new variable named "regexName" that stores the result of "regex",
// and uses a regex named "chain" to add this regex to the chain. This is a hack
// so that we can simply call RegexDelete on "chain" to delete all regexes
// chained together by "chain"
#define ADD_REGEX_CHAIN(chain, regexName, regex) \
  Regex *regexName = (regex); \
  chain = RegexFromUnion(2, chain, regexName);

// Returns the path of a file with name "filename" that is in the directory
// containing the current executable. The returned string is malloced, so must
// be freed by the user
char *SyntaxGetFilePath(const char *filename);
// Checks if a file exists at a particular file path
bool SyntaxFileExists(const char *path);
// Returns a lexer for fartlang. If lexer file with name SYNTAX_LEXER_FILENAME
// doesn't exist in the same directory as the current executable, create one
Lexer *SyntaxCreateLexer();
// Returns a parser for fartlang using fartlang "lexer". If parser file with
// name SYNTAX_PARSER_FILENAME doesn't exist in the same directory as the
// current executable, create one
Parser *SyntaxCreateParser(Lexer *lexer);
// A destructor for a single AST "node"; in other words, doesn't delete any
// other node in the AST other than "node"; for use in Parser
void SyntaxASTDestructor(void *node);

char *SyntaxGetFilePath(const char *filename) {
  char *path = malloc(PATH_MAX);
  ssize_t len = readlink("/proc/self/exe", path, PATH_MAX);
  int i;
  for (i = len - 1; path[i] != '/'; --i);
  strcpy(path + i + 1, filename);
  return path;
}

bool SyntaxFileExists(const char *path) {
  return access(path, R_OK) == 0;
}

Lexer *SyntaxCreateLexer() {
  char *lexerFilePath = SyntaxGetFilePath(SYNTAX_LEXER_FILENAME);
  Lexer *lexer;
  if (!SyntaxFileExists(lexerFilePath)) {
    // Create lexer if it does not exist

    // Initialization
    RegexInit();
    DFAInit();

    // Create all regex for tokens
    INIT_REGEX_CHAIN(chain);
    ADD_REGEX_CHAIN(chain, add_add_, RegexFromString("++"));
    ADD_REGEX_CHAIN(chain, sub_sub_, RegexFromString("--"));
    ADD_REGEX_CHAIN(chain, add_eq_, RegexFromString("+="));
    ADD_REGEX_CHAIN(chain, sub_eq_, RegexFromString("-="));
    ADD_REGEX_CHAIN(chain, mul_eq_, RegexFromString("*="));
    ADD_REGEX_CHAIN(chain, div_eq_, RegexFromString("/="));
    ADD_REGEX_CHAIN(chain, mod_eq_, RegexFromString("%="));
    ADD_REGEX_CHAIN(chain, lshift_eq_, RegexFromString("<<="));
    ADD_REGEX_CHAIN(chain, rshift_eq_, RegexFromString(">>="));
    ADD_REGEX_CHAIN(chain, bit_and_eq_, RegexFromString("&="));
    ADD_REGEX_CHAIN(chain, bit_xor_eq_, RegexFromString("^="));
    ADD_REGEX_CHAIN(chain, bit_or_eq_, RegexFromString("|="));
    ADD_REGEX_CHAIN(chain, letterOrUnderscore_, RegexFromUnion(2,
        REGEX_LETTERS, RegexFromLetter('_')));
    ADD_REGEX_CHAIN(chain, identifier_, RegexFromConcat(2,
        letterOrUnderscore_,
        RegexZeroOrMore(RegexFromUnion(2, letterOrUnderscore_, REGEX_DIGITS))));
    ADD_REGEX_CHAIN(chain, import_, RegexFromString("import"));
    ADD_REGEX_CHAIN(chain, as_, RegexFromString("as"));
    ADD_REGEX_CHAIN(chain, dot_, RegexFromLetter('.'));
    ADD_REGEX_CHAIN(chain, class_, RegexFromString("class"));
    ADD_REGEX_CHAIN(chain, lbrace_, RegexFromLetter('{'));
    ADD_REGEX_CHAIN(chain, rbrace_, RegexFromLetter('}'));
    ADD_REGEX_CHAIN(chain, static_, RegexFromString("static"));
    ADD_REGEX_CHAIN(chain, semicol_, RegexFromLetter(';'));
    ADD_REGEX_CHAIN(chain, const_, RegexFromString("const"));
    ADD_REGEX_CHAIN(chain, var_, RegexFromString("var"));
    ADD_REGEX_CHAIN(chain, lbrack_, RegexFromLetter('['));
    ADD_REGEX_CHAIN(chain, rbrack_, RegexFromLetter(']'));
    ADD_REGEX_CHAIN(chain, lparen_, RegexFromLetter('('));
    ADD_REGEX_CHAIN(chain, rparen_, RegexFromLetter(')'));
    ADD_REGEX_CHAIN(chain, i64_, RegexFromString("i64"));
    ADD_REGEX_CHAIN(chain, u64_, RegexFromString("u64"));
    ADD_REGEX_CHAIN(chain, i32_, RegexFromString("i32"));
    ADD_REGEX_CHAIN(chain, u32_, RegexFromString("u32"));
    ADD_REGEX_CHAIN(chain, i16_, RegexFromString("i16"));
    ADD_REGEX_CHAIN(chain, u16_, RegexFromString("u16"));
    ADD_REGEX_CHAIN(chain, i8_, RegexFromString("i8"));
    ADD_REGEX_CHAIN(chain, u8_, RegexFromString("u8"));
    ADD_REGEX_CHAIN(chain, f64_, RegexFromString("f64"));
    ADD_REGEX_CHAIN(chain, f32_, RegexFromString("f32"));
    ADD_REGEX_CHAIN(chain, bool_, RegexFromString("bool"));
    ADD_REGEX_CHAIN(chain, any_, RegexFromString("any"));
    ADD_REGEX_CHAIN(chain, void_, RegexFromString("void"));
    ADD_REGEX_CHAIN(chain, comma_, RegexFromLetter(','));
    ADD_REGEX_CHAIN(chain, eq_, RegexFromLetter('='));
    ADD_REGEX_CHAIN(chain, if_, RegexFromString("if"));
    ADD_REGEX_CHAIN(chain, else_, RegexFromString("else"));
    ADD_REGEX_CHAIN(chain, or_, RegexFromString("or"));
    ADD_REGEX_CHAIN(chain, and_, RegexFromString("and"));
    ADD_REGEX_CHAIN(chain, bit_or_, RegexFromLetter('|'));
    ADD_REGEX_CHAIN(chain, bit_xor_, RegexFromLetter('^'));
    ADD_REGEX_CHAIN(chain, bit_and_, RegexFromLetter('&'));
    ADD_REGEX_CHAIN(chain, eqeq_, RegexFromString("=="));
    ADD_REGEX_CHAIN(chain, neq_, RegexFromString("!="));
    ADD_REGEX_CHAIN(chain, lt_, RegexFromLetter('<'));
    ADD_REGEX_CHAIN(chain, leq_, RegexFromString("<="));
    ADD_REGEX_CHAIN(chain, gt_, RegexFromLetter('>'));
    ADD_REGEX_CHAIN(chain, geq_, RegexFromString(">="));
    ADD_REGEX_CHAIN(chain, lshift_, RegexFromString("<<"));
    ADD_REGEX_CHAIN(chain, rshift_, RegexFromString(">>"));
    ADD_REGEX_CHAIN(chain, add_, RegexFromLetter('+'));
    ADD_REGEX_CHAIN(chain, sub_, RegexFromLetter('-'));
    ADD_REGEX_CHAIN(chain, mul_, RegexFromLetter('*'));
    ADD_REGEX_CHAIN(chain, div_, RegexFromLetter('/'));
    ADD_REGEX_CHAIN(chain, mod_, RegexFromLetter('%'));
    ADD_REGEX_CHAIN(chain, not_, RegexFromLetter('!'));
    ADD_REGEX_CHAIN(chain, bit_not_, RegexFromLetter('~'));
    RegexRange *digitsNo0Range = RegexRangeFromRange('1', '9');
    RegexCharacterClass *digitsNo0Class = RegexCharacterClassFromRanges(
        1, digitsNo0Range);
    ADD_REGEX_CHAIN(chain, digitsNo0_, 
        RegexFromCharacterClass(digitsNo0Class));
    RegexRangeDelete(digitsNo0Range);
    RegexCharacterClassDelete(digitsNo0Class);
    RegexRange *A2FRange = RegexRangeFromRange('A', 'F');
    RegexRange *a2fRange = RegexRangeFromRange('a', 'f');
    RegexRange *digitsRange = RegexRangeFromRange('0', '9');
    RegexCharacterClass *digitsHexClass = RegexCharacterClassFromRanges(
        3, A2FRange, a2fRange, digitsRange);
    ADD_REGEX_CHAIN(chain, digitsHex_, RegexFromCharacterClass(digitsHexClass));
    ADD_REGEX_CHAIN(chain, digitsBin_,
        RegexFromUnion(2, RegexFromLetter('0'), RegexFromLetter('1')));
    RegexRangeDelete(A2FRange);
    RegexRangeDelete(a2fRange);
    RegexRangeDelete(digitsRange);
    RegexCharacterClassDelete(digitsHexClass);
    ADD_REGEX_CHAIN(chain, int_literal_dec_, RegexFromUnion(2,
        RegexFromConcat(2, digitsNo0_, RegexZeroOrMore(REGEX_DIGITS)),
        RegexFromLetter('0')));
    ADD_REGEX_CHAIN(chain, int_literal_, RegexFromConcat(2,
        RegexFromUnion(3,
            int_literal_dec_,
            RegexFromConcat(2,
                RegexFromString("0x"), RegexOneOrMore(digitsHex_)),
            RegexFromConcat(2,
                RegexFromString("0b"), RegexOneOrMore(digitsBin_))),
        RegexZeroOrOne(RegexFromUnion(8,
            i64_, u64_, i32_, u32_, i16_, u16_, i8_, u8_))));
    ADD_REGEX_CHAIN(chain, decimals_, RegexFromConcat(2,
        RegexZeroOrMore(REGEX_DIGITS), digitsNo0_));
    ADD_REGEX_CHAIN(chain, float_literal_, RegexFromConcat(2,
        RegexFromUnion(2,
            RegexFromConcat(2,
                int_literal_dec_, RegexZeroOrOne(RegexFromConcat(2,
                    dot_, RegexZeroOrOne(decimals_)))),
            RegexFromConcat(2, dot_, decimals_)),
        RegexZeroOrOne(RegexFromConcat(3,
            RegexFromLetter('e'), RegexZeroOrOne(sub_), int_literal_dec_))));
    RegexRange *quoteRange = RegexRangeFromLetter('"');
    RegexRange *tickRange = RegexRangeFromLetter('\'');
    RegexRange *backslashRange = RegexRangeFromLetter('\\');
    RegexCharacterClass *quoteBackslashClass = RegexCharacterClassFromRanges(2,
        quoteRange, backslashRange);
    RegexCharacterClass *notQuoteBackslashClass = RegexCharacterClassNegated(
        quoteBackslashClass);
    ADD_REGEX_CHAIN(chain, notQuoteBackslash_,
        RegexFromCharacterClass(notQuoteBackslashClass));
    RegexCharacterClass *tickBackslashClass = RegexCharacterClassFromRanges(2,
        tickRange, backslashRange);
    RegexCharacterClass *notTickBackslashClass = RegexCharacterClassNegated(
        tickBackslashClass);
    ADD_REGEX_CHAIN(chain, notTickBackslash_,
        RegexFromCharacterClass(notTickBackslashClass));
    RegexRangeDelete(quoteRange);
    RegexRangeDelete(tickRange);
    RegexRangeDelete(backslashRange);
    RegexCharacterClassDelete(quoteBackslashClass);
    RegexCharacterClassDelete(notQuoteBackslashClass);
    RegexCharacterClassDelete(tickBackslashClass);
    RegexCharacterClassDelete(notTickBackslashClass);
    ADD_REGEX_CHAIN(chain, quote_, RegexFromLetter('"'));
    ADD_REGEX_CHAIN(chain, backslash_, RegexFromLetter('\\'));
    ADD_REGEX_CHAIN(chain, string_literal_, RegexFromConcat(3,
        quote_, RegexZeroOrMore(RegexFromUnion(2,
            notQuoteBackslash_, RegexFromConcat(2, backslash_, REGEX_ANY))),
        quote_));
    ADD_REGEX_CHAIN(chain, tick_, RegexFromLetter('\''));
    ADD_REGEX_CHAIN(chain, char_literal_, RegexFromConcat(3,
        tick_, RegexFromUnion(2, notTickBackslash_, RegexFromConcat(2,
            backslash_, REGEX_ANY)),
        tick_));
    ADD_REGEX_CHAIN(chain, switch_, RegexFromString("switch"));
    ADD_REGEX_CHAIN(chain, case_, RegexFromString("case"));
    ADD_REGEX_CHAIN(chain, default_, RegexFromString("default"));
    ADD_REGEX_CHAIN(chain, true_, RegexFromString("true"));
    ADD_REGEX_CHAIN(chain, false_, RegexFromString("false"));
    ADD_REGEX_CHAIN(chain, this_, RegexFromString("this"));
    ADD_REGEX_CHAIN(chain, for_, RegexFromString("for"));
    RegexRange *newlineRange = RegexRangeFromLetter('\n');
    RegexCharacterClass *newlineClass = RegexCharacterClassFromRanges(1,
        newlineRange);
    RegexCharacterClass *notNewlineClass = RegexCharacterClassNegated(
        newlineClass);
    ADD_REGEX_CHAIN(chain, notNewline_,
        RegexFromCharacterClass(notNewlineClass));
    RegexRangeDelete(newlineRange);
    RegexCharacterClassDelete(newlineClass);
    RegexCharacterClassDelete(notNewlineClass);
    RegexRange *hashRange = RegexRangeFromLetter('#');
    RegexCharacterClass *hashClass = RegexCharacterClassFromRanges(1,
        hashRange);
    RegexCharacterClass *notHashClass = RegexCharacterClassNegated(hashClass);
    ADD_REGEX_CHAIN(chain, notHash_, RegexFromCharacterClass(notHashClass));
    RegexRangeDelete(hashRange);
    RegexCharacterClassDelete(hashClass);
    RegexCharacterClassDelete(notHashClass);
    ADD_REGEX_CHAIN(chain, hash_, RegexFromLetter('#'));
    ADD_REGEX_CHAIN(chain, newline_, RegexFromLetter('\n'));
    ADD_REGEX_CHAIN(chain, hashHash_, RegexFromString("##"));
    ADD_REGEX_CHAIN(chain, comment_, RegexFromUnion(2,
        RegexFromConcat(3, hash_, RegexZeroOrMore(notNewline_), newline_),
        RegexFromConcat(3, hashHash_, RegexZeroOrMore(RegexFromUnion(2,
            notHash_, RegexFromConcat(2, hash_, notHash_))), hashHash_)));
    ADD_REGEX_CHAIN(chain, ignore_, RegexFromUnion(5,
        RegexFromLetter(' '),
        RegexFromLetter('\t'),
        RegexFromLetter('\r'),
        RegexFromLetter('\n'),
        comment_));

    // Add all regex to config and check if their assigned token ID's are
    // as expected
    LexerConfig *lexerConfig = LexerConfigNew();
    assert(LexerConfigAddRegex(lexerConfig, add_add_) == ADD_ADD);
    assert(LexerConfigAddRegex(lexerConfig, sub_sub_) == SUB_SUB);
    assert(LexerConfigAddRegex(lexerConfig, add_eq_) == ADD_EQ);
    assert(LexerConfigAddRegex(lexerConfig, sub_eq_) == SUB_EQ);
    assert(LexerConfigAddRegex(lexerConfig, mul_eq_) == MUL_EQ);
    assert(LexerConfigAddRegex(lexerConfig, div_eq_) == DIV_EQ);
    assert(LexerConfigAddRegex(lexerConfig, mod_eq_) == MOD_EQ);
    assert(LexerConfigAddRegex(lexerConfig, lshift_eq_) == LSHIFT_EQ);
    assert(LexerConfigAddRegex(lexerConfig, rshift_eq_) == RSHIFT_EQ);
    assert(LexerConfigAddRegex(lexerConfig, bit_and_eq_) == BIT_AND_EQ);
    assert(LexerConfigAddRegex(lexerConfig, bit_xor_eq_) == BIT_XOR_EQ);
    assert(LexerConfigAddRegex(lexerConfig, bit_or_eq_) == BIT_OR_EQ);
    assert(LexerConfigAddRegex(lexerConfig, import_) == IMPORT);
    assert(LexerConfigAddRegex(lexerConfig, as_) == AS);
    assert(LexerConfigAddRegex(lexerConfig, dot_) == DOT);
    assert(LexerConfigAddRegex(lexerConfig, class_) == CLASS);
    assert(LexerConfigAddRegex(lexerConfig, lbrace_) == LBRACE);
    assert(LexerConfigAddRegex(lexerConfig, rbrace_) == RBRACE);
    assert(LexerConfigAddRegex(lexerConfig, static_) == STATIC);
    assert(LexerConfigAddRegex(lexerConfig, semicol_) == SEMICOL);
    assert(LexerConfigAddRegex(lexerConfig, const_) == CONST);
    assert(LexerConfigAddRegex(lexerConfig, var_) == VAR);
    assert(LexerConfigAddRegex(lexerConfig, lbrack_) == LBRACK);
    assert(LexerConfigAddRegex(lexerConfig, rbrack_) == RBRACK);
    assert(LexerConfigAddRegex(lexerConfig, lparen_) == LPAREN);
    assert(LexerConfigAddRegex(lexerConfig, rparen_) == RPAREN);
    assert(LexerConfigAddRegex(lexerConfig, i64_) == I64);
    assert(LexerConfigAddRegex(lexerConfig, u64_) == U64);
    assert(LexerConfigAddRegex(lexerConfig, i32_) == I32);
    assert(LexerConfigAddRegex(lexerConfig, u32_) == U32);
    assert(LexerConfigAddRegex(lexerConfig, i16_) == I16);
    assert(LexerConfigAddRegex(lexerConfig, u16_) == U16);
    assert(LexerConfigAddRegex(lexerConfig, i8_) == I8);
    assert(LexerConfigAddRegex(lexerConfig, u8_) == U8);
    assert(LexerConfigAddRegex(lexerConfig, f64_) == F64);
    assert(LexerConfigAddRegex(lexerConfig, f32_) == F32);
    assert(LexerConfigAddRegex(lexerConfig, bool_) == BOOL);
    assert(LexerConfigAddRegex(lexerConfig, any_) == ANY);
    assert(LexerConfigAddRegex(lexerConfig, void_) == VOID);
    assert(LexerConfigAddRegex(lexerConfig, comma_) == COMMA);
    assert(LexerConfigAddRegex(lexerConfig, eq_) == EQ);
    assert(LexerConfigAddRegex(lexerConfig, if_) == IF);
    assert(LexerConfigAddRegex(lexerConfig, else_) == ELSE);
    assert(LexerConfigAddRegex(lexerConfig, or_) == OR);
    assert(LexerConfigAddRegex(lexerConfig, and_) == AND);
    assert(LexerConfigAddRegex(lexerConfig, bit_or_) == BIT_OR);
    assert(LexerConfigAddRegex(lexerConfig, bit_xor_) == BIT_XOR);
    assert(LexerConfigAddRegex(lexerConfig, bit_and_) == BIT_AND);
    assert(LexerConfigAddRegex(lexerConfig, eqeq_) == EQEQ);
    assert(LexerConfigAddRegex(lexerConfig, neq_) == NEQ);
    assert(LexerConfigAddRegex(lexerConfig, lt_) == LT);
    assert(LexerConfigAddRegex(lexerConfig, leq_) == LEQ);
    assert(LexerConfigAddRegex(lexerConfig, gt_) == GT);
    assert(LexerConfigAddRegex(lexerConfig, geq_) == GEQ);
    assert(LexerConfigAddRegex(lexerConfig, lshift_) == LSHIFT);
    assert(LexerConfigAddRegex(lexerConfig, rshift_) == RSHIFT);
    assert(LexerConfigAddRegex(lexerConfig, add_) == ADD);
    assert(LexerConfigAddRegex(lexerConfig, sub_) == SUB);
    assert(LexerConfigAddRegex(lexerConfig, mul_) == MUL);
    assert(LexerConfigAddRegex(lexerConfig, div_) == DIV);
    assert(LexerConfigAddRegex(lexerConfig, mod_) == MOD);
    assert(LexerConfigAddRegex(lexerConfig, not_) == NOT);
    assert(LexerConfigAddRegex(lexerConfig, bit_not_) == BIT_NOT);
    assert(LexerConfigAddRegex(lexerConfig, int_literal_) == INT_LITERAL);
    assert(LexerConfigAddRegex(lexerConfig, float_literal_) == FLOAT_LITERAL);
    assert(LexerConfigAddRegex(lexerConfig, string_literal_) == STRING_LITERAL);
    assert(LexerConfigAddRegex(lexerConfig, char_literal_) == CHAR_LITERAL);
    assert(LexerConfigAddRegex(lexerConfig, switch_) == SWITCH);
    assert(LexerConfigAddRegex(lexerConfig, case_) == CASE);
    assert(LexerConfigAddRegex(lexerConfig, default_) == DEFAULT);
    assert(LexerConfigAddRegex(lexerConfig, true_) == TRUE);
    assert(LexerConfigAddRegex(lexerConfig, false_) == FALSE);
    assert(LexerConfigAddRegex(lexerConfig, this_) == THIS);
    assert(LexerConfigAddRegex(lexerConfig, for_) == FOR);
    assert(LexerConfigAddRegex(lexerConfig, identifier_) == IDENTIFIER);
    LexerConfigSetIgnoreRegex(lexerConfig, ignore_);

    // Create lexer and save to file
    lexer = LexerFromConfig(lexerConfig);
    FILE *lexerFile = fopen(lexerFilePath, "w");
    LexerToFile(lexer, lexerFile);
    fclose(lexerFile);

    // Cleanup
    RegexDelete(chain);
  } else {
    // Read lexer directly from file
    FILE *lexerFile = fopen(lexerFilePath, "r");
    lexer = LexerFromFile(lexerFile);
    fclose(lexerFile);
  }

  free(lexerFilePath);
  return lexer;
}

Parser *SyntaxCreateParser(Lexer *lexer) {
   char *parserFilePath = SyntaxGetFilePath(SYNTAX_PARSER_FILENAME);
   Parser *parser;
   if (!SyntaxFileExists(parserFilePath)) {
      // Create parser if it does not exist

      // Setup all CFG variables
      CFG *cfg = CFGNew();
      CFGSetNumTokens(cfg, NUM_TOKENS);
      assert(CFGAddVariable(cfg) == MODULE);
      assert(CFGAddVariable(cfg) == IMPORT_DECLS);
      assert(CFGAddVariable(cfg) == IMPORT_DECL);
      assert(CFGAddVariable(cfg) == MODULE_PATH);
      assert(CFGAddVariable(cfg) == MODULE_PATH_EXT);
      assert(CFGAddVariable(cfg) == CLASS_DECLS);
      assert(CFGAddVariable(cfg) == CLASS_DECL);
      assert(CFGAddVariable(cfg) == CLASS_VAR_DECL_STMTS);
      assert(CFGAddVariable(cfg) == CLASS_VAR_DECL_STMT);
      assert(CFGAddVariable(cfg) == CLASS_VAR_DECL_MODIFIERS);
      assert(CFGAddVariable(cfg) == VAR_DECL_STMT);
      assert(CFGAddVariable(cfg) == VAR_DECL);
      assert(CFGAddVariable(cfg) == VAR_DECL_MODIFIERS);
      assert(CFGAddVariable(cfg) == TYPE_OR_VAR);
      assert(CFGAddVariable(cfg) == TYPE);
      assert(CFGAddVariable(cfg) == PRIMITIVE_TYPES);
      assert(CFGAddVariable(cfg) == TYPE_OR_VOID);
      assert(CFGAddVariable(cfg) == TYPE_LIST);
      assert(CFGAddVariable(cfg) == TYPE_LIST_NONEMPTY);
      assert(CFGAddVariable(cfg) == VAR_INIT_LIST);
      assert(CFGAddVariable(cfg) == VAR_INIT);
      assert(CFGAddVariable(cfg) == EXPR);
      assert(CFGAddVariable(cfg) == EXPR_TERNARY);
      assert(CFGAddVariable(cfg) == EXPR_LOGIC_OR);
      assert(CFGAddVariable(cfg) == EXPR_LOGIC_AND);
      assert(CFGAddVariable(cfg) == EXPR_BIT_OR);
      assert(CFGAddVariable(cfg) == EXPR_BIT_XOR);
      assert(CFGAddVariable(cfg) == EXPR_BIT_AND);
      assert(CFGAddVariable(cfg) == EXPR_REL);
      assert(CFGAddVariable(cfg) == OP_REL);
      assert(CFGAddVariable(cfg) == EXPR_SHIFT);
      assert(CFGAddVariable(cfg) == OP_SHIFT);
      assert(CFGAddVariable(cfg) == EXPR_ADD);
      assert(CFGAddVariable(cfg) == OP_ADD);
      assert(CFGAddVariable(cfg) == EXPR_MUL);
      assert(CFGAddVariable(cfg) == OP_MUL);
      assert(CFGAddVariable(cfg) == EXPR_UNARY_OP);
      assert(CFGAddVariable(cfg) == UNARY_OP);
      assert(CFGAddVariable(cfg) == CAST_OP);
      assert(CFGAddVariable(cfg) == EXPR_ACCESS);
      assert(CFGAddVariable(cfg) == EXPR_LIST);
      assert(CFGAddVariable(cfg) == EXPR_LIST_NONEMPTY);
      assert(CFGAddVariable(cfg) == TERM);
      assert(CFGAddVariable(cfg) == METHOD_DECLS);
      assert(CFGAddVariable(cfg) == METHOD_DECL);
      assert(CFGAddVariable(cfg) == PARAM_LIST);
      assert(CFGAddVariable(cfg) == PARAM_LIST_NONEMPTY);
      assert(CFGAddVariable(cfg) == STMTS);
      assert(CFGAddVariable(cfg) == STMT);
      assert(CFGAddVariable(cfg) == EXPR_STMT);
      assert(CFGAddVariable(cfg) == ASSIGN_STMT);
      assert(CFGAddVariable(cfg) == ASSIGN);
      assert(CFGAddVariable(cfg) == ASSIGN_EQ);
      assert(CFGAddVariable(cfg) == OP_ASSIGN);
      assert(CFGAddVariable(cfg) == IF_STMT);
      assert(CFGAddVariable(cfg) == ELSE_IF_BODIES);
      assert(CFGAddVariable(cfg) == ELSE_BODY);
      assert(CFGAddVariable(cfg) == SWITCH_STMT);
      assert(CFGAddVariable(cfg) == CASES);
      assert(CFGAddVariable(cfg) == CASE_COND);
      assert(CFGAddVariable(cfg) == FOR_STMT);
      assert(CFGAddVariable(cfg) == FOR_INIT);
      assert(CFGAddVariable(cfg) == FOR_COND);
      assert(CFGAddVariable(cfg) == FOR_ITER);
      assert(CFGAddVariable(cfg) == WHILE_STMT);
      assert(CFGAddVariable(cfg) == BREAK_STMT);
      assert(CFGAddVariable(cfg) == RETURN_STMT);
      assert(CFGAddVariable(cfg) == METHOD_DECL_MODIFIERS);

      // Add all CFG rules
      ParserConfig *parserConfig = ParserConfigNew(
          lexer, cfg, true, SyntaxASTDestructor);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerModule,
          MODULE, 2, IMPORT_DECLS, CLASS_DECLS);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerImportDecls,
          IMPORT_DECLS, 2, IMPORT_DECLS, IMPORT_DECL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerImportDecls,
          IMPORT_DECLS, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerImportDecl,
          IMPORT_DECL, 4, IMPORT, MODULE_PATH, MODULE_PATH_EXT, SEMICOL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerModulePath,
          MODULE_PATH, 3, MODULE_PATH, DOT, IDENTIFIER);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerModulePath,
          MODULE_PATH, 1, IDENTIFIER);
      ParserAddRuleAndHander(parserConfig, SyntaxHandlerModulePathExt,
          MODULE_PATH, 2, AS, IDENTIFIER);
      ParserAddRuleAndHander(parserConfig, SyntaxHandlerModulePathExt,
          MODULE_PATH, 2, DOT, MUL);
      ParserAddRuleAndHander(parserConfig, SyntaxHandlerModulePathExt,
          MODULE_PATH, 0);

      // Create parser
      parser = ParserFromConfig(parserConfig);
      FILE *parserFile = fopen(parserFilePath, "w");
      ParserToFile(parser, parserFile);
      fclose(parserFile);

      // Cleanup
      CFGDelete(cfg);
   } else {
     // Read parser directly from file
     FILE *parserFile = fopen(parserFilePath, "r");
     parser = ParserFromFile(parserFile, lexer, SyntaxASTDestructor);
     fclose(parserFile);
   }
   free(parserFilePath);
   return parser;
}

SyntaxAST *SyntaxParseFile(FILE *file, const char *filename) {
  Lexer *lexer = SyntaxCreateLexer();

  // DEBUG
  LexerSetInputFile(lexer, file, filename);
  while (true) {
    LexerToken *token = LexerNextToken(lexer);
    if (token == LEXER_TOKEN_EOF)
      break;
    printf("tokenID: %d, location: %d:%d ~ %d:%d, length: %d, token: ",
           token->tokenID,
           token->loc.from.lineNo + 1, token->loc.from.charNo + 1,
           token->loc.to.lineNo + 1, token->loc.to.charNo + 1,
           token->length);
    for (int i = 0; i < token->length; ++i)
      putchar(token->str[i]);
    printf("\n");
    LexerTokenDelete(token);
  }

  LexerDelete(lexer);
  return NULL;
}

void SyntaxASTDestructor(void *node) {
  switch (node->kind) {
    case SYNTAX_AST_KIND_IDENTIFIER:
      free(node->string);
      break;
  }
  free(node);
}

void SyntaxASTDelete(SyntaxAST *node) {
  if (!node)
    return;
  SyntaxASTDelete(node->child);
  SyntaxASTDelete(node->sibling);
  SyntaxASTDestructor(node);
}

#include "parse/syntax/handlers.c"
