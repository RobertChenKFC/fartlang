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
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

// Constants
const char *SYNTAX_LEXER_FILENAME = "lexer.txt";
const char *SYNTAX_PARSER_FILENAME = "parser.txt";
const char *SYNTAX_AST_KIND_STRS[] = {
  SYNTAX_AST_FOREACH_KIND(SYNTAX_GEN_STR)
};
const char *SYNTAX_TYPE_STRS[] = {
  SYNTAX_FOREACH_TYPE(SYNTAX_GEN_STR)
};
const char *SYNTAX_OP_STRS[] = {
  SYNTAX_FOREACH_OP(SYNTAX_GEN_STR)
};
const char *SYNTAX_METHOD_TYPE_STRS[] = {
  SYNTAX_FOREACH_METHOD_TYPE(SYNTAX_GEN_STR)
};
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
  IS,
  AS,
  INTO,
  DOT,
  CLASS,
  LBRACE,
  RBRACE,
  STATIC,
  SEMICOL,
  COL,
  ARROW,
  CONST,
  VAR,
  FN,
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
  TRUE_LITERAL,
  FALSE_LITERAL,
  THIS,
  FOR,
  WHILE,
  BREAK,
  RETURN,
  NULL_LITERAL,
  IDENTIFIER,
  NUM_TOKENS,
  TOKEN_EOF = NUM_TOKENS
};
// CFG Variables
enum {
  MODULE = NUM_TOKENS + 1,
  IMPORT_DECLS,
  IMPORT_DECL,
  MODULE_PATH,
  MODULE_PATH_EXT,
  CLASS_DECLS,
  CLASS_DECL,
  VAR_DECL_STMTS,
  VAR_DECL_STMT,
  VAR_DECL,
  VAR_DECL_MODIFIERS,
  VAR_DECL_MODIFIER,
  TYPE,
  TYPE_FN_EXPR,
  TYPE_ARR_EXPR,
  TYPE_TERM,
  PRIMITIVE_TYPE,
  RETURN_TYPE,
  TYPE_LIST,
  TYPE_LIST_NONEMPTY,
  VAR_INIT_LIST,
  VAR_INIT,
  VAR_NAME,
  EXPR,
  EXPR_TERNARY,
  EXPR_LOGIC_OR,
  EXPR_LOGIC_AND,
  EXPR_BIT_OR,
  EXPR_BIT_XOR,
  EXPR_BIT_AND,
  EXPR_REL,
  EXPR_SHIFT,
  EXPR_ADD,
  EXPR_MUL,
  EXPR_CAST_OP,
  EXPR_UNARY_OP,
  EXPR_ACCESS,
  EXPR_LIST,
  EXPR_LIST_NONEMPTY,
  TERM,
  INT_LITERAL_TERM,
  METHOD_DECLS,
  METHOD_DECL,
  METHOD_DECL_PREFIX,
  METHOD_DECL_BODY,
  BODY,
  PARAM_LIST,
  PARAM_LIST_NONEMPTY,
  PARAM,
  STMTS,
  STMT,
  EXPR_STMT,
  ASSIGN_STMT,
  ASSIGN,
  OP_ASSIGN,
  IF_STMT,
  ELSE_IF_BODIES,
  ELSE_BODY,
  SWITCH_STMT,
  SWITCH_CASES,
  SWITCH_CASE,
  SWITCH_DEFAULT,
  LABEL,
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
Lexer *SyntaxCreateLexer(void);
// Returns a parser for fartlang using fartlang "lexer". If parser file with
// name SYNTAX_PARSER_FILENAME doesn't exist in the same directory as the
// current executable, create one
Parser *SyntaxCreateParser(Lexer *lexer);

char *SyntaxGetFilePath(const char *filename) {
  char *path = malloc(PATH_MAX);
#ifdef __APPLE__
  uint32_t len = PATH_MAX;
  assert(_NSGetExecutablePath(path, &len) == 0);
#else
  ssize_t len = readlink("/proc/self/exe", path, PATH_MAX);
#endif
  int i;
  for (i = len - 1; path[i] != '/'; --i);
  strcpy(path + i + 1, filename);
  return path;
}

bool SyntaxFileExists(const char *path) {
  return access(path, R_OK) == 0;
}

Lexer *SyntaxCreateLexer(void) {
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
    ADD_REGEX_CHAIN(chain, is_, RegexFromString("is"));
    ADD_REGEX_CHAIN(chain, as_, RegexFromString("as"));
    ADD_REGEX_CHAIN(chain, into_, RegexFromString("into"));
    ADD_REGEX_CHAIN(chain, dot_, RegexFromLetter('.'));
    ADD_REGEX_CHAIN(chain, class_, RegexFromString("class"));
    ADD_REGEX_CHAIN(chain, lbrace_, RegexFromLetter('{'));
    ADD_REGEX_CHAIN(chain, rbrace_, RegexFromLetter('}'));
    ADD_REGEX_CHAIN(chain, static_, RegexFromString("static"));
    ADD_REGEX_CHAIN(chain, semicol_, RegexFromLetter(';'));
    ADD_REGEX_CHAIN(chain, col_, RegexFromLetter(':'));
    ADD_REGEX_CHAIN(chain, arrow_, RegexFromString("->"));
    ADD_REGEX_CHAIN(chain, const_, RegexFromString("const"));
    ADD_REGEX_CHAIN(chain, var_, RegexFromString("var"));
    ADD_REGEX_CHAIN(chain, fn_, RegexFromString("fn"));
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
    ADD_REGEX_CHAIN(chain, not_, RegexFromString("not"));
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
    ADD_REGEX_CHAIN(chain, decimals_, RegexOneOrMore(REGEX_DIGITS));
    ADD_REGEX_CHAIN(chain, float_literal_, RegexFromConcat(3,
        RegexFromUnion(2,
            RegexFromConcat(2,
                int_literal_dec_, RegexZeroOrOne(RegexFromConcat(2,
                    dot_, RegexZeroOrOne(decimals_)))),
            RegexFromConcat(2, dot_, decimals_)),
        RegexZeroOrOne(RegexFromConcat(3,
            RegexFromLetter('e'), RegexZeroOrOne(RegexFromUnion(2, add_, sub_)),
            int_literal_dec_)),
        RegexZeroOrOne(RegexFromUnion(2, f64_, f32_))));
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
    ADD_REGEX_CHAIN(chain, while_, RegexFromString("while"));
    ADD_REGEX_CHAIN(chain, break_, RegexFromString("break"));
    ADD_REGEX_CHAIN(chain, return_, RegexFromString("return"));
    ADD_REGEX_CHAIN(chain, null_literal_, RegexFromString("null"));
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
    assert(LexerConfigAddRegex(lexerConfig, is_) == IS);
    assert(LexerConfigAddRegex(lexerConfig, as_) == AS);
    assert(LexerConfigAddRegex(lexerConfig, into_) == INTO);
    assert(LexerConfigAddRegex(lexerConfig, dot_) == DOT);
    assert(LexerConfigAddRegex(lexerConfig, class_) == CLASS);
    assert(LexerConfigAddRegex(lexerConfig, lbrace_) == LBRACE);
    assert(LexerConfigAddRegex(lexerConfig, rbrace_) == RBRACE);
    assert(LexerConfigAddRegex(lexerConfig, static_) == STATIC);
    assert(LexerConfigAddRegex(lexerConfig, semicol_) == SEMICOL);
    assert(LexerConfigAddRegex(lexerConfig, col_) == COL);
    assert(LexerConfigAddRegex(lexerConfig, arrow_) == ARROW);
    assert(LexerConfigAddRegex(lexerConfig, const_) == CONST);
    assert(LexerConfigAddRegex(lexerConfig, var_) == VAR);
    assert(LexerConfigAddRegex(lexerConfig, fn_) == FN);
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
    assert(LexerConfigAddRegex(lexerConfig, true_) == TRUE_LITERAL);
    assert(LexerConfigAddRegex(lexerConfig, false_) == FALSE_LITERAL);
    assert(LexerConfigAddRegex(lexerConfig, this_) == THIS);
    assert(LexerConfigAddRegex(lexerConfig, for_) == FOR);
    assert(LexerConfigAddRegex(lexerConfig, while_) == WHILE);
    assert(LexerConfigAddRegex(lexerConfig, break_) == BREAK);
    assert(LexerConfigAddRegex(lexerConfig, return_) == RETURN);
    assert(LexerConfigAddRegex(lexerConfig, null_literal_) == NULL_LITERAL);
    assert(LexerConfigAddRegex(lexerConfig, identifier_) == IDENTIFIER);
    LexerConfigSetIgnoreRegex(lexerConfig, ignore_);

    // Create lexer and save to file
    lexer = LexerFromConfig(lexerConfig);
    FILE *lexerFile = fopen(lexerFilePath, "w");
    LexerToFile(lexer, lexerFile);
    fclose(lexerFile);

    // Cleanup
    RegexDelete(chain);
    LexerConfigDelete(lexerConfig);
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
      assert(CFGAddVariable(cfg) == VAR_DECL_STMTS);
      assert(CFGAddVariable(cfg) == VAR_DECL_STMT);
      assert(CFGAddVariable(cfg) == VAR_DECL);
      assert(CFGAddVariable(cfg) == VAR_DECL_MODIFIERS);
      assert(CFGAddVariable(cfg) == VAR_DECL_MODIFIER);
      assert(CFGAddVariable(cfg) == TYPE);
      assert(CFGAddVariable(cfg) == TYPE_FN_EXPR);
      assert(CFGAddVariable(cfg) == TYPE_ARR_EXPR);
      assert(CFGAddVariable(cfg) == TYPE_TERM);
      assert(CFGAddVariable(cfg) == PRIMITIVE_TYPE);
      assert(CFGAddVariable(cfg) == RETURN_TYPE);
      assert(CFGAddVariable(cfg) == TYPE_LIST);
      assert(CFGAddVariable(cfg) == TYPE_LIST_NONEMPTY);
      assert(CFGAddVariable(cfg) == VAR_INIT_LIST);
      assert(CFGAddVariable(cfg) == VAR_INIT);
      assert(CFGAddVariable(cfg) == VAR_NAME);
      assert(CFGAddVariable(cfg) == EXPR);
      assert(CFGAddVariable(cfg) == EXPR_TERNARY);
      assert(CFGAddVariable(cfg) == EXPR_LOGIC_OR);
      assert(CFGAddVariable(cfg) == EXPR_LOGIC_AND);
      assert(CFGAddVariable(cfg) == EXPR_BIT_OR);
      assert(CFGAddVariable(cfg) == EXPR_BIT_XOR);
      assert(CFGAddVariable(cfg) == EXPR_BIT_AND);
      assert(CFGAddVariable(cfg) == EXPR_REL);
      assert(CFGAddVariable(cfg) == EXPR_SHIFT);
      assert(CFGAddVariable(cfg) == EXPR_ADD);
      assert(CFGAddVariable(cfg) == EXPR_MUL);
      assert(CFGAddVariable(cfg) == EXPR_CAST_OP);
      assert(CFGAddVariable(cfg) == EXPR_UNARY_OP);
      assert(CFGAddVariable(cfg) == EXPR_ACCESS);
      assert(CFGAddVariable(cfg) == EXPR_LIST);
      assert(CFGAddVariable(cfg) == EXPR_LIST_NONEMPTY);
      assert(CFGAddVariable(cfg) == TERM);
      assert(CFGAddVariable(cfg) == INT_LITERAL_TERM);
      assert(CFGAddVariable(cfg) == METHOD_DECLS);
      assert(CFGAddVariable(cfg) == METHOD_DECL);
      assert(CFGAddVariable(cfg) == METHOD_DECL_PREFIX);
      assert(CFGAddVariable(cfg) == METHOD_DECL_BODY);
      assert(CFGAddVariable(cfg) == BODY);
      assert(CFGAddVariable(cfg) == PARAM_LIST);
      assert(CFGAddVariable(cfg) == PARAM_LIST_NONEMPTY);
      assert(CFGAddVariable(cfg) == PARAM);
      assert(CFGAddVariable(cfg) == STMTS);
      assert(CFGAddVariable(cfg) == STMT);
      assert(CFGAddVariable(cfg) == EXPR_STMT);
      assert(CFGAddVariable(cfg) == ASSIGN_STMT);
      assert(CFGAddVariable(cfg) == ASSIGN);
      assert(CFGAddVariable(cfg) == OP_ASSIGN);
      assert(CFGAddVariable(cfg) == IF_STMT);
      assert(CFGAddVariable(cfg) == ELSE_IF_BODIES);
      assert(CFGAddVariable(cfg) == ELSE_BODY);
      assert(CFGAddVariable(cfg) == SWITCH_STMT);
      assert(CFGAddVariable(cfg) == SWITCH_CASES);
      assert(CFGAddVariable(cfg) == SWITCH_CASE);
      assert(CFGAddVariable(cfg) == SWITCH_DEFAULT);
      assert(CFGAddVariable(cfg) == LABEL);
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
          lexer, cfg, /*useLALR1=*/true, SyntaxASTDelete);
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
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerModulePathExt,
          MODULE_PATH_EXT, 2, AS, IDENTIFIER);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerModulePathExt,
          MODULE_PATH_EXT, 2, DOT, MUL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerModulePathExt,
          MODULE_PATH_EXT, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerClassDecls,
          CLASS_DECLS, 2, CLASS_DECLS, CLASS_DECL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerClassDecls,
          CLASS_DECLS, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerClassDecl,
          CLASS_DECL, 6, CLASS, IDENTIFIER, LBRACE,
          VAR_DECL_STMTS, METHOD_DECLS, RBRACE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarDeclStmts,
          VAR_DECL_STMTS, 2, VAR_DECL_STMTS, VAR_DECL_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarDeclStmts,
          VAR_DECL_STMTS, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarDeclStmt,
          VAR_DECL_STMT, 2, VAR_DECL, SEMICOL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarDecl,
          VAR_DECL, 3, VAR_DECL_MODIFIERS, VAR, VAR_INIT_LIST);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarDeclModifiers,
          VAR_DECL_MODIFIERS, 2, VAR_DECL_MODIFIERS, VAR_DECL_MODIFIER);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarDeclModifiers,
          VAR_DECL_MODIFIERS, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          VAR_DECL_MODIFIER, 1, STATIC);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          VAR_DECL_MODIFIER, 1, CONST);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          TYPE, 1, TYPE_FN_EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerType,
          TYPE_FN_EXPR, 5, FN, LPAREN, TYPE_LIST, RPAREN, RETURN_TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          TYPE_FN_EXPR, 1, TYPE_ARR_EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerType,
          TYPE_ARR_EXPR, 3, TYPE_ARR_EXPR, LBRACK, RBRACK);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          TYPE_ARR_EXPR, 1, TYPE_TERM);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          TYPE_TERM, 1, PRIMITIVE_TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerType,
          TYPE_TERM, 1, MODULE_PATH);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerParenTerm,
          TYPE_TERM, 3, LPAREN, TYPE, RPAREN);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, I64);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, U64);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, I32);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, U32);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, I16);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, U16);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, I8);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, U8);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, F64);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, F32);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, BOOL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPrimitiveType,
          PRIMITIVE_TYPE, 1, ANY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerReturnType,
          RETURN_TYPE, 2, ARROW, TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerReturnType,
          RETURN_TYPE, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          TYPE_LIST, 1, TYPE_LIST_NONEMPTY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerTypeList,
          TYPE_LIST, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerTypeList,
          TYPE_LIST_NONEMPTY, 3, TYPE_LIST_NONEMPTY, COMMA, TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerTypeList,
          TYPE_LIST_NONEMPTY, 1, TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarInitList,
          VAR_INIT_LIST, 3, VAR_INIT_LIST, COMMA, VAR_INIT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarInitList,
          VAR_INIT_LIST, 1, VAR_INIT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarInit,
          VAR_INIT, 3, VAR_NAME, EQ, EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarInit,
          VAR_INIT, 1, VAR_NAME);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarName,
          VAR_NAME, 3, IDENTIFIER, COL, TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVarName,
          VAR_NAME, 1, IDENTIFIER);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR, 1, EXPR_TERNARY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprTernary,
          EXPR_TERNARY, 5, EXPR_LOGIC_OR, IF, EXPR_LOGIC_OR,
          ELSE, EXPR_LOGIC_OR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_TERNARY, 1, EXPR_LOGIC_OR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprLogicOr,
          EXPR_LOGIC_OR, 3, EXPR_LOGIC_OR, OR, EXPR_LOGIC_AND);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_LOGIC_OR, 1, EXPR_LOGIC_AND);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprLogicAnd,
          EXPR_LOGIC_AND, 3, EXPR_LOGIC_AND, AND, EXPR_BIT_OR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_LOGIC_AND, 1, EXPR_BIT_OR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprBitOr,
          EXPR_BIT_OR, 3, EXPR_BIT_OR, BIT_OR, EXPR_BIT_XOR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_BIT_OR, 1, EXPR_BIT_XOR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprBitXor,
          EXPR_BIT_XOR, 3, EXPR_BIT_XOR, BIT_XOR, EXPR_BIT_AND);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_BIT_XOR, 1, EXPR_BIT_AND);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprBitAnd,
          EXPR_BIT_AND, 3, EXPR_BIT_AND, BIT_AND, EXPR_REL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_BIT_AND, 1, EXPR_REL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprLt,
          EXPR_REL, 3, EXPR_REL, LT, EXPR_SHIFT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprLeq,
          EXPR_REL, 3, EXPR_REL, LEQ, EXPR_SHIFT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprEqEq,
          EXPR_REL, 3, EXPR_REL, EQEQ, EXPR_SHIFT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprNeq,
          EXPR_REL, 3, EXPR_REL, NEQ, EXPR_SHIFT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprGt,
          EXPR_REL, 3, EXPR_REL, GT, EXPR_SHIFT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprGeq,
          EXPR_REL, 3, EXPR_REL, GEQ, EXPR_SHIFT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_REL, 1, EXPR_SHIFT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprLshift,
          EXPR_SHIFT, 3, EXPR_SHIFT, LSHIFT, EXPR_ADD);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprRshift,
          EXPR_SHIFT, 3, EXPR_SHIFT, RSHIFT, EXPR_ADD);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_SHIFT, 1, EXPR_ADD);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprAdd,
          EXPR_ADD, 3, EXPR_ADD, ADD, EXPR_MUL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprSub,
          EXPR_ADD, 3, EXPR_ADD, SUB, EXPR_MUL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_ADD, 1, EXPR_MUL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprMul,
          EXPR_MUL, 3, EXPR_MUL, MUL, EXPR_CAST_OP);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprDiv,
          EXPR_MUL, 3, EXPR_MUL, DIV, EXPR_CAST_OP);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprMod,
          EXPR_MUL, 3, EXPR_MUL, MOD, EXPR_CAST_OP);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_MUL, 1, EXPR_CAST_OP);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprCast,
          EXPR_CAST_OP, 3, EXPR_UNARY_OP, IS, TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprCast,
          EXPR_CAST_OP, 3, EXPR_UNARY_OP, AS, TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprCast,
          EXPR_CAST_OP, 3, EXPR_UNARY_OP, INTO, TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_CAST_OP, 1, EXPR_UNARY_OP);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprNeg,
          EXPR_UNARY_OP, 2, SUB, EXPR_ACCESS);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprNot,
          EXPR_UNARY_OP, 2, NOT, EXPR_ACCESS);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprBitNot,
          EXPR_UNARY_OP, 2, BIT_NOT, EXPR_ACCESS);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_UNARY_OP, 1, EXPR_ACCESS);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprCall,
          EXPR_ACCESS, 4, EXPR_ACCESS, LPAREN, EXPR_LIST, RPAREN);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprArrayAccess,
          EXPR_ACCESS, 4, EXPR_ACCESS, LBRACK, EXPR, RBRACK);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprArrayType,
          EXPR_ACCESS, 3, EXPR_ACCESS, LBRACK, RBRACK);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprMemberAccess,
          EXPR_ACCESS, 3, EXPR_ACCESS, DOT, IDENTIFIER);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprInc,
          EXPR_ACCESS, 2, EXPR_ACCESS, ADD_ADD);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprDec,
          EXPR_ACCESS, 2, EXPR_ACCESS, SUB_SUB);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_ACCESS, 1, TERM);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          EXPR_LIST, 1, EXPR_LIST_NONEMPTY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprList,
          EXPR_LIST, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprList,
          EXPR_LIST_NONEMPTY, 3, EXPR_LIST_NONEMPTY, COMMA, EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprList,
          EXPR_LIST_NONEMPTY, 1, EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          TERM, 1, INT_LITERAL_TERM);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerFloatLiteral,
          TERM, 1, FLOAT_LITERAL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerTrueLiteral,
          TERM, 1, TRUE_LITERAL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerFalseLiteral,
          TERM, 1, FALSE_LITERAL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerThisLiteral,
          TERM, 1, THIS);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerNullLiteral,
          TERM, 1, NULL_LITERAL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerStringLiteral,
          TERM, 1, STRING_LITERAL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerCharLiteral,
          TERM, 1, CHAR_LITERAL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerVariable,
          TERM, 1, IDENTIFIER);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          TERM, 1, PRIMITIVE_TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerParenTerm,
          TERM, 3, LPAREN, EXPR, RPAREN);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerIntLiteral,
          INT_LITERAL_TERM, 1, INT_LITERAL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMethodDecls,
          METHOD_DECLS, 2, METHOD_DECLS, METHOD_DECL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMethodDecls,
          METHOD_DECLS, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMethodDecl,
          METHOD_DECL, 7, METHOD_DECL_MODIFIERS,
          IDENTIFIER, LPAREN, PARAM_LIST, RPAREN, RETURN_TYPE, METHOD_DECL_BODY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMethodDeclModifiers,
          METHOD_DECL_MODIFIERS, 1, FN);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMethodDeclModifiers,
          METHOD_DECL_MODIFIERS, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          METHOD_DECL_BODY, 1, BODY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerNull,
          METHOD_DECL_BODY, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerBody,
          BODY, 3, LBRACE, STMTS, RBRACE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          PARAM_LIST, 1, PARAM_LIST_NONEMPTY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerParamList,
          PARAM_LIST, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerParamList,
          PARAM_LIST_NONEMPTY, 3, PARAM_LIST, COMMA, PARAM);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerParamList,
          PARAM_LIST_NONEMPTY, 1, PARAM);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerParam,
          PARAM, 3, IDENTIFIER, COL, TYPE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerStmts,
          STMTS, 2, STMTS, STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerStmts,
          STMTS, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, VAR_DECL_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, EXPR_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, ASSIGN_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, IF_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, SWITCH_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, FOR_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, WHILE_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, BREAK_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          STMT, 1, RETURN_STMT);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerExprStmt,
          EXPR_STMT, 2, EXPR, SEMICOL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerAssignStmt,
          ASSIGN_STMT, 2, ASSIGN, SEMICOL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerAssign,
          ASSIGN, 3, EXPR, OP_ASSIGN, ASSIGN);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerAssign,
          ASSIGN, 3, EXPR, OP_ASSIGN, EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, ADD_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, SUB_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, MUL_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, DIV_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, MOD_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, LSHIFT_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, RSHIFT_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, BIT_AND_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, BIT_XOR_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerOpAssign,
          OP_ASSIGN, 1, BIT_OR_EQ);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerIfStmt,
          IF_STMT, 5, IF, EXPR, BODY, ELSE_IF_BODIES, ELSE_BODY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerElseIfBodies,
          ELSE_IF_BODIES, 5, ELSE_IF_BODIES, ELSE, IF, EXPR, BODY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerElseIfBodies,
          ELSE_IF_BODIES, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerElseBody,
          ELSE_BODY, 2, ELSE, BODY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerNull,
          ELSE_BODY, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerSwitchStmt,
          SWITCH_STMT, 6, SWITCH, EXPR, LBRACE, SWITCH_CASES, SWITCH_DEFAULT,
          RBRACE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerSwitchCases,
          SWITCH_CASES, 2, SWITCH_CASES, SWITCH_CASE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerSwitchCases,
          SWITCH_CASES, 1, SWITCH_CASE);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerSwitchCase,
          SWITCH_CASE, 3, CASE, EXPR_LIST_NONEMPTY, BODY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerSwitchDefault,
          SWITCH_DEFAULT, 2, DEFAULT, BODY);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerSwitchDefault,
          SWITCH_DEFAULT, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerLabel,
          LABEL, 3, LBRACK, IDENTIFIER, RBRACK);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerLabel,
          LABEL, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerForStmt,
          FOR_STMT, 8, FOR, FOR_INIT, SEMICOL, FOR_COND, SEMICOL,
          FOR_ITER, BODY, LABEL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          FOR_INIT, 1, VAR_DECL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          FOR_INIT, 1, EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          FOR_INIT, 1, ASSIGN);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPlaceholder,
          FOR_INIT, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          FOR_COND, 1, EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPlaceholder,
          FOR_COND, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          FOR_ITER, 1, EXPR);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerMove,
          FOR_ITER, 1, ASSIGN);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerPlaceholder,
          FOR_ITER, 0);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerWhileStmt,
          WHILE_STMT, 4, WHILE, EXPR, BODY, LABEL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerBreakStmt,
          BREAK_STMT, 2, BREAK, SEMICOL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerBreakStmt,
          BREAK_STMT, 3, BREAK, IDENTIFIER, SEMICOL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerReturnStmt,
          RETURN_STMT, 2, RETURN, SEMICOL);
      ParserAddRuleAndHandler(parserConfig, SyntaxHandlerReturnStmt,
          RETURN_STMT, 3, RETURN, EXPR, SEMICOL);
      
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
     parser = ParserFromFile(parserFile, lexer, SyntaxASTDelete);
     fclose(parserFile);
   }
   free(parserFilePath);
   return parser;
}

void SyntaxSetupParser(void) {
  Lexer *lexer = SyntaxCreateLexer();
  Parser *parser = SyntaxCreateParser(lexer);
  LexerDelete(lexer);
  ParserDelete(parser);
}

SyntaxAST *SyntaxParseFile(FILE *file, const char *filename) {
  Lexer *lexer = SyntaxCreateLexer();
  Parser *parser = SyntaxCreateParser(lexer);

  LexerSetInputFile(lexer, file, filename);
  SyntaxAST *node = ParserParse(parser);

  LexerDelete(lexer);
  ParserDelete(parser);
  return node;
}

void SyntaxASTDelete(void *p) {
  if (!p)
    return;

  SyntaxAST *node = p;
  SyntaxASTDelete(node->firstChild);
  SyntaxASTDelete(node->sibling);

  SemaDeleteASTSemaInfo(node);

  switch (node->kind) {
    case SYNTAX_AST_KIND_IDENTIFIER:
    case SYNTAX_AST_KIND_CLASS_DECL:
    case SYNTAX_AST_KIND_VAR_INIT:
    case SYNTAX_AST_KIND_MEMBER_ACCESS:
    case SYNTAX_AST_KIND_PARAM:
    case SYNTAX_AST_KIND_LABEL:
      free(node->string);
      break;
    case SYNTAX_AST_KIND_IMPORT_DECL:
      free(node->import.namespace);
      break;
    case SYNTAX_AST_KIND_LITERAL:
      if (node->literal.type == SYNTAX_TYPE_STR)
        free(node->literal.strVal);
      break;
    case SYNTAX_AST_KIND_METHOD_DECL:
      free(node->method.name);
      break;
  }
  free(node);
}

#include "parse/syntax/handlers.c"
