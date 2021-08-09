#include "lex/regex/regex.h"
#include "lex/dfa/dfa.h"
#include "lex/lexer/lexer.h"
#include <stdio.h>
#include <assert.h>

// Forward declarations
bool ReadTokenUntilEOF(Lexer *lexer, FILE *file);

int main() {
  RegexInit();
  DFAInit();

  // 1. Test single token (no error)
  LexerConfig *config = LexerConfigNew();
  Regex *hello = RegexFromUnion(2,
      RegexFromString("Hello"),
      RegexFromString("hello"));
  int helloID = LexerConfigAddRegex(config, hello);
  assert(helloID == 0);
  Regex *whitespace = RegexFromUnion(4,
      RegexFromLetter(' '),
      RegexFromLetter('\t'),
      RegexFromLetter('\r'),
      RegexFromLetter('\n'));
  LexerConfigSetIgnoreRegex(config, whitespace);
  Lexer *lexer = LexerFromConfig(config);
  FILE *file = fopen("1.in", "r");
  LexerSetInputFile(lexer, file, "1.in");
  fclose(file);
  file = fopen("1.out", "w");
  bool noErrors = ReadTokenUntilEOF(lexer, file);
  assert(noErrors);
  fclose(file);
  RegexDelete(hello);
  LexerConfigDelete(config);

  // 2. Test single token (with error before end of file)
  file = fopen("2.in", "r");
  LexerSetInputFile(lexer, file, "2.in");
  fclose(file);
  file = fopen("2.out", "w");
  noErrors = ReadTokenUntilEOF(lexer, file);
  assert(!noErrors);
  fclose(file);

  // 3. Test single token (with error at end of file)
  file = fopen("3.in", "r");
  LexerSetInputFile(lexer, file, "3.in");
  fclose(file);
  file = fopen("3.out", "w");
  noErrors = ReadTokenUntilEOF(lexer, file);
  assert(!noErrors);
  fclose(file);
  LexerDelete(lexer);

  // 4. Test multiple tokens (no error)
  Regex *digitString = RegexOneOrMore(REGEX_DIGITS);
  Regex *letterDigitString = RegexOneOrMore(RegexFromUnion(2,
      REGEX_LETTERS, REGEX_DIGITS));
  Regex *dot = RegexFromLetter('.');
  Regex *ip = RegexFromConcat(7,
    digitString, dot, digitString, dot, digitString, dot, digitString);
  Regex *head = RegexFromConcat(2,
    letterDigitString,
    RegexZeroOrMore(RegexFromConcat(2,
        dot,
        letterDigitString)));
  Regex *email = RegexFromConcat(3,
    head,
    RegexFromLetter('@'),
    RegexFromUnion(2,
      RegexFromString("gmail.com"),
      RegexFromString("hotmail.com")
    ));
  Regex *website = RegexFromConcat(2,
    head,
    RegexFromUnion(2,
      RegexFromString(".com.tw"),
      RegexFromString(".edu.tw")));
  Regex *all = RegexFromUnion(3, ip, email, website);
  config = LexerConfigNew();
  int ipID = LexerConfigAddRegex(config, ip);
  assert(ipID == 0);
  int emailID = LexerConfigAddRegex(config, email);
  assert(emailID == 1);
  int websiteID = LexerConfigAddRegex(config, website);
  assert(websiteID == 2);
  LexerConfigSetIgnoreRegex(config, whitespace);
  lexer = LexerFromConfig(config);
  file = fopen("4.in", "r");
  LexerSetInputFile(lexer, file, "4.in");
  fclose(file);
  file = fopen("4.out", "w");
  noErrors = ReadTokenUntilEOF(lexer, file);
  fclose(file);
  assert(noErrors);
  RegexDelete(all);
  RegexDelete(whitespace);
  LexerConfigDelete(config);

  // 5. Lexer save and load
  file = fopen("lexer.txt", "w");
  LexerToFile(lexer, file);
  fclose(file);
  LexerDelete(lexer);
  file = fopen("lexer.txt", "r");
  lexer = LexerFromFile(file);
  fclose(file);
  file = fopen("5.in", "r");
  LexerSetInputFile(lexer, file, "5.in");
  fclose(file);
  file = fopen("5.out", "w");
  noErrors = ReadTokenUntilEOF(lexer, file);
  fclose(file);
  assert(noErrors);

  // 6. Test multiple tokens (with error before end of file)
  file = fopen("6.in", "r");
  LexerSetInputFile(lexer, file, "6.in");
  fclose(file);
  file = fopen("6.out", "w");
  noErrors = ReadTokenUntilEOF(lexer, file);
  fclose(file);
  assert(!noErrors);

  // 7. Test multiple tokens (with error at end of file)
  file = fopen("7.in", "r");
  LexerSetInputFile(lexer, file, "7.in");
  fclose(file);
  file = fopen("7.out", "w");
  noErrors = ReadTokenUntilEOF(lexer, file);
  fclose(file);
  assert(!noErrors);

  LexerDelete(lexer);
}

bool ReadTokenUntilEOF(Lexer *lexer, FILE *file) {
  while (true) {
    LexerToken *token = LexerNextToken(lexer);
    if (token == LEXER_TOKEN_EOF)
      return true;
    if (!token)
      return false;
    fprintf(file, "tokenID: %d, location: %d:%d ~ %d:%d, length: %d, token: ",
            token->tokenID,
            token->loc.from.lineNo + 1, token->loc.from.charNo + 1,
            token->loc.to.lineNo + 1, token->loc.to.charNo + 1,
            token->length);
    for (int i = 0; i < token->length; ++i)
      fputc(token->str[i], file);
    fprintf(file, "\n");
    LexerTokenDelete(token);
  }
}
