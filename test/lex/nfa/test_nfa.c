#include "lex/nfa/nfa.h"
#include <stdio.h>

int main() {
  // Initialize NFA
  NFAInit();

  // 1. Test regex base case - empty (cannot be tested directly, thus can only
  //    be constructed by empty string, which also contains a Kleene star
  //    operation)
  Regex *regex = REGEX_EMPTY_STRING;
  NFA *nfa = NFAFromRegex(regex);
  FILE *file = fopen("1.out", "w");
  NFAPrint(nfa, file);
  NFADelete(nfa);
  fclose(file);

  // 2. Test regex base case - letter
  regex = RegexFromLetter('a');
  nfa = NFAFromRegex(regex);
  file = fopen("2.out", "w");
  NFAPrint(nfa, file);
  RegexDelete(regex);
  NFADelete(nfa);
  fclose(file);

  // 3. Test inductive case - union
  regex = RegexFromUnion(2, RegexFromLetter('+'), RegexFromLetter('-'));
  nfa = NFAFromRegex(regex);
  file = fopen("3.out", "w");
  NFAPrint(nfa, file);
  RegexDelete(regex);
  NFADelete(nfa);
  fclose(file);

  // 4. Test inductive case - concatenation
  regex = RegexFromConcat(2, RegexFromLetter('*'), RegexFromLetter('/'));
  nfa = NFAFromRegex(regex);
  file = fopen("4.out", "w");
  NFAPrint(nfa, file);
  RegexDelete(regex);
  NFADelete(nfa);
  fclose(file);

  // 5. Test inductive case - Kleene star
  regex = RegexZeroOrMore(RegexFromLetter('?'));
  nfa = NFAFromRegex(regex);
  file = fopen("5.out", "w");
  NFAPrint(nfa, file);
  RegexDelete(regex);
  NFADelete(nfa);
  fclose(file);

  // 6. Complex regex 1
  Regex *name = RegexFromConcat(2,
      RegexOneOrMore(REGEX_LETTERS),
      RegexZeroOrMore(
          RegexFromConcat(2,
              RegexFromLetter('.'),
              RegexOneOrMore(REGEX_LETTERS))));
  Regex *domain = RegexFromUnion(3,
      RegexFromString(".com"),
      RegexFromString(".edu"),
      RegexFromString(".gov"));
  regex = RegexFromConcat(4, name, RegexFromLetter('@'), name, domain);
  nfa = NFAFromRegex(regex);
  file = fopen("6.out", "w");
  NFAPrint(nfa, file);
  RegexDelete(regex);
  NFADelete(nfa);
  fclose(file);

  // 7. Complex regex 2
  Regex *underscore = RegexFromLetter('_');
  Regex *identifier = RegexFromConcat(2,
      RegexFromUnion(2, REGEX_LETTERS, underscore),
      RegexZeroOrMore(
          RegexFromUnion(3, REGEX_LETTERS, REGEX_DIGITS, underscore)));
  regex = RegexFromConcat(4,
      identifier,
      RegexFromLetter('('),
      RegexZeroOrOne(
          RegexFromConcat(2,
              identifier,
              RegexZeroOrMore(
                  RegexFromConcat(2, RegexFromLetter(','), identifier)))),
      RegexFromLetter(')'));
  nfa = NFAFromRegex(regex);
  file = fopen("7.out", "w");
  NFAPrint(nfa, file);
  RegexDelete(regex);
  NFADelete(nfa);
  fclose(file);
}
