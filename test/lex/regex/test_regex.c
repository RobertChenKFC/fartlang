#include "lex/regex/regex.h"
#include <stdio.h>

int main() {
  // Initialize regex
  RegexInit();

  // 1. Test regex constants
  RegexPrint(REGEX_EMPTY_STRING);
  puts("");
  RegexPrint(REGEX_ANY);
  puts("");
  RegexPrint(REGEX_DIGITS);
  puts("");
  RegexPrint(REGEX_LETTERS);
  puts("");

  // 2. Test regex range (from letter) -> character class -> regex
  RegexRange *range = RegexRangeFromLetter('!');
  RegexCharacterClass *characterClass = RegexCharacterClassFromRanges(1, range);
  Regex *regex = RegexFromCharacterClass(characterClass);
  RegexPrint(regex);
  puts("");
  RegexRangeDelete(range);
  RegexCharacterClassDelete(characterClass);
  RegexDelete(regex);

  // 3. Test regex range (from range) -> character class -> regex
  range = RegexRangeFromRange('t', 'z');
  characterClass = RegexCharacterClassFromRanges(1, range);
  regex = RegexFromCharacterClass(characterClass);
  RegexPrint(regex);
  puts("");
  RegexRangeDelete(range);
  RegexCharacterClassDelete(characterClass);
  RegexDelete(regex);

  // 4. Test regex ranges -> character class -> regex
  range = RegexRangeFromLetter('^');
  RegexRange *range2 = RegexRangeFromRange('0', '7');
  RegexRange *range3 = RegexRangeFromLetter('3');
  RegexRange *range4 = RegexRangeFromRange('5', '9');
  characterClass = RegexCharacterClassFromRanges(
     4, range, range2, range3, range4);
  regex = RegexFromCharacterClass(characterClass);
  RegexPrint(regex);
  puts("");
  RegexRangeDelete(range);
  RegexRangeDelete(range2);
  RegexRangeDelete(range3);
  RegexRangeDelete(range4);
  RegexCharacterClassDelete(characterClass);
  RegexDelete(regex);

  // 5. Test negated regex character class
  range = RegexRangeFromRange('a', 'z');
  range2 = RegexRangeFromRange('0', '9');
  range3 = RegexRangeFromRange(0, 31);
  characterClass = RegexCharacterClassFromRanges(3, range, range2, range3);
  RegexCharacterClass *characterClass2 = RegexCharacterClassNegated(
      characterClass);
  regex = RegexFromCharacterClass(characterClass2);
  RegexPrint(regex);
  puts("");
  RegexRangeDelete(range);
  RegexRangeDelete(range2);
  RegexRangeDelete(range3);
  RegexCharacterClassDelete(characterClass);
  RegexCharacterClassDelete(characterClass2);
  RegexDelete(regex);

  // 6. Test regex union
  regex = RegexFromUnion(2, REGEX_DIGITS, REGEX_LETTERS);
  RegexPrint(regex);
  puts("");
  RegexDelete(regex);

  // 7. Test regex concatenation
  regex = RegexFromConcat(2, REGEX_DIGITS, REGEX_LETTERS);
  RegexPrint(regex);
  puts("");
  RegexDelete(regex);

  // 8. Test regex from string
  regex = RegexFromString("hello!");
  RegexPrint(regex);
  puts("");
  RegexDelete(regex);

  // 9. Test regex zero or more
  regex = RegexZeroOrMore(REGEX_DIGITS);
  RegexPrint(regex);
  puts("");
  RegexDelete(regex);

  // 10. Test regex one or more
  regex = RegexOneOrMore(REGEX_LETTERS);
  RegexPrint(regex);
  puts("");
  RegexDelete(regex);

  // 11. Test regex zero or one
  regex = RegexZeroOrOne(RegexFromLetter('.'));
  RegexPrint(regex);
  puts("");
  RegexDelete(regex);

  // 12. Test reusing regex
  regex = RegexFromString("hello");
  Regex *regex2 = RegexFromConcat(2, regex, regex);
  RegexPrint(regex2);
  puts("");
  RegexDelete(regex2);

  // 13. Test all
  range = RegexRangeFromRange('x', 'z');
  range2 = RegexRangeFromRange('6', '9');
  characterClass = RegexCharacterClassFromRanges(2, range, range2);
  characterClass2 = RegexCharacterClassNegated(characterClass);
  regex = RegexZeroOrMore(RegexFromCharacterClass(characterClass2));
  regex2 = RegexOneOrMore(RegexFromUnion(4,
      RegexFromLetter('+'),
      RegexFromLetter('-'),
      RegexFromLetter('*'),
      RegexFromLetter('/')));
  Regex *regex3 = RegexZeroOrOne(RegexFromString("optional"));
  Regex *regex4 = RegexFromUnion(2, regex, regex2);
  Regex *regex5 = RegexFromConcat(2, regex4, regex3);
  RegexPrint(regex5);
  puts("");
  RegexRangeDelete(range);
  RegexRangeDelete(range2);
  RegexCharacterClassDelete(characterClass);
  RegexCharacterClassDelete(characterClass2);
  RegexDelete(regex5);
}
