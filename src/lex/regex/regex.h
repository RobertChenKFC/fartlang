#ifndef REGEX_H
#define REGEX_H

// Forward declarations
typedef struct RegexRange RegexRange;
typedef struct RegexCharacterClass RegexCharacterClass;
typedef struct Regex Regex;

// Regex range which represents a continuous set of ASCII characters
struct RegexRange {
  unsigned char from, to;
};

// Regex character class which represents a set of ASCII characters
struct RegexCharacterClass {
  unsigned char characters[128];
  int numChars;
};

// Regex that accepts a single alphabet
typedef struct {
  unsigned char a;
} RegexLetter;

// A list structure that can link multiple regexes together
typedef struct RegexListNode {
  Regex *cur;
  struct RegexListNode *next;
} RegexListNode;
typedef struct {
  RegexListNode *head, *tail;
} RegexList;

// Regex that accepts the union of the languages of a list of regexes
typedef struct {
  RegexList list;
} RegexUnion;

// Regex that accepts the concatenattion of the languages of a list of regexes
typedef struct {
  RegexList list;
} RegexConcat;

// Regex that accepts zero or more occurrences of the language of a regex
typedef struct {
  Regex *regex;
} RegexStar;

// Polymorphic type regex; an optional "name" can also be stored
typedef enum {
  REGEX_NULL,
  REGEX_LETTER,
  REGEX_UNION,
  REGEX_CONCAT,
  REGEX_STAR,
} RegexType;
struct Regex {
  union {
    RegexLetter regexLetter;
    RegexUnion regexUnion;
    RegexConcat regexConcat;
    RegexStar regexStar;
  };
  RegexType type;
  char *name;
};

// The regex that accepts only the empty string
extern Regex *REGEX_EMPTY_STRING;
// The regex that accepts any character
extern Regex *REGEX_ANY;
// The regex that accepts digits 0-9
extern Regex *REGEX_DIGITS;
// The regex that accepts upper case letters A-Z and lower case letters a-z
extern Regex *REGEX_LETTERS;

// Initializes all constant regexes
void RegexInit();
// Given a letter "a", returns a range for character class use
RegexRange *RegexRangeFromLetter(unsigned char a);
// Given a letter "from" and a letter "to", returns a range from "from" to "to"
RegexRange *RegexRangeFromRange(unsigned char from, unsigned char to);
// Delete a regex range created with the above functions
void RegexRangeDelete(RegexRange *range);
// Given a number "n" and "n" ranges, returns a character class that contains
// all ranges
RegexCharacterClass *RegexCharacterClassFromRanges(int n, ...);
// Given a character class "c", returns the negated character class of "c"
RegexCharacterClass *RegexCharacterClassNegated(RegexCharacterClass *c);
// Delete a regex character class created with the above functions
void RegexCharacterClassDelete(RegexCharacterClass *c);
// Given a letter "a", returns a regex that accepts only the single-letter
// string "a"
Regex *RegexFromLetter(unsigned char a);
// Given a number "n" and "n" regexes, returns a regex that accepts the union
// of their languages
Regex *RegexFromUnion(int n, ...);
// Given a number "n" and "n" regexes, returns a regex that accepts the
// concatenation of their languages
Regex *RegexFromConcat(int n, ...);
// Given a string "s", returns a regex that accepts only "s"
Regex *RegexFromString(const char *s);
// Given a regex, returns a regex that accepts zero or more occurrences of
// its language
Regex *RegexZeroOrMore(Regex *regex);
// Given a regex, returns a regex that accepts one or more occurrences of
// its language
Regex *RegexOneOrMore(Regex *regex);
// Given a regex, returns a regex that accepts zero or one occurrence of
// its language
Regex *RegexZeroOrOne(Regex *regex);
// Given a character class "c", returns a regex
Regex *RegexFromCharacterClass(RegexCharacterClass *c);
// Delete a regex created with the above functions; note that this function
// recursively deletes the regexes, thus if a regex is used as an argument
// to create another regex, do not pass it to this function
void RegexDelete(Regex *regex);
// Print the content of a regex as a string
void RegexPrint(Regex *regex);

#endif // REGEX_H
