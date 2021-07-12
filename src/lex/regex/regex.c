#include "lex/regex/regex.h"
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// Regex range which represents a continuous set of ASCII characters
struct RegexRange {
  unsigned char from, to;
};

// Regex character class which represents a set of ASCII characters
struct RegexCharacterClass {
  unsigned char characters[128];
  int numChars;
};

// Regex that accepts the empty language
typedef struct {} RegexNull;

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

// Polymorphic type regex
typedef enum {
  REGEX_NULL,
  REGEX_LETTER,
  REGEX_UNION,
  REGEX_CONCAT,
  REGEX_STAR,
} RegexType;
struct Regex {
  union {
    RegexNull regexNull;
    RegexLetter regexLetter;
    RegexUnion regexUnion;
    RegexConcat regexConcat;
    RegexStar regexStar;
  };
  RegexType type;
};

// A vector structure that can contain multiple pointers and dynamically
// increase its size
typedef struct {
  void **arr;
  int size, capacity;
} Vector;

// Constants
Regex *REGEX_EMPTY_STRING;
Regex *REGEX_ANY;
Regex *REGEX_DIGITS;
Regex *REGEX_LETTERS;
#define IS_REGEX_CONSTANT(r) ((r) == REGEX_EMPTY_STRING || \
                              (r) == REGEX_ANY          || \
                              (r) == REGEX_DIGITS       || \
                              (r) == REGEX_LETTERS)
#define DEFAULT_VEC_SIZE 64
#define IS_CONTROL_CHARACTER(a) (((a) >= 0 && (a) <= 31) || (a) == 127)
bool IS_SPECIAL_CHARACTER[128];

// Helper functions
// Creates a new regex object on the heap with given "type"
Regex* RegexNew(RegexType type);
// Vector version of RegexCharacterClassFromRanges
RegexCharacterClass *RegexCharacterClassFromRangeVector(
    int n, RegexRange *vec[]);
// Vector version of RegexFromUnion
Regex *RegexFromUnionVector(int n, Regex *vec[]);
// Vector version of RegexFromConcat
Regex *RegexFromConcatVector(int n, Regex *vec[]);
// Add regex to the end of list
void RegexListAdd(RegexList *list, Regex *regex);
// Create a new empty pointer vector with defualt capacity
Vector *VectorNew();
// Delete a vector created with VectorNew
void VectorDelete(Vector *vec);
// Add regex to the end of vector
void VectorAdd(Vector *vec, void *ptr);
// Add entire regex list, including each individual regex and the node that
// contains it, to a deletion vector
void RegexListRequestDelete(RegexList *list, Vector *vec);
// Add regex and all its child to a deletion vector
void RegexRequestDelete(Regex *regex, Vector *vec);
// A compare function for two regex pointers "a" and "b"; used for sorting
int RegexPtrCmp(const void *a, const void *b);

Regex* RegexNew(RegexType type) {
  Regex *regex = calloc(1, sizeof(Regex));
  regex->type = type;
  return regex;
}

void RegexInit() {
  Regex *regexes[128];

  // Initialize empty string regex
  REGEX_EMPTY_STRING = RegexZeroOrMore(RegexNew(REGEX_NULL));

  // Initialize any character regex
  for (int i = 0; i < 128; ++i)
    regexes[i] = RegexFromLetter((unsigned char)i);
  REGEX_ANY = RegexFromUnionVector(128, regexes);

  // Initialize digits regex
  for (int i = 0; i <= 9; ++i)
    regexes[i] = RegexFromLetter('0' + i);
  REGEX_DIGITS = RegexFromUnionVector(10, regexes);

  // Intialize letters regex
  for (int i = 0; i < 26; ++i) {
    regexes[i] = RegexFromLetter('A' + i);
    regexes[i + 26] = RegexFromLetter('a' + i);
  }
  REGEX_LETTERS = RegexFromUnionVector(52, regexes);

  // Initialize special character array
  IS_SPECIAL_CHARACTER['('] = true;
  IS_SPECIAL_CHARACTER[')'] = true;
  IS_SPECIAL_CHARACTER['|'] = true;
  IS_SPECIAL_CHARACTER['*'] = true;
}

RegexRange *RegexRangeFromLetter(unsigned char a) {
  return RegexRangeFromRange(a, a);
}

RegexRange *RegexRangeFromRange(unsigned char from, unsigned char to) {
  RegexRange *range = malloc(sizeof(RegexRange));
  range->from = from;
  range->to = to;
  return range;
}

void RegexRangeDelete(RegexRange *range) {
  free(range);
}

RegexCharacterClass *RegexCharacterClassFromRanges(int n, ...) {
  static RegexRange *buf[DEFAULT_VEC_SIZE];

  RegexRange **vec;
  if (n > DEFAULT_VEC_SIZE)
    vec = malloc(sizeof(RegexRange*) * n);
  else
    vec = buf;

  va_list list;
  va_start(list, n);
  for (int i = 0; i < n; ++i)
    vec[i] = va_arg(list, RegexRange*);
  va_end(list);

  RegexCharacterClass *c = RegexCharacterClassFromRangeVector(n, vec);

  if (n > DEFAULT_VEC_SIZE)
    free(vec);

  return c;
}

RegexCharacterClass *RegexCharacterClassFromRangeVector(
    int n, RegexRange *vec[]) {
  RegexCharacterClass *c = malloc(sizeof(RegexCharacterClass));
  unsigned char *characters = c->characters;
  int numChars = 0;

  bool chosen[128] = {};
  for (int i = 0; i < n; ++i) {
    RegexRange *range = vec[i];
    for (unsigned char a = range->from; a <= range->to; ++a) {
      if (!chosen[a]) {
        characters[numChars++] = a;
        chosen[a] = true;
      }
    }
  }

  c->numChars = numChars;
  return c;
}

RegexCharacterClass *RegexCharacterClassNegated(RegexCharacterClass *c) {
  bool chosen[128] = {};
  unsigned char *characters = c->characters;
  int numChars = c->numChars;
  for (int i = 0; i < numChars; ++i)
    chosen[characters[i]] = true;

  RegexCharacterClass *c2 = malloc(sizeof(RegexCharacterClass));
  characters = c2->characters;
  numChars = 0;
  for (unsigned char i = 0; i < 128; ++i) {
    if (!chosen[i])
      characters[numChars++] = i;
  }
  c2->numChars = numChars;
  return c2;
}

void RegexCharacterClassDelete(RegexCharacterClass *c) {
  free(c);
}

Regex *RegexFromLetter(unsigned char a) {
  Regex *regex = RegexNew(REGEX_LETTER);
  regex->regexLetter.a = a;
  return regex;
}

Regex *RegexFromUnion(int n, ...) {
  static Regex *buf[DEFAULT_VEC_SIZE];

  Regex **vec;
  if (n > DEFAULT_VEC_SIZE)
    vec = malloc(sizeof(Regex*) * n);
  else
    vec = buf;

  va_list list;
  va_start(list, n);
  for (int i = 0; i < n; ++i)
    vec[i] = va_arg(list, Regex*);
  va_end(list);

  Regex *regex = RegexFromUnionVector(n, vec);

  if (n > DEFAULT_VEC_SIZE)
    free(vec);

  return regex;
}

Regex *RegexFromUnionVector(int n, Regex *vec[]) {
  Regex *regex = RegexNew(REGEX_UNION);
  RegexList *list = &regex->regexUnion.list;
  for (int i = 0; i < n; ++i)
    RegexListAdd(list, vec[i]);
  return regex;
}

Regex *RegexFromConcat(int n, ...) {
  static Regex *buf[DEFAULT_VEC_SIZE];

  Regex **vec;
  if (n > DEFAULT_VEC_SIZE)
    vec = malloc(sizeof(Regex*) * n);
  else
    vec = buf;

  va_list list;
  va_start(list, n);
  for (int i = 0; i < n; ++i)
    vec[i] = va_arg(list, Regex*);
  va_end(list);

  Regex *regex = RegexFromConcatVector(n, vec);

  if (n > DEFAULT_VEC_SIZE)
    free(vec);

  return regex;
}

Regex *RegexFromConcatVector(int n, Regex *vec[]) {
  Regex *regex = RegexNew(REGEX_CONCAT);
  RegexList *list = &regex->regexUnion.list;
  for (int i = 0; i < n; ++i)
    RegexListAdd(list, vec[i]);
  return regex;
}

Regex *RegexFromString(const char *s) {
  static Regex *buf[DEFAULT_VEC_SIZE];

  int n = strlen(s);
  Regex **vec;
  if (n > DEFAULT_VEC_SIZE)
    vec = malloc(sizeof(Regex*) * n);
  else
    vec = buf;

  for (int i = 0; i < n; ++i)
    vec[i] = RegexFromLetter(s[i]);

  Regex *regex = RegexFromConcatVector(n, vec);

  if (n > DEFAULT_VEC_SIZE)
    free(vec);

  return regex;
}

Regex *RegexZeroOrMore(Regex *regex) {
  Regex *regex2 = RegexNew(REGEX_STAR);
  regex2->regexStar.regex = regex;
  return regex2;
}

Regex *RegexOneOrMore(Regex *regex) {
  Regex *regex2 = RegexZeroOrMore(regex);
  return RegexFromConcat(2, regex, regex2);
}

Regex *RegexZeroOrOne(Regex *regex) {
  return RegexFromUnion(2, REGEX_EMPTY_STRING, regex);
}

Regex *RegexFromCharacterClass(RegexCharacterClass *c) {
  static Regex *buf[DEFAULT_VEC_SIZE];

  Regex **vec;
  int n = c->numChars;
  if (n > DEFAULT_VEC_SIZE)
    vec = malloc(sizeof(Regex*) * n);
  else
    vec = buf;

  unsigned char *characters = c->characters;
  for (int i = 0; i < n; ++i)
    vec[i] = RegexFromLetter(characters[i]);

  Regex *regex = RegexFromUnionVector(n, vec);

  if (n > DEFAULT_VEC_SIZE)
    free(vec);

  return regex;
}

Vector *VectorNew() {
  Vector *vec = malloc(sizeof(Vector));
  vec->arr = malloc(sizeof(void*) * DEFAULT_VEC_SIZE);
  vec->capacity = DEFAULT_VEC_SIZE;
  vec->size = 0;
  return vec;
}

void VectorDelete(Vector *vec) {
  free(vec->arr);
  free(vec);
}

void VectorAdd(Vector *vec, void *ptr) {
  if (vec->size == vec->capacity) {
    vec->capacity *= 2;
    vec->arr = realloc(vec->arr, sizeof(void*) * vec->capacity);
  }
  vec->arr[vec->size++] = ptr;
}

void RegexRequestDelete(Regex *regex, Vector *vec) {
  if (IS_REGEX_CONSTANT(regex))
    return;
  switch (regex->type) {
    case REGEX_NULL:
      break;
    case REGEX_LETTER:
      break;
    case REGEX_UNION:
      RegexListRequestDelete(&regex->regexUnion.list, vec);
      break;
    case REGEX_CONCAT:
      RegexListRequestDelete(&regex->regexConcat.list, vec);
      break;
    case REGEX_STAR:
      RegexRequestDelete(regex->regexStar.regex, vec);
      break;
  }
  VectorAdd(vec, regex);
}

void RegexDelete(Regex *regex) {
  Vector *vec = VectorNew();
  RegexRequestDelete(regex, vec);

  qsort(vec->arr, vec->size, sizeof(Regex*), RegexPtrCmp);

  for (int i = 0; i < vec->size; ++i) {
    if (i == vec->size - 1 || vec->arr[i] != vec->arr[i + 1])
      free(vec->arr[i]);
  }

  VectorDelete(vec);
}

int RegexPtrCmp(const void *a, const void *b) {
  void * const *pa = a, * const *pb = b;
  if (*pa < *pb)
    return -1;
  if (*pa > *pb)
    return 1;
  return 0;
}

void RegexListAdd(RegexList *list, Regex *regex) {
  RegexListNode *node = malloc(sizeof(RegexListNode));
  node->cur = regex;
  node->next = NULL;
  if (list->tail == NULL) {
    list->head = list->tail = node;
  } else {
    list->tail->next = node;
    list->tail = node;
  }
}

void RegexListRequestDelete(RegexList *list, Vector *vec) {
  for (RegexListNode *node = list->head; node; node = node->next) {
    RegexRequestDelete(node->cur, vec);
    VectorAdd(vec, node);
  }
}

void RegexPrint(Regex *regex) {
  char a;
  switch (regex->type) {
    case REGEX_NULL:
      printf("∅");
      break;
    case REGEX_LETTER:
      a = regex->regexLetter.a;
      if (IS_CONTROL_CHARACTER(a)) {
        printf("\\x%02x", (int)a);
      } else {
        if (IS_SPECIAL_CHARACTER[a])
          putchar('\\');
        putchar(a);
      }
      break;
    case REGEX_UNION:
      for (RegexListNode *node = regex->regexUnion.list.head; node;
           node = node->next) {
        if (node != regex->regexUnion.list.head)
          putchar('|');
        putchar('(');
        RegexPrint(node->cur);
        putchar(')');
      }
      break;
    case REGEX_CONCAT:
      for (RegexListNode *node = regex->regexConcat.list.head; node;
           node = node->next) {
        putchar('(');
        RegexPrint(node->cur);
        putchar(')');
      }
      break;
    case REGEX_STAR:
      putchar('(');
      RegexPrint(regex->regexStar.regex);
      printf(")*");
      break;
  }
}
