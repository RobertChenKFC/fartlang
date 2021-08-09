#include "lex/regex/regex.h"
#include "lex/nfa/nfa.h"
#include "lex/dfa/dfa.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

// Forward declarations
void NFAToDFATest();
void DFAMinimizationTest();
void NFAsToDFATest();

int main() {
  RegexInit();
  DFAInit();
  NFAToDFATest();
  DFAMinimizationTest();
  NFAsToDFATest();
}

void NFAToDFATest() {
  // 1. Directly convert NFA (1)
  FA *nfa = FANew();
  FAState *state = FAStateNew(FA_ACCEPT_NONE);
  FAState *state2 = FAStateNew(FA_ACCEPT_NONE);
  FAState *state3 = FAStateNew(0);
  FAAddState(nfa, state);
  FAAddState(nfa, state2);
  FAAddState(nfa, state3);
  FAStateAddTransition(state, '0', state);
  FAStateAddTransition(state, '1', state);
  FAStateAddTransition(state, '1', state2);
  FAStateAddTransition(state2, '0', state3);
  FAStateAddTransition(state2, '1', state3);
  FA *dfa = DFAFromNFA(nfa);
  FILE *file = fopen("1.out", "w");
  FAPrint(dfa, file);
  fclose(file);
  FADelete(nfa);
  FADelete(dfa);

  // 2. Directly convert NFA (2)
  nfa = FANew();
  state = FAStateNew(FA_ACCEPT_NONE);
  state2 = FAStateNew(FA_ACCEPT_NONE);
  state3 = FAStateNew(0);
  FAAddState(nfa, state);
  FAAddState(nfa, state2);
  FAAddState(nfa, state3);
  FAStateAddTransition(state, '0', state);
  FAStateAddTransition(state, '1', state2);
  FAStateAddTransition(state2, '0', state2);
  FAStateAddTransition(state2, '0', state3);
  FAStateAddTransition(state2, '1', state2);
  FAStateAddTransition(state3, '0', state3);
  FAStateAddTransition(state3, '1', state3);
  FAStateAddTransition(state3, '1', state2);
  dfa = DFAFromNFA(nfa);
  file = fopen("2.out", "w");
  FAPrint(dfa, file);
  fclose(file);
  FADelete(nfa);
  FADelete(dfa);

  // 3. Directly convert NFA (3)
  nfa = FANew();
  state = FAStateNew(FA_ACCEPT_NONE);
  state2 = FAStateNew(FA_ACCEPT_NONE);
  state3 = FAStateNew(0);
  FAAddState(nfa, state);
  FAAddState(nfa, state2);
  FAAddState(nfa, state3);
  FAStateAddTransition(state, '0', state2);
  FAStateAddTransition(state, '0', state3);
  FAStateAddTransition(state, '1', state);
  FAStateAddTransition(state, FA_EPSILON, state2);
  FAStateAddTransition(state2, '1', state2);
  FAStateAddTransition(state2, FA_EPSILON, state3);
  FAStateAddTransition(state3, '0', state3);
  FAStateAddTransition(state3, '1', state3);
  dfa = DFAFromNFA(nfa);
  file = fopen("3.out", "w");
  FAPrint(dfa, file);
  fclose(file);
  FADelete(nfa);
  FADelete(dfa);

  // 4. Directly convert NFA (4)
  nfa = FANew();
  state = FAStateNew(FA_ACCEPT_NONE);
  state2 = FAStateNew(FA_ACCEPT_NONE);
  state3 = FAStateNew(FA_ACCEPT_NONE);
  FAState *state4 = FAStateNew(0);
  FAAddState(nfa, state);
  FAAddState(nfa, state2);
  FAAddState(nfa, state3);
  FAAddState(nfa, state4);
  FAStateAddTransition(state, 'a', state);
  FAStateAddTransition(state, 'b', state2);
  FAStateAddTransition(state, FA_EPSILON, state3);
  FAStateAddTransition(state2, 'c', state4);
  FAStateAddTransition(state3, 'b', state3);
  FAStateAddTransition(state3, 'c', state4);
  dfa = DFAFromNFA(nfa);
  file = fopen("4.out", "w");
  FAPrint(dfa, file);
  fclose(file);
  FADelete(nfa);
  FADelete(dfa);

  // 5. Convert from simple regex (1)
  Regex *regex = RegexFromUnion(2,
      RegexFromString("hello"),
      RegexFromString("world"));
  nfa = NFAFromRegex(regex);
  dfa = DFAFromNFA(nfa);
  file = fopen("5.out", "w");
  FAPrint(dfa, file);
  fclose(file);
  RegexDelete(regex);
  FADelete(nfa);
  FADelete(dfa);

  // 6. Convert from simple regex (2)
  regex = RegexFromConcat(2,
      RegexFromString("hello"),
      RegexFromString("world"));
  nfa = NFAFromRegex(regex);
  dfa = DFAFromNFA(nfa);
  file = fopen("6.out", "w");
  FAPrint(dfa, file);
  fclose(file);
  RegexDelete(regex);
  FADelete(nfa);
  FADelete(dfa);

  // 7. Convert from simple regex (3)
  regex = RegexZeroOrMore(RegexFromString("hello"));
  nfa = NFAFromRegex(regex);
  dfa = DFAFromNFA(nfa);
  file = fopen("7.out", "w");
  FAPrint(dfa, file);
  fclose(file);
  RegexDelete(regex);
  FADelete(nfa);
  FADelete(dfa);

  // 8. Convert complex regex
  regex = RegexFromConcat(2,
    RegexFromUnion(3,
      RegexZeroOrMore(RegexFromString("hello")),
      RegexOneOrMore(RegexFromString("hell no")),
      RegexZeroOrOne(RegexFromString("help!"))),
    RegexZeroOrMore(RegexFromConcat(2,
      RegexFromLetter('a'),
      RegexZeroOrMore(RegexFromString("bca")))));
  nfa = NFAFromRegex(regex);
  dfa = DFAFromNFA(nfa);
  file = fopen("8.out", "w");
  FAPrint(dfa, file);
  fclose(file);
  RegexDelete(regex);
  FADelete(nfa);
  FADelete(dfa);
}

void DFAMinimizationTest() {
  // 9. Directly convert DFA (1)
  FA *dfa = FANew();
  FAState *state0 = FAStateNew(FA_ACCEPT_NONE);
  FAState *state1 = FAStateNew(0);
  FAState *state2 = FAStateNew(0);
  FAState *state3 = FAStateNew(FA_ACCEPT_NONE);
  FAState *state4 = FAStateNew(0);
  FAState *state5 = FAStateNew(FA_ACCEPT_NONE);
  FAAddState(dfa, state0);
  FAAddState(dfa, state1);
  FAAddState(dfa, state2);
  FAAddState(dfa, state3);
  FAAddState(dfa, state4);
  FAAddState(dfa, state5);
  FAStateAddTransition(state0, '0', state3);
  FAStateAddTransition(state0, '1', state1);
  FAStateAddTransition(state1, '0', state2);
  FAStateAddTransition(state1, '1', state5);
  FAStateAddTransition(state2, '0', state2);
  FAStateAddTransition(state2, '1', state5);
  FAStateAddTransition(state3, '0', state0);
  FAStateAddTransition(state3, '1', state4);
  FAStateAddTransition(state4, '0', state2);
  FAStateAddTransition(state4, '1', state5);
  FAStateAddTransition(state5, '0', state5);
  FAStateAddTransition(state5, '1', state5);
  FA *minDFA = DFAMinimize(dfa);
  FILE *file = fopen("9.out", "w");
  FAPrint(minDFA, file);
  fclose(file);
  FADelete(dfa);
  FADelete(minDFA);

  // 10. Directly convert DFA (2)
  dfa = FANew();
  state0 = FAStateNew(FA_ACCEPT_NONE);
  state1 = FAStateNew(FA_ACCEPT_NONE);
  state2 = FAStateNew(FA_ACCEPT_NONE);
  state3 = FAStateNew(0);
  state4 = FAStateNew(FA_ACCEPT_NONE);
  state5 = FAStateNew(0);
  FAAddState(dfa, state0);
  FAAddState(dfa, state1);
  FAAddState(dfa, state2);
  FAAddState(dfa, state3);
  FAAddState(dfa, state4);
  FAAddState(dfa, state5);
  FAStateAddTransition(state0, '0', state1);
  FAStateAddTransition(state0, '1', state3);
  FAStateAddTransition(state1, '0', state0);
  FAStateAddTransition(state1, '1', state3);
  FAStateAddTransition(state2, '0', state1);
  FAStateAddTransition(state2, '1', state4);
  FAStateAddTransition(state3, '0', state5);
  FAStateAddTransition(state3, '1', state5);
  FAStateAddTransition(state4, '0', state3);
  FAStateAddTransition(state4, '1', state3);
  FAStateAddTransition(state5, '0', state5);
  FAStateAddTransition(state5, '1', state5);
  minDFA = DFAMinimize(dfa);
  file = fopen("10.out", "w");
  FAPrint(minDFA, file);
  fclose(file);
  FADelete(dfa);
  FADelete(minDFA);

  // 11. Directly convert DFA (3)
  dfa = FANew();
  state0 = FAStateNew(FA_ACCEPT_NONE);
  state1 = FAStateNew(FA_ACCEPT_NONE);
  state2 = FAStateNew(FA_ACCEPT_NONE);
  state3 = FAStateNew(0);
  state4 = FAStateNew(0);
  state5 = FAStateNew(FA_ACCEPT_NONE);
  FAAddState(dfa, state0);
  FAAddState(dfa, state1);
  FAAddState(dfa, state2);
  FAAddState(dfa, state3);
  FAAddState(dfa, state4);
  FAAddState(dfa, state5);
  FAStateAddTransition(state0, '0', state1);
  FAStateAddTransition(state0, '1', state2);
  FAStateAddTransition(state1, '0', state2);
  FAStateAddTransition(state1, '1', state3);
  FAStateAddTransition(state2, '0', state2);
  FAStateAddTransition(state2, '1', state4);
  FAStateAddTransition(state3, '0', state3);
  FAStateAddTransition(state3, '1', state3);
  FAStateAddTransition(state4, '0', state4);
  FAStateAddTransition(state4, '1', state4);
  FAStateAddTransition(state5, '0', state5);
  FAStateAddTransition(state5, '1', state4);
  minDFA = DFAMinimize(dfa);
  file = fopen("11.out", "w");
  FAPrint(minDFA, file);
  fclose(file);
  FADelete(dfa);
  FADelete(minDFA);

  // 12. NFA to minimized DFA
  FA *nfa = FANew();
  state0 = FAStateNew(FA_ACCEPT_NONE);
  state1 = FAStateNew(FA_ACCEPT_NONE);
  state2 = FAStateNew(0);
  FAAddState(nfa, state0);
  FAAddState(nfa, state1);
  FAAddState(nfa, state2);
  FAStateAddTransition(state0, 'a', state0);
  FAStateAddTransition(state0, 'a', state1);
  FAStateAddTransition(state0, 'b', state0);
  FAStateAddTransition(state1, 'b', state2);
  FAStateAddTransition(state2, 'a', state2);
  FAStateAddTransition(state2, 'b', state2);
  dfa = DFAFromNFA(nfa);
  minDFA = DFAMinimize(dfa);
  file = fopen("12.out", "w");
  FAPrint(minDFA, file);
  fclose(file);
  FADelete(nfa);
  FADelete(dfa);
  FADelete(minDFA);

  // 13. Regex to minimized DFA (1)
  RegexRange *abRange = RegexRangeFromRange('a', 'b');
  RegexCharacterClass *abClass = RegexCharacterClassFromRanges(1, abRange);
  Regex *ab = RegexFromCharacterClass(abClass);
  RegexRange *bcRange = RegexRangeFromRange('b', 'c');
  RegexCharacterClass *bcClass = RegexCharacterClassFromRanges(1, bcRange);
  Regex *bc = RegexFromCharacterClass(bcClass);
  RegexCharacterClass *abcClass = RegexCharacterClassFromRanges(
      2, abRange, bcRange);
  Regex *abc = RegexFromCharacterClass(abcClass);
  Regex *regex = RegexFromUnion(2,
      RegexFromConcat(3,
          RegexZeroOrMore(ab),
          RegexZeroOrOne(RegexFromLetter('c')),
          RegexZeroOrMore(ab)),
      RegexFromConcat(3,
          RegexZeroOrMore(abc),
          bc,
          abc));
  nfa = NFAFromRegex(regex);
  dfa = DFAFromNFA(nfa);
  minDFA = DFAMinimize(dfa);
  file = fopen("13.out", "w");
  FAPrint(minDFA, file);
  fclose(file);
  RegexRangeDelete(abRange);
  RegexRangeDelete(bcRange);
  RegexCharacterClassDelete(abClass);
  RegexCharacterClassDelete(bcClass);
  RegexCharacterClassDelete(abcClass);
  RegexDelete(regex);
  FADelete(nfa);
  FADelete(dfa);
  FADelete(minDFA);

  // 14. Regex to minimized DFA (2)
  Regex *fart = RegexFromConcat(3,
    RegexFromString("fart"),
    RegexZeroOrOne(RegexFromUnion(3,
      RegexFromLetter('y'),
      RegexFromString("ish"),
      RegexFromString("er"))),
    RegexZeroOrMore(REGEX_DIGITS));
  regex = RegexFromConcat(5,
    RegexFromString("https://"),
    fart,
    RegexFromUnion(2, RegexFromString(".com"), RegexFromString(".org")),
    RegexZeroOrMore(RegexFromConcat(2, fart, RegexFromLetter('/'))),
    RegexZeroOrOne(RegexFromConcat(2, fart, RegexFromString(".html"))));
  nfa = NFAFromRegex(regex);
  dfa = DFAFromNFA(nfa);
  minDFA = DFAMinimize(dfa);
  file = fopen("14.out", "w");
  FAPrint(minDFA, file);
  fclose(file);
  RegexDelete(regex);
  FADelete(nfa);
  FADelete(dfa);
  FADelete(minDFA);
}

void NFAsToDFATest() {
  // 15. Multiple disjoint regexes to minimized DFA
  Regex *regex = RegexFromUnion(3,
      RegexFromString("hello"),
      RegexFromString("hell"),
      RegexFromString("hey"));
  Regex *regex2 = RegexFromConcat(2,
      RegexFromString("fart"),
      RegexZeroOrMore(REGEX_DIGITS));
  FA *nfa = NFAFromRegex(regex);
  FA *nfa2 = NFAFromRegex(regex2);
  Vector *nfas = VectorNew();
  VectorAdd(nfas, nfa);
  VectorAdd(nfas, nfa2);
  FA *dfa = DFAFromNFAs(nfas);
  FA *minDFA = DFAMinimize(dfa);
  FILE *file = fopen("15.out", "w");
  FAPrint(minDFA, file);
  fclose(file);
  RegexDelete(regex);
  RegexDelete(regex2);
  VectorDelete(nfas);
  FADelete(nfa);
  FADelete(nfa2);
  FADelete(dfa);
  FADelete(minDFA);

  // 16. Multiple regexes to minimized DFA
  regex = RegexFromUnion(3,
      RegexFromString("hello"),
      RegexFromString("hell"),
      RegexFromString("hi"));
  regex2 = RegexFromConcat(2,
      RegexOneOrMore(REGEX_LETTERS),
      RegexZeroOrMore(RegexFromConcat(2,
          RegexFromLetter('.'),
          RegexOneOrMore(REGEX_LETTERS))));
  Regex *regex3 = RegexFromConcat(2,
      regex2,
      RegexFromUnion(2, RegexFromString(".com"), RegexFromString(".org")));
  nfa = NFAFromRegex(regex);
  nfa2 = NFAFromRegex(regex2);
  FA *nfa3 = NFAFromRegex(regex3);
  nfas = VectorNew();
  VectorAdd(nfas, nfa);
  VectorAdd(nfas, nfa3);
  VectorAdd(nfas, nfa2);
  dfa = DFAFromNFAs(nfas);
  minDFA = DFAMinimize(dfa);
  file = fopen("16.out", "w");
  FAPrint(minDFA, file);
  fclose(file);
  RegexDelete(regex);
  RegexDelete(regex3);
  VectorDelete(nfas);
  FADelete(nfa);
  FADelete(nfa2);
  FADelete(nfa3);
  FADelete(dfa);
  FADelete(minDFA);
}
