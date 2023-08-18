#!/usr/bin/env python3

import argparse
import re


def parse_args():
  parser = argparse.ArgumentParser(
      description="Convert a bison grammar file into various different cfg "
                  "formats and remove useless rules and tokens"
  )
  parser.add_argument("bison_file", help="input bison grammar file")
  fmt = parser.add_mutually_exclusive_group(required=True)
  fmt.add_argument("--fart", dest="fmt", action="store_const", const="fart",
                   help="convert to fartlang cfg input format")
  fmt.add_argument("--ucalgary", dest="fmt", action="store_const",
                   const="ucalgary", help="convert to University of Calgary "
                                          "context free grammar checker input "
                                          "format")
  fmt.add_argument("--ellerre", dest="fmt", action="store_const",
                   const="ellerre", help="convert to Ellerre LR automata "
                                         "generator input format")
  fmt.add_argument("--jsmachines", dest="fmt", action="store_const",
                   const="jsmachines", help="convert to JSMachines LR(1) "
                                            "parser generator input format")
  fmt.add_argument("--bison", dest="fmt", action="store_const",
                   const="bison", help="convert back to bison grammar")
  parser.add_argument("test_num", type=int, help="number of test")
  args = parser.parse_args()
  return args


def to_camel_case(string):
  return "".join([x.title() for x in string.split("_")])


def to_upper_camel_case(string):
  string = to_camel_case(string)
  string = string[0].upper() + string[1:]
  return string


def to_lower_camel_case(string):
  string = to_camel_case(string)
  string = string[0].lower() + string[1:]
  return string


def remove_useless_tokens_and_rules(tokens, rules, start_variable):
  stack = [start_variable]
  useful_variables = set()
  useful_tokens = set()

  while stack:
    variable = stack.pop()
    useful_variables.add(variable)
    for rhs in rules[variable]:
      for x in rhs:
        if x in tokens:
          useful_tokens.add(x)
        elif x not in useful_variables:
          stack.append(x)

  tokens.clear()
  for token in useful_tokens:
    tokens.append(token)

  for variable in list(rules):
    if variable not in useful_variables:
      del rules[variable]


def read_bison_file(bison_file):
  with open(bison_file, "r") as infile:
    bison_grammar = infile.read()

  bison_grammar = re.sub("(//[^\n]*\n)|(/\\*([^/]|(/+[^/\\*]))*(/+)?\\*/)",
                         "", bison_grammar)
  bison_tokens, bison_grammar, _ = bison_grammar.split("%%")

  tokens = set()
  for line in bison_tokens.split("\n"):
    if line.startswith("%token"):
      for token in line.split()[1:]:
        tokens.add(token)
    elif line.startswith("%start"):
      start_variable = line.split()[1]

  rules = dict()
  for rule in bison_grammar.split(";")[:-1]:
    lhs, rhses = rule.split(":")
    lhs = to_upper_camel_case(re.sub("\\s", "", lhs))
    for rhs in rhses.split("|"):
      rules.setdefault(lhs, [])
      rhs = [to_lower_camel_case(x) if x in tokens else to_upper_camel_case(x)
             for x in rhs.split()]
      rules[lhs].append(rhs)

  tokens = [to_lower_camel_case(x) for x in tokens]
  start_variable = to_upper_camel_case(start_variable)
  remove_useless_tokens_and_rules(tokens, rules, start_variable)

  return tokens, rules, start_variable


def to_c_variable(string):
  keywords = {
    "int", "long", "float", "char", "for", "do", "while", "struct", "union",
    "enum", "if", "else", "goto", "return", "typedef", "extern", "void", "auto",
    "switch", "case", "static", "short", "alignof", "atomic", "restrict",
    "register", "bool", "double", "break", "signed", "unsigned", "sizeof",
    "inline", "const", "default", "volatile", "continue", "complex", "noreturn",
    "imaginary", "alignas"
  }
  result = string + "Keyword" if string in keywords else string
  return result


def fart(tokens, rules, start_variable, test_num):
  print(f"void test{test_num}() {{")

  print("  CFG *cfg = CFGNew();")
  print(f"  CFGSetNumTokens(cfg, {len(tokens)});")
  for i, token in enumerate(tokens):
    print(f"  int {to_c_variable(token)} = {i};")
  print()

  print(f"  int {to_c_variable(start_variable)} = CFGAddVariable(cfg);")
  for variable in rules:
    if variable == start_variable:
      continue
    print(f"  int {to_c_variable(variable)} = CFGAddVariable(cfg);")
  print()

  for variable in rules:
    for rhs in rules[variable]:
      print(f"  CFGAddRule(cfg, {to_c_variable(variable)}, {len(rhs)}", end="")
      for x in rhs:
        print(f", {to_c_variable(x)}", end="")
      print(");")
    print()

  print("  LR1StateGraph *graph = LR1StateGraphFromCFG(cfg);")
  print()

  print("  Vector *tokens = VectorNew();")
  for token in tokens:
    print(f"  VectorAdd(tokens, \"{token}\");")
  print()

  print("  Vector *variables = VectorNew();")
  print(f"  VectorAdd(variables, \"{start_variable}\");")
  for variable in rules:
    if variable == start_variable:
      continue
    print(f"  VectorAdd(variables, \"{variable}\");")
  print()

  print(f"  FILE *file = fopen(\"{test_num}.xml\", \"w\");")
  print("  LR1StateGraphPrintXML(cfg, graph, tokens, variables, file);")
  print("  fclose(file);")
  print("  VectorDelete(tokens);")
  print("  VectorDelete(variables);")
  print("  CFGDelete(cfg);")
  print("  LR1StateGraphDelete(graph);")

  print("}")


def ucalgary(tokens, rules, start_variable, test_num):
  def print_variable(variable):
    line = initial_line = f"{variable} ->"
    whitespace = " " * (len(initial_line) - 1) + "|"
    for rhs in rules[variable]:
      print(line, end="")
      for x in rhs:
        print(f" {x}", end="")
      print()
      line = whitespace
    print(" " * (len(initial_line) - 1) + ".")

  print_variable(start_variable)
  for variable in rules:
    if variable != start_variable:
      print_variable(variable)


def ellerre(tokens, rules, start_variable, test_num):
  def print_variable(variable):
    for rhs in rules[variable]:
      print(f"{variable} =>", end="")
      for x in rhs:
        print(f" {x}", end="")
      print(";", end="")

  print_variable(start_variable)
  for variable in rules:
    if variable != start_variable:
      print_variable(variable)
  print()


def jsmachines(tokens, rules, start_variable, test_num):
  def print_variable(variable):
    for rhs in rules[variable]:
      print(f"{variable} ->", end="")
      for x in rhs:
        print(f" {x}", end="")
      print()

  print_variable(start_variable)
  for variable in rules:
    if variable != start_variable:
      print_variable(variable)


def bison(tokens, rules, start_variable, test_num):
  print("%token " + " ".join(tokens))
  print("%define lr.type canonical-lr")
  print(f"%start {start_variable}")
  print("%define lr.keep-unreachable-state true")

  print(f"%%")

  for variable in rules:
    print(variable)
    for i, rhs in enumerate(rules[variable]):
      if i == 0:
        print("  :", end="")
      else:
        print("  |", end="")
      for x in rhs:
        print(f" {x}", end="")
      print()
    print("  ;")

  print(f"%%")

def main():
  args = parse_args()
  tokens, rules, start_variable = read_bison_file(args.bison_file)
  functions = {
    "fart": fart,
    "ucalgary": ucalgary,
    "ellerre": ellerre,
    "jsmachines": jsmachines,
    "bison": bison,
  }
  functions[args.fmt](tokens, rules, start_variable, args.test_num)


if __name__ == "__main__":
  main()


