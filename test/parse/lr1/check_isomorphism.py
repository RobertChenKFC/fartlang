#!/usr/bin/env python3

import sys
import xml.etree.ElementTree as ET
from argparse import ArgumentParser


def print_error_and_exit(msg):
    print(f"\033[1m\033[91mError\033[0m: {msg}", file=sys.stderr)
    exit(-1)


def parse_args():
    parser = ArgumentParser(
        description="Check that the LR(1) automatons generated by fartlang and "
                    "bison are isomorphic."
    )
    parser.add_argument(
        "fartlang_xml", help="Path to fartlang's output XML file."
    )
    parser.add_argument(
        "bison_xml", help="Path to bison's output XML file."
    )
    return parser.parse_args()


def print_symbol_string(symbol_string):
    return f"{symbol_string[0]} -> " + "".join(symbol_string[1:])


def rule_to_symbol_string(rule):
    for side in rule:
        if side.tag == "lhs":
            lhs = side.text
        else:
            rhs = []
            for symbol in side:
                if symbol.tag == "symbol":
                    rhs.append(symbol.text)
    symbol_string = (lhs, *rhs)
    return symbol_string


def get_rule_mapping(fartlang_rules, bison_rules):
    bison_rule_to_number = dict()
    for i, rule in enumerate(bison_rules):
        number = int(rule.attrib["number"])
        bison_rule_to_number[rule_to_symbol_string(rule)] = number

    fartlang_to_bison = [0] * len(fartlang_rules)
    fartlang_ruleset = []
    for i, rule in enumerate(fartlang_rules):
        number = int(rule.attrib["number"])
        symbol_string = rule_to_symbol_string(rule)
        if symbol_string not in bison_rule_to_number:
            print_error_and_exit(
                f"rule \"{print_symbol_string(symbol_string)}\" in fartlang is "
                f"not present in bison"
            )
        fartlang_to_bison[number] = bison_rule_to_number[symbol_string]
        fartlang_ruleset.append(symbol_string)

    for symbol_string in set(
        bison_rule_to_number.keys()
    ).difference(fartlang_ruleset):
        print_error_and_exit(
            f"rule \"{print_symbol_string(symbol_string)}\" in bison is "
            f"not present in fartlang"
        )

    return fartlang_to_bison


def get_grammar_and_automaton(xml):
    for child in xml:
        if child.tag == "grammar":
            grammar = child
        elif child.tag == "automaton":
            automaton = child
    return grammar, automaton


def get_rules(grammar):
    for child in grammar:
        if child.tag == "rules":
            return child


def state_to_itemset(state, to_bison_rule_no=None):
    for child in state:
        if child.tag == "itemset":
            itemset_xml = child
            break

    itemset = set()
    for item in itemset_xml:
        rule_no = int(item.attrib["rule-number"])
        if to_bison_rule_no is not None:
            rule_no = to_bison_rule_no[rule_no]
        dot = int(item.attrib["dot"])
        lookaheads = []
        for lookaheads_xml in item:
            if lookaheads_xml.tag == "lookaheads":
                for symbol in lookaheads_xml:
                    lookaheads.append(symbol.text)
        itemset.add((rule_no, dot, *sorted(lookaheads)))
    return frozenset(itemset)


def get_automaton_states_and_edges(automaton, to_bison_rule_no=None):
    states = dict()
    edges = dict()
    for state in automaton:
        state_no = int(state.attrib["number"])
        states[state_no] = state_to_itemset(
            state, to_bison_rule_no=to_bison_rule_no
        )
        edges.setdefault(state_no, dict())
        if state_no == 0:
            init = state_no
        for actions in state:
            if actions.tag == "actions":
                for child in actions:
                    if child.tag == "transitions":
                        for transition in child:
                            edges[state_no][
                                transition.attrib["symbol"]
                            ] = int(transition.attrib["state"])
    return states, edges


def get_printed_itemset(itemset):
    printed_itemset = ""
    for rule, dot, *lookaheads in itemset:
        printed_itemset += f"  rule {rule}; dot {dot}"
        if lookaheads:
            printed_itemset += "; lookaheads " + ", ".join(lookaheads)
        printed_itemset += "\n"
    return printed_itemset


def print_different_itemsets_and_exit(
    fartlang_state_no, fartlang_itemset,
    bison_state_no, bison_itemset,
    symbol=None, bison_parent_state_no=None, bison_parent_itemset=None
):
    if symbol is not None:
        error_msg = (
            f"parent state {bison_parent_state_no}:\n" +
            get_printed_itemset(bison_parent_itemset) +
            f"transitioned via symbol {symbol} yields differing states:\n"
        )
    else:
        error_msg = "differing initial states:\n\n"
    error_msg += (
        f"fartlang state {fartlang_state_no}:\n" +
        get_printed_itemset(fartlang_itemset)
    )
    error_msg += (
        f"bison state {bison_state_no}:\n" +
        get_printed_itemset(bison_itemset)
    )
    print_error_and_exit(error_msg)
    


def traverse_automata(
    fartlang_automaton, bison_automaton, fartlang_to_bison_rule_no
):
    fartlang_states, fartlang_edges = get_automaton_states_and_edges(
        fartlang_automaton, to_bison_rule_no=fartlang_to_bison_rule_no
    )
    bison_states, bison_edges = get_automaton_states_and_edges(bison_automaton)

    fartlang_to_bison_state_no = dict()
    visited = set()
    if fartlang_states[0] != bison_states[0]:
        print_different_itemsets_and_exit(
            0, fartlang_states[0], 0, bison_states[0]
        )
    fartlang_to_bison_state_no[0] = 0
    stack = [(0, 0, 0)]
    visited.add(0)

    while stack:
        bison_parent_state_no, fartlang_state_no, bison_state_no = stack.pop()
        visited.add(bison_state_no)

        for symbol, bison_next_state_no in bison_edges[bison_state_no].items():
            fartlang_next_state_no = fartlang_edges[fartlang_state_no][symbol]
            fartlang_next_itemset = fartlang_states[fartlang_next_state_no]
            bison_next_itemset = bison_states[bison_next_state_no]
            if fartlang_next_itemset != bison_next_itemset:
                print_different_itemsets_and_exit(
                    fartlang_next_state_no, fartlang_next_itemset,
                    bison_next_state_no, bison_next_itemset,
                    symbol=symbol, bison_parent_state_no=bison_state_no,
                    bison_parent_itemset=bison_states[bison_state_no]
                )
            if fartlang_next_state_no in fartlang_to_bison_state_no:
                mapped_bison_state_no = fartlang_to_bison_state_no[
                    fartlang_next_state_no
                ]
                if mapped_bison_state_no != bison_next_state_no:
                    print_error_and_exit(
                        f"fartlang state {fartlang_next_state_no} mapped to "
                        f"both bison state {mapped_bison_state_no} and "
                        f"bison state {bison_next_state_no}"
                    )
            else:
                fartlang_to_bison_state_no[
                    fartlang_next_state_no
                ] = bison_next_state_no
            if bison_next_state_no not in visited:
                visited.add(bison_next_state_no)
                stack.append((
                    bison_state_no, fartlang_next_state_no, bison_next_state_no
                ))


def main():
    args = parse_args()
    fartlang_xml = ET.parse(args.fartlang_xml).getroot()
    bison_xml = ET.parse(args.bison_xml).getroot()

    fartlang_grammar, fartlang_automaton = get_grammar_and_automaton(
        fartlang_xml
    )
    bison_grammar, bison_automaton = get_grammar_and_automaton(
        bison_xml
    )

    fartlang_rules = get_rules(fartlang_grammar)
    bison_rules = get_rules(bison_grammar)
    fartlang_to_bison_rule_no = get_rule_mapping(fartlang_rules, bison_rules)

    traverse_automata(
        fartlang_automaton, bison_automaton, fartlang_to_bison_rule_no
    )


if __name__ == "__main__":
    main()

    
