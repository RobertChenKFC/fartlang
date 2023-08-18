#!/usr/bin/env python3

from argparse import ArgumentParser
import re


def get_bison_num_states(bison_file):
    with open(bison_file, "r") as infile:
        bison_file = infile.read().split("\n")

    state_num = -1
    for line in bison_file:
        match = re.search(r"State (\d+)", line)
        if match:
            state_num = max(state_num, int(match.group(1)))

    return state_num + 1


def parse_args():
    parser = ArgumentParser(
        description="Extract number of states from bison output file and store "
                    "it in another file."
    )
    parser.add_argument("bison_file", help="Path to bison output file.")
    parser.add_argument("output_file", help="Path to output file.")
    return parser.parse_args()


def main():
    args = parse_args()
    num_states = get_bison_num_states(args.bison_file)
    with open(args.output_file, "w") as outfile:
        outfile.write(f"{num_states}\n")


if __name__ == "__main__":
    main()

