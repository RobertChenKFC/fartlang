#!/usr/bin/env python3

import sys

output_path = sys.argv[1]
targets = ",".join(f"'{target}'" for target in sys.argv[2:])
with open(output_path, "w") as outfile:
    outfile.write(f"targets=[{targets}]\n")
