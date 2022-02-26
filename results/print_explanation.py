from sys import argv
from ast import literal_eval as make_tuple

if len(argv) < 2:
    raise ValueError("Please provide filename")

exp = False
indent = 0
output = []
for line in open(argv[1]).readlines():
    if line.startswith("Out:"):
        if "EXPLANATION" in line:
            exp = True
        elif exp:
            if "___" in line:
                continue

            timestep, sym, acc, time = line.split()[1:]
            sym = sym[1:-2]

            if sym == "+-->":
                indent += 1
            elif sym == "+--<":
                indent -= 1
            else:
                if sym != "NIL":
                    print(" " * indent, sym)
                #if indent == 0:
                #    #print(" " * indent, sym)
                #    output.append(sym)
                output.append(("   "*indent) + sym)

act = []
acts = []
state = 0
for sym in output:
    act.append(sym)
    if state == 0:
        state = 1
    elif state == 1:
        if ")" in sym or sym == "NIL":
            acts.append(tuple(act))
            act = []
            state = 0

for act in acts:
    print(" ".join(act))
