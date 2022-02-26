from sys import argv

if len(argv) < 2:
    raise ValueError("Please provide filename")

mem_auto, mem_hetero, bind = 0,0,0
for line in open(argv[1]).readlines():
    if "mem             mem            auto" in line:
        mem_auto = int(line.split("|")[1].split(":")[1].strip().split(" ")[0])
    elif "mem             mem          hetero" in line:
        mem_hetero = int(line.split("|")[1].split(":")[1].strip().split(" ")[0])
    elif "mem            bind          hetero" in line:
        bind = int(line.split("|")[1].split(":")[1].strip().split(" ")[0])

print("%d & %d & %d" % (mem_auto - 43, mem_hetero - 86, bind - 59))
