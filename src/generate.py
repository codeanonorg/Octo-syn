"""
  A script to generate the alphanumerical subsets of x86_64 assembly
"""
from capstone import *
import json


"""
  Alphanumerical characters
"""
ALPHANUM = [chr(i + ord('a')) for i in range(26)] + \
    [chr(i + ord('A')) for i in range(26)] + \
    [chr(i + ord('0')) for i in range(10)]


def generate_comb(n):
    """
      Generate every words of size n composed of alphanumerical characters
    """
    if n == 1:
        return ALPHANUM
    else:
        comb = generate_comb(n-1)
        return [alpha + c for c in comb for alpha in ALPHANUM]


def disasm(code):
    """
      Disassemble a given bytes array 
    """
    md = Cs(CS_ARCH_X86, CS_MODE_64)
    asm = []
    for i in md.disasm(code, 0x1000):
        asm.append(f"{i.mnemonic} {i.op_str}")
    return asm


D = dict()
for e in generate_comb(2):
    asm = disasm(e.encode())
    if (len(asm) > 0):
        D[e] = asm

with open("./data.json", "w") as f:
    f.write(json.dumps(D, indent=2, sort_keys=True))
