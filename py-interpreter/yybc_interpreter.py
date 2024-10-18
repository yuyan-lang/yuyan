
from yybc_ast import *


import pickle

import sys



instrs = []
strings = []
op = []
stack = [None] * 100000
stack_ptr = 0
functions = {}
labels = {}
files = {}

pc = 0
inst_count = 0

def start_interpret():
    global pc, stack_ptr, inst_count
    while True:
        inst_count += 1
        instr = instrs[pc]
        print(inst_count, end=": ")
        print(inst_to_text(instr))
        pc += 1
        match instr:
            case LoadParam(idx):
                op.append(stack[stack_ptr - 3 - idx])
            case LoadLocal(idx):
                op.append(stack[stack_ptr + idx])
            case StoreLocal(idx):
                val = op.pop()
                stack[stack_ptr + idx] = val
            case ReadTuple():
                idx = op.pop()
                tuple = op.pop()
                op.append(tuple[idx])
            case WriteTuple():
                new_val = op.pop()
                idx = op.pop()
                tuple = op.pop()
                tuple[idx] = new_val
            case MakeTuple(length):
                vals = [op.pop() for _ in range(length)]
                op.append(list(reversed(vals)))
            case CallFuncPtr(nargs, locals):
                func = op.pop()
                args = [op.pop() for _ in range(nargs)]
                for i in range(nargs):
                    stack[stack_ptr + locals + i] = args[i]
                stack[stack_ptr + locals + nargs] = stack_ptr
                stack[stack_ptr + locals + nargs + 1] = pc
                stack_ptr += locals + nargs + 2
                print("Calling", func, strings[func])
                pc = functions[func]
            case Return():
                val = op.pop()
                pc = stack[stack_ptr - 1]
                stack_ptr = stack[stack_ptr - 2]
                op.append(val)
            case ExternalCall(name, nargs):
                raise NotImplementedError()
            case IntConst(val):
                op.append(val)
            case StringConst(val):
                op.append(strings[val])
            case BranchIfFalse(target):
                raise NotImplementedError()
            case Branch(target):
                raise NotImplementedError()
            case Label(val):
                raise NotImplementedError()
            case BeginFunc(name):
                pass
            case EndFunc(name):
                raise ValueError("EndFunc")
            case PopOpStack():
                raise NotImplementedError()
            case BoolConst(val):
                op.append(val)
            case UnitConst():
                op.append([])
            case FuncRef(idx):
                op.append(idx)
            case UpdateFileRef(idx):
                files[idx] = op.pop()
            case FileRef(idx):
                op.append(files[idx])
            case _:
                raise ValueError("Unknown instruction", instr)





if __name__ == "__main__":
    filename = sys.argv[1]
    data = pickle.load(open(filename, "rb"))
    instrs = data["instrs"]
    strings = data["strings"]

    for i, instr in enumerate(instrs):
        if isinstance(instr, BeginFunc):
            functions[instr.name] = i
        if isinstance(instr, Label):
            labels[instr.int] = i

    start_interpret()

