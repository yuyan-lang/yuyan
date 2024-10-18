
from yybc_ast import *


import pickle

import sys
import os


def do_external_call(name: str, args: List[Any]):
    global CurrentExceptionHandler
    match name, args:
        case "yyRunningOnWindows", []:
            return os.name == "nt"
        case "yyIntEqTest", [a,b]:
            return a == b
        case "yyNewRef", [v]:
            return [v]
        case "yyReadRef", [[v]]:
            return v
        case "yyWriteRef", [v, arr]:
            arr[0] = v
            return []
        case "yyIntAdd", [a,b]:
            return a + b
        case "yyStringEq", [a,b]:
            return a == b
        case "yyNewRefArrayGeneric", [l]:
            return [None] * l
        case "yyWriteRefArray", [v, idx, arr]:
            arr[idx] = v
            return []
        case "yyReadRefArray", [arr, idx]:
            return arr[idx]
        case "yyCodePointsConcat", [[elems, l]]:
            assert len(elems) == l
            return "".join(s for s in elems)
        case "yyGetCurrentWorkingDirectory", []:
            return os.getcwd()
        case "yyGetCodePoints", [s]:
            res = [c for c in s]
            return [res, len(res)]
        case "yyGetCommandLineArgs", []:
            res = [StrVal(arg) for arg in sys.argv[2:]]
            return ArrayVal([ArrayVal(res), IntVal(len(res))])
        case "yyGetCurrentLocalDateTimeFmt", [StrVal(fmt)]:
            return StrVal(datetime.now().strftime(fmt))
        case "yyPrintlnStdErr", [StrVal(s)]:
            print(s, file=sys.stderr)
            return EmptyVal
        case "获取当前异常处理器", []:
            return CurrentExceptionHandler
        case "设置当前异常处理器", [v]:
            CurrentExceptionHandler = v
            return EmptyVal
        case "yyExceptCallCC", [val, old_handler]:
            CurrentExceptionHandler = old_handler
            raise CCException(val)
        case "yyGlobalExcept", [StrVal(name)]:
            print(f"Unhandled exception: {name}", file=sys.stderr)
            exit(1)
        case "yyIsSubstring", [StrVal(sub), StrVal(s)]:
            return BoolVal(sub in s)
        case "yyPrintln", [StrVal(s)]:
            print(s)
            return EmptyVal
        case "yyPathExists", [StrVal(path)]:
            return BoolVal(os.path.exists(path))
        case "yyGetFileModifiedTime", [StrVal(path)]:
            return IntVal(int(os.path.getmtime(path)))
        case "yyReadFileSync", [StrVal(path)]:
            with open(path, "r") as f:
                return StrVal(f.read())
        case "yy_豫言字符串获取字节数组", [StrVal(s)]:
            return args[0]
        case "yyStringByteArrayGetLength", [StrVal(s)]:
            return IntVal(len(s))
        case "yy_豫言字符串获取字节序数当前字符", [StrVal(s), IntVal(idx)]:
            return StrVal(s[idx])
        case "yy_豫言字符串匹配", [StrVal(s), IntVal(startIdx), StrVal(pattern)]:
            return BoolVal(s.find(pattern, startIdx, startIdx+len(pattern)+1) == startIdx)
        case "yyIsPathRegularFile", [StrVal(path)]:
            return BoolVal(os.path.isfile(path))
        case "yyIsPathDirectory", [StrVal(path)]:
            return BoolVal(os.path.isdir(path))
        case "yyWriteFileSync", [StrVal(path), StrVal(content)]:
            with open(path, "w") as f:
                f.write(content)
            return EmptyVal
        case "yyProcessExit", [IntVal(code)]:
            exit(code)
        case _:
            raise ValueError(f"Unknown external call {name} on {args}")


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
        if inst_count >= 43520:
            a = 1
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
                    stack[stack_ptr + locals + i] = args[-i-1]
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
                args = [op.pop() for _ in range(nargs)]
                call_name = strings[name]
                op.append(do_external_call(call_name, args))
            case IntConst(val):
                op.append(val)
            case StringConst(val):
                op.append(strings[val])
            case BranchIfFalse(target):
                val = op.pop()
                if not val:
                    pc = labels[target]
            case Branch(target):
                pc = labels[target]
            case Label(val):
                pass
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

