from __future__ import annotations
# this is an experiment project to understand performance bottlenecks

# my plan is to directly interpret the type checking result json file


import sys
import os
import json
from ast_util import *
from datetime import datetime
from dataclasses import dataclass
sys.setrecursionlimit(1000000000)
BASE_PATH = "./.yybuild.v0.1.0rc2+0019.nosync"
PATH_SUFFIX = ".擦除后形式.json"

STACK = []

@dataclass
class ArrayVal:
    elems: List[Value]

    def __post_init__(self):
        assert all(elem == None or isinstance(elem, Value) for elem in self.elems)

@dataclass
class BoolVal:
    val: bool

@dataclass
class IntVal:
    val: int

@dataclass
class StrVal:
    val: str

@dataclass
class FuncVal:
    env: Stack
    body: Abt

@dataclass
class DataTupleVal:
    idx: int
    elem: ArrayVal

Value = ArrayVal | BoolVal | IntVal | StrVal | FuncVal  | DataTupleVal
EmptyVal = ArrayVal([])
GLOBAL_FILE_REFS = {}
GLOBAL_FUNC_REFS = {}
GLOBAL_ASTS = {}

class Stack:
    def __init__(self, init=[], struct=[]):
        self.stack = init
        self.structure = struct
    
    def push(self, val):
        assert isinstance(val, Value)
        return Stack(self.stack + [val], self.structure + [])

    def hint_structure(self, proj_label):
        return Stack(self.stack + [], self.structure + [proj_label])


    def access(self, idx):
        assert 0 < idx <= len(self.stack)
        return self.stack[-idx]

    def __repr__(self):
        return ".".join(self.structure) + ": " + ", ".join(str(val) for val in self.stack)


CurrentExceptionHandler = FuncVal(Stack([]), N(NT_MultiArgLam(1),[Binding("except_name", N(NT_ExternalCall("yyGlobalExcept"), [BoundVar(0)]))]))
class CCException(Exception):
    def __init__(self, val):
        self.val = val
    
def after_interpret_ast_decorator(func):
    def wrapper(*args):
        result = func(*args)
        assert isinstance(result, Value)
        return result
    return wrapper

def do_external_call(name: str, args: List[Value]):
    global CurrentExceptionHandler
    match name, args:
        case "yyRunningOnWindows", []:
            return BoolVal(os.name == "nt")
        case "yyNewRef", [v]:
            return ArrayVal([v])
        case "yyReadRef", [ArrayVal([v])]:
            return v
        case "yyWriteRef", [v, ArrayVal([_])]:
            args[1].elems[0] = v
            return EmptyVal
        case "yyIntAdd", [IntVal(a), IntVal(b)]:
            return IntVal(a + b)
        case "yyStringEq", [StrVal(a), StrVal(b)]:
            return BoolVal(a == b)
        case "yyNewRefArrayGeneric", [IntVal(l)]:
            return ArrayVal([None] * l)
        case "yyWriteRefArray", [v, IntVal(idx), ArrayVal(arr)]:
            arr[idx] = v
            return EmptyVal
        case "yyReadRefArray", [ArrayVal(arr), IntVal(idx)]:
            return arr[idx]
        case "yyCodePointsConcat", [ArrayVal([ArrayVal(elems), IntVal(l)])]:
            assert len(elems) == l
            assert all(isinstance(elem, StrVal) for elem in elems)
            return StrVal("".join(s.val for s in elems))
        case "yyGetCurrentWorkingDirectory", []:
            return StrVal(os.getcwd())
        case "yyGetCodePoints", [StrVal(s)]:
            res = [StrVal(c) for c in s]
            return ArrayVal([ArrayVal(res), IntVal(len(res))])
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


def interpret_rec_struct(stack: Stack, proj_labels: List[str], ast: Abt):
    """
    The reference of stack is always on top.

    """
    rec_struct = stack.access(1)
    assert isinstance(rec_struct, ArrayVal)
    assert len(rec_struct.elems) == len(proj_labels)
    match ast:
        case N(NT_StructEntry(label), [cur, next]):
            val = interpret_ast(stack.hint_structure(label), cur)
            if label is not None:
                rec_struct.elems[proj_labels.index(label)] = val
            interpret_rec_struct(stack, proj_labels, next)
            return
        case N(NT_EmptyStructEntry(), []):
            return
        case _:
            raise ValueError(f"Unknown interpretation {ast}")


# @after_interpret_ast_decorator
def interpret_func_call(stack: Stack, func: Abt, args: List[Value]):
    if len(args) == 0:
        return interpret_ast(stack, func)
    else:
        match func:
            case Binding(_, next):
                return interpret_func_call(stack.push(args[0]), next, args[1:])
            case _:
                raise ValueError(f"Expected binding, got {func}")
    

# @after_interpret_ast_decorator
def interpret_ast(stack: Stack, ast: Abt):
    match ast:
        case BoundVar(idx):
            return stack.access(idx)
        case N(NT_StructRec(proj_labels), [Binding(_, next)]):
            struct_val = ArrayVal([None] * len(proj_labels))
            interpret_rec_struct(stack.push(struct_val), proj_labels, next)
            return struct_val
        case N(NT_FileRef(filename), []):
            return do_interprete_file(filename)
        case N(NT_EmptyVal(), []):
            return EmptyVal
        case N(NT_Builtin(name), args):
            args_val = [interpret_ast(stack, arg) for arg in args]
            match name, args_val:
                case "内建爻阳", []:
                    return BoolVal(True)
                case "内建爻阴", []:
                    return BoolVal(False)
                case "内建有元", []:
                    return EmptyVal
                case "内建函数整数相等", [IntVal(a), IntVal(b)]:
                    return BoolVal(a == b)
                case "内建函数整数减", [IntVal(a), IntVal(b)]:
                    return IntVal(a - b)
                case "内建函数整数大于", [IntVal(a), IntVal(b)]:
                    return BoolVal(a > b)
                case _:
                    raise ValueError(f"Unknown builtin {name} on {args_val}")
        case N(NTTupleProj(idx), [arg]):
            val = interpret_ast(stack, arg)
            if isinstance(val, ArrayVal):
                return val.elems[idx]
            else:
                raise ValueError(f"Expected array value, got {val}")
        case N(NT_AnnotatedVar(_), [arg]):
            return interpret_ast(stack, arg)
        case N(NT_MultiArgLam(arg_count), _):
            return FuncVal(stack, ast)
        case N(NT_TupleCons(), args):
            vals = [interpret_ast(stack, arg) for arg in args]
            return ArrayVal(vals)
        case N(NT_DataTupleCons(idx, length), [arg]):
            val = interpret_ast(stack, arg)
            if isinstance(val, ArrayVal):
                assert len(val.elems) == length
                return DataTupleVal(idx, val)
            else:
                raise ValueError(f"Expected array value, got {val}")
        case N(NT_MultiArgFuncCall(arg_count), [func, *args]):
            assert arg_count == len(args)
            func_val = interpret_ast(stack, func)
            if isinstance(func_val, FuncVal):
                match func_val.body:
                    case N(NT_MultiArgLam(lam_arg_count), [body]):
                        assert lam_arg_count == arg_count
                        args_val = [interpret_ast(stack, arg) for arg in args]
                        return interpret_func_call(func_val.env, body, args_val)
                    case _:
                        raise ValueError(f"Expected multi arg lambda, got {func_val.body}")
        case N(NT_IfThenElse(), [cond, then_branch, else_branch]):
            cond_val = interpret_ast(stack, cond)
            if isinstance(cond_val, BoolVal):
                if cond_val.val:
                    return interpret_ast(stack, then_branch)
                else:
                    return interpret_ast(stack, else_branch)
            else:
                raise ValueError(f"Expected bool value, got {cond_val}")
        case N(NT_StringConst(val), []):
            return StrVal(val)
        case N(NT_IntConst(val), []):
            return IntVal(val)
        case N(NT_ExternalCall(name), args):
            args_val = [interpret_ast(stack, arg) for arg in args]
            return do_external_call(name, args_val)
        case N(NT_LetIn(), [cur, Binding(_, next)]):
            val = interpret_ast(stack, cur)
            return interpret_ast(stack.push(val), next)
        case N(NT_DataTupleProjIdx(), [arg]):
            val = interpret_ast(stack, arg)
            if isinstance(val, DataTupleVal):
                return IntVal(val.idx)
            else:
                raise ValueError(f"Expected data tuple value, got {val}")
        case N(NT_DataTupleProjTuple(), [arg]):
            val = interpret_ast(stack, arg)
            if isinstance(val, DataTupleVal):
                return val.elem
            else:
                raise ValueError(f"Expected data tuple value, got {val}")
        case N(NT_CallCC(), [Binding(_, next)]):
            global CurrentExceptionHandler
            old_handler = CurrentExceptionHandler
            cc_handler = FuncVal(stack.push(old_handler), N(NT_MultiArgLam(1), [Binding("cc_val", N(NT_ExternalCall("yyExceptCallCC"), [BoundVar(1), BoundVar(2)]))]))
            try:
                return interpret_ast(stack.push(cc_handler), next)
            except CCException as e:
                return e.val
        case N(NT_CallCCRet(), [cc, ret_val]):
            return interpret_ast(stack, N(NT_MultiArgFuncCall(1), [cc, ret_val]))
        case N(NT_FuncRef(name), []):
            if name in GLOBAL_FUNC_REFS:
                return GLOBAL_FUNC_REFS[name]
            else:
                raise ValueError(f"Unknown function reference {name}")
        case _:
            raise ValueError(f"Unknown interpretation {ast}")

def file_path_to_key(path):
    # if path begins with cwd (absolute), remove it
    cwd = os.getcwd()
    if path.startswith(cwd):
        # Remove cwd from path and return the rest
        path = path[len(cwd):].lstrip(os.sep)
    # if path ends with 。豫, remove it
    if path.endswith("。豫"):
        path = path[:-len("。豫")]
    return path


def do_interprete_file(path):
    path = file_path_to_key(path)
    if path in GLOBAL_FILE_REFS:
        return GLOBAL_FILE_REFS[path]
    print(f"Interpreting {path}")
    ast = GLOBAL_ASTS[path]
    result = interpret_ast(Stack([], [path]), ast)
    GLOBAL_FILE_REFS[path] = result
    print(f"Interpreted {path}")
    return result

def do_load_files(path):
    path = file_path_to_key(path)
    if path in GLOBAL_ASTS:
        return
    print(f"Loading {path}")
    path = file_path_to_key(path)
    json_path = f"{BASE_PATH}/{path}{PATH_SUFFIX}"
    with open(json_path, "r") as f:
        json_data = json.load(f)
        ast = decode_json_to_ast(json_data)
    GLOBAL_ASTS[path] = ast
    for dep in find_all_file_refs(ast):
        do_load_files(dep)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python interpreter.py <path_no_extension> <args>")
        sys.exit(1)
    path = sys.argv[1]
    do_load_files(path)
    do_interprete_file(path)


    
