from __future__ import annotations
# this is an experiment project to understand performance bottlenecks

# my plan is to directly compile the type checking result json file


import sys
import os
import json
from ast_util import *
from datetime import datetime
from dataclasses import dataclass
from files_manager import *
sys.setrecursionlimit(1000000000)

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


def compile_rec_struct(stack: Stack, proj_labels: List[str], ast: Abt):
    """
    The reference of stack is always on top.

    """
    rec_struct = stack.access(1)
    assert isinstance(rec_struct, ArrayVal)
    assert len(rec_struct.elems) == len(proj_labels)
    match ast:
        case N(NT_StructEntry(label), [cur, next]):
            val = compile_ast(stack.hint_structure(label), cur)
            if label is not None:
                rec_struct.elems[proj_labels.index(label)] = val
            compile_rec_struct(stack, proj_labels, next)
            return
        case N(NT_EmptyStructEntry(), []):
            return
        case _:
            raise ValueError(f"Unknown compileation {ast}")


# @after_compile_ast_decorator
def compile_func_call(stack: Stack, func: Abt, args: List[Value]):
    if len(args) == 0:
        return compile_ast(stack, func)
    else:
        match func:
            case Binding(_, next):
                return compile_func_call(stack.push(args[0]), next, args[1:])
            case _:
                raise ValueError(f"Expected binding, got {func}")
    

# @after_compile_ast_decorator
def compile_ast(stack: Stack, ast: Abt) -> Value:
    match ast:
        case BoundVar(idx):
            return stack.access(idx)
        case N(NT_StructRec(proj_labels), [Binding(_, next)]):
            struct_val = ArrayVal([None] * len(proj_labels))
            compile_rec_struct(stack.push(struct_val), proj_labels, next)
            return struct_val
        case N(NT_FileRef(filename), []):
            path = file_path_to_key(filename)
            if path in GLOBAL_FILE_REFS:
                return GLOBAL_FILE_REFS[path]
            else:
                raise ValueError(f"Unknown file reference {filename}")
        case N(NT_EmptyVal(), []):
            return EmptyVal
        case N(NT_Builtin(name), args):
            args_val = [compile_ast(stack, arg) for arg in args]
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
            val = compile_ast(stack, arg)
            if isinstance(val, ArrayVal):
                return val.elems[idx]
            else:
                raise ValueError(f"Expected array value, got {val}")
        case N(NT_AnnotatedVar(_), [arg]):
            return compile_ast(stack, arg)
        case N(NT_MultiArgLam(arg_count), _):
            return FuncVal(stack, ast)
        case N(NT_TupleCons(), args):
            vals = [compile_ast(stack, arg) for arg in args]
            return ArrayVal(vals)
        case N(NT_DataTupleCons(idx, length), [arg]):
            val = compile_ast(stack, arg)
            if isinstance(val, ArrayVal):
                assert len(val.elems) == length
                return DataTupleVal(idx, val)
            else:
                raise ValueError(f"Expected array value, got {val}")
        case N(NT_MultiArgFuncCall(arg_count), [func, *args]):
            assert arg_count == len(args)
            func_val = compile_ast(stack, func)
            if isinstance(func_val, FuncVal):
                match func_val.body:
                    case N(NT_MultiArgLam(lam_arg_count), [body]):
                        assert lam_arg_count == arg_count
                        args_val = [compile_ast(stack, arg) for arg in args]
                        return compile_func_call(func_val.env, body, args_val)
                    case _:
                        raise ValueError(f"Expected multi arg lambda, got {func_val.body}")
            else:
                raise ValueError(f"Expected function value, got {func_val}")
        case N(NT_IfThenElse(), [cond, then_branch, else_branch]):
            cond_val = compile_ast(stack, cond)
            if isinstance(cond_val, BoolVal):
                if cond_val.val:
                    return compile_ast(stack, then_branch)
                else:
                    return compile_ast(stack, else_branch)
            else:
                raise ValueError(f"Expected bool value, got {cond_val}")
        case N(NT_StringConst(val), []):
            return StrVal(val)
        case N(NT_IntConst(val), []):
            return IntVal(val)
        case N(NT_ExternalCall(name), args):
            args_val = [compile_ast(stack, arg) for arg in args]
            return do_external_call(name, args_val)
        case N(NT_LetIn(), [cur, Binding(_, next)]):
            val = compile_ast(stack, cur)
            return compile_ast(stack.push(val), next)
        case N(NT_ConsecutiveStmt(), [cur, next]):
            compile_ast(stack, cur)
            return compile_ast(stack, next)
        case N(NT_DataTupleProjIdx(), [arg]):
            val = compile_ast(stack, arg)
            if isinstance(val, DataTupleVal):
                return IntVal(val.idx)
            else:
                raise ValueError(f"Expected data tuple value, got {val}")
        case N(NT_DataTupleProjTuple(), [arg]):
            val = compile_ast(stack, arg)
            if isinstance(val, DataTupleVal):
                return val.elem
            else:
                raise ValueError(f"Expected data tuple value, got {val}")
        case N(NT_CallCC(), [Binding(_, next)]):
            global CurrentExceptionHandler
            old_handler = CurrentExceptionHandler
            cc_handler = FuncVal(stack.push(old_handler), N(NT_MultiArgLam(1), [Binding("cc_val", N(NT_ExternalCall("yyExceptCallCC"), [BoundVar(1), BoundVar(2)]))]))
            try:
                return compile_ast(stack.push(cc_handler), next)
            except CCException as e:
                return e.val
        case N(NT_CallCCRet(), [cc, ret_val]):
            return compile_ast(stack, N(NT_MultiArgFuncCall(1), [cc, ret_val]))
        case N(NT_GlobalFuncRef(name), []):
            if name in GLOBAL_FUNC_REFS:
                return GLOBAL_FUNC_REFS[name]
            else:
                raise ValueError(f"Unknown function reference {name}")
        case N(NT_GlobalFuncDecl(name), [v]):
            GLOBAL_FUNC_REFS[name] = FuncVal(Stack(), v)
            return EmptyVal
        case _:
            raise ValueError(f"Unknown compileation {ast}")
    # raise ValueError(f"??? Unknown compileation {ast}")


def do_compile_files():

    for path in ORDERED_FILES:
        print(f"compileing {path}")
        ast = GLOBAL_INPUT_ASTS[path]
        match ast:
            case N(NT_StructRec(proj_labels), [Binding(_, next)]):
                struct_val = ArrayVal([None] * len(proj_labels))
                GLOBAL_FILE_REFS[path] = struct_val
                compile_rec_struct(Stack([struct_val], [path]), proj_labels, next)
            case _:
                raise ValueError(f"Unknown compileation {ast}")
        print(f"compileed {path}")



if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python compiler.py <path_no_extension> <args>")
        sys.exit(1)
    path = sys.argv[1]
    do_load_files(path)
    do_compile_files()


    
