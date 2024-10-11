from __future__ import annotations
# this is an experiment project to understand performance bottlenecks

# my plan is to directly compile the type checking result json file


import resource, sys
resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY,resource.RLIM_INFINITY))
sys.setrecursionlimit(10**6)
import os
import json
from ast_util import *
from datetime import datetime
from dataclasses import dataclass
from yuyan_import import *
from closure_convert import *
from anf_convert import *
from recursion_rewrite import *
print("recursion limit", sys.getrecursionlimit())   

def compile_immediate(immediate: Abt) -> str:
    match immediate:
        case _:
            raise ValueError(f"Unknown immediate {immediate}")

# @after_compile_ast_decorator
def compile_ast(ast: Abt) -> str:
    match ast:
        case BoundVar(idx):
            raise ValueError(f"Unbound variable {idx}")
        case FreeVar(v):
            return v
        case N(NT_ConsecutiveStmt(), [cur, next]):
            return compile_immediate(cur) + ";\n" + compile_ast(next)
        case N(NT_FileRef(filename), []):
            return filename + "_ref"
        case N(NT_EmptyVal(), []):
            return "0"
        case N(NT_Builtin(name), args):
            args_ast = [compile_ast( arg) for arg in args]
            match name, args_ast:
                case "内建爻阳", []:
                    return "1"
                case "内建爻阴", []:
                    return "0"
                case "内建有元", []:
                    return "1"
                case "内建函数整数相等", [arg1, arg2]:
                    return "yyIntEq(" + arg1 + " == " + arg2 + ")"
                case "内建函数整数减", [arg1, arg2]:
                    return "yyIntSub(" + arg1 + " - " + arg2 + ")"
                case "内建函数整数大于", [arg1, arg2]:
                    return "yyIntGt(" + arg1 + " > " + arg2 + ")"
                case _:
                    raise ValueError(f"Unknown builtin {name} on {args_val}")
        case _:
            raise ValueError(f"Unknown compile ast {ast}")
        # case N(NT_TupleProj(idx), [arg]):
            
        #     val = compile_ast(stack, arg)
        #     if isinstance(val, ArrayVal):
        #         return val.elems[idx]
        #     else:
        #         raise ValueError(f"Expected array value, got {val}")
        # case N(NT_AnnotatedVar(_), [arg]):
        #     return compile_ast(stack, arg)
        # case N(NT_MultiArgLam(arg_count), _):
        #     return FuncVal(stack, ast)
        # case N(NT_TupleCons(), args):
        #     vals = [compile_ast(stack, arg) for arg in args]
        #     return ArrayVal(vals)
        # case N(NT_DataTupleCons(idx, length), [arg]):
        #     val = compile_ast(stack, arg)
        #     if isinstance(val, ArrayVal):
        #         assert len(val.elems) == length
        #         return DataTupleVal(idx, val)
        #     else:
        #         raise ValueError(f"Expected array value, got {val}")
        # case N(NT_MultiArgFuncCall(arg_count), [func, *args]):
        #     assert arg_count == len(args)
        #     func_val = compile_ast(stack, func)
        #     if isinstance(func_val, FuncVal):
        #         match func_val.body:
        #             case N(NT_MultiArgLam(lam_arg_count), [body]):
        #                 assert lam_arg_count == arg_count
        #                 args_val = [compile_ast(stack, arg) for arg in args]
        #                 return compile_func_call(func_val.env, body, args_val)
        #             case _:
        #                 raise ValueError(f"Expected multi arg lambda, got {func_val.body}")
        #     else:
        #         raise ValueError(f"Expected function value, got {func_val}")
        # case N(NT_IfThenElse(), [cond, then_branch, else_branch]):
        #     cond_val = compile_ast(stack, cond)
        #     if isinstance(cond_val, BoolVal):
        #         if cond_val.val:
        #             return compile_ast(stack, then_branch)
        #         else:
        #             return compile_ast(stack, else_branch)
        #     else:
        #         raise ValueError(f"Expected bool value, got {cond_val}")
        # case N(NT_StringConst(val), []):
        #     return StrVal(val)
        # case N(NT_IntConst(val), []):
        #     return IntVal(val)
        # case N(NT_ExternalCall(name), args):
        #     args_val = [compile_ast(stack, arg) for arg in args]
        #     return do_external_call(name, args_val)
        # case N(NT_LetIn(), [cur, Binding(_, next)]):
        #     val = compile_ast(stack, cur)
        #     return compile_ast(stack.push(val), next)
        # case N(NT_ConsecutiveStmt(), [cur, next]):
        #     compile_ast(stack, cur)
        #     return compile_ast(stack, next)
        # case N(NT_DataTupleProjIdx(), [arg]):
        #     val = compile_ast(stack, arg)
        #     if isinstance(val, DataTupleVal):
        #         return IntVal(val.idx)
        #     else:
        #         raise ValueError(f"Expected data tuple value, got {val}")
        # case N(NT_DataTupleProjTuple(), [arg]):
        #     val = compile_ast(stack, arg)
        #     if isinstance(val, DataTupleVal):
        #         return val.elem
        #     else:
        #         raise ValueError(f"Expected data tuple value, got {val}")
        # case N(NT_CallCC(), [Binding(_, next)]):
        #     global CurrentExceptionHandler
        #     old_handler = CurrentExceptionHandler
        #     cc_handler = FuncVal(stack.push(old_handler), N(NT_MultiArgLam(1), [Binding("cc_val", N(NT_ExternalCall("yyExceptCallCC"), [BoundVar(1), BoundVar(2)]))]))
        #     try:
        #         return compile_ast(stack.push(cc_handler), next)
        #     except CCException as e:
        #         return e.val
        # case N(NT_CallCCRet(), [cc, ret_val]):
        #     return compile_ast(stack, N(NT_MultiArgFuncCall(1), [cc, ret_val]))
        # case N(NT_GlobalFuncRef(name), []):
        #     if name in GLOBAL_FUNC_REFS:
        #         return GLOBAL_FUNC_REFS[name]
        #     else:
        #         raise ValueError(f"Unknown function reference {name}")
        # case N(NT_GlobalFuncDecl(name), [v]):
        #     GLOBAL_FUNC_REFS[name] = FuncVal(Stack(), v)
        #     return EmptyVal
        # case _:
        #     raise ValueError(f"Unknown compileation {ast}")
    # raise ValueError(f"??? Unknown compileation {ast}")

def do_compile_func(name: str, func: Abt) -> List[str]:
    result = []
    match func:
        case N(NT_MultiArgLam(arg_count), [body]):
            arg_names, real_body = unbind_abt_list(body, arg_count)
            result.append("yyvalue " + name + "(" + ", ".join([f"yyvalue {arg}" for arg in arg_names]) + ") {")
            result.append(compile_ast(real_body))
            result.append("}")
            return result
        case _:
            raise ValueError(f"Expected multi arg lambda, got {func}")
    

def do_compile_funcs(func_dict):
    lines = []
    for name, func in func_dict.items():
        lines.extend(do_compile_func(name, func))
    with open("output.c", "w") as f:
        f.write("\n".join(lines))

    



if __name__ == "__main__":
    os.makedirs(".yybuild.nosync/py/", exist_ok=True)
    if len(sys.argv) < 2:
        print("Usage: python compiler.py <path_no_extension> <args>")
        sys.exit(1)
    path = sys.argv[1]
    asts = do_load_files(path)
    converted = closure_convert_top_level(asts)
    rewritten = recursion_rewrite_top_level(converted)
    anf = anf_convert_top_level(rewritten)
    do_compile_funcs(anf)


    
