from __future__ import annotations
# this is an experiment project to understand performance bottlenecks

# my plan is to directly compile the type checking result json file


import resource, sys
resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY,resource.RLIM_INFINITY))
sys.setrecursionlimit(10**9)
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

def compile_immediate(immediate: Abt, store_result: str) -> List[str]:
    match immediate:
        case N(NT_EmptyVal(), []):
            return [f"yyvalue {store_result} = unit_to_yyvalue();"]
        case N(NT_IntConst(val), []):
            return [f"yyvalue {store_result} = int_to_yyvalue({val});"]
        case N(NT_DecimalNumber(integral, fractional), []):
            return [f"yyvalue {store_result} = double_to_yyvalue({integral}.{fractional});"]
        case N(NT_StringConst(val), []):
            return [f"yyvalue {store_result} = string_to_yyvalue(\"{val}\");"]
        case N(NT_TupleCons(), args):
            return [f"yyvalue {store_result} = tuple_to_yyvalue({{{len(args)}, {', '.join([f"{arg}" for arg in args])}}});"]
        case N(NT_TupleProj(idx), [arg]):
            return [f"yyvalue {store_result} = yyvalue_to_tuple({arg})[yyvalue_to_int({idx})];"]
        case N(NT_Builtin(name), args):
            match name, args:
                case "内建爻阳", []:
                    return [f"yyvalue {store_result} = yyvalue_to_bool(true);"]
                case "内建爻阴", []:
                    return [f"yyvalue {store_result} = yyvalue_to_bool(false);"]
                case "内建有元", []:
                    return [f"yyvalue {store_result} = unit_to_yyvalue();"]
                case "内建函数整数相等", [arg1, arg2]:
                    return [f"yyvalue {store_result} = bool_to_yyvalue(yyvalue_to_int({arg1}) == yyvalue_to_int({arg2}));"]
                case "内建函数整数减", [arg1, arg2]:
                    return [f"yyvalue {store_result} = int_to_yyvalue(yyvalue_to_int({arg1}) - yyvalue_to_int({arg2}));"]
                case "内建函数整数大于", [arg1, arg2]:
                    return [f"yyvalue {store_result} = bool_to_yyvalue(yyvalue_to_int({arg1}) > yyvalue_to_int({arg2}));"]
                case _:
                    raise ValueError(f"Unknown builtin {name} ")

        case N(NT_IfThenElse(), [cond, then_branch, else_branch]):
            return ([f"yyvalue {store_result};", 
                    f"if (yyvalue_to_bool({cond})) {{"]
                    + compile_ast(then_branch) +
                    [f"}} else {{"]
                    + compile_ast(else_branch) +
                    ["}"])
        case N(NT_DataTupleCons(idx, length), [arg]):
            return [f"yyvalue {store_result} = raw_constructor_tuple_to_yyvalue({idx}, {length}, yyvalue_to_tuple({arg}));"]
        case N(NT_DataTupleProjIdx(), [arg]):
            return [f"yyvalue {store_result} = int_to_yyvalue(yyvalue_to_constructor_tuple_idx({arg}));"]
        case N(NT_DataTupleProjTuple(), [arg]):
            return [f"yyvalue {store_result} = raw_tuple_to_yyvalue(yyvalue_to_constructor_tuple_length({arg}), yyvalue_to_constructor_tuple_tuple({arg}));"]
        case N(NT_ExternalCall(name), args):
            return [f"yyvalue {store_result} = {name}({', '.join([f'{arg}' for arg in args])});"]
        case N(NT_CallCC(), [func]):
            return [f"yyvalue {store_result} = TODO_CALLCC({func});"]
        case N(NT_CallCCRet(), [func, arg]):
            return [f"yyvalue {store_result} = TODO_CALLCCRET({func}, {arg});"]
        case N(NT_GlobalFuncRef(name), []):
            return [f"yyvalue {store_result} = funcptr_to_yyvalue({name});"]
        case N(NT_WriteGlobalFileRef(name), [arg]):
            return [f"{name} = arg;", f"yyvalue {store_result} = unit_to_yyvalue();"]
        case N(NT_UpdateStruct(index), [arg1, arg2]):
            return [f"yyvalue_to_tuple({arg1})[{index}] = {arg2};", f"yyvalue {store_result} = unit_to_yyvalue();"]
        case N(NT_MultiArgFuncCall(arg_count), [func, *args]):
            return [f"yyvalue {store_result} = funcptr_to_yyvalue({func})({', '.join([f'{arg}' for arg in args])}) /* TODO!!! */;"]
        case N(NT_FileRef(filename), []):
            return [f"yyvalue {store_result} = {filename};"]
        case FreeVar(name):
            return [f"yyvalue {store_result} = {name};"]

        case _:
            raise ValueError(f"Unknown immediate {ast_to_ir(immediate)}")

# @after_compile_ast_decorator
def compile_ast(ast: Abt) -> List[str]:
    match ast:
        case N(NT_LetIn(), [cur, next]):
            assert isinstance(next, Binding)
            next_name, next_body = unbind_abt(next)
            return compile_immediate(cur, next_name) + compile_ast(next_body)
        case N(NT_ConsecutiveStmt(), [cur, next]):
            discard_name = global_unique_name("discard")
            return compile_immediate(cur, discard_name) + compile_ast(next)
        case N(NT_CallCC(), [next]):
            assert isinstance(next, Binding)
            next_name, next_body = unbind_abt(next)
            return [f"yyvalue {next_name} = TODO_CALLCC;"] + compile_ast(next_body)
        case FreeVar(name):
            return ["return " + name + ";"]
        case _:
            raise ValueError(f"Unknown compile ast {ast}")
        

def do_compile_func(name: str, func: Abt) -> List[str]:
    result = []
    match func:
        case N(NT_MultiArgLam(arg_count), [body]):
            arg_names, real_body = unbind_abt_list(body, arg_count)
            result.append("yyvalue " + name + "(" + ", ".join([f"yyvalue {arg}" for arg in arg_names]) + ") {")
            try:
                result.append("\n".join(compile_ast(real_body)))
                result.append("\n}")
                return result
            except ValueError as e:
                print("Error compiling function", name)
                raise
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


    
