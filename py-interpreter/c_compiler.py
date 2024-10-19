from __future__ import annotations
# this is an experiment project to understand performance bottlenecks

# my plan is to directly compile the type checking result json file

# ACTUALLY THIS FILE SHOULD BE ABANDONED, I NOW REALIZE THAT I SHOULD HAVE MY OWN BYTECODE
# WHICH IS A STACK MACHINE SIMILAR TO JVM

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
import pass_utils
from anf_convert import *
from recursion_rewrite import *
from tqdm import tqdm

import shutil
print("recursion limit", sys.getrecursionlimit())   


def label_escape(label: str) -> str:
    return (label.replace(":", "_").replace("-", "_")
            .replace(".", "_").replace("/", "_")
            .replace("【", "SQ_L_OPEN").replace("】", "SQ_R_CLOSE")
            .replace("，", "COMMA").replace("。", "PERIOD")
    )
def python_string_to_c_string(s):
    # Escape backslashes and double quotes
    s = s.replace('\\', '\\\\')  # Escape backslashes
    s = s.replace('"', '\\"')     # Escape double quotes
    s = s.replace('\n', '\\n')    # Escape newlines
    s = s.replace('\t', '\\t')     # Escape tabs
    s = s.replace('%', '%%')       # Escape percent signs for printf

    # Escape trigraphs by replacing ?? with ?\?
    s = s.replace('??', '?\\?')  # Escape trigraphs

    # Return the string wrapped in double quotes for C syntax
    return f'"{s}"'
# we use the same convention as yybcvm, 
    # /**
    #  * Stack will be arranged like this
    #  * 
    #  * Yuyan Call Routine
    #  * 
    #  * 
    #  * For function call, the following values will be pushed to the stack
    #  * arg n
    #  * arg n-1
    #  * ...
    #  * arg 2
    #  * arg 1
    #  * arg 0 (! arguments starts from 0)
    #  * previous stack_ptr
    #  * previous pc
    #  * (SP)
    #  * local 0
    #  * local 1
    #  * ...
    #  *
    #  * 
    #  * 
    #  * 
    #  * 
    #  * 
    # */
def return_routine() -> List[str]:
    return_address_name = global_unique_name("return_address")
    return [
        f"void *{return_address_name} = (void*)yyvalue_to_staticptr(stack_ptr[-1]);",
        "stack_ptr = (yyvalue*)yyvalue_to_stackptr(stack_ptr[-2]);",
        f"goto *{return_address_name};",
    ]

def compile_immediate(params: List[str], locals: List[str], immediate: Abt) -> str:
    match immediate:
        case FreeVar(name):
            assert not (name in params and name in locals)
            if name in params:
                # 0th param is stack_ptr-3
                return f"stack_ptr[-{params.index(name) + 3}]"
            elif name in locals:
                return f"stack_ptr[{locals.index(name)}]"
        case _:
            raise ValueError("Not an immediate: ", immediate)

def compile_expression(params: List[str], locals: List[str], expr: Abt) -> List[str]:
    def ci(imm):
        return compile_immediate(params, locals, imm)
    match expr:
        case N(NT_EmptyVal(), []):
            return [f"rax = unit_to_yyvalue();"]
        case N(NT_IntConst(val), []):
            return [f"rax = int_to_yyvalue({val});"]
        case N(NT_DecimalNumber(integral, fractional), []):
            return [f"rax = double_to_yyvalue({integral}.{fractional});"]
        case N(NT_StringConst(val), []):
            return [f"rax = static_string_to_yyvalue({python_string_to_c_string(val)});"]
        case N(NT_TupleCons(), args):
            return [f"rax = tuple_to_yyvalue({len(args)}, (yyvalue[])" + '{' + ', '.join([compile_immediate(params, locals, arg) for arg in args]) + "});"]
        case N(NT_TupleProj(idx), [arg]):
            return [f"rax = yyvalue_to_tuple({ci(arg)})[{idx}];"]
        case N(NT_Builtin(name), args):
            match name, args:
                case "内建爻阳", []:
                    return [f"rax = bool_to_yyvalue(true);"]
                case "内建爻阴", []:
                    return [f"rax = bool_to_yyvalue(false);"]
                case "内建有元", []:
                    return [f"rax = unit_to_yyvalue();"]
                case "内建函数整数相等", [arg1, arg2]:
                    return [f"rax = bool_to_yyvalue(yyvalue_to_int({ci(arg1)}) == yyvalue_to_int({ci(arg2)}));"]
                case "内建函数整数减", [arg1, arg2]:
                    return [f"rax = int_to_yyvalue(yyvalue_to_int({ci(arg1)}) - yyvalue_to_int({ci(arg2)}));"]
                case "内建函数整数大于", [arg1, arg2]:
                    return [f"rax = bool_to_yyvalue(yyvalue_to_int({ci(arg1)}) > yyvalue_to_int({ci(arg2)}));"]
                case _:
                    raise ValueError(f"Unknown builtin {name} ")

        case N(NT_IfThenElse(), [cond, then_branch, else_branch]):
            store_result_name = global_unique_name("if_result");
            assert store_result_name not in locals
            next_locals = locals + [store_result_name]
            return ([
                    f"if (yyvalue_to_bool({ci(cond)})) {{"]
                    + compile_ast(params, next_locals, store_result_name, then_branch) +
                    [f"}} else {{"]
                    + compile_ast(params, next_locals, store_result_name, else_branch) +
                    ["}",
                     f"rax = {compile_immediate(params, next_locals, FreeVar(store_result_name))};"]
                    )
        case N(NT_DataTupleCons(idx, length), [arg]):
            return [f"rax = raw_constructor_tuple_to_yyvalue({idx}, {length}, yyvalue_to_tuple({ci(arg)}));"]
        case N(NT_DataTupleProjIdx(), [arg]):
            return [f"rax = int_to_yyvalue(yyvalue_to_constructor_tuple_idx({ci(arg)}));"]
        case N(NT_DataTupleProjTuple(), [arg]):
            return [f"rax = raw_tuple_to_yyvalue(yyvalue_to_constructor_tuple_length({ci(arg)}), yyvalue_to_constructor_tuple_tuple({ci(arg)}));"]
        case N(NT_ExternalCall(name), args):
            return [f"rax = {name}({', '.join([f'{ci(arg)}' for arg in args])});"]
        case N(NT_CallCCRet(), [func, arg]):
            saved_stack_ptr_name = global_unique_name("saved_stack_ptr") # this is the current stack ptr name, we need to 
            saved_label_name = global_unique_name("saved_label")
            return [
                f"rax = {ci(arg)};"
                f"yyvalue *{saved_stack_ptr_name} = yyvalue_to_stackptr({ci(func)});",
                f"stack_ptr = yyvalue_to_stackptr({saved_stack_ptr_name}[-2]);"
                f"void *{saved_label_name} = (void *)yyvalue_to_staticptr({saved_stack_ptr_name}[-1]);",
                f"goto *{saved_label_name};"
                ]
        case N(NT_GlobalFuncRef(name), []):
            return [f"rax = staticptr_to_yyvalue((yyvalue *)(&&{label_escape(name)}));"]
        case N(NT_WriteGlobalFileRef(name), [arg]):
            return [f"{label_escape(name)} = {ci(arg)};",
                    f"rax = unit_to_yyvalue();"]
        case N(NT_UpdateStruct(index), [arg1, arg2]):
            return [f"yyvalue_to_tuple({ci(arg1)})[{index}] = {ci(arg2)};"
                    f"rax = unit_to_yyvalue();"]
        case N(NT_FileRef(filename), []):
            return [f"rax = {label_escape(filename)};"]
        case N(NT_MultiArgFuncCall(arg_count), [func, *args]):
            continuation_label_name = global_unique_name("continuation_label")
            func_ptr_name = global_unique_name("called_func_ptr")
            stack_reservation = len(locals)
            args_len = len(args)
            assert args_len == arg_count
            return ([f"stack_ptr[{stack_reservation + i}] = {ci(arg)};" for i, arg in enumerate(reversed(args))] +
                    [f"void* {func_ptr_name} = yyvalue_to_staticptr({ci(func)});",
                     f"stack_ptr[{stack_reservation + args_len}] = stackptr_to_yyvalue(stack_ptr);",
                     f"stack_ptr[{stack_reservation + args_len + 1}] = staticptr_to_yyvalue((void*)(&&{continuation_label_name}));",
                     f"stack_ptr = stack_ptr + {stack_reservation + args_len + 2};", 
                     f"yy_pre_function_call_gc();"
                     f"goto *((void*){func_ptr_name});",
                    f"{continuation_label_name}:",
                    f"rax = rax;"]
            )
        case FreeVar(name):
            return [f"rax = {ci(expr)};"]
        case _:
            raise ValueError(f"Unknown expr {ast_to_ir(expr)}")

# @after_compile_ast_decorator
def compile_ast(params: List[str], locals: List[str], destination_local: str,  ast: Abt) -> List[str]:
    match ast:
        case N(NT_LetIn(), [cur, next]):
            assert isinstance(next, Binding)
            next_name, next_body = unbind_abt_no_repeat(next, locals + params)   
            assert next_name not in locals
            next_locals = locals + [next_name]
            return (compile_expression(params, locals, cur) +
                     [f"stack_ptr[{len(locals)}] = rax;"] + 
                       compile_ast(params, next_locals, destination_local, next_body)
            )
        case N(NT_ConsecutiveStmt(), [cur, next]):
            discard_name = global_unique_name("discard")
            return compile_expression(cur, discard_name) + compile_ast(next)
        case N(NT_CallCC(), [next]):
            assert isinstance(next, Binding)
            next_name, next_body = unbind_abt_no_repeat(next, locals + params)
            assert next_name not in locals
            next_locals = locals + [next_name]
            return ([f"stack_ptr[{len(locals)}] = stackptr_to_yyvalue(stack_ptr);"] +
                     compile_ast(params, next_locals, destination_local, next_body)
            )
        case FreeVar(name):
            return [f"{compile_immediate(params, locals, FreeVar(destination_local))} = {compile_immediate(params, locals, ast)};"]
        case _:
            raise ValueError(f"Unknown compile ast {ast}")
        

def do_compile_func(name: str, func: Abt) -> List[str]:
    result = []
    match func:
        case N(NT_MultiArgLam(arg_count), [body]):
            arg_names, real_body = unbind_abt_list(body, arg_count)
            return_val_name = global_unique_name("return_val")
            arg_names, real_body = unbind_abt_list(body, arg_count)
            result.append(f"{label_escape(name)}:")
            try:
                result.extend(compile_ast(arg_names, [return_val_name], return_val_name, real_body))
                result.append(f"rax = stack_ptr[0]; // {return_val_name}")
                result.extend(return_routine())
                return result
            except ValueError as e:
                print("Error compiling function", name)
                raise
        case _:
            raise ValueError(f"Expected multi arg lambda, got {func}")
    


def flatten(lst):
    return [item for sublist in lst for item in sublist]

def do_compile_funcs(func_dict):
    read_file_refs = list(set(flatten(find_all_file_refs(v) for _, v in func_dict.items())))
    update_file_refs = list(set(flatten(find_all_update_file_refs(v) for _, v in func_dict.items())))
    if not all (name in update_file_refs for name in read_file_refs):
        print("File refs not updated", [name for name in read_file_refs if name not in update_file_refs])
    file_refs = update_file_refs
    lines = []
    lines.extend(
        ['#include "../native/common_include.h"',
         '#include "../native/yystdlib_include.h"',    
         '#include "../native/garbage_collector.h"',    
         ]
    )
    lines.extend("yyvalue " + label_escape(name) + " = 0;" for name in file_refs)
    lines.extend(["int64_t yy_runtime_start() {"]
                  +["yy_register_gc_rootpoint(&" + label_escape(name) + ");" for name in file_refs]
                  +[
                  "yyvalue rax = unit_to_yyvalue(); // used for storing return value of functions",
                  "goto entryMain;",
    ])
    for name, func in tqdm(func_dict.items(), desc="Compiling to C"):
        lines.extend(do_compile_func(name, func))
    lines.extend(["return 0;",
                  "}"])
    with open(pass_utils.get_artifact_path("output.c"), "w") as f:
        f.write("\n".join(lines))

    
def do_make_exec():
    shutil.copyfile(pass_utils.get_artifact_path("output.c"), "./yybcvm/pygen/output.c")
    cmd = "make -C ./yybcvm pygen PYGEN_EXEC_PATH=../"+pass_utils.get_artifact_path("output.exe")
    print(cmd)
    os.system(cmd)
    print("[OK] [DONE] RUN PROGRAM WITH THIS CMD:")
    print(pass_utils.get_artifact_path("output.exe"))




if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python compiler.py <path_no_extension> <args>")
        sys.exit(1)
    path = sys.argv[1]
    pass_utils.INPUT_PATH_KEY = file_path_to_key(path)
    os.makedirs(get_artifact_path(""), exist_ok=True)
    asts = do_load_files(path)
    converted = closure_convert_top_level(asts)
    rewritten = recursion_rewrite_top_level(converted)
    anf = anf_convert_top_level(rewritten)
    do_compile_funcs(anf)
    do_make_exec()


    
