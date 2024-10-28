from __future__ import annotations
# this is an experiment project to understand performance bottlenecks

# my plan is to directly compile the type checking result json file

# ACTUALLY THIS FILE SHOULD BE ABANDONED, I NOW REALIZE THAT I SHOULD HAVE MY OWN BYTECODE
# WHICH IS A STACK MACHINE SIMILAR TO JVM

import resource, sys
# resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY,resource.RLIM_INFINITY))
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
from yybc_ast import *
import pass_utils
import struct
from tqdm import tqdm
print("recursion limit", sys.getrecursionlimit())   


def flatten(lst):
    return [item for sublist in lst for item in sublist]

def unbind_no_repeat(abt: Binding, local: List[str], params: List[str]) -> Tuple[str, Abt]:
    return unbind_abt_no_repeat(abt, local + params)

# should assume opstack is empty and should leave opstack to a single entry on top
def compile_immediate(params: List[str], locals: List[str], immediate: Abt, strings: List[str]) -> List[Instruction]:
    match immediate:
        case N(NT_EmptyVal(), []):
            return [UnitConst()]
        case N(NT_IntConst(val), []):
            return [IntConst(val)]
        case N(NT_DecimalNumber(integral, fractional), []):
            return [DecimalConst(integral, fractional)]
        case N(NT_StringConst(val), []):
            index = len(strings)
            strings.append(val)
            return [StringConst(index)]
        case N(NT_TupleCons(), args):
            return flatten([compile_immediate(params, locals, arg, strings) for arg in args])  + [MakeTuple(len(args))]
        case N(NT_TupleProj(idx), [arg]):
            return compile_immediate(params, locals, arg, strings) + [
                IntConst(idx), 
                ReadTuple(),
            ]
        case N(NT_Builtin(name), args):
            match name, args:
                case "内建爻阳", []:
                    return [BoolConst(True)]
                case "内建爻阴", []:
                    return [BoolConst(False)]
                case "内建有元", []:
                    return [UnitConst()]
                case "内建函数整数相等", [arg1, arg2]:
                    return compile_immediate(params, locals, arg2, strings) + compile_immediate(params, locals, arg1, strings) + [ExternalCall(strings.index("yyIntEqTest"), 2)]
                case "内建函数整数减", [arg1, arg2]:
                    return compile_immediate(params, locals, arg2, strings) + compile_immediate(params, locals, arg1, strings) + [ExternalCall(strings.index("yyIntSub"), 2)]
                case "内建函数整数大于", [arg1, arg2]:
                    return compile_immediate(params, locals, arg2, strings) + compile_immediate(params, locals, arg1, strings) + [ExternalCall(strings.index("yyIntGtTest"), 2)]
                case _:
                    raise ValueError(f"Unknown builtin {name} ")

        case N(NT_IfThenElse(), [cond, then_branch, else_branch]):
            false_start_label = global_unique_name("false_start_label")
            false_end_label_name = global_unique_name("false_end_label")
            false_start_label_idx = len(strings)
            strings.append(false_start_label)
            false_end_label_name_idx = len(strings)
            strings.append(false_end_label_name)
            if_result_name = global_unique_name("if_result")
            assert if_result_name not in locals
            next_locals = locals + [if_result_name]
            if_result_name_idx = len(locals)
            return (
                compile_immediate(params, locals, cond, strings) +
                [BranchIfFalse(false_start_label_idx)] +
                compile_ast(params, next_locals, if_result_name, then_branch, strings) +
                [Branch(false_end_label_name_idx), Label(false_start_label_idx)] +
                compile_ast(params, next_locals, if_result_name, else_branch, strings) +
                [Label(false_end_label_name_idx)] + 
                [LoadLocal(if_result_name_idx)]
            )

        case N(NT_DataTupleCons(idx, length), [arg]):
            return [IntConst(idx)] + compile_immediate(params, locals, arg, strings) + [MakeTuple(2)]
        case N(NT_DataTupleProjIdx(), [arg]):
            return compile_immediate(params, locals, arg, strings) + [IntConst(0), ReadTuple()]
        case N(NT_DataTupleProjTuple(), [arg]):
            return compile_immediate(params, locals, arg, strings) + [IntConst(1), ReadTuple()]
        case N(NT_ExternalCall(name), args):
            return flatten([compile_immediate(params, locals, arg, strings) for arg in reversed(args)]) + [ExternalCall(strings.index(name), len(args))]
        case N(NT_CallCCRet(), [func, arg]):
            return compile_immediate(params, locals, arg, strings) + compile_immediate(params, locals, func, strings) + [ExternalCall(strings.index("yyCallCCRet"), 2)]
        case N(NT_GlobalFuncRef(name), []):
            return [FuncRef(strings.index(name))]
        case N(NT_WriteGlobalFileRef(name), [arg]):
            return compile_immediate(params, locals, arg, strings) + [UpdateFileRef(strings.index(name)), UnitConst()]
        case N(NT_UpdateStruct(index), [arg1, arg2]):
            return compile_immediate(params, locals, arg1, strings) + [IntConst(index)] + compile_immediate(params, locals, arg2, strings) + [WriteTuple(), UnitConst()]
        case N(NT_MultiArgFuncCall(arg_count), [func, *args]):
            return flatten([compile_immediate(params, locals, arg, strings) for arg in reversed(args)]) + compile_immediate(params, locals, func, strings) + [CallFuncPtr(arg_count, len(locals))]
        case N(NT_FileRef(filename), []):
            return [FileRef(strings.index(filename))]
        case FreeVar(name):
            assert not (name in params and name in locals)
            if name in params:
                return [LoadParam(params.index(name))]
            elif name in locals:
                return [LoadLocal(locals.index(name))]
            else:
                raise ValueError(f"Unknown free var {name}")
        case _:
            raise ValueError(f"Unknown immediate {ast_to_ir(immediate)}")

# @after_compile_ast_decorator
def compile_ast(params: List[str], locals: List[str], destination_local: str,  ast: Abt, strings: List[str]) -> List[Instruction]:
    match ast:
        case N(NT_LetIn(), [cur, next]):
            assert isinstance(next, Binding)
            next_name, next_body = unbind_no_repeat(next, locals, params)
            assert next_name not in locals
            next_locals = locals + [next_name]
            return (
                compile_immediate(params, locals, cur, strings) +
                [StoreLocal(len(locals))] + 
                compile_ast(params, next_locals, destination_local, next_body, strings)
            )
        case N(NT_ConsecutiveStmt(), [cur, next]):
            return (
                compile_immediate(params, locals, cur, strings) +
                [PopOpStack()] +
                compile_ast(params, locals, destination_local, next, strings)
            )
        case N(NT_CallCC(), [next]):
            next_name, next_body = unbind_no_repeat(next, locals, params)
            assert next_name not in locals
            next_locals = locals + [next_name]
            return (
                [ExternalCall(strings.index("yyCallCC"), 0),
                 StoreLocal(len(locals))] +
                compile_ast(params, next_locals, destination_local, next_body, strings)
            )
        case FreeVar(name):
            assert not (name in params and name in locals)
            if name in params:
                return [LoadParam(params.index(name))] + [StoreLocal(locals.index(destination_local))]
            else:
                return [LoadLocal(locals.index(name))] + [StoreLocal(locals.index(destination_local))]
        case _:
            raise ValueError(f"Unknown compile ast {ast}")
        

def do_compile_func(name: str, func: Abt, strings: List[str]) -> List[Instruction]:
    result: List[Instruction] = []
    match func:
        case N(NT_MultiArgLam(arg_count), [body]):
            arg_names, real_body = unbind_abt_list(body, arg_count)
            return_val_name = global_unique_name("return_val")
            result.append(BeginFunc(strings.index(name)))
            try:
                result.extend(compile_ast(arg_names, [return_val_name], return_val_name, real_body, strings))
                result.extend([
                    LoadLocal(0),
                    Return(),
                    EndFunc(strings.index(name)),
                ])
                return result
            except ValueError as e:
                print("Error compiling function", name)
                raise
        case _:
            raise ValueError(f"Expected multi arg lambda, got {func}")
    
all_external_call_names = [
"yyTopExceptHandler",
"yyRunningOnWindows",
"yyNewRef",
"yyReadRef",
"yyWriteRef",
"yyIntAdd",
"yyIntMult",
"yyIntDiv",
"yyIntSub",
"yyIntEqTest",
"yyIntGtTest",
"yyIntToString",
"yyStringEq",
"yyStringToInt",
"yyGetCommandLineProgramName",
"yyGetCommandLineArgs",
"yyPrintln",
"yyPrintStr",
"yyPrintlnStdErr",
"yyNewRefArrayGeneric",
"yyWriteRefArray",
"yyReadRefArray",
"yyCodePointsConcat",
"yyGetCurrentWorkingDirectory",
"yyGetCodePoints",
"yyGetCommandLineArgs",
"yyGetCurrentLocalDateTimeFmt",
"yyPrintlnStdErr",
"获取当前异常处理器",
"设置当前异常处理器",
"yyIsSubstring",
"yyPrintln",
"yyPathExists",
"yyGetFileModifiedTime",
"yyReadFileSync",
"yy_豫言字符串获取字节数组",
"yyStringByteArrayGetLength",
"yy_豫言字符串获取字节序数当前字符",
"yy_豫言字符串匹配",
"yyIsPathRegularFile",
"yyIsPathDirectory",
"yyWriteFileSync",
"yyProcessExit",
"yyCallCC",
"yyCallCCRet",
'yyDeleteFileSync', 'yyPrintGeneric', 'yyDoubleToString', 'yyRunningOnMacOS', 'yy_豫言字符串获取JSON字符串', 
'yyGetRandomInt', 'yyDoubleAdd', 'yyIntToDouble', 'yyDoubleToInt', 'yy_豫言子字符串从字节序数开始', 'yyDoubleSub', 
'yyListDirectorySync', 'yyGetRandomDouble', 'yy_豫言不安全转换', 'yyStringByteLength', 'yyNewRefArray', 
'yyGetCurrentLocalDateTimeStr', 'yyRunProcessSyncPipeOutput', 'yyDoubleMult', 'yyStringToDouble', 'yyDoubleDiv', 
'yyRunProcessGetOutputSync', 'yy_豫言字符转整数', 'yyRunningOnLinux', 'yyRunProcessSync', 'yyCurrentNanosecondTime'
]

def write_binary_file(filename, instrs, strings, external_calls, file_refs, func_dict):
    with open(filename, "wb") as f:
        # Header fields
        num_external_calls = len(external_calls)
        num_file_refs = len(file_refs)
        num_funcs = len(func_dict)
        num_strings = len(strings)
        
        # Calculate offset for the strings section
        instr_bytes = bytearray()
        for instr in instrs:
            instr_bytes += write_instr_to_bytes(instr)
        strings_offset = 64 + len(instr_bytes)  # Header is 64 bytes

        # Write header
        header = struct.pack(
            ">QQQQQQQQ",
            num_external_calls,
            num_file_refs,
            num_funcs,
            num_strings,
            strings_offset,
            0, 0, 0  # Reserved fields
        )
        f.write(header)

        # Write instructions
        f.write(instr_bytes)

        # Write strings
        for s in strings:
            f.write(s.encode('utf-8') + b'\x00')


def do_compile_funcs(func_dict):
    referenced_external_call_names = list(set(flatten(find_external_call_names(v) for _, v in func_dict.items())))
    names_not_present = [n for n in referenced_external_call_names if n not in all_external_call_names]
    if len(names_not_present) > 0:
        print("External call names not present in list", names_not_present)
    read_file_refs = list(set(flatten(find_all_file_refs(v) for _, v in func_dict.items())))
    update_file_refs = list(set(flatten(find_all_update_file_refs(v) for _, v in func_dict.items())))
    if not all (name in update_file_refs for name in read_file_refs):
        print("File refs not updated", [name for name in read_file_refs if name not in update_file_refs])
    file_refs = update_file_refs
    strings = [n for n in all_external_call_names] + file_refs + list(func_dict.keys())
    if os.path.exists(pass_utils.get_artifact_path("/output_bc.pickle")):
        print("Loading from yybc pickle")
        cache = pickle.load(open(pass_utils.get_artifact_path("/output_bc.pickle"), "rb"))
        strings = cache["strings"]
        instrs = cache["instrs"]
    else:
        instrs = []
        for name, func in tqdm(func_dict.items(), desc="Compiling to YYBC"):
            instrs.extend(do_compile_func(name, func, strings))
        pickle.dump({"strings": strings, "instrs": instrs}, open(pass_utils.get_artifact_path("output_bc.pickle"), "wb"))
    with open(pass_utils.get_artifact_path("output.yybcir"), "w") as f:
        f.write(f"EXTERNCALLS {len(all_external_call_names)} FILES {len(file_refs)} FUNCS {len(func_dict)} STRINGS {len(strings)}\n")
        f.write("\n".join([inst_to_text(instr) for instr in instrs]) + "\n")
        f.write("\n".join([f"STRING {i} {json.dumps(s)}" for i, s in enumerate(strings)]) + "\n")
    write_binary_file(pass_utils.get_artifact_path("output.yybcb"), instrs, strings, all_external_call_names, file_refs, func_dict)
    print("[Done] RUN USING CMD:")
    print(f"./yybcvm/build/native/vmopt.exe {pass_utils.get_artifact_path('output.yybcb')}")
# it is expected that VM calculates the index into the file list by subtracting the number of builtins

    



if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python compiler.py <path_no_extension> <args>")
        sys.exit(1)
    path = sys.argv[1]
    options = sys.argv[2:]
    pass_utils.INPUT_PATH_KEY = file_path_to_key(path)
    os.makedirs(pass_utils.get_artifact_path(""), exist_ok=True)
    asts = do_load_files(path)
    converted = closure_convert_top_level(asts)
    rewritten = recursion_rewrite_top_level(converted)
    anf = anf_convert_top_level(rewritten)
    do_compile_funcs(anf)


    
