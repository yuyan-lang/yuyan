from __future__ import annotations
from typing import *

from ast_util import *
from tqdm import tqdm
from pass_utils import *

def anf_convert_list(asts: List[Abt], tail: Callable[[List[FreeVar]], Abt]) -> Abt:
    def rec(sofar: List[FreeVar], rest: List[Abt]) -> Abt:
        if len(rest) == 0:
            return tail(sofar)
        else:
            return anf_convert(rest[0], lambda x: rec(sofar + [x], rest[1:]))
    return rec([], asts)

def anf_convert_immediate(ast: Abt, tail: Callable[[FreeVar], Abt]) -> Abt:
    return N(NT_LetIn(), [ast, construct_binding("anf_end", lambda x: tail(FreeVar(x)))])

def anf_convert(ast: Abt, tail: Callable[[FreeVar], Abt]) -> Abt:
    match ast:
        case FreeVar(_):
            return tail(ast)
        case N(NT_FileRef(filename), []):
            return anf_convert_immediate(ast, tail)
        case N(NT_EmptyVal(), []):
            return anf_convert_immediate(ast, tail)
        case N(NT_Builtin(name), args):
            return anf_convert_list(args, lambda args_val: anf_convert_immediate(N(NT_Builtin(name), args_val), tail)) # type: ignore
        case N(NT_TupleProj(idx), [arg]):
            return anf_convert(arg, lambda arg_val: anf_convert_immediate(N(NT_TupleProj(idx), [arg_val]), tail))
        case N(NT_AnnotatedVar(_), [arg]):
            return anf_convert_immediate(arg, tail)
        case N(NT_MultiArgLam(arg_count), _):
            raise ValueError("MultiArgLam should not be in ANF")
        case N(NT_TupleCons(), args):
            return anf_convert_list(args, lambda args_val: anf_convert_immediate(N(NT_TupleCons(), args_val), tail)) # type: ignore
        case N(NT_DataTupleCons(idx, length), [arg]):
            return anf_convert(arg, lambda arg_val: anf_convert_immediate(N(NT_DataTupleCons(idx, length), [arg_val]), tail))
        case N(NT_MultiArgFuncCall(arg_count), [func, *args]):
            return anf_convert_list([func, *args], lambda args_val: anf_convert_immediate(N(NT_MultiArgFuncCall(arg_count), args_val),tail)) # type: ignore
        case N(NT_IfThenElse(), [cond, then_branch, else_branch]):
            tail_end = construct_binding("if_then_else_end", lambda x: tail(FreeVar(x)))
            return N(NT_LetIn(), 
                     [anf_convert(cond, lambda cond_val: N(NT_IfThenElse(), [cond_val, anf_convert(then_branch, lambda x: x), anf_convert(else_branch, lambda x: x)])),
                        tail_end])
        case N(NT_StringConst(val), []):
            return anf_convert_immediate(ast, tail)
        case N(NT_IntConst(val), []):
            return anf_convert_immediate(ast, tail)
        case N(NT_DecimalNumber(integral, fractional), []):
            return anf_convert_immediate(ast, tail)
        case N(NT_ExternalCall(name), args):
            return anf_convert_list(args, lambda args_val: anf_convert_immediate(N(NT_ExternalCall(name), args_val), tail)) # type: ignore
        case N(NT_LetIn(), [cur, bnd]):
            assert isinstance(bnd, Binding)
            bnd_name, bnd_body = unbind_abt(bnd)
            return anf_convert(cur, lambda cur_val: N(NT_LetIn(), [cur_val, abstract_over_abt(anf_convert(bnd_body, tail), bnd_name)]))
        case N(NT_ConsecutiveStmt(), [cur, next]):
            return anf_convert(cur, lambda _: anf_convert(next, tail))
        case N(NT_DataTupleProjIdx(), [arg]):
            return anf_convert(arg, lambda arg_val: anf_convert_immediate(N(NT_DataTupleProjIdx(), [arg_val]), tail))
        case N(NT_DataTupleProjTuple(), [arg]):
            return anf_convert(arg, lambda arg_val: anf_convert_immediate(N(NT_DataTupleProjTuple(), [arg_val]), tail))
        case N(NT_CallCC(), [bnd]):
            assert isinstance(bnd, Binding)
            bnd_name, bnd_body = unbind_abt(bnd)
            return N(NT_CallCC(), [abstract_over_abt(anf_convert(bnd_body, tail), bnd_name)])
        case N(NT_CallCCRet(), [cc, ret_val]):
            return anf_convert_list([cc, ret_val], lambda args_val: anf_convert_immediate(N(NT_CallCCRet(), args_val), tail))
        case N(NT_GlobalFuncRef(name), []):
            return anf_convert_immediate(ast, tail)
        case N(NT_GlobalFuncDecl(name), [v]):
            raise ValueError("GlobalFuncDecl should not be in ANF")
        case N(NT_WriteGlobalFileRef(name), [arg]):
            return anf_convert(arg, lambda arg_val: anf_convert_immediate(N(NT_WriteGlobalFileRef(name), [arg_val]), tail))
        case N(NT_UpdateStruct(idx), args):
            return anf_convert_list(args, lambda args_val: anf_convert_immediate(N(NT_UpdateStruct(idx), args_val), tail))
        case _:
            raise ValueError(f"Unknown ANF_CONVERT {ast}")

def anf_convert_func(fun: Abt):
    match fun:
        case N(NT_MultiArgLam(arg_count), [body]):
            assert isinstance(body, Abt)
            abs_names, actual_body = unbind_abt_list(body, arg_count)
            return N(NT_MultiArgLam(arg_count), [abstract_over_abt_list(anf_convert(actual_body, lambda x: x), abs_names)])

        

@cached_pass("anf_convert")
def anf_convert_top_level(funcs: Dict[str, Abt]):
    return {name: anf_convert_func(func) for name, func in tqdm(funcs.items(), desc="ANF Convert")}