from __future__ import annotations

from ast_util import *
from yuyan_import import *
from tqdm import tqdm
from pass_utils import *
import pickle
# ALL_CLOSURE_FUNCS = {}
# ALL_CLOSURE_ASTS = {}


def closure_convert(ast: Abt, func_prefix: str, ALL_CLOSURE_FUNCS: Dict[str, Abt]) -> Abt:
    match ast:
        case N(NT_StructEntry(label), [cur, next]):
            return N(NT_StructEntry(label), [closure_convert(cur, func_prefix+"::"+(label if label is not None else "___"), ALL_CLOSURE_FUNCS), closure_convert(next, func_prefix, ALL_CLOSURE_FUNCS)])
        case N(NT_MultiArgLam(arg_count), [args_body]):
            converted_body = closure_convert(args_body, func_prefix, ALL_CLOSURE_FUNCS)
            func_unique_name = unique_name(func_prefix, list(ALL_CLOSURE_FUNCS.keys()))
            body_free_vars = collect_free_vars(converted_body)
            block_unique_name = unique_name(func_prefix, body_free_vars)
            substituted_body = substitute_list(
                [N(NT_TupleProj(i), [FreeVar(block_unique_name)]) for i in range(len(body_free_vars))], 
                body_free_vars, converted_body
            )
            ALL_CLOSURE_FUNCS[func_unique_name] = N(NT_MultiArgLam(arg_count+1), [abstract_over_abt(substituted_body, block_unique_name)])
            return N(NT_TupleCons(), [N(NT_GlobalFuncRef(func_unique_name), []), N(NT_TupleCons(), [FreeVar(n) for n in body_free_vars])])
        case N(NT_MultiArgFuncCall(arg_count), [func, *args]):
            after_func = closure_convert(func, func_prefix, ALL_CLOSURE_FUNCS)
            after_args = [closure_convert(arg, func_prefix, ALL_CLOSURE_FUNCS) for arg in args]
            return N(NT_LetIn(), [after_func, 
                construct_binding(
                    "func_closure", 
                    lambda x: N(
                        NT_MultiArgFuncCall(arg_count+1), 
                        [
                            N(NT_TupleProj(0), [FreeVar(x)]),
                            N(NT_TupleProj(1), [FreeVar(x)]),
                            *after_args
                        ]))]) 
        case _:
            return map_abt(ast, lambda x: closure_convert(x, func_prefix, ALL_CLOSURE_FUNCS))


@cached_pass("closure_convert")
def closure_convert_top_level(inputs: Dict[str, Abt]) -> Dict[str, Abt]:
    ALL_CLOSURE_FUNCS: Dict[str, Abt] = {"entryMain": None} # entryMain should appear first

    for file, ast in tqdm(inputs.items(), desc="Closure conversion"):
        ALL_CLOSURE_FUNCS[file + "_init"] = N(NT_MultiArgLam(0), 
            [N(NT_WriteGlobalFileRef(file), [closure_convert(ast, file, ALL_CLOSURE_FUNCS)])])
    init_ast = N(NT_ExternalCall("yyProcessExit"), [N(NT_IntConst(0), [])])
    for file in reversed(inputs.keys()):
        init_ast = N(NT_ConsecutiveStmt(), [
            N(NT_MultiArgFuncCall(0), [N(NT_GlobalFuncRef(file + "_init"), [])]),
            init_ast])

    ALL_CLOSURE_FUNCS["entryMain"] = N(NT_MultiArgLam(0), [init_ast])

    return ALL_CLOSURE_FUNCS
