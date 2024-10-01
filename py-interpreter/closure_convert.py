
from ast_util import *
from files_manager import *

ALL_CLOSURE_FUNCS = {}
ALL_CLOSURE_ASTS = {}


def closure_convert(ast: Abt, func_prefix: str):
    match ast:
        case N(NT_StructEntry(label), [cur, next]):
            return N(NT_StructEntry(label), [closure_convert(cur, func_prefix+"::"+label), closure_convert(func_prefix)])
        case N(NT_MultiArgLam(arg_count), [args_body]):
            converted_body = closure_convert(args_body, func_prefix)
            func_unique_name = unique_name(func_prefix, ALL_CLOSURE_FUNCS.keys())
            body_free_vars = collect_free_vars(converted_body)
            block_unique_name = unique_name(func_prefix, body_free_vars)
            substituted_body = substitute_list(
                [N(NT_TupleProj(i), [FreeVar(block_unique_name)]) for i in range(body_free_vars)], 
                body_free_vars, converted_body
            )
            ALL_CLOSURE_FUNCS[func_unique_name] = N(NT_MultiArgLam(arg_count+1), abstract_over_abt(substituted_body, block_unique_name))
            return N(NT_TupleCons, [N(NT_GlobalFuncDecl(func_unique_name), []), N(NT_TupleCons, [FreeVar(n) for n in body_free_vars])])
        case _:
            return map_abt(ast, lambda x: closure_convert(x, func_prefix))



def closure_convert_top_level():
    for file, ast in GLOBAL_INPUT_ASTS.items():
        ALL_CLOSURE_ASTS[file] = closure_convert(ast, file)
    
