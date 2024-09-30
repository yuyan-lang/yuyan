
from ast_util import *
from files_manager import *

ALL_CLOSURE_FUNCS = {}
ALL_CLOSURE_ASTS = {}


def closure_convert(ast: Abt, func_prefix: str):
    match ast:
        case N(NT_StructEntry(label), [cur, next]):
            return N(NT_StructEntry(label), [closure_convert(cur, func_prefix+"::"+label), closure_convert(func_prefix)])
        case N(NT_MultiArgLam(arg_count), [args_body]):
            

def closure_convert_top_level():
    for file, ast in GLOBAL_INPUT_ASTS.items():
        ALL_CLOSURE_ASTS[file] = closure_convert(ast, file)
    
