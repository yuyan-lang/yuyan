from __future__ import annotations

from ast_util import *
from files_manager import *
from tqdm import tqdm
import pickle
ALL_CLOSURE_FUNCS = {}
# ALL_CLOSURE_ASTS = {}


def closure_convert(ast: Abt, func_prefix: str):
    match ast:
        case N(NT_StructEntry(label), [cur, next]):
            if label is None:
                label = "__"
            return N(NT_StructEntry(label), [closure_convert(cur, func_prefix+"::"+label), closure_convert(next, func_prefix)])
        case N(NT_MultiArgLam(arg_count), [args_body]):
            converted_body = closure_convert(args_body, func_prefix)
            func_unique_name = unique_name(func_prefix, ALL_CLOSURE_FUNCS.keys())
            body_free_vars = collect_free_vars(converted_body)
            block_unique_name = unique_name(func_prefix, body_free_vars)
            substituted_body = substitute_list(
                [N(NT_TupleProj(i), [FreeVar(block_unique_name)]) for i in range(len(body_free_vars))], 
                body_free_vars, converted_body
            )
            ALL_CLOSURE_FUNCS[func_unique_name] = N(NT_MultiArgLam(arg_count+1), [abstract_over_abt(substituted_body, block_unique_name)])
            return N(NT_TupleCons, [N(NT_GlobalFuncDecl(func_unique_name), []), N(NT_TupleCons, [FreeVar(n) for n in body_free_vars])])
        case _:
            return map_abt(ast, lambda x: closure_convert(x, func_prefix))

def recursion_rewrite(ast: Abt):
    def recursion_rewrite_sub(sub: Abt, labels: List[str], name: str):
        match sub:
            case N(NT_EmptyStructEntry(), []):
                return FreeVar(name)
            case N(NT_StructEntry(label), [cur, next]):
                if label is None:
                    return N(NT_ConsecutiveStmt(), [recursion_rewrite(cur), recursion_rewrite_sub(next, labels, name)])
                else:
                    next_rewrite = recursion_rewrite_sub(next, labels, name)
                    abs_name = unique_name("rec_rewrite", collect_free_vars(next_rewrite))
                    return N(NT_LetIn(), [
                            N(NT_LetIn(), [
                                recursion_rewrite(cur), 
                                abstract_over_abt(
                                    N(NT_ConsecutiveStmt(), 
                                      [N(NT_UpdateStruct(labels.index(label)), [FreeVar(name), FreeVar(abs_name)]), 
                                      next_rewrite
                                      ]), 
                                    abs_name
                                )
                            ])
                        ])
            case _:
                raise ValueError("Invalid recursion_rewrite", sub)
    match ast:
        case N(NT_StructRec(labels), bnd):
            rec_name, rec_body = unbind_abt(bnd)
            return N(NT_LetIn(), [N(NT_TupleCons, [N(NT_EmptyVal(), [])]* len(labels))], 
                        abstract_over_abt(recursion_rewrite_sub(rec_body, labels, rec_name), rec_name))


def closure_convert_top_level():
    if os.path.exists("closure_funcs.pickle"):
        funcs = pickle.load(open("closure_funcs.pickle", "rb"))
        for name, func in funcs.items():
            ALL_CLOSURE_FUNCS[name] = func
        return

    for file in tqdm(ORDERED_FILES, desc="Closure conversion"):
        ast = GLOBAL_INPUT_ASTS[file]
        ALL_CLOSURE_FUNCS[file + "_init"] = N(NT_MultiArgLam(0), 
            [N(NT_WriteGlobalFileRef(file), [closure_convert(ast, file)])])
    init_ast = N(NT_EmptyVal(), [])
    for file in reversed(ORDERED_FILES):
        init_ast = N(NT_ConsecutiveStmt(), [
            N(NT_MultiArgFuncCall(0), [N(NT_GlobalFuncRef(file + "_init"), [])]),
            init_ast])

    ALL_CLOSURE_FUNCS["entryMain"] = N(NT_MultiArgLam(0), [init_ast])

    pickle.dump(ALL_CLOSURE_FUNCS, open("closure_funcs.pickle", "wb"))
    
