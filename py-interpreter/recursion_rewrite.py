from __future__ import annotations
from typing import *

from ast_util import *
from tqdm import tqdm

from pass_utils import *


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
                                recursion_rewrite(cur), 
                                abstract_over_abt(
                                    N(NT_ConsecutiveStmt(), 
                                      [N(NT_UpdateStruct(labels.index(label)), [FreeVar(name), FreeVar(abs_name)]), 
                                      next_rewrite
                                      ]), 
                                    abs_name
                                )
                            ])
            case _:
                raise ValueError("Invalid recursion_rewrite", sub)
    match ast:
        case N(NT_StructRec(labels), [bnd]):
            rec_name, rec_body = unbind_abt(bnd)
            return N(NT_LetIn(), [N(NT_TupleCons(), [N(NT_EmptyVal(), [])]* len(labels)), 
                        abstract_over_abt(recursion_rewrite_sub(rec_body, labels, rec_name), rec_name)])
        case _:
            return map_abt(ast, recursion_rewrite)


@cached_pass("recursion_rewrite")
def recursion_rewrite_top_level(funcs):
    return {name: recursion_rewrite(func) for name, func in tqdm(funcs.items(), desc="Recursion rewrite")}