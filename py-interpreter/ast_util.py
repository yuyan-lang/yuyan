

from __future__ import annotations
from typing import *
from dataclasses import dataclass
import random




@dataclass
class N:
    n: NodeType
    children: List[Abt]

    def __post_init__(self):
        match self.n:
            case NT_LetIn():
                assert len(self.children) == 2

@dataclass
class FreeVar:
    name: str 

@dataclass
class BoundVar:
    idx: int

@dataclass
class Binding:
    name: str
    next: Abt

@dataclass
class NTUndef:
    content: str

@dataclass
class NT_StructRec:
    proj_labels: List[str]

@dataclass
class NT_StructEntry:
    label: Optional[str]

@dataclass
class NT_EmptyStructEntry:
    pass

@dataclass
class NT_FileRef:
    filename: str

@dataclass
class NT_EmptyVal:
    pass

@dataclass
class NT_Builtin:
    name: str

@dataclass
class NT_TupleProj:
    idx: int

@dataclass
class NT_AnnotatedVar:
    annotation: str

@dataclass
class NT_MultiArgLam:
    arg_count: int

@dataclass
class NT_MultiArgFuncCall:
    arg_count: int

@dataclass
class NT_TupleCons:
    pass

@dataclass
class NT_DataTupleCons:
    idx: int
    length: int

@dataclass
class NT_DataTupleProjIdx:
    pass

@dataclass
class NT_DataTupleProjTuple:
    pass

@dataclass
class NT_IfThenElse:
    pass

@dataclass
class NT_StringConst:
    val: str

@dataclass
class NT_IntConst:
    val: int

@dataclass
class NT_ExternalCall:
    name: str

@dataclass
class NT_LetIn:
    pass

@dataclass
class NT_ConsecutiveStmt:
    pass

@dataclass
class NT_CallCC:
    pass


@dataclass
class NT_CallCCRet:
    pass

@dataclass
class NT_GlobalFuncRef:
    name: str

@dataclass
class NT_GlobalFuncDecl:
    name: str

@dataclass
class NT_WriteGlobalFileRef:
    filename: str

@dataclass
class NT_UpdateStruct:
    index: int

NodeType = (NTUndef | NT_StructRec | NT_StructEntry | NT_EmptyStructEntry | NT_FileRef | NT_EmptyVal | NT_Builtin 
        | NT_TupleProj | NT_AnnotatedVar | NT_MultiArgLam | NT_MultiArgFuncCall | NT_TupleCons | NT_DataTupleCons | NT_DataTupleProjIdx | NT_DataTupleProjTuple | NT_IfThenElse | NT_StringConst | NT_IntConst | NT_ExternalCall | NT_LetIn | NT_CallCC | NT_CallCCRet | NT_GlobalFuncRef | NT_GlobalFuncDecl
        | NT_UpdateStruct | NT_ConsecutiveStmt | NT_WriteGlobalFileRef
)



Abt = N | FreeVar | BoundVar  | Binding
Abts = Dict[str, Abt]

def map_abt(abt: Abt, f: Callable[[Abt], Abt]):
    match abt:
        case N(n, children):
            return N(n, [f(child) for child in children])
        case FreeVar(_):
            return abt
        case BoundVar(_):
            raise ValueError("Cannot map over BoundVar")
        case Binding(name, next):
            var_name, body_next = unbind_abt(abt)
            # print("Unbinding", var_name, body_next, abt)
            return abstract_over_abt(f(body_next), var_name)


def collect_free_vars(abt: Abt) -> List[str]:
    def traverse(abt: Abt) -> List[str]:
        match abt:
            case N(_, children):
                return sum([collect_free_vars(child) for child in children], [])
            case FreeVar(name):
                return [name]
            case BoundVar(_):
                return []
            case Binding(_, next):
                return collect_free_vars(next)
    result = traverse(abt)
    # deduplicate
    return list(set(result))

def unique_name(reference: str, existing: List[str]) -> str:
    name = reference
    while name in existing:
        name += random.choice("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
    return name


def unbind_abt(abt: Binding) -> Tuple[str, Abt]:
    var_name = unique_name(abt.name, collect_free_vars(abt.next))
    def traverse(abt: Abt, idx : int) -> Abt:
        match abt:
            case N(n, children):
                return N(n, [traverse(child, idx) for child in children])
            case FreeVar(name):
                return abt
            case BoundVar(i):
                if i == idx:
                    return FreeVar(var_name)
                else:
                    return abt
            case Binding(_, next):
                return Binding(abt.name, traverse(abt.next, idx + 1))
    return var_name, traverse(abt.next, 1)

def unbind_abt_list(abt: Binding, num: Optional[int]) -> Tuple[List[str], Abt]:
    var_names = []
    while isinstance(abt, Binding):
        var_name, abt = unbind_abt(abt)
        var_names.append(var_name)
    if num is not None:
        assert len(var_names) == num
    return var_names, abt

global_unique_name_counter = 0
global_unique_name_suffix = "_u"

def construct_binding(reference_name: str, body_cons: Callable[[str], Abt]) -> Abt:
    global global_unique_name_counter
    global_unique_name_counter += 1
    reference_name = reference_name + global_unique_name_suffix + str(global_unique_name_counter)
    return abstract_over_abt(body_cons(reference_name), reference_name)

def abstract_over_abt(abt: Abt, var_name: str) -> Binding:
    def traverse(abt: Abt, idx: int) -> Abt:
        match abt:
            case N(n, children):
                return N(n, [traverse(child, idx) for child in children])
            case FreeVar(name):
                if name == var_name:
                    return BoundVar(idx)
                else:
                    return abt
            case BoundVar(_):
                return abt
            case Binding(_, next):
                return Binding(abt.name, traverse(abt.next, idx + 1))
    return Binding(var_name, traverse(abt, 1))
    
def abstract_over_abt_list(abt: Abt, var_names: List[str]) -> Abt:
    while len(var_names) > 0:
        abt = abstract_over_abt(abt, var_names.pop())
    return abt


def substitute(replacement: Abt, var_name: str, abt: Abt) -> Abt:
    def traverse(abt: Abt) -> Abt:
        match abt:
            case N(n, children):
                return N(n, [traverse(child) for child in children])
            case FreeVar(name):
                if name == var_name:
                    return replacement
                else:
                    return abt
            case BoundVar(_):
                return abt
            case Binding(_, next):
                return Binding(abt.name, traverse(abt.next))
    return traverse(abt)

def substitute_list(replacements: List[Abt], var_names: List[str], abt: Abt) -> Abt:
    while len(replacements) > 0:
        abt = substitute(replacements.pop(), var_names.pop(), abt)
    return abt

def instantiate(replacement: Abt, abt: Binding) -> Abt:
    var_name, body = unbind_abt(abt)
    return substitute(replacement, var_name, body)






def find_all_file_refs(ast: Abt) -> List[str]:
    match ast:
        case N(NT_FileRef(filename), _):
            return [filename]
        case N(_, children):
            return sum([find_all_file_refs(child) for child in children], [])
        case Binding(_, next):
            return find_all_file_refs(next)
        case _:
            return []

def find_all_sub_abts_by_predicate(ast: Abt, predicate: Callable[[Abt], bool]) -> List[Abt]:
    if predicate(ast):
        return [ast]
    else:
        match ast:
            case N(_, children):
                return sum([find_all_sub_abts_by_predicate(child, predicate) for child in children], [])
            case Binding(_, next):
                return find_all_sub_abts_by_predicate(next, predicate)
            case FreeVar(_):
                return []
            case BoundVar(_):
                return []