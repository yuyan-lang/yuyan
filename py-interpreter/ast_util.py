

from __future__ import annotations
from typing import *
from dataclasses import dataclass
import random



CHECK_ARITY = True


@dataclass
class N:
    n: NodeType
    children: List[Abt]

    def __post_init__(self):
        if CHECK_ARITY:
            expected_arity = arity_of_nt(self.n)
            if expected_arity is not None:
                if len(self.children) != len(expected_arity):
                    raise ValueError(f"Arity mismatch on {self.n} with {self.children}")
                for i, (child, expected) in enumerate(zip(self.children, expected_arity)):
                    if get_iterative_binder_length(child) != expected:
                        raise ValueError(f"Arity mismatch on {self.n} with {self.children} at {i}th child {child}")

                    


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

    def __post_init__(self):
        raise ValueError("NTUndef should not be used on " , self.content)

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
class NT_DecimalNumber:
    integral: str
    fractional: str

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

def arity_of_nt(nt: NodeType) -> Optional[List[int]]:
    """
    Arity, 
    e.g. LetIn[a, x.b] -> [0, 1]
    e.g. MultiArgLam(3)[a.b.c.body] -> [3]
    e.g. ExternalCall(name) ->  None (any plain arguments)
    """
    match nt:
        case NT_StructRec(_):
            return [1]
        case NT_StructEntry(_):
            return [0, 0]
        case NT_EmptyStructEntry():
            return []
        case NT_FileRef(_):
            return []
        case NT_EmptyVal():
            return []
        case NT_Builtin(name):
            match name:
                case "内建爻阳":
                    return []
                case "内建爻阴":
                    return []
                case "内建有元":
                    return []
                case "内建函数整数相等":
                    return [0, 0]
                case "内建函数整数减":
                    return [0, 0]
                case "内建函数整数大于":
                    return [0, 0]
                case _:
                    raise ValueError(f"Unknown builtin {name} on {args_val}")
        case NT_TupleProj(_):
            return [0]
        case NT_AnnotatedVar(_):
            return [0]
        case NT_MultiArgLam(arg_count):
            return [arg_count]
        case NT_MultiArgFuncCall(arg_count):
            return [0] * (arg_count + 1)
        case NT_TupleCons():
            return None
        case NT_DataTupleCons(_, _):
            return [0]
        case NT_DataTupleProjIdx():
            return [0]
        case NT_DataTupleProjTuple():
            return [0]
        case NT_IfThenElse():
            return [0, 0, 0]
        case NT_StringConst(_):
            return []
        case NT_IntConst(_):
            return []
        case NT_ExternalCall(_):
            return None
        case NT_LetIn():
            return [0,1]
        case NT_ConsecutiveStmt():
            return [0,0]
        case NT_CallCC():
            return [1]
        case NT_CallCCRet():
            return [0, 0]
        case NT_GlobalFuncRef(_):
            return []
        case NT_GlobalFuncDecl(_):
            return [0]
        case NT_UpdateStruct(_):
            return [0, 0]
        case NT_WriteGlobalFileRef(_):
            return [0]
        case NTUndef(_):
            raise ValueError("NTUndef should not be used")


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
            
def get_iterative_binder_length(abt: Abt) -> int:
    length = 0
    while isinstance(abt, Binding):
        length += 1
        abt = abt.next
    return length