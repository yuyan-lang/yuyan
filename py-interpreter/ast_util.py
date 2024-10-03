

from __future__ import annotations
from typing import List, Optional, Callable, Tuple
from dataclasses import dataclass
import random




@dataclass
class N:
    n: NodeType
    children: List[Abt]

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
        | NT_UpdateStruct
)



Abt = N | FreeVar | BoundVar  | Binding

def map_abt(abt: Abt, f: Callable[[Abt], Abt]):
    match abt:
        case N(n, children):
            return N(n, [map_abt(child, f) for child in children])
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
    
def abstract_over_abt_list(abt: Abt, var_names: List[str]) -> Binding:
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

def decode_json_to_node_type(data: dict) -> NodeType:
    if isinstance(data, str):
        match data:
            case '空值节点':
                return NT_EmptyVal()
            case '空结构节点':
                return NT_EmptyStructEntry()
            case '元组构造节点':
                return NT_TupleCons()
            case '爻分支节点':
                return NT_IfThenElse()
            case '内联虑':
                return NT_LetIn()
            case '顺序执行节点':
                return NT_ConsecutiveStmt()
            case '唯一构造器序数元组投影序数节点':
                return NT_DataTupleProjIdx()
            case '唯一构造器序数元组投影元组节点':
                return NT_DataTupleProjTuple()
            case '续延调用节点':
                return NT_CallCC()
            case '续延调用返回节点':
                return NT_CallCCRet()
            case _:
                return NTUndef(data)
    match data["名称"]:
        case '结构递归节点':
            return NT_StructRec(data["串"])
        case '结构节点':
            return NT_StructEntry(data["标签名"])
        case '文件引用节点':
            return NT_FileRef(data["串"])
        case '展开后内建节点':
            return NT_Builtin(data["常量"])
        case '元组解构节点':
            return NT_TupleProj(data["序数"])
        case '自由变量标注节点':
            return NT_AnnotatedVar(data["投影名"])
        case '字符串节点':
            return NT_StringConst(data["串"])
        case '拉姆达抽象':
            form = data["参式"]
            match form['名称']:
                case '多参数形':
                    return NT_MultiArgLam(form["个数"])
                case _:
                    raise ValueError(f"Unknown lambda form {form['名称']}")
        case '函数调用':
            form = data["参式"]
            match form['名称']:
                case '多参数形':
                    return NT_MultiArgFuncCall(form["个数"])
                case _:
                    raise ValueError(f"Unknown call form {form['名称']}")
        case '唯一构造器序数元组节点':
            return NT_DataTupleCons(data["序数"], data["元组长"])
        case '解析后外部调用节点无类型':
            return NT_ExternalCall(data["串"])
        case '整数节点':
            return NT_IntConst(data["数"])
        case '函数引用节点':
            return NT_GlobalFuncRef(data["函数名"])
        case '函数全局声明节点':
            return NT_GlobalFuncDecl(data["函数名"])
        case _:
            return NTUndef(data)


        
            
        


def decode_json_to_ast(data: dict) -> Abt:
    if isinstance(data, int):
        return BoundVar(data)
    match data["类型"]:
        case "式节点":
            return N(decode_json_to_node_type(data["节点名称"]), [decode_json_to_ast(child) for child in data["参数"]])
        case "绑定":
            return Binding(data["可能名"], decode_json_to_ast(data["绑定体"]))
        case _:
            raise ValueError(f"Unknown type {data['类型']}")