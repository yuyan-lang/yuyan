

from __future__ import annotations
from typing import List, Optional
from dataclasses import dataclass




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
class NTTupleProj:
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
class NT_CallCC:
    pass


@dataclass
class NT_CallCCRet:
    pass

@dataclass
class NT_FuncRef:
    name: str

NodeType = NTUndef 


Abt = N | FreeVar | BoundVar  | Binding


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
            return NTTupleProj(data["序数"])
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
            return NT_FuncRef(data["函数名"])
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