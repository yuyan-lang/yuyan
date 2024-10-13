import json
from ast_util import *

def name_escape(name: str) -> str:
    # .[];\"():,
    return name.replace(".", "_").replace("[", "_").replace("]", "_").replace(";", "_").replace("\"", "_").replace("(", "_").replace(")", "_").replace(":", "_").replace(",", "_")

def string_escape(s: str) -> str:
    """
    Returns the json string, 
    e.g. stringescape('a"b') -> "a\"b" ("a\\\"b")
    """
    return json.dumps(s, ensure_ascii=False)

def ast_node_to_ir(nt: NodeType) -> str:
    match nt:
        case NT_EmptyStructEntry():
            return "EmptyStructEntry"
        case NT_StructEntry(label):
            return f"StructEntry({string_escape(label) if label is not None else ''})"
        case NT_EmptyVal():
            return "EmptyVal"
        case NT_FileRef(filename):
            return f"FileRef({string_escape(filename)})"
        case NT_Builtin(name):
            return f"Builtin({string_escape(name)})"
        case NT_TupleProj(idx):
            return f"TupleProj({idx})"
        case NT_AnnotatedVar(annotation):
            return f"AnnotatedVar({string_escape(annotation)})"
        case NT_MultiArgLam(arg_count):
            return f"MultiArgLam({arg_count})"
        case NT_MultiArgFuncCall(arg_count):
            return f"MultiArgFuncCall({arg_count})"
        case NT_TupleCons():
            return "TupleCons"
        case NT_DataTupleCons(idx, length):
            return f"DataTupleCons({idx}, {length})"
        case NT_DataTupleProjIdx():
            return "DataTupleProjIdx"
        case NT_DataTupleProjTuple():
            return "DataTupleProjTuple"
        case NT_IfThenElse():
            return "IfThenElse"
        case NT_StringConst(val):
            return f"StringConst({string_escape(val)})"
        case NT_IntConst(val):
            return f"IntConst({val})"
        case NT_ExternalCall(name):
            return f"ExternalCall({string_escape(name)})"
        case NT_LetIn():
            return "LetIn"
        case NT_ConsecutiveStmt():
            return "ConsecutiveStmt"
        case NT_CallCC():
            return "CallCC"
        case NT_CallCCRet():
            return "CallCCRet"
        case NT_GlobalFuncRef(name):
            return f"GlobalFuncRef({string_escape(name)})"
        case NT_GlobalFuncDecl(name):
            return f"GlobalFuncDecl({string_escape(name)})"
        case NT_WriteGlobalFileRef(filename):
            return f"WriteGlobalFileRef({string_escape(filename)})"
        case NT_UpdateStruct(index):
            return f"UpdateStruct({index})"
        case NT_StructRec(labels):
            return f"StructRec({','.join([string_escape(label) for label in labels])})"
        case NT_DecimalNumber(integral, fractional):
            return f"DecimalNumber({string_escape(integral)},{string_escape(fractional)})"
        case _:
            raise ValueError(f"Unknown NodeType {nt}")


def ast_to_ir(ast: Abt, pretty: bool=False) -> str:
    match ast:
        case FreeVar(name):
            assert ("." not in name), f"FreeVar name {name} contains '.'"
            assert name.strip() == name, f"FreeVar name {name} has leading/trailing whitespace"
            return name_escape(name)
        case BoundVar(idx):
            return str(idx)
        case N(n, children):
            return f"{ast_node_to_ir(n)}[{'; '.join([ast_to_ir(child, pretty) for child in children])}]"
        case Binding(name, next):
            return (name_escape(name) if name else "_") + "." + ast_to_ir(next, pretty)

def asts_to_ir(asts: Dict[str, Abt], pretty: bool=False) -> str:
    return "\n".join([f"{string_escape(name)}: {ast_to_ir(ast, pretty)}" for name, ast in asts.items()])
