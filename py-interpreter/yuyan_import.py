from __future__ import annotations

import os
import json
from ast_util import *
BASE_PATH = "./.yybuild.v0.1.0rc2+0019.nosync"
DEPS_PATH_SUFFIX = ".文件依赖.json"
OPT_PATH_SUFFIX = ".擦除后形式.json"
# OPT_PATH_SUFFIX = ".优化后形式.json"


# GLOBAL_INPUT_ASTS = {}
DEPS = {}

ORDERED_FILES = []

def file_path_to_key(path):
    # if path begins with cwd (absolute), remove it
    cwd = os.getcwd()
    if path.startswith(cwd):
        # Remove cwd from path and return the rest
        path = path[len(cwd):].lstrip(os.sep)
    # if path ends with 。豫, remove it
    if path.endswith("。豫"):
        path = path[:-len("。豫")]
    return path

def do_load_deps(path):
    path = file_path_to_key(path)
    if path in DEPS:
        return
    dep_json_path = f"{BASE_PATH}/{path}{DEPS_PATH_SUFFIX}"
    with open(dep_json_path, "r") as f:
        dep_json = json.load(f)
        DEPS[path] = [file_path_to_key(dep) for dep in dep_json]
    for dep in DEPS[path]:
        do_load_deps(dep)
    ORDERED_FILES.append(path)

def do_load_files(path) -> Abts:
    do_load_deps(path)
    for i, path in enumerate(ORDERED_FILES):
        assert all(dep in ORDERED_FILES[:i] for dep in DEPS[path])
    result = {}
    for file in ORDERED_FILES:
        json_path = f"{BASE_PATH}/{file}{OPT_PATH_SUFFIX}"
        with open(json_path, "r") as f:
            json_data = json.load(f)
            ast = decode_json_to_ast(json_data)
        result[file] = ast
    return result


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