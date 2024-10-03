from __future__ import annotations

import os
import json
from ast_util import *
BASE_PATH = "./.yybuild.v0.1.0rc2+0019.nosync"
DEPS_PATH_SUFFIX = ".文件依赖.json"
OPT_PATH_SUFFIX = ".擦除后形式.json"
# OPT_PATH_SUFFIX = ".优化后形式.json"


GLOBAL_INPUT_ASTS = {}
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

def do_load_files(path):
    do_load_deps(path)
    for i, path in enumerate(ORDERED_FILES):
        assert all(dep in ORDERED_FILES[:i] for dep in DEPS[path])

    for file in ORDERED_FILES:
        json_path = f"{BASE_PATH}/{file}{OPT_PATH_SUFFIX}"
        with open(json_path, "r") as f:
            json_data = json.load(f)
            ast = decode_json_to_ast(json_data)
        GLOBAL_INPUT_ASTS[file] = ast
