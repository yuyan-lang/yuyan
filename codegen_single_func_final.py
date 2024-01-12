
import argparse
import subprocess
from multiprocessing import cpu_count
from multiprocessing.pool import ThreadPool
from concurrent.futures import ThreadPoolExecutor
import sys
import os
from typing import *    

import queue
import threading
import multiprocessing
import subprocess
import json
import pprint
import re
from datetime import datetime


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("--path-prefix")
    parser.add_argument("--prelude")
    parser.add_argument("--code-data")
    parser.add_argument("--global-data")
    parser.add_argument("--deduplicate-text")

    args = parser.parse_args()


    def get_path(name):
        return args.path_prefix + "." + name + ".json"
    def open_get_path(name):
        if isinstance(name, str):
            with open(get_path(name)) as f:
                return json.load(f)
        else:
            raise ValueError("Unrecognized name: ", (name))

    result_file_text = []
    result_file_text.extend((open_get_path(args.prelude)))

    deduplicated = set()
    for file in (open_get_path(args.deduplicate_text)):
        for line in (open_get_path(file)):
            deduplicated.add(line)
    result_file_text.extend(list(deduplicated))

    for file in (open_get_path(args.global_data)):
        result_file_text.extend((open_get_path(file)))

    for file in (open_get_path(args.code_data)):
        with open(get_path(file)) as f:
            result_file_text.append(f.read())
    
    open(args.path_prefix + ".ll", "w").writelines(result_file_text)

