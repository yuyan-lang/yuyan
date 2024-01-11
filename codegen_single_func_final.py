
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
    parser.add_argument("--deduplicate-text")

    args = parser.parse_args()

    args.deduplicate_text = args.deduplicate_text.split()
    args.code_data = args.code_data.split()

    def get_path(name):
        return args.path_prefix + "." + name + ".json"
    result_file_text = []
    result_file_text.extend(json.load(open(get_path(args.prelude))))

    deduplicated = set()
    for file in args.deduplicate_text:
        for line in json.load(open(get_path(file))):
            deduplicated.add(line)
    result_file_text.extend(list(deduplicated))

    for file in args.code_data:
        result_file_text.extend(json.load(open(get_path(file))))
    
    open(args.path_prefix + ".ll", "w").writelines(result_file_text)

