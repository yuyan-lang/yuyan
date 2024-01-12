
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


LINUX_PARALLEL_FACTOR = .8
DEFAULT_PARALLEL_FACTOR = 1.0
yy_bs_global_args = []
yy_bs_main_file = None

STG_DEPENDENCY_ANALYSIS = "dependency-analysis"
STG_PARSE = "parse"
STG_TYPE_CHECK = "type-check"
STG_TYPE_CHECK_AND_ERASE = "type-check-and-erase" # this is duplicate work, type check is duplicated, but erase takes long, so worth paying extra
STG_TYPE_CHECK_AND_ERASE_THROUGH_CODEGEN = "type-check-and-erase-through-codegen" # this is duplicate work, type check is duplicated, but erase takes long, so worth paying extra
STG_PRE_CLOSURE_CONVERT = "pre-closure-convert"
STG_ANF = "anf"
STG_TYPE_CHECK_ERASE_CLO_CONV_SINGLE_FUNC = "type-check-erase-clo-conv-single-func" # this is duplicate work, type check is duplicated, but erase takes long, so worth paying extra
STG_ANF_AND_PRE_CODEGEN_SINGLE_FUNC = "anf-and-pre-codegen-single-func"
STG_CODEGEN_SINGLE_FUNC = "codegen-single-func"
STG_CODEGEN_SINGLE_FUNC_FINAL = "codegen-single-func-final"

# STG_OPTIMIZE_HALF = "optimize-half"
# STG_CPS_TRANSFORM = "cps-transform"
# STG_CLOSURE_CONVERT = "closure-convert"
# STG_CLOSURE_OPT = "closure-opt"
STG_PRE_CODEGEN = "pre-codegen"
STG_ALL_CODEGEN = "all-codegen"
STG_CODEGEN = "codegen"

log_file = open(".yybuild.nosync/yy_parallel_log.txt", "w+")


num_cpu_limit = None
stages = [STG_DEPENDENCY_ANALYSIS, 
          STG_PARSE, 
          STG_TYPE_CHECK, 
          ]
# stage_concurrency_limit = [100 for s in stages]
# stage_processing_order = [stages.index(STG_DEPENDENCY_ANALYSIS),
#                         stages.index(STG_PARSE),
#                         stages.index(STG_TYPE_CHECK),
#                         stages.index(STG_TYPE_CHECK_AND_ERASE),
#                         stages.index(STG_PRE_CLOSURE_CONVERT),
#                         stages.index(STG_ANF),
#                         stages.index(STG_PRE_CODEGEN),
#                         stages.index(STG_CODEGEN),
#                         # stages.index(STG_CLOSURE_CONVERT),
#                         # stages.index(STG_OPTIMIZE_HALF),
#                         # stages.index(STG_CPS_TRANSFORM),
#                         # stages.index(STG_CLOSURE_OPT),
#                          ] # process parse -> tc -> codegen  -> optimize-half -> cps
# def worker(task):
#     command = ["./yy_bs", "--mode=worker", "--worker-task=" + task[0]] + task[1] + yy_bs_global_args
#     print("" + " ".join(command))
#     process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
#     if process.returncode != 0:
#         return None, f"Error during {task[0]} on {task[1]}: {process.stderr.decode('utf-8')}"
#     else:
#         return task
def process_pp_dictionary(original_dict, show_summary = True):
    new_dict = {}

    for key, value in original_dict.items():
        new_dict[key] = []
        for item in value:
            if isinstance(item, tuple):
                tuple_key, *tuple_value = item
                if len(tuple_value) == 1 or show_summary:
                    if (any((tk[0] == tuple_key) for tk in new_dict[key])):
                        continue
                    else:
                        new_dict[key].append((tuple_key, len([v[1] for v in value if v[0] == tuple_key])))
                elif len(tuple_value) == 2:
                    if not (any(tk[0] == tuple_key for tk in new_dict[key])):
                        new_dict[key].append([tuple_key, {}])
                    index = next(i for i, v in enumerate(new_dict[key]) if v[0] == tuple_key)
                    file_name, *_ = tuple_value
                    if file_name not in new_dict[key][index][1]:
                        names = ([v[2] for v in value if v[0] == tuple_key and v[1] == file_name])
                        if len(names) <= 3:
                            new_dict[key][index][1][file_name] = names
                        else:
                            new_dict[key][index][1][file_name] = len(names)
                else:
                    raise ValueError("Error: tuple length is not 1 or 2", item)
                    
            else:
                new_dict[key].append(item)

    return {k : [len(v), new_dict[k]] for k, v in original_dict.items()}


def get_function_names(file: str):
    command = ["./yy_bs", "--mode=worker", "--worker-task=get-function-names"] + [file] + yy_bs_global_args
    print("" + " ".join(command))
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    if process.returncode != 0:
        print("ERROR OCCURRED")
        print(" ".join(command), stdout.decode(), stderr.decode(), sep="\n")
        os.abort()
    else:
        return stdout.decode().split()

def get_block_names(file, function_name):
    command = ["./yy_bs", "--mode=worker", "--worker-task=get-block-names", f"-Dfunction_name={function_name}"] + [file] + yy_bs_global_args
    # print("" + " ".join(command))
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    if process.returncode != 0:
        print("ERROR OCCURRED")
        print(" ".join(command), stdout.decode(), stderr.decode(), sep="\n")
        os.abort()
    else:
        return stdout.decode().split()

def extract_block_name(strings):
    pattern = re.compile(r'-Dblock_name=([^,]+)')

    func_names = []
    for string in strings:
        match = pattern.search(string)
        if match:
            func_names.append(match.group(1))

    if len(func_names) == 1:
        return func_names[0]
    else:
        raise ValueError("Error: None or multiple block names found.", func_names, strings)

def extract_func_name(strings):
    pattern = re.compile(r'-Dfunction_name=([^,]+)')

    func_names = []
    for string in strings:
        match = pattern.search(string)
        if match:
            func_names.append(match.group(1))

    if len(func_names) == 1:
        return func_names[0]
    else:
        raise ValueError("Error: None or multiple function names found.", func_names, strings)

def exec_worker(args):
    command = ["./yy_bs", "--mode=worker", "--worker-task=exec-gen"] + args + yy_bs_global_args
    print("" + " ".join(command))
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    log_file.write(datetime.now().strftime("%Y-%m-%d %H:%M:%S") + "\n"+ " ".join(command) + "\nSTDOUT: \n" + stdout.decode() + "\nSTDERR: \n" + stderr.decode() + "\nRET: \n" + str(process.returncode) + "\n")
    if process.returncode != 0:
        return args, f"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \
                      f"\nError during exec-fun on {args[0]}: \n{' '.join(command)}\nstderr:\n{stderr.decode('utf-8')}\nstdout:{stdout.decode('utf-8')}\nexit code:{process.returncode}"
    else:
        print(stdout.decode('utf-8'))
        return args, None

def worker(task, retry_count=0):
    stage, file_and_args = task
    command = ["./yy_bs", "--mode=worker", "--worker-task=" + (stage)] + file_and_args + yy_bs_global_args 
    # print("" + " ".join(command))
    process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    log_file.write(datetime.now().strftime("%Y-%m-%d %H:%M:%S") + "\n"+ " ".join(command) + "\nSTDOUT: \n" + process.stdout.decode() + "\nSTDERR: \n" + process.stderr.decode() + "\nRET: \n" + str(process.returncode) + "\n")
    if process.returncode != 0:
        print(f"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \
                      f"\nError during {task[0]} on {task[1][0]}: \n{' '.join(command)}\n stderr is: \n{process.stderr.decode('utf-8')}\nstdout:{process.stdout.decode('utf-8')}\nexit code:{process.returncode}")
        if retry_count >= 0:
            print("" + " ".join(command))
            os.abort()
            return (task, process.stdout.decode().split()), (f"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \
                      f"\nError during {task[0]} on {task[1][0]}: \n{' '.join(command)}\n stderr is: \n{process.stderr.decode('utf-8')}\nstdout:{process.stdout.decode('utf-8')}\nexit code:{process.returncode}")
        else:
            return worker(task, retry_count=retry_count + 1)
    else:
        # print("Completed")
        # print("" + " ".join(command))
        # print(process.stdout.decode())
        return (task, process.stdout.decode().split()), None

# def dependency_analysis(file):
#     command = ["./yy_bs", "--mode=worker", "--worker-task=dependency-analysis"] + [file] + yy_bs_global_args
#     print("" + " ".join(command))
#     process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

#     if process.returncode != 0:
#         return None, f"Error during dependency analysis: {process.stderr.decode('utf-8')}"
#     else:
#         dependencies = process.stdout.decode().split()
        
#     return dependencies, None

# def build_dep_graph(input_file):
#     graph = {input_file: []}
    
#     to_visit = [input_file]

#     while to_visit:
#         current_file = to_visit.pop()
#         current_deps, error = dependency_analysis(current_file)

#         if error is not None:
#             return None, error

#         for dep in current_deps:
#             if dep not in graph:
#                 graph[dep] = []
#                 to_visit.append(dep)
        
#         graph[current_file] = current_deps


#     return graph, None

def execute_plan():
    completed = {k : [] for k in stages}
    executing = {k : [] for k in stages}
    scheduled = {k : [] for k in stages}
    errored = {k : [] for k in stages}
    error_msgs = []

    deps = {}
    deps_to_process = [yy_bs_main_file]

    results_ready = multiprocessing.Manager().Event()

    
    function_names : Dict[str, List[str]] = {}
    block_names : Dict[str, Dict[str, List[str]]] = {}


    def convert_to_override_list(input_list):
        override_list = []
        for item in input_list:
            override_list.extend(["--override-add-file-dependency", item])
        return override_list

    def get_exec_args():
        exec_args = []
        assert deps_to_process == [], "expecting all dependencies to be processed when generating executables"
        candidate = [k for k in deps.keys() if k != yy_bs_main_file]
        while len(exec_args) < len(candidate):
            for k in candidate:
                if k not in exec_args and all(dep in exec_args for dep in deps[k]):
                    exec_args.append(k)
        return exec_args

    def get_file_args(cur_filename):
        if cur_filename in deps:
            return [cur_filename] + convert_to_override_list(
                # graph[cur_filename] if cur_filename != yy_bs_main_file else 
                list(deps[cur_filename]) if cur_filename != yy_bs_main_file else get_exec_args())
        else:
            return [cur_filename]

    def update_schedule():
        nonlocal scheduled, completed, deps_to_process, deps
        print("Updating schedule...")

        for file in deps_to_process + list(deps.keys()):
            for i, stage in enumerate(stages):
                if stage == STG_ANF_AND_PRE_CODEGEN_SINGLE_FUNC or stage == STG_CODEGEN_SINGLE_FUNC or stage == STG_CODEGEN_SINGLE_FUNC_FINAL:
                    continue
                if (
                    file not in scheduled[stage] and 
                    file not in executing[stage] and
                    file not in completed[stage] and 
                    file not in errored[stage] and
                    ((file not in deps and i == stages.index(STG_DEPENDENCY_ANALYSIS)) or 
                        (file in deps 
                            and (all(dep in completed[stage] for dep in deps[file]) 
                                or (i > stages.index(STG_TYPE_CHECK)) 
                                ) 
                            and  (all(file in completed[prev_stage] for prev_stage in stages[:i]))
                        )
                    )
                ):
                    # vvv outdated
                    #TODO: optimize for main file need to wait for all opt file to finish
                    # if (file == yy_bs_main_file
                    #     # and i == stages.index(STG_OPTIMIZE_HALF)
                    #     and (not all(file in completed[stage] for file in get_exec_args()))):
                    #     continue
                    scheduled[stage].append(file)
                    if (stage == STG_TYPE_CHECK and i+1 < len(stages) and 
                        (stages[i+1] == STG_TYPE_CHECK_AND_ERASE 
                         or stages[i+1] == STG_TYPE_CHECK_AND_ERASE_THROUGH_CODEGEN
                         or stages[i+1] == STG_TYPE_CHECK_ERASE_CLO_CONV_SINGLE_FUNC
                         )):
                        scheduled[stages[i+1]].append(file)

    update_schedule()

    def process_result(future):
        nonlocal results_ready, deps, deps_to_process
        (result, error) = future.result()
        (comp_stage, [comp_file, *extra_args]), out_lines = result
        if error:
            errored[comp_stage].append(comp_file)
            error_msgs.append(error)
        else:
            if comp_stage == STG_DEPENDENCY_ANALYSIS:
                deps[comp_file] = out_lines
                deps_to_process.remove(comp_file)
                for f in out_lines:
                    if f not in deps and f not in deps_to_process:
                        deps_to_process.append(f)
                completed[comp_stage].append(comp_file)
                executing[comp_stage].remove(comp_file)
            elif comp_stage == STG_TYPE_CHECK_ERASE_CLO_CONV_SINGLE_FUNC:
                function_names[comp_file] = get_function_names(comp_file)
                if STG_ANF_AND_PRE_CODEGEN_SINGLE_FUNC in scheduled:
                    scheduled[STG_ANF_AND_PRE_CODEGEN_SINGLE_FUNC].extend([(comp_file, f) for f in function_names[comp_file]])
                completed[comp_stage].append(comp_file)
                executing[comp_stage].remove(comp_file)
            elif comp_stage == STG_ANF_AND_PRE_CODEGEN_SINGLE_FUNC:
                if comp_file not in block_names:
                    block_names[comp_file] = {}
                function_name = extract_func_name(extra_args)
                block_names[comp_file][function_name] = get_block_names(comp_file, function_name)
                scheduled[STG_CODEGEN_SINGLE_FUNC].extend([(comp_file, function_name, f) for f in block_names[comp_file][function_name]])
                completed[comp_stage].append((comp_file, function_name))
                executing[comp_stage].remove((comp_file, function_name))
            elif comp_stage == STG_CODEGEN_SINGLE_FUNC:
                function_name = extract_func_name(extra_args)
                block_name = extract_block_name(extra_args)
                completed[comp_stage].append((comp_file, function_name, block_name))
                executing[comp_stage].remove((comp_file, function_name, block_name))
                if all((comp_file, f) in completed[STG_ANF_AND_PRE_CODEGEN_SINGLE_FUNC] for f in function_names[comp_file])\
                    and all((comp_file, f, b) in completed[STG_CODEGEN_SINGLE_FUNC] for f in function_names[comp_file] for b in block_names[comp_file][f]):
                    scheduled[STG_CODEGEN_SINGLE_FUNC_FINAL].append(comp_file)
            else:
                completed[comp_stage].append(comp_file)
                executing[comp_stage].remove(comp_file)
                print("completed", comp_stage, comp_file)
        results_ready.set()
    def print_stat():
        scheduled_pp = process_pp_dictionary(scheduled)
        executing_pp = process_pp_dictionary(executing, show_summary=False)
        pprint.pprint("=======================================")
        pprint.pprint("=======================================")
        pprint.pprint("============== Scheduled ==============")
        pprint.pprint(scheduled_pp, sort_dicts=False, compact=True)
        pprint.pprint("============== Executing ==============")
        pprint.pprint(executing_pp, sort_dicts=False, compact=True)
        pprint.pprint("============== Completed ==============")
        pprint.pprint({k: len(v) for k, v in completed.items()}, sort_dicts=False)
        pprint.pprint("============== Errored ==============")
        pprint.pprint(errored, sort_dicts=False)
        print(error_msgs)
        pprint.pprint("=======================================")
        pprint.pprint("=======================================")
        pprint.pprint("=======================================")


    with ThreadPoolExecutor(max_workers=num_cpu_limit) as executor:
        while any(len(stg) > 0 for stg in scheduled.values()) or any(len(stg) > 0 for stg in executing.values()):
            for i in range(len(stages)):
                stage = stages[i]
                while (scheduled[stage] 
                    and sum(len(stg) for stg in executing.values()) < num_cpu_limit
                    ):
                    if stage == STG_ANF_AND_PRE_CODEGEN_SINGLE_FUNC:
                        file_name, function_name = scheduled[stage].pop()
                        print("Scheduling", (stage, file_name, function_name))
                        executing[stage].append((file_name, function_name))
                        executor.submit(worker, (stage, get_file_args(file_name) + ["-Dfunction_name=" + function_name])).add_done_callback(process_result)
                    elif stage == STG_CODEGEN_SINGLE_FUNC:
                        file_name, function_name, block_name = scheduled[stage].pop()
                        print("Scheduling", (stage, file_name, function_name, block_name))
                        executing[stage].append((file_name, function_name, block_name))
                        executor.submit(worker, (stage, get_file_args(file_name) + ["-Dfunction_name=" + function_name, "-Dblock_name=" + block_name])).add_done_callback(process_result)
                    else:
                        file_name = scheduled[stage].pop()
                        print("Scheduling", (stage, file_name))
                        executing[stage].append(file_name)
                        executor.submit(worker, (stage, get_file_args(file_name))).add_done_callback(process_result)
            print_stat()
            print("Waiting for updates...")
            results_ready.wait()
            results_ready.clear()
            update_schedule()
    
    print_stat()
    for file in deps_to_process:
        print(f"File {file} is due for dependency-analysis but not done")
    for file in deps.keys():
        for i, stage in enumerate(stages):
            if (file not in completed[stage]):
                if not all(dep in completed[stage] for dep in deps[file]):
                    print(f"File {file} is due for {stage} but not all dependencies are completed: {[dep for dep in deps[file] if dep not in completed[stage]]}")
                if not all(file in completed[prev_stage] for prev_stage in stages[:i]):
                    print(f"File {file} is due for {stage} but not all previous stages are completed: {[prev_stage for prev_stage in stages[:i] if file not in completed[prev_stage]]}")



    t, error = exec_worker([yy_bs_main_file, *convert_to_override_list(get_exec_args())])
    if error:
        print(error)
        os._exit(1)
    return "Compilation finished successfully"



if __name__ == "__main__":

    num_cpu = cpu_count()
    if sys.platform == "linux" and num_cpu > 8:
        default_cpu_limit = num_cpu / LINUX_PARALLEL_FACTOR
    else:
        default_cpu_limit = num_cpu / DEFAULT_PARALLEL_FACTOR
    # Create parser, add arguments and parse them
    parser = argparse.ArgumentParser()
    parser.add_argument("input_file")
    # parser.add_argument("--cache-file", help="Specify a local JSON file to cache the dependency graph.")
    parser.add_argument("-j", "--num-cpu", type=int, default=default_cpu_limit, help="Number of CPU cores to use for compilation")
    parser.add_argument("--extra", default=None)
    parser.add_argument("--codegen-concurrency-limit", default=100)

    args = parser.parse_args()

    print(args)
    if args.extra:
        yy_bs_global_args = args.extra.split(" ")


    if "--type-check-only" in yy_bs_global_args:
        pass
    elif "--type-check-and-erase-only" in yy_bs_global_args:
        if "--very-parallel" in yy_bs_global_args:
            stages.extend([STG_TYPE_CHECK_ERASE_CLO_CONV_SINGLE_FUNC])
        else:
            raise ValueError("Error: --type-check-and-erase-only is not supported without --very-parallel")
    elif "--debug" in yy_bs_global_args:
          stages.extend([STG_TYPE_CHECK_AND_ERASE, 
            STG_PRE_CLOSURE_CONVERT,
            STG_ANF, 
            STG_ALL_CODEGEN,
            ])
    elif "--very-parallel" in yy_bs_global_args:
        stages.extend([
            STG_TYPE_CHECK_ERASE_CLO_CONV_SINGLE_FUNC,
            STG_ANF_AND_PRE_CODEGEN_SINGLE_FUNC,
            STG_CODEGEN_SINGLE_FUNC,
            STG_CODEGEN_SINGLE_FUNC_FINAL
        ])
    else:
        stages.append(STG_TYPE_CHECK_AND_ERASE_THROUGH_CODEGEN)
        # stage_processing_order = stage_processing_order[:(stages.index(STG_TYPE_CHECK)+1)]

    yy_bs_main_file = args.input_file
    num_cpu_limit = args.num_cpu
    # stage_concurrency_limit[5] = int(args.codegen_concurrency_limit)

    # if args.cache_file:
    #     # If a cache file is provided, attempt to load the cached dependency graph
    #     try:
    #         with open(args.cache_file, "r") as f:
    #             graph = json.load(f)
    #         print("Loaded dependency graph from cache.")
    #     except FileNotFoundError:
    #         graph, error = build_dep_graph(args.input_file)
    #         if error is not None:
    #             print(error)
    #         else:
    #             # Save the freshly built graph to the cache file for future use
    #             with open(args.cache_file, "w") as f:
    #                 json.dump(graph, f)
    # else:
    #     # If no cache file is provided, build the dependency graph as usual
    #     graph, error = build_dep_graph(args.input_file)
    #     if error is not None:
    #         print(error)

    # pprint.pprint({k : len(v) for (k,v) in graph.items()})
    print(execute_plan())