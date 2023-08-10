
import argparse
import subprocess
from multiprocessing import cpu_count
from multiprocessing.pool import ThreadPool
from concurrent.futures import ThreadPoolExecutor
import sys
import os

import queue
import threading
import multiprocessing
import subprocess
import json
import pprint

yy_bs_global_args = []
yy_bs_main_file = None

num_cpu_limit = None
stage_concurrency_limit = [100, 100, 100, 100, 100]
stage_processing_order = [0,1,4,2,3] # process parse -> tc -> codegen  -> optimize-half -> cps
# def worker(task):
#     command = ["./yy_bs", "--mode=worker", "--worker-task=" + task[0]] + task[1] + yy_bs_global_args
#     print("" + " ".join(command))
#     process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
#     if process.returncode != 0:
#         return None, f"Error during {task[0]} on {task[1]}: {process.stderr.decode('utf-8')}"
#     else:
#         return task

def exec_worker(args):
    command = ["./yy_bs", "--mode=worker", "--worker-task=exec-gen"] + args + yy_bs_global_args
    print("" + " ".join(command))
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    if process.returncode != 0:
        return None, f"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \
                      f"\nError during exec-fun on {args[0]}: \n{' '.join(command)}\nstderr:\n{stderr.decode('utf-8')}\nstdout:{stdout.decode('utf-8')}\nexit code:{process.returncode}"
    else:
        print(stdout.decode('utf-8'))
        return args, None

def worker(task):
    stage, file = task
    optimize_task = 'optimize' if file[0] == yy_bs_main_file else 'optimize-half'
    command = ["./yy_bs", "--mode=worker", "--worker-task=" + (stage if stage != 'optimize-half' else optimize_task)] + file + yy_bs_global_args
    print("" + " ".join(command))
    process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if process.returncode != 0:
        return None, f"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \
                      f"\nError during {task[0]} on {task[1][0]}: \n{' '.join(command)}\n stderr is: \n{process.stderr.decode('utf-8')}\nstdout:{process.stdout.decode('utf-8')}\nexit code:{process.returncode}"
    else:
        return task, None

def dependency_analysis(file):
    command = ["./yy_bs", "--mode=worker", "--worker-task=dependency-analysis"] + [file] + yy_bs_global_args
    print("" + " ".join(command))
    process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    if process.returncode != 0:
        return None, f"Error during dependency analysis: {process.stderr.decode('utf-8')}"
    else:
        dependencies = process.stdout.decode().split()
        
    return dependencies, None

def build_dep_graph(input_file):
    graph = {input_file: []}
    
    to_visit = [input_file]

    while to_visit:
        current_file = to_visit.pop()
        current_deps, error = dependency_analysis(current_file)

        if error is not None:
            return None, error

        for dep in current_deps:
            if dep not in graph:
                graph[dep] = []
                to_visit.append(dep)
        
        graph[current_file] = current_deps


    return graph, None

def execute_plan(graph):
    stages = ["parse", "type-check-and-anf", "optimize-half", "cps-transform", "codegen"]
    completed = {'parse': [], 'type-check-and-anf': [], 'optimize-half': [], 'cps-transform': [], 'codegen': []}
    executing = {'parse': [], 'type-check-and-anf': [], 'optimize-half': [], 'cps-transform': [], 'codegen': []}
    scheduled =  {'parse': [], 'type-check-and-anf': [], 'optimize-half': [], 'cps-transform': [], 'codegen': []}

    all_files = list(graph.keys())

    results_ready = multiprocessing.Manager().Event()

    def update_schedule():
        nonlocal scheduled, completed, stages, all_files
        for file in all_files:
            for i, stage in enumerate(stages):
                if (
                    file not in scheduled[stage] and 
                    file not in executing[stage] and
                    file not in completed[stage] and 
                    (all(dep in completed[stage] for dep in graph[file]) 
                        or (i > 1 and file != yy_bs_main_file) ## optimize-half do not require dependencies, unless it's the entry file
                        or i > 2 # cps-transform, and codegen do not require dependencies
                        ) and  
                    (all(file in completed[prev_stage] for prev_stage in stages[:i]))
                ):
                    scheduled[stage].append(file)

    update_schedule()

    def convert_to_override_list(input_list):
        override_list = []
        for item in input_list:
            override_list.extend(["--override-add-file-dependency", item])
        return override_list

    exec_args = []
    candidate = [k for k in graph.keys() if k != yy_bs_main_file]
    while len(exec_args) < len(candidate):
        for k in candidate:
            if k not in exec_args and all(dep in exec_args for dep in graph[k]):
                exec_args.append(k)

    def get_file_args(cur_filename):
        return [cur_filename] + convert_to_override_list(
            # graph[cur_filename] if cur_filename != yy_bs_main_file else 
            list(graph[cur_filename]) if cur_filename != yy_bs_main_file else exec_args)


    def process_result(future):
        nonlocal results_ready
        result, error = future.result()
        if error:
            print(error)
            os._exit(1)
        # print("processing result", result)
        comp_stage, comp_file = result
        completed[comp_stage].append(comp_file[0])
        executing[comp_stage].remove(comp_file[0])
        print("completed", comp_stage, comp_file[0])
        results_ready.set()
    def print_stat():
        pprint.pprint("=======================================")
        pprint.pprint("=======================================")
        pprint.pprint("============== Scheduled ==============")
        pprint.pprint(scheduled)
        pprint.pprint("============== Executing ==============")
        pprint.pprint(executing)
        pprint.pprint("============== Completed ==============")
        pprint.pprint({k: len(v) for k, v in completed.items()})
        pprint.pprint("=======================================")
        pprint.pprint("=======================================")
        pprint.pprint("=======================================")


    with ThreadPoolExecutor(max_workers=num_cpu_limit) as executor:
        while any(len(stg) > 0 for stg in scheduled.values()) or any(len(stg) > 0 for stg in executing.values()):
            for i in stage_processing_order:
                stage = stages[i]
                while (scheduled[stage] 
                    and sum(len(stg) for stg in executing.values()) < num_cpu_limit
                    and len(executing[stage]) < stage_concurrency_limit[i]):
                    file_name = scheduled[stage].pop()
                    print("Scheduling", (stage, file_name))
                    executing[stage].append(file_name)
                    executor.submit(worker, (stage, get_file_args(file_name))).add_done_callback(process_result)
            print_stat()
            results_ready.wait()
            results_ready.clear()
            update_schedule()
    
    print_stat()
    for file in all_files:
        for i, stage in enumerate(stages):
            if (file not in completed[stage]):
                if not all(dep in completed[stage] for dep in graph[file]):
                    print(f"File {file} is due for {stage} but not all dependencies are completed: {[dep for dep in graph[file] if dep not in completed[stage]]}")
                if not all(file in completed[prev_stage] for prev_stage in stages[:i]):
                    print(f"File {file} is due for {stage} but not all previous stages are completed: {[prev_stage for prev_stage in stages[:i] if file not in completed[prev_stage]]}")



    t, error = exec_worker([yy_bs_main_file, *convert_to_override_list(exec_args)])
    if error:
        print(error)
        os._exit(1)
    return "Compilation finished successfully"



if __name__ == "__main__":
    # Create parser, add arguments and parse them
    parser = argparse.ArgumentParser()
    parser.add_argument("input_file")
    parser.add_argument("--cache-file", help="Specify a local JSON file to cache the dependency graph.")
    parser.add_argument("-j", "--num-cpu", type=int, default=cpu_count(), help="Number of CPU cores to use for compilation")
    parser.add_argument("--extra", default=None)
    parser.add_argument("--codegen-concurrency-limit", default=2)

    args = parser.parse_args()

    print(args)
    if args.extra:
        yy_bs_global_args = args.extra.split(" ")
    yy_bs_main_file = args.input_file
    num_cpu_limit = args.num_cpu
    stage_concurrency_limit[4] = int(args.codegen_concurrency_limit)

    if args.cache_file:
        # If a cache file is provided, attempt to load the cached dependency graph
        try:
            with open(args.cache_file, "r") as f:
                graph = json.load(f)
            print("Loaded dependency graph from cache.")
        except FileNotFoundError:
            graph, error = build_dep_graph(args.input_file)
            if error is not None:
                print(error)
            else:
                # Save the freshly built graph to the cache file for future use
                with open(args.cache_file, "w") as f:
                    json.dump(graph, f)
    else:
        # If no cache file is provided, build the dependency graph as usual
        graph, error = build_dep_graph(args.input_file)
        if error is not None:
            print(error)

    pprint.pprint({k : len(v) for (k,v) in graph.items()})
    print(execute_plan(graph))