
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
# def worker(task):
#     command = ["./yy_bs", "--mode=worker", "--worker-task=" + task[0]] + task[1] + yy_bs_global_args
#     print("" + " ".join(command))
#     process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
#     if process.returncode != 0:
#         return None, f"Error during {task[0]} on {task[1]}: {process.stderr.decode('utf-8')}"
#     else:
#         return task

def worker(task):
    stage, file = task
    optimize_task = 'optimize' if file[0] == yy_bs_main_file else 'optimize-no'
    command = ["./yy_bs", "--mode=worker", "--worker-task=" + (stage if stage != 'optimize-no' else optimize_task)] + file + yy_bs_global_args
    print("" + " ".join(command))
    process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if process.returncode != 0:
        return None, f"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" \
                      f"\nError during {task[0]} on {task[1][0]}: \n{' '.join(command)}\n stderr is: \n{process.stderr.decode('utf-8')}"
    else:
        return task, None

def dependency_analysis(file, compile_arguments):
    command = ["./yy_bs", "--mode=worker", "--worker-task=dependency-analysis"] + compile_arguments + [file]
    print("" + " ".join(command))
    process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    if process.returncode != 0:
        return None, f"Error during dependency analysis: {process.stderr.decode('utf-8')}"
    else:
        dependencies = process.stdout.decode().split()
        
    return dependencies, None

def build_dep_graph(input_file, compile_arguments):
    graph = {input_file: []}
    
    to_visit = [input_file]

    while to_visit:
        current_file = to_visit.pop()
        current_deps, error = dependency_analysis(current_file, compile_arguments)

        if error is not None:
            return None, error

        for dep in current_deps:
            if dep not in graph:
                graph[dep] = []
                to_visit.append(dep)
        
        graph[current_file] = current_deps


    return graph, None

def execute_plan(graph):
    stages = ["parse", "type-check-and-anf", "optimize-no", "cps-transform", "codegen"]
    completed = {'parse': [], 'type-check-and-anf': [], 'optimize-no': [], 'cps-transform': [], 'codegen': []}
    executing = {'parse': [], 'type-check-and-anf': [], 'optimize-no': [], 'cps-transform': [], 'codegen': []}
    scheduled =  {'parse': [], 'type-check-and-anf': [], 'optimize-no': [], 'cps-transform': [], 'codegen': []}

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
                        # or (i > 1 and file != yy_bs_main_file) ## optimizedo not require dependencies, unless it's the entry file
                        or i > 1
                        ) and  # optimize, cps-transform, and codegen do not require dependencies
                    (all(file in completed[prev_stage] for prev_stage in stages[:i]))
                ):
                    scheduled[stage].append(file)

    update_schedule()

    def get_file_args(cur_filename):
        def convert_to_override_list(input_list):
            override_list = []
            for item in input_list:
                override_list.extend(["--override-add-file-dependency", item])
            return override_list
        return [cur_filename] + convert_to_override_list(
            graph[cur_filename] if cur_filename != yy_bs_main_file else list(graph.keys()))


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
            for stage in stages:
                while scheduled[stage]:
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

    t, error = worker(("exec-gen", get_file_args(yy_bs_main_file)))
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
    parser.add_argument("--extra", default=[])

    args = parser.parse_args()

    print(args)
    yy_bs_global_args = args.extra.split(" ")
    yy_bs_main_file = args.input_file
    num_cpu_limit = args.num_cpu

    if args.cache_file:
        # If a cache file is provided, attempt to load the cached dependency graph
        try:
            with open(args.cache_file, "r") as f:
                graph = json.load(f)
            print("Loaded dependency graph from cache.")
        except FileNotFoundError:
            graph, error = build_dep_graph(args.input_file, args.extra)
            if error is not None:
                print(error)
            else:
                # Save the freshly built graph to the cache file for future use
                with open(args.cache_file, "w") as f:
                    json.dump(graph, f)
    else:
        # If no cache file is provided, build the dependency graph as usual
        graph, error = build_dep_graph(args.input_file, args.extra)
        if error is not None:
            print(error)

    pprint.pprint({k : len(v) for (k,v) in graph.items()})
    print(execute_plan(graph))