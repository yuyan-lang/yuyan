
import argparse
import subprocess
from multiprocessing import cpu_count
from multiprocessing.pool import ThreadPool
import queue
import threading
import multiprocessing
import subprocess
import json
import pprint

yy_bs_global_args = []

# def worker(task):
#     command = ["./yy_bs", "--mode=worker", "--worker-task=" + task[0]] + task[1] + yy_bs_global_args
#     print("Running command: " + " ".join(command))
#     process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
#     if process.returncode != 0:
#         return None, f"Error during {task[0]} on {task[1]}: {process.stderr.decode('utf-8')}"
#     else:
#         return task

def worker(task):
    command = ["./yy_bs", "--mode=worker", "--worker-task=" + task[0]] + task[1] + yy_bs_global_args
    print("Running command: " + " ".join(command))
    # process = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # if process.returncode != 0:
    #     return None, f"Error during {task[0]} on {task[1]}: {process.stderr.decode('utf-8')}"
    # else:
    return task

def dependency_analysis(file, compile_arguments):
    command = ["./yy_bs", "--mode=worker", "--worker-task=dependency-analysis"] + compile_arguments + [file]
    print("Running command: " + " ".join(command))
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
                    all(dep in completed[stage] for dep in graph[file]) and
                    all(file in completed[prev_stage] for prev_stage in stages[:i])
                ):
                    scheduled[stage].append(file)

    update_schedule()


    def process_result(result):
        nonlocal results_ready
        print("processing result", result)
        comp_stage, comp_file = result
        completed[comp_stage].append(comp_file)
        executing[comp_stage].remove(comp_file)
        results_ready.set()


    with ThreadPool(processes=cpu_count()) as pool:
        while any(len(stg) > 0 for stg in scheduled.values()):
            print(scheduled)
            for stage in stages:
                while scheduled[stage]:
                    file_name = scheduled[stage].pop()
                    print("scheduling", (stage, file_name))
                    executing[stage].append(file_name)
                    pool.apply_async(worker, (stage, file_name), callback=process_result)
            results_ready.wait()
            results_ready.clear()
            update_schedule()
    
    return "Compilation finished successfully"



if __name__ == "__main__":
    # Create parser, add arguments and parse them
    parser = argparse.ArgumentParser()
    parser.add_argument("input_file")
    parser.add_argument("extra", nargs="*", default=[])
    parser.add_argument("--cache-file", help="Specify a local JSON file to cache the dependency graph.")
    args = parser.parse_args()

    print(args)
    yy_bs_global_args = args.extra

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