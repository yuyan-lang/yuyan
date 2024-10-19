
import os
import pickle
import sys
from ir_serialization import *
from ir_deserialization import *
import multiprocessing

# define a decorator with an file name
# the decorator should take file name as an argument
# e.g. @cached_pass("recursion_rewrite") def recursion_rewrite(ast: Dict[str, Abt]) -> Dict[str, Abt]: ...

INPUT_PATH_KEY=""

def get_artifact_path(name):
    return ".yybuild.nosync/py/" +INPUT_PATH_KEY + "/"+ name 

def cached_pass(name):
    def decorator(func):
        def wrapper(ast):
            if os.path.exists(get_artifact_path(name + ".lamir")):
                return ir_to_asts(open(get_artifact_path(name + ".lamir"), "r").read(), ".yybuild.nosync/py/" + name + ".lamir")
            else:
                result = func(ast)
                ir = asts_to_ir(result)
                with open(get_artifact_path(name + ".lamir"), "w") as f:
                    f.write(ir)
                return result
        return wrapper
    return decorator


MULTI_PROCESSING_WORKERS = 1

def func_wrapper(farg):
    [func, arg] = farg
    return func(*arg)

# because we have global unique names during AST processing, this cannot be used
def do_multi_process(func, arg_list, desc=None):
    """
    arg_list is a list of tuple of arguments, shared structures should use 
    manager = multiprocessing.Manager()
    shared_dict = manager.dict()  
    """

    if MULTI_PROCESSING_WORKERS == 1:
        results = [func_wrapper((func, arg)) for arg in tqdm(arg_list, desc=desc)]
        return results
    else:
        with multiprocessing.Pool(processes=MULTI_PROCESSING_WORKERS) as pool:
            results = list(tqdm(pool.imap(func_wrapper, [(func, arg) for arg in arg_list]), total=len(arg_list), desc=desc))

        return results