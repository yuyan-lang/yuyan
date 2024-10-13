
import os
import pickle
import sys
from ir_serialization import *
from ir_deserialization import *

# define a decorator with an file name
# the decorator should take file name as an argument
# e.g. @cached_pass("recursion_rewrite") def recursion_rewrite(ast: Dict[str, Abt]) -> Dict[str, Abt]: ...

def cached_pass(name):
    def decorator(func):
        def wrapper(ast):
            if os.path.exists(".yybuild.nosync/py/" + name + ".lamir"):
                return ir_to_asts(open(".yybuild.nosync/py/" + name + ".lamir", "r").read(), ".yybuild.nosync/py/" + name + ".lamir")
            else:
                result = func(ast)
                ir = asts_to_ir(result)
                with open(".yybuild.nosync/py/" + name + ".lamir", "w") as f:
                    f.write(ir)
                return result
        return wrapper
    return decorator

# def cached_pass(name):
#     def decorator(func):
#         def wrapper(ast):
#             if os.path.exists(".yybuild.nosync/py/" + name + ".pickle"):
#                 return pickle.load(open(".yybuild.nosync/py/" + name + ".pickle", "rb"))
#             else:
#                 result = func(ast)
#                 print("recursion limit", sys.getrecursionlimit())   
#                 pickle.dump(result, open(".yybuild.nosync/py/" + name + ".pickle", "wb"), protocol=pickle.HIGHEST_PROTOCOL)
#                 return result
#         return wrapper
#     return decorator