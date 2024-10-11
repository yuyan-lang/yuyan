
import os
import pickle
import sys

# define a decorator with an file name
# the decorator should take file name as an argument
# e.g. @cached_pass("recursion_rewrite") def recursion_rewrite(ast: Dict[str, Abt]) -> Dict[str, Abt]: ...
def cached_pass(name):
    def decorator(func):
        def wrapper(ast):
            if os.path.exists(".yybuild.nosync/py/" + name + ".pickle"):
                return pickle.load(open(".yybuild.nosync/py/" + name + ".pickle", "rb"))
            else:
                result = func(ast)
                print("recursion limit", sys.getrecursionlimit())   
                pickle.dump(result, open(".yybuild.nosync/py/" + name + ".pickle", "wb"), protocol=pickle.HIGHEST_PROTOCOL)
                return result
        return wrapper
    return decorator