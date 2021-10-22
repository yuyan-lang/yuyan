import numpy as np

d = {1:"一", 2:"二", 3:"三",
4:"四",5:"五",6:"六",7:"七",8:"八",9:"九",10:"十"}


arr = np.random.randint(1, 10, size=1000)
print("arr=[" + (", " .join([str(x) for x in list(arr)])) + "]")
print("速序于「「" + ("」，「".join([d[x] for x  in arr])) + "」，「空」」")