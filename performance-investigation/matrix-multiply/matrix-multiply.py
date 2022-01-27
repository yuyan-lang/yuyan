#https://medium.com/swlh/a-performance-comparison-between-c-java-and-python-df3890545f6d

import random
import time
import sys

n = 2048 if len(sys.argv) < 2 else int(sys.argv[1])

#populate the matrices with random values between 0.0 and 1.0
A = [[random.random() for row in range(n)] for col in range(n)]
B = [[random.random() for row in range(n)] for col in range(n)]
C = [[0 for row in range(n)] for col in range(n)]

start = time.time()
#matrix multiplication
for i in range(n):
    for j in range(n):
        for k in range(n):
            C[i][j] += A[i][k] * B[k][j]

end = time.time()
print("%0.6f" % (end-start))