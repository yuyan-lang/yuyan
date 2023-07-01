import sys

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

if len(sys.argv) != 2:
    print("Usage: python fibonacci.py <n>")
    sys.exit(1)

n = int(sys.argv[1])
if n < 0:
    print("n must be a non-negative integer.")
    sys.exit(1)

result = fibonacci(n)
print(f"The {n}th number in the Fibonacci sequence is: {result}")