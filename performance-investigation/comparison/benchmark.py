import sys


def fibonacci(n):
    if n < 2:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)


def sieve(limit):
    is_prime = bytearray(b"\x01") * (limit + 1)
    is_prime[0] = 0
    is_prime[1] = 0
    prime = 2
    while prime * prime <= limit:
        if is_prime[prime]:
            multiple = prime * prime
            while multiple <= limit:
                is_prime[multiple] = 0
                multiple += prime
        prime += 1
    count = 0
    value = 2
    while value <= limit:
        if is_prime[value]:
            count += 1
        value += 1
    return count


def matrix_multiply(size):
    total = size * size
    a = [0] * total
    b = [0] * total
    c = [0] * total
    for index in range(total):
        row = index // size
        col = index - row * size
        a[index] = row + col + 1
        b[index] = row * 2 + col + 1
    for row in range(size):
        row_offset = row * size
        for col in range(size):
            total_value = 0
            for k in range(size):
                total_value += a[row_offset + k] * b[k * size + col]
            c[row_offset + col] = total_value
    return c[0] + c[-1]


def swap(values, left, right):
    values[left], values[right] = values[right], values[left]


def partition(values, low, high):
    middle = low + (high - low) // 2
    swap(values, middle, high)
    pivot = values[high]
    store = low
    for index in range(low, high):
        if values[index] < pivot:
            swap(values, index, store)
            store += 1
    swap(values, store, high)
    return store


def quicksort(values, low, high):
    if low < high:
        split = partition(values, low, high)
        quicksort(values, low, split - 1)
        quicksort(values, split + 1, high)


def quicksort_benchmark(length):
    values = list(range(length, 0, -1))
    quicksort(values, 0, length - 1)
    return values[0] + values[length // 2] + values[-1]


def main():
    if len(sys.argv) != 3:
        raise SystemExit(f"usage: {sys.argv[0]} <fib|sieve|matrix|quicksort> <size>")
    algorithm = sys.argv[1]
    size = int(sys.argv[2])
    functions = {
        "fib": fibonacci,
        "sieve": sieve,
        "matrix": matrix_multiply,
        "quicksort": quicksort_benchmark,
    }
    print(functions[algorithm](size))


if __name__ == "__main__":
    main()
