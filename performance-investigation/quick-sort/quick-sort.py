import sys
import time
import random

def quicksort(arr):
    if len(arr) <= 1:
        return arr
    pivot = arr[0]
    smaller = [x for x in arr[1:] if x < pivot]
    equal = [x for x in arr if x == pivot]
    larger = [x for x in arr[1:] if x > pivot]
    return quicksort(smaller) + equal + quicksort(larger)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python quicksort.py <length>")
        sys.exit(1)
    
    n = int(sys.argv[1])
    array = [random.random() for _ in range(n)]
    
    start_time = time.time()
    sorted_array = quicksort(array)
    end_time = time.time()
    
    time_diff = end_time - start_time
    print(f"{time_diff:.6f}")
