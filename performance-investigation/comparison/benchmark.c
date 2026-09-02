#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int64_t fibonacci(int64_t n) {
    if (n < 2) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

static int64_t sieve(int64_t limit) {
    uint8_t *is_prime = malloc((size_t)limit + 1);
    if (is_prime == NULL) {
        fputs("allocation failed\n", stderr);
        exit(1);
    }
    memset(is_prime, 1, (size_t)limit + 1);
    is_prime[0] = 0;
    is_prime[1] = 0;
    for (int64_t prime = 2; prime * prime <= limit; prime++) {
        if (is_prime[prime]) {
            for (int64_t multiple = prime * prime; multiple <= limit; multiple += prime) {
                is_prime[multiple] = 0;
            }
        }
    }
    int64_t count = 0;
    for (int64_t value = 2; value <= limit; value++) {
        count += is_prime[value] != 0;
    }
    free(is_prime);
    return count;
}

static int64_t matrix_multiply(int64_t size) {
    int64_t total = size * size;
    int64_t *a = calloc((size_t)total, sizeof(*a));
    int64_t *b = calloc((size_t)total, sizeof(*b));
    int64_t *c = calloc((size_t)total, sizeof(*c));
    if (a == NULL || b == NULL || c == NULL) {
        fputs("allocation failed\n", stderr);
        exit(1);
    }
    for (int64_t index = 0; index < total; index++) {
        int64_t row = index / size;
        int64_t col = index - row * size;
        a[index] = row + col + 1;
        b[index] = row * 2 + col + 1;
    }
    for (int64_t row = 0; row < size; row++) {
        for (int64_t col = 0; col < size; col++) {
            int64_t sum = 0;
            for (int64_t k = 0; k < size; k++) {
                sum += a[row * size + k] * b[k * size + col];
            }
            c[row * size + col] = sum;
        }
    }
    int64_t checksum = c[0] + c[total - 1];
    free(a);
    free(b);
    free(c);
    return checksum;
}

static void swap(int64_t *values, int64_t left, int64_t right) {
    int64_t value = values[left];
    values[left] = values[right];
    values[right] = value;
}

static int64_t partition(int64_t *values, int64_t low, int64_t high) {
    int64_t middle = low + (high - low) / 2;
    swap(values, middle, high);
    int64_t pivot = values[high];
    int64_t store = low;
    for (int64_t index = low; index < high; index++) {
        if (values[index] < pivot) {
            swap(values, index, store);
            store++;
        }
    }
    swap(values, store, high);
    return store;
}

static void quicksort(int64_t *values, int64_t low, int64_t high) {
    if (low < high) {
        int64_t split = partition(values, low, high);
        quicksort(values, low, split - 1);
        quicksort(values, split + 1, high);
    }
}

static int64_t quicksort_benchmark(int64_t length) {
    int64_t *values = malloc((size_t)length * sizeof(*values));
    if (values == NULL) {
        fputs("allocation failed\n", stderr);
        exit(1);
    }
    for (int64_t index = 0; index < length; index++) {
        values[index] = length - index;
    }
    quicksort(values, 0, length - 1);
    int64_t checksum = values[0] + values[length / 2] + values[length - 1];
    free(values);
    return checksum;
}

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "usage: %s <fib|sieve|matrix|quicksort> <size>\n", argv[0]);
        return 1;
    }
    int64_t size = strtoll(argv[2], NULL, 10);
    int64_t result;
    if (strcmp(argv[1], "fib") == 0) {
        result = fibonacci(size);
    } else if (strcmp(argv[1], "sieve") == 0) {
        result = sieve(size);
    } else if (strcmp(argv[1], "matrix") == 0) {
        result = matrix_multiply(size);
    } else if (strcmp(argv[1], "quicksort") == 0) {
        result = quicksort_benchmark(size);
    } else {
        fputs("unknown algorithm\n", stderr);
        return 1;
    }
    printf("%lld\n", (long long)result);
    return 0;
}
