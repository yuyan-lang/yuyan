function fibonacci(n) {
    if (n < 2) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

function sieve(limit) {
    const isPrime = new Uint8Array(limit + 1);
    isPrime.fill(1);
    isPrime[0] = 0;
    isPrime[1] = 0;
    for (let prime = 2; prime * prime <= limit; prime++) {
        if (isPrime[prime]) {
            for (let multiple = prime * prime; multiple <= limit; multiple += prime) {
                isPrime[multiple] = 0;
            }
        }
    }
    let count = 0;
    for (let value = 2; value <= limit; value++) {
        if (isPrime[value]) {
            count++;
        }
    }
    return count;
}

function matrixMultiply(size) {
    const total = size * size;
    const a = new Float64Array(total);
    const b = new Float64Array(total);
    const c = new Float64Array(total);
    for (let index = 0; index < total; index++) {
        const row = Math.floor(index / size);
        const col = index - row * size;
        a[index] = row + col + 1;
        b[index] = row * 2 + col + 1;
    }
    for (let row = 0; row < size; row++) {
        for (let col = 0; col < size; col++) {
            let sum = 0;
            for (let k = 0; k < size; k++) {
                sum += a[row * size + k] * b[k * size + col];
            }
            c[row * size + col] = sum;
        }
    }
    return c[0] + c[total - 1];
}

function swap(values, left, right) {
    const value = values[left];
    values[left] = values[right];
    values[right] = value;
}

function partition(values, low, high) {
    const middle = low + Math.floor((high - low) / 2);
    swap(values, middle, high);
    const pivot = values[high];
    let store = low;
    for (let index = low; index < high; index++) {
        if (values[index] < pivot) {
            swap(values, index, store);
            store++;
        }
    }
    swap(values, store, high);
    return store;
}

function quicksort(values, low, high) {
    if (low < high) {
        const split = partition(values, low, high);
        quicksort(values, low, split - 1);
        quicksort(values, split + 1, high);
    }
}

function quicksortBenchmark(length) {
    const values = new Float64Array(length);
    for (let index = 0; index < length; index++) {
        values[index] = length - index;
    }
    quicksort(values, 0, length - 1);
    return values[0] + values[Math.floor(length / 2)] + values[length - 1];
}

if (process.argv.length !== 4) {
    console.error(`usage: node ${process.argv[1]} <fib|sieve|matrix|quicksort> <size>`);
    process.exit(1);
}

const algorithm = process.argv[2];
const size = Number.parseInt(process.argv[3], 10);
const functions = {
    fib: fibonacci,
    sieve,
    matrix: matrixMultiply,
    quicksort: quicksortBenchmark,
};
console.log(functions[algorithm](size));
