import java.util.Arrays;

public final class Benchmark {
    private static long fibonacci(long n) {
        if (n < 2) {
            return n;
        }
        return fibonacci(n - 1) + fibonacci(n - 2);
    }

    private static long sieve(int limit) {
        boolean[] isPrime = new boolean[limit + 1];
        Arrays.fill(isPrime, true);
        isPrime[0] = false;
        isPrime[1] = false;
        for (int prime = 2; (long) prime * prime <= limit; prime++) {
            if (isPrime[prime]) {
                for (int multiple = prime * prime; multiple <= limit; multiple += prime) {
                    isPrime[multiple] = false;
                }
            }
        }
        long count = 0;
        for (int value = 2; value <= limit; value++) {
            if (isPrime[value]) {
                count++;
            }
        }
        return count;
    }

    private static long matrixMultiply(int size) {
        int total = size * size;
        long[] a = new long[total];
        long[] b = new long[total];
        long[] c = new long[total];
        for (int index = 0; index < total; index++) {
            int row = index / size;
            int col = index - row * size;
            a[index] = row + col + 1L;
            b[index] = row * 2L + col + 1L;
        }
        for (int row = 0; row < size; row++) {
            for (int col = 0; col < size; col++) {
                long sum = 0;
                for (int k = 0; k < size; k++) {
                    sum += a[row * size + k] * b[k * size + col];
                }
                c[row * size + col] = sum;
            }
        }
        return c[0] + c[total - 1];
    }

    private static void swap(long[] values, int left, int right) {
        long value = values[left];
        values[left] = values[right];
        values[right] = value;
    }

    private static int partition(long[] values, int low, int high) {
        int middle = low + (high - low) / 2;
        swap(values, middle, high);
        long pivot = values[high];
        int store = low;
        for (int index = low; index < high; index++) {
            if (values[index] < pivot) {
                swap(values, index, store);
                store++;
            }
        }
        swap(values, store, high);
        return store;
    }

    private static void quicksort(long[] values, int low, int high) {
        if (low < high) {
            int split = partition(values, low, high);
            quicksort(values, low, split - 1);
            quicksort(values, split + 1, high);
        }
    }

    private static long quicksortBenchmark(int length) {
        long[] values = new long[length];
        for (int index = 0; index < length; index++) {
            values[index] = length - index;
        }
        quicksort(values, 0, length - 1);
        return values[0] + values[length / 2] + values[length - 1];
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            throw new IllegalArgumentException("usage: Benchmark <fib|sieve|matrix|quicksort> <size>");
        }
        String algorithm = args[0];
        int size = Integer.parseInt(args[1]);
        long result = switch (algorithm) {
            case "fib" -> fibonacci(size);
            case "sieve" -> sieve(size);
            case "matrix" -> matrixMultiply(size);
            case "quicksort" -> quicksortBenchmark(size);
            default -> throw new IllegalArgumentException("unknown algorithm: " + algorithm);
        };
        System.out.println(result);
    }
}
