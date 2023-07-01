
#include <stdio.h>
#include <stdlib.h>

int fibonacci(int n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <n>\n", argv[0]);
        return 1;
    }

    int n = atoi(argv[1]);
    if (n < 0) {
        printf("n must be a non-negative integer.\n");
        return 1;
    }

    int result = fibonacci(n);
    printf("The %dth number in the Fibonacci sequence is: %d\n", n, result);

    return 0;
}
