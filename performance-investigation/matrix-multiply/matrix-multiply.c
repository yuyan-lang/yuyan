// https://medium.com/swlh/a-performance-comparison-between-c-java-and-python-df3890545f6d
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

void * mallocArray(int N) {
    double ** arr = malloc(N*sizeof(double *));
    for(int i=0; i< N; i++) arr[i] = malloc(N*sizeof(double));
    return arr;
}

void dowork(int n){

    // double A[n][n];
    // double B[n][n];
    // double C[n][n];
    double **A = mallocArray(n);
    double **B = mallocArray(n);
    double **C = mallocArray(n);

    //populate the matrices with random values between 0.0 and 1.0
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            int64_t randomValue1 = rand();
            int64_t randomValue2 = rand();

            double v1 = (double) randomValue1 / (double)RAND_MAX;
            double v2 = (double)  randomValue2 / (double) RAND_MAX;
            A[i][j] = v1;
            B[i][j] = v2;
            C[i][j] = 0;
        }
    }

    struct timespec start, end;
    double time_spent;

    //matrix multiplication
    clock_gettime(CLOCK_REALTIME, &start);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            for (int k = 0; k < n; k++) {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }
    clock_gettime(CLOCK_REALTIME, &end);
    time_spent = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    printf("%f", time_spent);

}


int main(int argc, char * argv[]) {
    int64_t n;
    if (argc <= 1)
    {
        n = 2048;
    } else {
        n = strtol(argv[1], NULL, 10);
    }
    dowork(n);
    return 0;
}
