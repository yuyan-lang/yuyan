// https://medium.com/swlh/a-performance-comparison-between-c-java-and-python-df3890545f6d
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define n 2048

double A[n][n];
double B[n][n];
double C[n][n];

int main() {

    //populate the matrices with random values between 0.0 and 1.0
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {

            A[i][j] = (double) rand() / (double) RAND_MAX;
            B[i][j] = (double) rand() / (double) RAND_MAX;
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
    printf("Elapsed time in seconds: %f \n", time_spent);
    return 0;
}
