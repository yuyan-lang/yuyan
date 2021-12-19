

#include <stdlib.h>
#include <stdio.h>


int * allocateArray(int size) {
    int * x = malloc(size * 4);
    return x;
}
