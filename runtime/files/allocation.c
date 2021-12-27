

#include <stdlib.h>
#include <stdio.h>

#include "gc.h"

int * allocateArray(int size) {
    int * x = GC_MALLOC(size * 8);
    return x;
}
