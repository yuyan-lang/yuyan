

#include <stdlib.h>
#include <stdio.h>

#include "gc.h"

uint64_t * allocateArray(int size) {
    uint64_t * x = GC_MALLOC(size * 8);
    return x;
}
