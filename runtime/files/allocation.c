

#include <stdlib.h>
#include <stdio.h>

// #include "gc.h"
#include "globalInclude.h"

uint64_t * allocateArray(uint64_t size) {
    uint64_t * x = GC_MALLOC(size * 8);
    return x;
}
