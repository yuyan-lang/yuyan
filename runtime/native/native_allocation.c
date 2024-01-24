

#include <stdlib.h>
#include <stdio.h>

// #include "gc.h"
#include "native_include.h"


yy_ptr yy_gcAllocateArray(uint64_t size) {
    yy_ptr x = yy_gcAllocateBytes(size * 8);
    return x;
}

void* yy_gcAllocateBytes(uint64_t size) {
    yy_ptr x;
    if (use_libgc)
    {
        x = GC_MALLOC(size);
    } else {
        x = yy_fastgc_malloc(size);
    }
    // yy_ptr x = malloc(size); // for testing
    return x;
}

void* yy_gcReallocateBytes(void* ptr, uint64_t size) {
    yy_ptr x;
    if (use_libgc)
    {
        x = GC_REALLOC(ptr, size);
    } else {
        x = yy_fastgc_realloc(ptr, size);
    }
    // yy_ptr x = malloc(size); // for testing
    return x;
}