

#include <stdlib.h>
#include <stdio.h>

// #include "gc.h"
#include "globalInclude.h"

yy_ptr allocateArray(uint64_t size) {
    yy_ptr x = GC_MALLOC(size * 8);
    return x;
}
