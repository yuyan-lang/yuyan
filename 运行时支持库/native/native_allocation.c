

#include "common_include.h"
#include "garbage_collector.h"
#include "marshall.h"

yyvalue yy_gcAllocateArray(uint64_t size) {
    yyvalue* x = (yyvalue*)yy_gcAllocateBytes(size * sizeof(yyvalue));
    return raw_tuple_to_yyvalue(size, x);
}

void* yy_gcAllocateBytes(uint64_t size) {
    void* x;
    if (use_libgc)
    {
        x = GC_MALLOC(size);
    } else {
        x = yy_gc_malloc_bytes(size);
    }
    // yyvalue x = malloc(size); // for testing
    return x;
}

void* yy_gcReallocateBytes(void* ptr, uint64_t old_size, uint64_t new_size) {
    void* x;
    if (use_libgc)
    {
        x = GC_REALLOC(ptr, new_size);
    } else {
        x = yy_gc_realloc_bytes(ptr, old_size, new_size);
    }
    // yyvalue x = malloc(size); // for testing
    return x;
}