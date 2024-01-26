

#include "common_include.h"
#include "garbage_collector.h"
#include "marshall.h"

yyvalue yy_gcAllocateTuple(uint64_t size) {
    yyvalue* x = (yyvalue*)yy_gc_malloc_array(size);
    yyvalue ret = raw_tuple_to_yyvalue(size, x);
    verify_yyvalue(ret, true, 0);
    return ret;
}



yyvalue yy_gcAllocateTuple_yyrt(yyvalue size) {
    yyvalue* x = (yyvalue*)yy_gc_malloc_array(yyvalue_to_int(size));
    return raw_tuple_to_yyvalue(size, x);
}

// void* yy_gcAllocateBytes(uint64_t size) {
//     if (during_gc){
//         printf("GC ERROR: GC is running, cannot allocate memory, check gc implementation\n");
//         abort();
//     }
//     void* x;
//     x = yy_gc_malloc_bytes(size);
//     // yyvalue x = malloc(size); // for testing
//     return x;
// }

// void* yy_gcReallocateBytes(void* ptr, uint64_t old_size, uint64_t new_size) {
//     void* x;
//     x = yy_gc_realloc_bytes(ptr, old_size, new_size);
//     // yyvalue x = malloc(size); // for testing
//     return x;
// }