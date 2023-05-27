#include "wasm_include.h"

yy_ptr yy_gcAllocateArray(uint64_t size) {
    // Webassembly GC in proposal, will replace with real thing 
    // when it is implemented https://webassembly.org/roadmap/
    yy_ptr x = malloc(size * 8); 
    return x;
}

void* yy_gcAllocateBytes(uint64_t size) {
    yy_ptr x = malloc(size);
    return x;
}