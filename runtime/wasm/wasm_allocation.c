#include "wasm_include.h"
// extern yy_ptr emscripten_allocate(size_t);

// extern void* array.new(uint64_t size);

void* yy_gcAllocateBytes(uint64_t size) {
    // yy_ptr x = (yy_ptr) EM_ASM_PTR({
    //     var byteSize = $0;
    //     var buffer = new ArrayBuffer(Number(byteSize));
    //     return buffer;
    // }, size);
    yy_ptr x = malloc(size); // TODO: use GC allocate
    return x;
}

yy_ptr yy_gcAllocateArray(uint64_t size) {
    // Webassembly GC in proposal, will replace with real thing 
    // when it is implemented https://webassembly.org/roadmap/
    uint64_t byteSize = size * 8;
    return yy_gcAllocateBytes(byteSize);
}
