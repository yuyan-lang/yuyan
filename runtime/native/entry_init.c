
#include "native_include.h"


uv_loop_t *uv_global_loop;

void optional_entry_initialization(){
        // initialize garbage collection
    GC_INIT();
    
        // initialize uv default loop (can replace)
    uv_global_loop = uv_default_loop();



}