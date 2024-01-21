
#include "common_include.h"


uv_loop_t *uv_global_loop;

void optional_entry_initialization(){
        // initialize garbage collection
    if (use_libgc){
        GC_INIT();
    } else {
        yy_gc_init();
    }
        // initialize uv default loop (can replace)
    uv_global_loop = uv_default_loop();



}