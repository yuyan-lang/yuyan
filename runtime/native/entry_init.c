
#include "native_include.h"


uv_loop_t *uv_global_loop;

void optional_entry_initialization(){
        // initialize garbage collection
    if (use_libgc){
        GC_INIT();
        GC_enable_incremental();
#ifdef __linux__
        GC_expand_hp(60719476736); // about 60G
#endif
    } else {
        yy_fastgc_init();  // Initialize with 32MB buffer size
    }
        // initialize uv default loop (can replace)
    uv_global_loop = uv_default_loop();



}