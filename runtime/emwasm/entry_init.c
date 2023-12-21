
#include "emwasm_include.h"


// uv_loop_t *uv_global_loop;

void optional_entry_initialization(){
        // initialize garbage collection
    GC_INIT();
    // GC_enable_incremental();
    // GC_expand_hp(60719476736); // about 60 GB   



}