#include "../common/common_include.h"
#include "gc.h" // https://hboehm.info/gc/ libgc 
#include <uv.h> 

#include "fast_gc.h"
#include "garbage_collector.h"

extern uv_loop_t *uv_global_loop;

void readStreamUntilEofIntoDataAync(uv_stream_t *stream);