
#include "globalInclude.h"

#include "gc.h"

extern int entryMain(); // the entry to yy llvm ir


int global_argc = 0;
char** global_argv = NULL;

uv_loop_t *uv_global_loop;
int main(int argc, char* argv[]) {

    // save global paramters
    global_argc = argc;
    global_argv = argv;

    // initialize garbage collection
    GC_INIT();

    // uv_replace_allocator(&GC_MALLOC, GC_REALLOC, GC_MALLOC, GC_FREE);

    // initialize uv
    uv_global_loop = uv_default_loop();
    


    return entryMain();
}




int informResult (uint64_t result[]) {
    //informResultRec(stderr, result, 0);
    // do not inform result as we're moving to a mature language
    return 0;
}

