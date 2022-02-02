
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


    // initialize global exception handler
    initialize_global_exception_handler();

    // initialize uv default loop (can replace)
    uv_global_loop = uv_default_loop();
    


    return entryMain();
}




int informResult (uint64_t result[]) {
    fprintf(stderr, "代码已经运行成功，结果是：");
    informResultRec(stderr, result, 0);
    fprintf(stderr, "\n");
    // do not inform result as we're moving to a mature language
    return 0;
}

