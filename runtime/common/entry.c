
#include "common_include.h"

#include "gc.h"

extern int entryMain(); // the entry to yy llvm ir

extern void initialize_global_exception_handler();

int global_argc = 0;
char** global_argv = NULL;

int main(int argc, char* argv[]) {

    // save global paramters
    global_argc = argc;
    global_argv = argv;

    optional_entry_initialization();

    // initialize global exception handler
    initialize_global_exception_handler();
    yy_豫言初始化全局异常处理器();


    // initialize random seed
    srand ( time ( NULL));

    


    return entryMain();
}




int informResult (uint64_t result[]) {
    // fprintf(stderr, "代码已经运行成功，结果是：");
    // informResultRec(stderr, result, 0);
    // fprintf(stderr, "\n");
    // do not inform result as we're moving to a mature language
    return 0;
}

