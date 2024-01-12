
#include "common_include.h"

#include "gc.h"

extern void optional_entry_initialization();

int global_argc = 0;
char** global_argv = NULL;

// runtime configurations
int64_t use_libgc = 1;

extern int64_t entryMain(); // the entry to yy llvm ir

int main(int argc, char* argv[]) {

    // save global paramters
    global_argc = argc;
    global_argv = argv;

    // Check if the first command line argument matches the desired pattern
    if (argc > 1 && strcmp(argv[1], "@yy:uselibgc=0") == 0) {
        use_libgc = 0;

        // Update global_argv to exclude the first argument
        global_argc--;
        global_argv++;
    }
    
    // disable GC by setting env var
    // Specify the name of the environment variable you want to read
    const char *env_var_name = "YY_DISABLE_GC";
    char *env_var_value = getenv(env_var_name);
    if (env_var_value != NULL && strcmp(env_var_value, "1") == 0) {
        use_libgc = 0;
    } 


    optional_entry_initialization();

    // initialize global exception handler
    initialize_global_exception_handler();
    yy_豫言初始化全局异常处理器();


    // initialize random seed
    srand ( time ( NULL));

    


#ifdef OLD_ENTRY
    return entryMain();
#else
    return yy_runtime_start();
#endif
}





int informResult (uint64_t result[]) {
    // fprintf(stderr, "代码已经运行成功，结果是：");
    // informResultRec(stderr, result, 0);
    // fprintf(stderr, "\n");
    // do not inform result as we're moving to a mature language
    return 0;
}

