
#include "common_include.h"

#include "gc.h"

extern void optional_entry_initialization();

int global_argc = 0;
char** global_argv = NULL;

// runtime configurations
int64_t use_libgc = 1;

extern int64_t entryMain(); // the entry to yy llvm ir

// Function to process @yy: arguments
void processYYArguments(int argc, char* argv[]) {
    char* program_name = argv[0];
    int total_consumed_argc = 0;
    for (int i = 1; i < argc; i++)
    {
        if (strncmp(argv[i], "@yy:uselibgc=0", 14) == 0) {
            use_libgc = 0;
            total_consumed_argc++;
        } 
        // Add more options as needed
        // else if (strncmp(argv[i], "@yy:another_option", length_of_option) == 0) {
        //     // Handle another option
        // }
    }

    // Update global_argv to exclude processed arguments
    global_argc -= (total_consumed_argc );
    global_argv += (total_consumed_argc);

    // overwrite consumed arguments
    global_argv[0] = program_name;
}


int main(int argc, char* argv[]) {

    // save global paramters
    global_argc = argc;
    global_argv = argv;

    processYYArguments(argc, argv);
    
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

    


    return entryMain();
}





int informResult (uint64_t result[]) {
    // fprintf(stderr, "代码已经运行成功，结果是：");
    // informResultRec(stderr, result, 0);
    // fprintf(stderr, "\n");
    // do not inform result as we're moving to a mature language
    return 0;
}

