
#include "common_include.h"

#include "gc.h"

extern void optional_entry_initialization();

int global_argc = 0;
char** global_argv = NULL;


// Function to process @yy: arguments
void processYYArguments(int argc, char* argv[]) {
    char* program_name = argv[0];
    int total_consumed_argc = 0;
    for (int i = 1; i < argc; i++)
    {
        if (strncmp(argv[i], "@yy:useprofiler=1", 16) == 0) {
            use_profiler = true;
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


    optional_entry_initialization();

    // initialize global exception handler
    yy_豫言初始化全局异常处理器();


    // initialize random seed
    srand ( time ( NULL));

    


    return yy_runtime_start();
}




