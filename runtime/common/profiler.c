#include "common_include.h"


FILE* profileFile;
yy_ptr *local_stack_ptr;
extern yy_ptr* stack_ptr;
extern yy_ptr* stack;
extern yy_function_type current_function;
extern pthread_mutex_t stack_ptr_mutex;
extern int64_t entryMain(); 
bool use_profiler;
void *interruptFunction(void *arg)
{
    // fprintf(stderr, "Starting Profiler Thread: Stack offset: %ld\n", stack_ptr - stack);
    while (1) {
        // prevent loop optimization
        // fprintf(stderr, "s");
        // if (stack_ptr == NULL){
        //     continue;
        // }
        // if (stack_ptr == NULL){
        //     continue;
        // }
        // if (current_function == NULL) {
        //     continue;
        // }
        // Declare a local stack pointer for manipulation
        pthread_mutex_lock(&stack_ptr_mutex);
        local_stack_ptr = stack_ptr;
        fprintf(profileFile, "%p ", (void*)current_function);
        int64_t stack_offset;
        while (local_stack_ptr - stack > 3)
        {
            // Write function pointer address to the file
            fprintf(profileFile, "%p ", (local_stack_ptr[-2]));
            stack_offset = ((int64_t)(local_stack_ptr[-3]));
            local_stack_ptr = (local_stack_ptr - 3 - stack_offset);
        }
        pthread_mutex_unlock(&stack_ptr_mutex);

        // fprintf(stderr, "Profiler: Stack offset: %ld\n", stack_ptr - stack);
        fprintf(profileFile, "\n");
        
        // Flush the file to ensure data is written immediately
        fflush(profileFile);        


        // Sleep for 50 milliseconds
        usleep(50000);
    }
}

void start_yy_profiler() {
    pthread_t interruptThread;


    // Open the profile file before execution
    profileFile = fopen("yy_profiledata.txt", "w");
    if (profileFile == NULL) {
        errorAndAbort("Error opening profile file\n");
        return;
    }
    
    fprintf(profileFile, "%s %p\n", global_argv[0], (void*)entryMain);
    fflush(profileFile);

    // Create a thread for the interrupt function
    if (pthread_create(&interruptThread, NULL, interruptFunction, NULL)) {
        errorAndAbort("Error creating profiler thread\n");
        return;
    }

    return;
}
