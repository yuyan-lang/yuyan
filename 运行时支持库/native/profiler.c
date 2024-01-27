#include "common_include.h"


// FILE* profileFile;
// yyvalue *local_stack_ptr;

// void fprint_stack_trace(FILE *f, const char * separator){
//     local_stack_ptr = stack_ptr;
//     if (current_function != NULL){
//         fprintf(f, "%p ", (void*)current_function);
//     }
//     int64_t stack_offset;
//     while (local_stack_ptr - stack > 3)
//     {
//         // Write function pointer address to the file
//         fprintf(f, "%p", yyvalue_to_funcptr(local_stack_ptr[-2]));
//         fprintf(f, "%s", separator);
//         stack_offset = (yyvalue_to_int(local_stack_ptr[-3]));
//         local_stack_ptr = (local_stack_ptr - 3 - stack_offset);
//     }

// }

bool use_profiler;
// void *interruptFunction(void *arg)
// {
//     // fprintf(stderr, "Starting Profiler Thread: Stack offset: %ld\n", stack_ptr - stack);
//     while (1) {
//         // prevent loop optimization
//         // fprintf(stderr, "s");
//         // if (stack_ptr == NULL){
//         //     continue;
//         // }
//         // if (stack_ptr == NULL){
//         //     continue;
//         // }
//         // if (current_function == NULL) {
//         //     continue;
//         // }
//         // Declare a local stack pointer for manipulation
//         pthread_mutex_lock(&stack_ptr_mutex);
//         fprint_stack_trace(profileFile, " ");
//         pthread_mutex_unlock(&stack_ptr_mutex);

//         // fprintf(stderr, "Profiler: Stack offset: %ld\n", stack_ptr - stack);
//         fprintf(profileFile, "\n");
        
//         // Flush the file to ensure data is written immediately
//         fflush(profileFile);        


//         // Sleep for 50 milliseconds
//         usleep(50000);
//     }
// }

// void start_yy_profiler() {
//     pthread_t interruptThread;


//     // Open the profile file before execution
//     profileFile = fopen("yy_profiledata.txt", "w");
//     if (profileFile == NULL) {
//         errorAndAbort("Error opening profile file\n");
//         return;
//     }
    
//     fprintf(profileFile, "%s %p\n", global_argv[0], (void*)entryMain);
//     fflush(profileFile);

//     // Create a thread for the interrupt function
//     if (pthread_create(&interruptThread, NULL, interruptFunction, NULL)) {
//         errorAndAbort("Error creating profiler thread\n");
//         return;
//     }

//     return;
// }
