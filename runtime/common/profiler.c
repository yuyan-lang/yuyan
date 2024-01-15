#include "common_include.h"
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>


FILE* profileFile;
extern yy_ptr* stack_ptr;
extern yy_ptr* stack;

void* interruptFunction(void* arg) {
    while (1) {
        // Declare a local stack pointer for manipulation
        yy_ptr* local_stack_ptr = stack_ptr;

        while (local_stack_ptr != stack) {
            // Write function pointer address to the file
            fprintf(profileFile, "%p ", *(void**)(local_stack_ptr - 2));
            local_stack_ptr = *(yy_ptr**)(local_stack_ptr - 3 - *(int*)(local_stack_ptr - 3));
        }

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
    profileFile = fopen("yyprofiledata.txt", "w");
    if (profileFile == NULL) {
        errorAndAbort("Error opening profile file\n");
        return;
    }

    // Create a thread for the interrupt function
    if (pthread_create(&interruptThread, NULL, interruptFunction, NULL)) {
        errorAndAbort("Error creating profiler thread\n");
        return;
    }

    return;
}