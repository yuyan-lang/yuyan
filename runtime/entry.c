


#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
extern int entryMain(); // the entry to yy llvm ir


int global_argc = 0;
char** global_argv = NULL;
int main(int argc, char* argv[]) {
    global_argc = argc;
    global_argv = argv;
    return entryMain();
}


int informResult (int64_t result[]) {
    // assume it is a tuple
    int64_t header = result[0];
    int type = (header >> (64 - 5)) &  0b11111;
    int length = (header >> (64 - 15)) &  0b1111111111;

    switch (type)
    {
    case 1:
        fprintf(stderr,"Received a function closure (length is %d). Did you define a function?\n", length);
        for (int i = 0; i < length ; i ++){
            fprintf(stderr,"%d : ", i);
            informResult((int64_t *)result[i+1]);
        }
        break;
    case 2:
        fprintf(stderr,"卷");
        break;
    case 3:
        fprintf(stderr,"PROD");
        break;
    case 4:
        fprintf(stderr,"SUM");
        break;
    
    
    default:
        fprintf(stderr, "ERROR: result is not interpretable. because the type %d is not defined for the runtime.\n", type);
        break;
    }

    return 0;
}