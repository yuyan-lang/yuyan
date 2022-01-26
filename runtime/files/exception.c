


#include <stdlib.h>
#include <stdio.h>

int internalError(){
    printf("An Internal Error has occurred. The program is likely \
    to be compiled incorrectly. Type Checking perhaps is not sound.\n");
    return -1;
}

uint64_t c_runtime_internal_error(){
    fprintf(stderr, "An Internal Error has occurred. The runtime library is likely \
    to be written incorrectly. Type Checking perhaps is not sound.\n");
    return -1;
}

uint64_t throwException(char* errMsg){
    
    fprintf(stderr, "%s", errMsg);
    exit(-1);
    return -1;
}

uint64_t yyThrowException(char* errMsg){
    
    fprintf(stderr, "%s", errMsg);
    exit(-1);
    return -1;
}