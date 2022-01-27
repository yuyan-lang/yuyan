

#include "globalInclude.h"


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

uint64_t yyThrowException(yy_ptr err){
    
    fprintf(stderr, "%s", addr_to_string(err));
    exit(-1);
    return -1;
}