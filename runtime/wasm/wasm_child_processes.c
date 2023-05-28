#include "wasm_include.h"





// https://stackoverflow.com/questions/14751504/capture-a-child-processs-stdout-with-libuv
// program is a string var, arguments is a list of string vars!
yy_ptr yyRunProcessGetOutputSync(yy_ptr program, yy_ptr arguments)
{
    char* programName = addr_to_string(program);
    char argumentCount = iso_list_get_length(arguments);
    yy_ptr* argumentArray = iso_list_get_elements(arguments);
    char* args[argumentCount+2];
    args[0] = programName;
    for(int i = 0; i < argumentCount; i ++){
        args[i+1] = addr_to_string(argumentArray[i]);
    }
    args[argumentCount+1]= NULL;

    // Calculate the required buffer size
    size_t bufferSize = 1024;
    for(int i = 1; i <= argumentCount; i++){
        bufferSize += strlen(args[i]) + 1; // +1 for space separator
    }

    char* errorBuffer = (char*)yy_gcAllocateBytes(bufferSize);
    if(errorBuffer == NULL){
        // Error: Failed to allocate memory
        errorAndAbort("Memory allocation failed!");
    }

    // Append the argument list to the error message
    for(int i = 0; i < argumentCount; i++){
        strcat(errorBuffer, args[i+1]);
        strcat(errorBuffer, " ");
    }
    strcat(errorBuffer, "\n");

    errorAndAbort(errorBuffer);
    return NULL;
}

yy_ptr yyRunProcessSync(yy_ptr program, yy_ptr arguments)
{
     char* programName = addr_to_string(program);
    char argumentCount = iso_list_get_length(arguments);
    yy_ptr* argumentArray = iso_list_get_elements(arguments);
    char* args[argumentCount+2];
    args[0] = programName;
    for(int i = 0; i < argumentCount; i ++){
        args[i+1] = addr_to_string(argumentArray[i]);
    }
    args[argumentCount+1]= NULL;

    // Calculate the required buffer size
    size_t bufferSize = 1024;
    for(int i = 1; i <= argumentCount; i++){
        bufferSize += strlen(args[i]) + 1; // +1 for space separator
    }

    char* errorBuffer = (char*)yy_gcAllocateBytes(bufferSize);
    if(errorBuffer == NULL){
        // Error: Failed to allocate memory
        errorAndAbort("Memory allocation failed!");
    }

    // Append the argument list to the error message
    for(int i = 0; i < argumentCount; i++){
        strcat(errorBuffer, args[i+1]);
        strcat(errorBuffer, " ");
    }
    strcat(errorBuffer, "\n");

    errorAndAbort(errorBuffer);
    return NULL;
}

yy_ptr yyRunProcessSyncPipeOutput(yy_ptr program, yy_ptr arguments)
{
     char* programName = addr_to_string(program);
    char argumentCount = iso_list_get_length(arguments);
    yy_ptr* argumentArray = iso_list_get_elements(arguments);
    char* args[argumentCount+2];
    args[0] = programName;
    for(int i = 0; i < argumentCount; i ++){
        args[i+1] = addr_to_string(argumentArray[i]);
    }
    args[argumentCount+1]= NULL;

    // Calculate the required buffer size
    size_t bufferSize = 1024;
    for(int i = 1; i <= argumentCount; i++){
        bufferSize += strlen(args[i]) + 1; // +1 for space separator
    }

    char* errorBuffer = (char*)yy_gcAllocateBytes(bufferSize);
    if(errorBuffer == NULL){
        // Error: Failed to allocate memory
        errorAndAbort("Memory allocation failed!");
    }

    // Append the argument list to the error message
    for(int i = 0; i < argumentCount; i++){
        strcat(errorBuffer, args[i+1]);
        strcat(errorBuffer, " ");
    }
    strcat(errorBuffer, "\n");

    errorAndAbort(errorBuffer);
    return NULL;
}

