#include "common_include.h"

yy_ptr yyGetCommandLineProgramName(){
    return string_to_addr(global_argv[0]);
}


yy_ptr yyGetCommandLineArgs(){

    if(global_argc < 2){
        yy_ptr elems[] = {};
        return array_to_iso_addr(0, elems);
    }

    yy_ptr argPtrString[global_argc - 1];
    
    for (int i = 0; i < global_argc - 1; i ++){
        argPtrString[i] = string_to_addr(global_argv[i+1]);
    }

    yy_ptr result =  array_to_iso_addr(global_argc - 1, argPtrString);
    return result;
}