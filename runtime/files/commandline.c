#include "globalInclude.h"

yy_ptr yyGetCommandLineProgramName(){
    return string_to_addr(global_argv[0]);
}
yy_ptr yyGetCommandLineArgs(){
    if(global_argc < 2){
        return iso_list_nil_to_addr();
    }
    yy_ptr argPtrString[global_argc - 1];
    for (int i = 0; i < global_argc - 1; i ++){
        argPtrString[i] = string_to_addr(global_argv[i+1]);
    }
    return array_to_iso_addr(global_argc - 1, argPtrString);
}