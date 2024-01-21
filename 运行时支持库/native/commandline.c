#include "common_include.h"

yyvalue yyGetCommandLineProgramName(){
    return string_to_yyvalue(global_argv[0]);
}


yyvalue yyGetCommandLineArgs(){

    if(global_argc < 2){
        yyvalue elems[] = {};
        return array_to_iso_addr(0, elems);
    }

    yyvalue argPtrString[global_argc - 1];
    
    for (int i = 0; i < global_argc - 1; i ++){
        argPtrString[i] = string_to_yyvalue(global_argv[i+1]);
    }

    yyvalue result =  array_to_iso_addr(global_argc - 1, argPtrString);
    return result;
}