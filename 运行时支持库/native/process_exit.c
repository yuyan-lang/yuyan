

#include "common_include.h"


yyvalue yyProcessExit(yyvalue exitStatusAddr){
    int64_t st = yyvalue_to_int(exitStatusAddr);
    exit(st);
    return unit_to_yyvalue();
}