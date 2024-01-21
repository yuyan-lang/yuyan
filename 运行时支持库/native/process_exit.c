

#include "common_include.h"


yyvalue yyProcessExit(yyvalue exitStatusAddr){
    int64_t st = addr_to_int(exitStatusAddr);
    exit(st);
}