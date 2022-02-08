

#include "globalInclude.h"


yy_ptr yyProcessExit(yy_ptr exitStatusAddr){
    int64_t st = addr_to_int(exitStatusAddr);
    exit(st);
}