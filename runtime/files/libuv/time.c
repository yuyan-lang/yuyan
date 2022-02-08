
#include "../globalInclude.h"

yy_ptr yyCurrentNanosecondTime(){
    uint64_t currentTime = uv_hrtime();
    return int_to_addr((int64_t) currentTime);
}