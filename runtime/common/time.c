
#include "common_include.h"

// yy_ptr yyCurrentNanosecondTime(){
//     uint64_t currentTime = uv_hrtime();
//     return int_to_addr((int64_t) currentTime);
// }

yy_ptr yyCurrentNanosecondTime() {
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &time);

    // Convert seconds and nanoseconds to nanoseconds
    int64_t nanoseconds = time.tv_sec * 1000000000 + time.tv_nsec;

    // Return the nanoseconds
    return int_to_addr(nanoseconds);
}