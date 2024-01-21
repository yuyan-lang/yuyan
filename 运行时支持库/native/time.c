
#include "common_include.h"

// yyvalue yyCurrentNanosecondTime(){
//     uint64_t currentTime = uv_hrtime();
//     return int_to_yyvalue((int64_t) currentTime);
// }

yyvalue yyCurrentNanosecondTime() {
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &time);

    // Convert seconds and nanoseconds to nanoseconds
    int64_t nanoseconds = time.tv_sec * 1000000000 + time.tv_nsec;

    double nanoseconds_double = (double) nanoseconds;

    // Return the nanoseconds
    return double_to_yyvalue(nanoseconds_double);
}