
#include "common_include.h"

yyvalue yyCurrentNanosecondTime() {
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &time);

    // Convert seconds and nanoseconds to nanoseconds
    int64_t nanoseconds = time.tv_sec * 1000000000 + time.tv_nsec;

    double nanoseconds_double = (double) nanoseconds;

    // Return the nanoseconds
    yyvalue ret =  double_to_yyvalue(nanoseconds_double);
    return ret;
}
