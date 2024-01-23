
#include "../common_include.h"


#include <time.h>


yyvalue yyGetCurrentLocalDateTimeStr() {
    time_t t = time(NULL);
    char *result = ctime(&t);
    return malloc_string_to_yyvalue(strlen(result) + 1, result);
}

yyvalue yyGetCurrentLocalDateTimeFmt(yyvalue fmt) {
    time_t t = time(NULL);
    struct tm *tm = localtime(&t);
    int bufsz = yyvalue_get_strlen(fmt) * 2;
    char buffer[bufsz];
    strftime(buffer, bufsz, yyvalue_to_string(fmt), tm);
    yyvalue ret = malloc_string_to_yyvalue(strlen(buffer) + 1, buffer);
    return ret;
}

