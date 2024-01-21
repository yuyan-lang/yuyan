
#include "../common_include.h"


#include <time.h>


yyvalue yyGetCurrentLocalDateTimeStr() {
    time_t t = time(NULL);
    char *result = ctime(&t);
    return string_to_yyvalue(result);
}

yyvalue yyGetCurrentLocalDateTimeFmt(yyvalue fmt) {
    time_t t = time(NULL);
    struct tm *tm = localtime(&t);
    int bufsz = yyvalue_get_strlen(fmt) * 2;
    char buffer[bufsz];
    strftime(buffer, bufsz, yyvalue_to_string(fmt), tm);
    char *result = strdup(buffer);
    return string_to_yyvalue(result);
}

