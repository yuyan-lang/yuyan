
#include "../common_include.h"


#include <time.h>


yyvalue yyGetCurrentLocalDateTimeStr() {
    time_t t = time(NULL);
    char *result = ctime(&t);
    return result;
}

char * yyGetCurrentLocalDateTimeFmt(char * fmt) {
    time_t t = time(NULL);
    struct tm *tm = localtime(&t);
    int bufsz = strlen(fmt) * 2;
    char buffer[bufsz];
    strftime(buffer, bufsz, fmt, tm);
    char *result = strdup(buffer);
    return result;
}

