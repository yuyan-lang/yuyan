
#include "../公共包含.h"


#include <time.h>


豫言值 豫言_获取当前本地日期时间字符串() {
    time_t 时刻 = time(NULL);
    char *结果 = ctime(&时刻);
    return 复制字符串为豫言值(strlen(结果) + 1, 结果);
}

豫言值 豫言_格式化当前本地日期时间(豫言值 格式) {
    time_t 时刻 = time(NULL);
    struct tm *本地时刻 = localtime(&时刻);
    int 缓冲区大小 = 获取豫言_字符串长度(格式) * 2;
    char 缓冲区[缓冲区大小];
    strftime(缓冲区, 缓冲区大小, 豫言值转字符串(格式), 本地时刻);
    return 复制字符串为豫言值(strlen(缓冲区) + 1, 缓冲区);
}
