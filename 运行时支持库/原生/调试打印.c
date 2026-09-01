#include "公共包含.h"

#include <stdarg.h>


void 打印调试参数(uint64_t 数量, const char *函数名, ...) {
    va_list 参数;
    va_start(参数, 函数名);
    fprintf(stderr, "%s 的调用参数：", 函数名);

    豫言值 参数值;
    参数值 = va_arg(参数, 豫言值);
    fprintf(stderr, "返回值 = ");
    打印豫言值(参数值, 0);
    fprintf(stderr, ", ");

    for (int 序数 = 1; 序数 < 数量; 序数++) {
        参数值 = va_arg(参数, 豫言值);
        fprintf(stderr, "参数 %d = ", 序数);
        打印豫言值(参数值, 0);
        if (序数 != 数量 - 1) {
            fprintf(stderr, ", ");
        }
    }
    va_end(参数);

    fprintf(stderr, "\n");
}
