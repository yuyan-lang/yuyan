
#ifndef 豫言调试打印_H
#define 豫言调试打印_H

#include "公共包含.h"
void 打印豫言值(豫言值 值, uint64_t 深度);
void 打印调试参数(uint64_t 数量, const char *函数名, ...);


#define 调试记录(数量, ...)                                        \
    do                                                        \
    {                                                         \
        打印调试参数(数量, __func__, __VA_ARGS__); \
    } while (0)
#endif
