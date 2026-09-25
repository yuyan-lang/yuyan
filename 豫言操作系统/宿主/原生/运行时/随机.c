#include "公共包含.h"

豫言值 豫言_获取随机整数(豫言值 上界值) {
    uint64_t 上界 = 豫言值转整数(上界值);
    return 整数转豫言值(rand() % 上界);
}

豫言值 豫言_获取随机小数() {
    double 随机数 = (double)rand() / RAND_MAX;
    return 小数转豫言值(随机数);
}
