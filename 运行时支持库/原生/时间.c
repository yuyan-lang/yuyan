
#include "公共包含.h"

豫言值 豫言_获取当前纳秒时间() {
    struct timespec 时刻;
    clock_gettime(CLOCK_MONOTONIC, &时刻);

    // 把秒和纳秒统一换算成纳秒。
    int64_t 纳秒 = 时刻.tv_sec * 1000000000 + 时刻.tv_nsec;

    double 纳秒小数 = (double) 纳秒;

    return 小数转豫言值(纳秒小数);
}
