#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int64_t 斐波那契(int64_t 数) {
    if (数 < 2) {
        return 数;
    }
    return 斐波那契(数 - 1) + 斐波那契(数 - 2);
}

static int64_t 素数筛(int64_t 上限) {
    uint8_t *是否质数 = malloc((size_t)上限 + 1);
    if (是否质数 == NULL) {
        fputs("内存分配失败\n", stderr);
        exit(1);
    }
    memset(是否质数, 1, (size_t)上限 + 1);
    是否质数[0] = 0;
    是否质数[1] = 0;
    for (int64_t 质数 = 2; 质数 * 质数 <= 上限; 质数++) {
        if (是否质数[质数]) {
            for (int64_t 倍数 = 质数 * 质数; 倍数 <= 上限; 倍数 += 质数) {
                是否质数[倍数] = 0;
            }
        }
    }
    int64_t 个数 = 0;
    for (int64_t 数值 = 2; 数值 <= 上限; 数值++) {
        个数 += 是否质数[数值] != 0;
    }
    free(是否质数);
    return 个数;
}

static int64_t 矩阵乘法(int64_t 边长) {
    int64_t 总长 = 边长 * 边长;
    int64_t *矩阵甲 = calloc((size_t)总长, sizeof(*矩阵甲));
    int64_t *矩阵乙 = calloc((size_t)总长, sizeof(*矩阵乙));
    int64_t *矩阵丙 = calloc((size_t)总长, sizeof(*矩阵丙));
    if (矩阵甲 == NULL || 矩阵乙 == NULL || 矩阵丙 == NULL) {
        fputs("内存分配失败\n", stderr);
        exit(1);
    }
    for (int64_t 序数 = 0; 序数 < 总长; 序数++) {
        int64_t 行 = 序数 / 边长;
        int64_t 列 = 序数 - 行 * 边长;
        矩阵甲[序数] = 行 + 列 + 1;
        矩阵乙[序数] = 行 * 2 + 列 + 1;
    }
    for (int64_t 行 = 0; 行 < 边长; 行++) {
        for (int64_t 列 = 0; 列 < 边长; 列++) {
            int64_t 总和 = 0;
            for (int64_t 中间序数 = 0; 中间序数 < 边长; 中间序数++) {
                总和 += 矩阵甲[行 * 边长 + 中间序数] * 矩阵乙[中间序数 * 边长 + 列];
            }
            矩阵丙[行 * 边长 + 列] = 总和;
        }
    }
    int64_t 校验和 = 矩阵丙[0] + 矩阵丙[总长 - 1];
    free(矩阵甲);
    free(矩阵乙);
    free(矩阵丙);
    return 校验和;
}

static void 交换(int64_t *数组, int64_t 左序, int64_t 右序) {
    int64_t 数值 = 数组[左序];
    数组[左序] = 数组[右序];
    数组[右序] = 数值;
}

static int64_t 分区(int64_t *数组, int64_t 下界, int64_t 上界) {
    int64_t 中点 = 下界 + (上界 - 下界) / 2;
    交换(数组, 中点, 上界);
    int64_t 枢轴 = 数组[上界];
    int64_t 存放序数 = 下界;
    for (int64_t 序数 = 下界; 序数 < 上界; 序数++) {
        if (数组[序数] < 枢轴) {
            交换(数组, 序数, 存放序数);
            存放序数++;
        }
    }
    交换(数组, 存放序数, 上界);
    return 存放序数;
}

static void 快速排序(int64_t *数组, int64_t 下界, int64_t 上界) {
    if (下界 < 上界) {
        int64_t 分区序数 = 分区(数组, 下界, 上界);
        快速排序(数组, 下界, 分区序数 - 1);
        快速排序(数组, 分区序数 + 1, 上界);
    }
}

static int64_t 快速排序基准(int64_t 长度) {
    int64_t *数组 = malloc((size_t)长度 * sizeof(*数组));
    if (数组 == NULL) {
        fputs("内存分配失败\n", stderr);
        exit(1);
    }
    for (int64_t 序数 = 0; 序数 < 长度; 序数++) {
        数组[序数] = 长度 - 序数;
    }
    快速排序(数组, 0, 长度 - 1);
    int64_t 校验和 = 数组[0] + 数组[长度 / 2] + 数组[长度 - 1];
    free(数组);
    return 校验和;
}

int main(int 参数数量, char **参数值) {
    if (参数数量 != 3) {
        fprintf(stderr, "用法：%s <斐波那契|素数筛|矩阵乘法|快速排序> <规模>\n", 参数值[0]);
        return 1;
    }
    int64_t 规模 = strtoll(参数值[2], NULL, 10);
    int64_t 结果;
    if (strcmp(参数值[1], "斐波那契") == 0) {
        结果 = 斐波那契(规模);
    } else if (strcmp(参数值[1], "素数筛") == 0) {
        结果 = 素数筛(规模);
    } else if (strcmp(参数值[1], "矩阵乘法") == 0) {
        结果 = 矩阵乘法(规模);
    } else if (strcmp(参数值[1], "快速排序") == 0) {
        结果 = 快速排序基准(规模);
    } else {
        fputs("未知算法\n", stderr);
        return 1;
    }
    printf("%lld\n", (long long)结果);
    return 0;
}
