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

/* 文言：同题直书于 C，专其型；汉语：手写 C 的专用类型参照，非豫言后端等价物。 */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

typedef struct 树 { struct 树 *左, *右; uint64_t 值; } 树;
static 树 *建树(uint64_t 深) {
 树 *物 = malloc(sizeof *物); if (!物) abort();
 物->值 = 1; 物->左 = 深 ? 建树(深-1) : NULL; 物->右 = 深 ? 建树(深-1) : NULL; return 物;
}
static uint64_t 树和(树 *物) { return 物->左 ? 树和(物->左)+树和(物->右) : 物->值; }
static void 释树(树 *物) { if (物->左) { 释树(物->左); 释树(物->右); } free(物); }

#include <time.h>
/* 文言：钟单调，参每轮读取。汉语：保持算法，volatile 输入阻止跨轮将纯计算提升出去。 */
static double 单调秒(void) { struct timespec t; clock_gettime(CLOCK_MONOTONIC, &t); return t.tv_sec + t.tv_nsec * 1e-9; }
/* 文言：造消相接，试去中树。汉语：诊断用的等价递归融合；保留两个递归表达式，不手写闭式公式。 */
static uint64_t 融合树(uint64_t 深) { return 深 ? 融合树(深-1)+融合树(深-1) : 1; }
int main(int argc, char **argv) {
 if(argc != 3) return 2;
 volatile int64_t 规模 = strtoll(argv[2], 0, 10);
 for(int 轮=0; 轮<=5; ++轮) {
  int64_t 数=规模, 果=0;
  double 始=单调秒();
  if(!strcmp(argv[1],"斐波那契")) 果=斐波那契(数);
  else if(!strcmp(argv[1],"素数筛")) 果=素数筛(数);
  else if(!strcmp(argv[1],"矩阵乘法")) 果=矩阵乘法(数);
  else if(!strcmp(argv[1],"快速排序")) 果=快速排序基准(数);
  else if(!strcmp(argv[1],"树基准")) { 果=融合树(数); }
  else if(!strcmp(argv[1],"数组基准")) { int64_t *组=malloc(数*sizeof(*组)); if(!组 && 数) return 1;
   for(int64_t i=数;i;i--)组[i-1]=i;
   for(int64_t i=数;i;i--)果+=组[i-1]; free(组);
  } else return 2;
  double 秒=单调秒()-始;
  printf("%s,%d,%.9f,%lld\n",argv[1],轮,秒,(long long)果);
 }
 return 0;
}

/* 文言：批行以免时钟之微。汉语：相同批量循环，深度在四个值间变化，累加结果可观察。 */
__attribute__((export_name("batch"))) uint64_t 批量融合(uint64_t 深, uint32_t 次) { uint64_t 和=0; for (uint32_t i=0;i<次;i++) 和+=融合树(深+(i&3)); return 和; }
