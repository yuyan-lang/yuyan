// 文言：筛素数、快速排序、二叉树、八皇后、SHA-256、浮点与格式化，诸算并试，输出可与他处逐字相校。
// 汉语：综合计算：素数筛、qsort、二叉搜索树（malloc 与 memory.grow）、八皇后、SHA-256、libm、printf 各种格式；输出确定，用来与 Node 自带 WASI 的运行结果逐字对照。
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static uint32_t 种 = 12345;
static uint32_t 随机(void) { 种 = 种 * 1664525u + 1013904223u; return 种; }
static int 比较(const void *a, const void *b) {
  int x = *(const int *)a, y = *(const int *)b;
  return (x > y) - (x < y);
}
typedef struct 节点 { int 值; struct 节点 *左, *右; } 节点;
static 节点 *插入(节点 *根, int 值) {
  if (!根) { 根 = malloc(sizeof(节点)); 根->值 = 值; 根->左 = 根->右 = NULL; return 根; }
  if (值 < 根->值) 根->左 = 插入(根->左, 值); else 根->右 = 插入(根->右, 值);
  return 根;
}
static int 深度(节点 *根) { if (!根) return 0; int a = 深度(根->左), b = 深度(根->右); return 1 + (a > b ? a : b); }
static void 释放(节点 *根) { if (!根) return; 释放(根->左); 释放(根->右); free(根); }
static int 皇后(int n, int 行, unsigned 列, unsigned 斜一, unsigned 斜二) {
  if (行 == n) return 1;
  int 数 = 0;
  for (int c = 0; c < n; c++) {
    unsigned a = 1u << c, b = 1u << (行 + c), d = 1u << (行 - c + n);
    if ((列 & a) || (斜一 & b) || (斜二 & d)) continue;
    数 += 皇后(n, 行 + 1, 列 | a, 斜一 | b, 斜二 | d);
  }
  return 数;
}
static uint32_t 右转(uint32_t x, int n) { return (x >> n) | (x << (32 - n)); }
static void 散列(const unsigned char *消息, size_t 长, uint32_t 输出[8]) {
  static const uint32_t K[64] = {
    0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
    0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
    0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
    0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
    0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
    0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
    0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
    0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2 };
  uint32_t h[8] = {0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19};
  unsigned char 块[128]; memset(块, 0, sizeof 块);
  memcpy(块, 消息, 长); 块[长] = 0x80;
  size_t 总块 = (长 + 9 <= 64) ? 1 : 2;
  uint64_t 位 = (uint64_t)长 * 8;
  for (int i = 0; i < 8; i++) 块[总块 * 64 - 1 - i] = (unsigned char)(位 >> (8 * i));
  for (size_t b = 0; b < 总块; b++) {
    uint32_t w[64];
    for (int i = 0; i < 16; i++) w[i] = ((uint32_t)块[b*64+i*4] << 24) | ((uint32_t)块[b*64+i*4+1] << 16) | ((uint32_t)块[b*64+i*4+2] << 8) | 块[b*64+i*4+3];
    for (int i = 16; i < 64; i++) {
      uint32_t s0 = 右转(w[i-15],7) ^ 右转(w[i-15],18) ^ (w[i-15] >> 3);
      uint32_t s1 = 右转(w[i-2],17) ^ 右转(w[i-2],19) ^ (w[i-2] >> 10);
      w[i] = w[i-16] + s0 + w[i-7] + s1;
    }
    uint32_t a=h[0],bb=h[1],c=h[2],d=h[3],e=h[4],f=h[5],g=h[6],hh=h[7];
    for (int i = 0; i < 64; i++) {
      uint32_t S1 = 右转(e,6) ^ 右转(e,11) ^ 右转(e,25), ch = (e & f) ^ (~e & g);
      uint32_t t1 = hh + S1 + ch + K[i] + w[i];
      uint32_t S0 = 右转(a,2) ^ 右转(a,13) ^ 右转(a,22), maj = (a & bb) ^ (a & c) ^ (bb & c);
      uint32_t t2 = S0 + maj;
      hh=g; g=f; f=e; e=d+t1; d=c; c=bb; bb=a; a=t1+t2;
    }
    h[0]+=a; h[1]+=bb; h[2]+=c; h[3]+=d; h[4]+=e; h[5]+=f; h[6]+=g; h[7]+=hh;
  }
  memcpy(输出, h, sizeof h);
}

int main(void) {
  enum { N = 20000 };
  static unsigned char 合[N + 1];
  int 个数 = 0; long 和 = 0;
  for (int i = 2; i <= N; i++)
    if (!合[i]) { 个数++; 和 += i; for (long j = (long)i * i; j <= N; j += i) 合[j] = 1; }
  printf("素数 %d 个，和 %ld\n", 个数, 和);

  int *数组 = malloc(sizeof(int) * 5000);
  for (int i = 0; i < 5000; i++) 数组[i] = (int)((随机() >> 8) % 100000);
  qsort(数组, 5000, sizeof(int), 比较);
  uint64_t 校验 = 1469598103934665603ull;
  for (int i = 0; i < 5000; i++) { 校验 ^= (uint64_t)数组[i]; 校验 *= 1099511628211ull; }
  printf("排序后首 %d 末 %d 校验 %llu\n", 数组[0], 数组[4999], (unsigned long long)校验);
  节点 *根 = NULL;
  for (int i = 0; i < 2000; i++) 根 = 插入(根, 数组[(i * 7919) % 5000] + i);
  printf("树深 %d\n", 深度(根));
  释放(根); free(数组);

  double 巴 = 0; for (int i = 1; i <= 2000; i++) 巴 += 1.0 / ((double)i * i);
  printf("巴塞尔 %.12f 距 %.3e\n", 巴, 3.14159265358979323846 * 3.14159265358979323846 / 6 - 巴);
  printf("exp1=%.12f log2=%.12f cos1=%.12f atan1=%.12f pow=%.6f tanh=%.9f\n", exp(1.0), log(2.0), cos(1.0), atan(1.0), pow(2.0, 0.5), tanh(0.5));
  float 甲 = 1.1f, 乙 = 2.2f; printf("单精度 %.7f %.7f %.7f\n", 甲 * 乙, 甲 / 乙, sqrtf(乙));

  char 缓[128];
  snprintf(缓, sizeof 缓, "%s-%05d-%x-%c-%8.3f|%-6s|%+d", "abc", 42, 255, 'Z', 3.14159, "ab", 7);
  printf("%s\n", 缓);
  printf("strtod %.1f strtol %ld atoi %d\n", strtod("1.5e3xyz", NULL), strtol("-12345", NULL, 10), atoi("789"));
  int64_t a = INT64_C(-9223372036854775807) - 1; uint64_t b = UINT64_MAX;
  printf("i64 %lld u64 %llu 除 %lld 余 %lld\n", (long long)a, (unsigned long long)b, (long long)(a / 7), (long long)(a % 7));
  printf("八皇后 %d 个解\n", 皇后(8, 0, 0, 0, 0));
  uint32_t 输出[8]; 散列((const unsigned char *)"abc", 3, 输出);
  printf("sha256(abc)=");
  for (int i = 0; i < 8; i++) printf("%08x", 输出[i]);
  printf("\n");
  return 0;
}
