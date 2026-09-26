// 文言：排序：诸行依字节序而排，可倒可按数。
// 汉语：排序（sort）：用法 排序 [-r] [-n] [文件…]；按字节序比较行，-r 倒序，-n 按行首的整数比较（相等再按字节序）；无文件则读标准输入。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static int 倒 = 0, 数 = 0;
static int 比较(const void *a, const void *b) {
  const char *x = *(char *const *)a, *y = *(char *const *)b; int r;
  if (数) { long p = atol(x), q = atol(y); r = (p > q) - (p < q); if (r == 0) r = strcmp(x, y); }
  else r = strcmp(x, y);
  return 倒 ? -r : r;
}
static char **行们 = NULL; static size_t 个数 = 0, 容 = 0;
static void 读入(FILE *f) {
  char 缓[4096];
  while (fgets(缓, sizeof 缓, f)) {
    size_t n = strlen(缓); if (n && 缓[n - 1] == '\n') 缓[--n] = 0;
    if (个数 == 容) { 容 = 容 ? 容 * 2 : 64; 行们 = realloc(行们, 容 * sizeof(char *)); }
    行们[个数++] = strdup(缓);
  }
}
int main(int argc, char **argv) {
  int i = 1, 文件数 = 0;
  for (; i < argc; i++) {
    if (!strcmp(argv[i], "-r")) 倒 = 1; else if (!strcmp(argv[i], "-n")) 数 = 1;
    else { FILE *f = fopen(argv[i], "r"); if (!f) { fprintf(stderr, "排序：无法打开 %s\n", argv[i]); return 1; } 读入(f); fclose(f); 文件数++; }
  }
  if (!文件数) 读入(stdin);
  qsort(行们, 个数, sizeof(char *), 比较);
  for (size_t k = 0; k < 个数; k++) puts(行们[k]);
  return 0;
}
