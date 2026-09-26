// 文言：去重：相邻之同行并为一。
// 汉语：去重（uniq）：用法 去重 [-c] [文件]；把相邻的相同行合成一行，-c 在行首加出现次数（宽 7 的整数、一个空格）；先排序再去重才能去掉全部重复。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(int argc, char **argv) {
  int 计 = 0, i = 1;
  if (i < argc && !strcmp(argv[i], "-c")) { 计 = 1; i++; }
  FILE *f = stdin;
  if (i < argc) { f = fopen(argv[i], "r"); if (!f) { fprintf(stderr, "去重：无法打开 %s\n", argv[i]); return 1; } }
  char 前[4096] = "", 缓[4096]; long 数 = 0; int 有 = 0;
  while (fgets(缓, sizeof 缓, f)) {
    size_t n = strlen(缓); if (n && 缓[n - 1] == '\n') 缓[--n] = 0;
    if (有 && !strcmp(缓, 前)) { 数++; continue; }
    if (有) { if (计) printf("%7ld ", 数); puts(前); }
    strcpy(前, 缓); 数 = 1; 有 = 1;
  }
  if (有) { if (计) printf("%7ld ", 数); puts(前); }
  return 0;
}
