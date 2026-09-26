// 文言：头：印每一文件之首若干行，无文件则读标准入。
// 汉语：头（head）：用法 头 [-n N] [文件…]，默认 10 行；有多个文件时每个前加 “==> 文件 <==” 的标题。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static int 印首(FILE *f, long n) {
  char 缓[4096]; long 已 = 0;
  while (已 < n && fgets(缓, sizeof 缓, f)) {
    fputs(缓, stdout);
    if (strchr(缓, '\n')) 已++;
  }
  return 0;
}
int main(int argc, char **argv) {
  long n = 10; int i = 1;
  if (i + 1 < argc && !strcmp(argv[i], "-n")) { n = atol(argv[i + 1]); i += 2; }
  if (i >= argc) return 印首(stdin, n);
  int 多 = argc - i > 1, 状态 = 0, 已 = 0;
  for (; i < argc; i++) {
    FILE *f = fopen(argv[i], "r");
    if (!f) { fprintf(stderr, "头：无法打开 %s\n", argv[i]); 状态 = 1; continue; }
    if (多) { printf("%s==> %s <==\n", 已 ? "\n" : "", argv[i]); 已 = 1; }
    印首(f, n); fclose(f);
  }
  return 状态;
}
