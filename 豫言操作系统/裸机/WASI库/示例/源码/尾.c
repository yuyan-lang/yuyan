// 文言：尾：印末若干行。
// 汉语：尾（tail）：用法 尾 [-n N] [文件…]，默认 10 行；读入全部再从后数起，多个文件时加标题。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static char *读全(FILE *f, size_t *长) {
  size_t 容 = 4096, 用 = 0; char *缓 = malloc(容);
  size_t 读;
  while ((读 = fread(缓 + 用, 1, 容 - 用, f)) > 0) { 用 += 读; if (用 == 容) { 容 *= 2; 缓 = realloc(缓, 容); } }
  *长 = 用; return 缓;
}
static void 印尾(FILE *f, long n) {
  size_t 长; char *缓 = 读全(f, &长);
  long 行数 = 0; for (size_t i = 0; i < 长; i++) if (缓[i] == '\n') 行数++;
  if (长 > 0 && 缓[长 - 1] != '\n') 行数++;
  long 跳 = 行数 > n ? 行数 - n : 0; size_t 起 = 0;
  for (long k = 0; k < 跳; k++) { while (起 < 长 && 缓[起] != '\n') 起++; if (起 < 长) 起++; }
  fwrite(缓 + 起, 1, 长 - 起, stdout); free(缓);
}
int main(int argc, char **argv) {
  long n = 10; int i = 1;
  if (i + 1 < argc && !strcmp(argv[i], "-n")) { n = atol(argv[i + 1]); i += 2; }
  if (i >= argc) { 印尾(stdin, n); return 0; }
  int 多 = argc - i > 1, 状态 = 0, 已 = 0;
  for (; i < argc; i++) {
    FILE *f = fopen(argv[i], "r");
    if (!f) { fprintf(stderr, "尾：无法打开 %s\n", argv[i]); 状态 = 1; continue; }
    if (多) { printf("%s==> %s <==\n", 已 ? "\n" : "", argv[i]); 已 = 1; }
    印尾(f, n); fclose(f);
  }
  return 状态;
}
