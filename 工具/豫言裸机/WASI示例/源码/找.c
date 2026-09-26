// 文言：找：依字面之词而寻其行。
// 汉语：找（grep -F）：用法 找 [-v] [-n] [-c] 关键词 [文件…]；打印含关键词的行（字面子串），-v 反选，-n 加行号，-c 只打印匹配行数；无文件读标准输入；有多个文件时行首加 文件:。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static int 反 = 0, 号 = 0, 计 = 0;
static int 扫(FILE *f, const char *词, const char *名) {
  char 缓[4096]; long 行 = 0, 中 = 0;
  while (fgets(缓, sizeof 缓, f)) {
    行++;
    int 含 = strstr(缓, 词) != NULL;
    if (含 != 反) { 中++; if (!计) { if (名) printf("%s:", 名); if (号) printf("%ld:", 行); fputs(缓, stdout); if (!strchr(缓, '\n')) putchar('\n'); } }
  }
  if (计) { if (名) printf("%s:", 名); printf("%ld\n", 中); }
  return 中 > 0;
}
int main(int argc, char **argv) {
  int i = 1;
  for (; i < argc && argv[i][0] == '-' && argv[i][1]; i++) {
    if (!strcmp(argv[i], "-v")) 反 = 1; else if (!strcmp(argv[i], "-n")) 号 = 1; else if (!strcmp(argv[i], "-c")) 计 = 1; else break;
  }
  if (i >= argc) { fprintf(stderr, "用法：找 [-v] [-n] [-c] 关键词 [文件…]\n"); return 2; }
  const char *词 = argv[i++]; int 中 = 0;
  if (i >= argc) return 扫(stdin, 词, NULL) ? 0 : 1;
  int 多 = argc - i > 1;
  for (; i < argc; i++) {
    FILE *f = fopen(argv[i], "r");
    if (!f) { fprintf(stderr, "找：无法打开 %s\n", argv[i]); continue; }
    if (扫(f, 词, 多 ? argv[i] : NULL)) 中 = 1;
    fclose(f);
  }
  return 中 ? 0 : 1;
}
