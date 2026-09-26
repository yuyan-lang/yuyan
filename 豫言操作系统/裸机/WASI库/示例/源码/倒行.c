// 文言：倒行：诸行倒序而出。
// 汉语：倒行（tac）：读标准输入或给定文件的全部内容，按行倒序输出。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(int argc, char **argv) {
  FILE *f = stdin;
  if (argc > 1) { f = fopen(argv[1], "r"); if (!f) { fprintf(stderr, "倒行：无法打开 %s\n", argv[1]); return 1; } }
  size_t 容 = 4096, 用 = 0; char *缓 = malloc(容); size_t 读;
  while ((读 = fread(缓 + 用, 1, 容 - 用, f)) > 0) { 用 += 读; if (用 == 容) { 容 *= 2; 缓 = realloc(缓, 容); } }
  size_t 终 = 用;
  if (终 && 缓[终 - 1] == '\n') 终--;
  while (终 > 0 || 用 > 0) {
    size_t 起 = 终; while (起 > 0 && 缓[起 - 1] != '\n') 起--;
    fwrite(缓 + 起, 1, 终 - 起, stdout); putchar('\n');
    if (起 == 0) break;
    终 = 起 - 1;
  }
  return 0;
}
