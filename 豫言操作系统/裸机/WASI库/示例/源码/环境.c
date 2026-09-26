// 文言：环境：陈诸环境变量，并依参数名而 getenv。
// 汉语：环境：按名字排序打印 environ 里的全部环境变量，再对每个参数打印 getenv 的结果。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern char **environ;
static int 比较(const void *a, const void *b) { return strcmp(*(char *const *)a, *(char *const *)b); }
int main(int argc, char **argv) {
  int n = 0; while (environ && environ[n]) n++;
  char *拷[64]; for (int i = 0; i < n && i < 64; i++) 拷[i] = environ[i];
  qsort(拷, n, sizeof(char *), 比较);
  printf("环境变量 %d 个\n", n);
  for (int i = 0; i < n; i++) printf("  %s\n", 拷[i]);
  for (int i = 1; i < argc; i++) { const char *值 = getenv(argv[i]); printf("getenv(%s) = %s\n", argv[i], 值 ? 值 : "(空)"); }
  return 0;
}
