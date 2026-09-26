// 文言：标准出与标准错交替，atexit 之回调，参数之陈，首参为退出码。
// 汉语：标准输出与标准错误交替输出（各自 fflush）、atexit 回调、逐个打印命令行参数；有参数时以第一个参数作退出码 exit。
#include <stdio.h>
#include <stdlib.h>
static void 退出前(void) { printf("atexit 已运行\n"); }
int main(int argc, char **argv) {
  atexit(退出前);
  fprintf(stderr, "错误流一行\n");
  printf("标准输出一行\n");
  fflush(stdout);
  fputs("错误流二\n", stderr);
  for (int i = 1; i < argc; i++) printf("参数 %d 是 [%s]\n", i, argv[i]);
  if (argc > 1) exit(atoi(argv[1]));
  return 0;
}
