// 文言：示之以命令行、浮点之出、退出码。
// 汉语：打印参数个数与各参数，再打印几个浮点数（printf 的 %f 与 libm 的 sqrt、sin），最后以退出码 3 结束。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
int main(int argc, char **argv) {
  printf("hello wasi %d\n", argc);
  for (int i = 0; i < argc; i++) printf("arg%d=%s\n", i, argv[i]);
  printf("pi=%f sqrt2=%.10f sin1=%.10f\n", 3.14159265358979, sqrt(2.0), sin(1.0));
  return 3;
}
