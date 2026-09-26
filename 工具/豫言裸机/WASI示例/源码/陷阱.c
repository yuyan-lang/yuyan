// 文言：依参数致四种陷阱，验其止而不害他任务。
// 汉语：按参数触发陷阱：1 abort（unreachable）、2 整数除零、3 线性内存越界、4 间接调用越界；无参数则正常结束。
#include <stdio.h>
#include <stdlib.h>
int main(int argc, char **argv) {
  int 种类 = argc > 1 ? atoi(argv[1]) : 0;
  printf("陷阱种类 %d\n", 种类);
  fflush(stdout);
  volatile int 零 = 0;
  if (种类 == 1) abort();
  if (种类 == 2) printf("%d\n", 100 / 零);
  if (种类 == 3) *(volatile int *)0xFFFFFFF0u = 1;
  if (种类 == 4) { void (*f)(void) = (void (*)(void))(long)(零 + 999999); f(); }
  printf("无陷阱\n");
  return 0;
}
