// 文言：验时钟与随机：单调之钟不退，分辨率为正，getentropy 二取而异。
// 汉语：时钟与随机数：单调时钟不倒退、分辨率为正、实时时钟非负、两次 getentropy 结果不同。
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
int main(void) {
  struct timespec 甲, 乙, 分;
  clock_getres(CLOCK_MONOTONIC, &分);
  clock_gettime(CLOCK_MONOTONIC, &甲);
  volatile unsigned x = 0;
  for (unsigned i = 0; i < 2000000; i++) x += i;
  clock_gettime(CLOCK_MONOTONIC, &乙);
  long long 差 = (乙.tv_sec - 甲.tv_sec) * 1000000000LL + (乙.tv_nsec - 甲.tv_nsec);
  printf("单调时钟 %s\n", 差 >= 0 ? "不倒退" : "倒退");
  printf("分辨率 %s\n", (分.tv_sec * 1000000000LL + 分.tv_nsec) > 0 ? "正" : "零");
  printf("实时时钟 %s\n", time(NULL) >= 0 ? "非负" : "负");
  unsigned char 一[16], 二[16];
  int 码一 = getentropy(一, sizeof 一), 码二 = getentropy(二, sizeof 二);
  printf("随机 %d %d %s\n", 码一, 码二, memcmp(一, 二, sizeof 一) != 0 ? "两次不同" : "两次相同");
  return 0;
}
