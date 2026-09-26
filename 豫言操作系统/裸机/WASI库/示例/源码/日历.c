// 文言：日历：核今之日时与诸钟。
// 汉语：日历：打印 gmtime(time(NULL)) 的年月日、小时与星期（0 是周日），再核对实时钟睡 30 毫秒后前进、单调钟同样前进、time() 与 clock_gettime 的秒数相符。壳测试里 QEMU 的 -rtc base 固定了起点，所以前三项的输出是确定的。
#include <stdio.h>
#include <time.h>
static long long 差(struct timespec a, struct timespec b) {
  return (b.tv_sec - a.tv_sec) * 1000000000LL + (b.tv_nsec - a.tv_nsec);
}
int main(void) {
  time_t t = time(NULL);
  struct tm *g = gmtime(&t);
  printf("日期 %04d-%02d-%02d %02d 星期 %d\n", g->tm_year + 1900, g->tm_mon + 1, g->tm_mday, g->tm_hour, g->tm_wday);
  struct timespec 实一, 实二, 单一, 单二, 请 = {0, 30000000};
  clock_gettime(CLOCK_REALTIME, &实一);
  clock_gettime(CLOCK_MONOTONIC, &单一);
  nanosleep(&请, NULL);
  clock_gettime(CLOCK_REALTIME, &实二);
  clock_gettime(CLOCK_MONOTONIC, &单二);
  long long 实差 = 差(实一, 实二), 单差 = 差(单一, 单二);
  printf("实时 %s\n", (实差 >= 20000000LL && 实差 < 5000000000LL) ? "前进" : "异常");
  printf("单调 %s\n", (单差 >= 20000000LL && 单差 < 5000000000LL) ? "前进" : "异常");
  printf("秒数 %s\n", (实一.tv_sec >= t && 实一.tv_sec <= t + 1) ? "相符" : "不符");
  return 0;
}
