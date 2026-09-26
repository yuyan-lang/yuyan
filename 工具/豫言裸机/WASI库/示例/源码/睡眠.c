// 文言：验睡眠：usleep、nanosleep、sleep 皆经 poll_oneoff 之时钟订阅而候。
// 汉语：睡眠：usleep、nanosleep 与 sleep 落到 poll_oneoff 的时钟订阅（执行器忙等到截止的滴答），用单调时钟核对睡够了。
#include <stdio.h>
#include <time.h>
#include <unistd.h>
static long long 毫秒(struct timespec t) { return t.tv_sec * 1000LL + t.tv_nsec / 1000000; }
int main(void) {
  struct timespec 甲, 乙;
  clock_gettime(CLOCK_MONOTONIC, &甲);
  usleep(300000);
  clock_gettime(CLOCK_MONOTONIC, &乙);
  long long 差 = 毫秒(乙) - 毫秒(甲);
  printf("usleep 300 毫秒：%s\n", 差 >= 290 ? "足够" : "不足");
  struct timespec 请求 = {0, 120 * 1000000L};
  clock_gettime(CLOCK_MONOTONIC, &甲);
  nanosleep(&请求, 0);
  clock_gettime(CLOCK_MONOTONIC, &乙);
  差 = 毫秒(乙) - 毫秒(甲);
  printf("nanosleep 120 毫秒：%s\n", 差 >= 110 ? "足够" : "不足");
  sleep(1);
  clock_gettime(CLOCK_MONOTONIC, &乙);
  差 = 毫秒(乙) - 毫秒(甲);
  printf("sleep 1 秒：%s\n", 差 >= 1100 ? "足够" : "不足");
  return 0;
}
