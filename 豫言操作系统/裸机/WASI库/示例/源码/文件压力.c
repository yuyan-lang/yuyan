// 文言：验回收：反复整读一大文件，所生之垃圾须由回收收之。
// 汉语：文件压力：写一个 100000 字节的文件，再打开读完关闭 400 次（共约 40 MB 的堆分配，堆只有 32 MiB 量级），检验文件缓冲的回收；在 Node 夹具里几百毫秒，子任务里因文件走邮箱而极慢，所以不在壳测试里运行。
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(void) {
  enum { 大小 = 100000, 次数 = 400 };
  unsigned char *缓 = malloc(大小);
  for (int i = 0; i < 大小; i++) 缓[i] = (unsigned char)(i * 7 + (i >> 8));
  FILE *f = fopen("/压力.bin", "wb");
  if (!f) { printf("无法建文件\n"); return 1; }
  fwrite(缓, 1, 大小, f); fclose(f);
  unsigned long 总和 = 0;
  for (int k = 0; k < 次数; k++) {
    f = fopen("/压力.bin", "rb");
    if (!f) { printf("第 %d 次打不开\n", k); return 1; }
    size_t n = fread(缓, 1, 大小, f); fclose(f);
    if (n != 大小) { printf("第 %d 次读到 %zu\n", k, n); return 1; }
    总和 += 缓[k] + 缓[大小 - 1 - k];
  }
  printf("完成 %d 次，总和 %lu\n", 次数, 总和);
  return 0;
}
