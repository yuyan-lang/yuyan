// 文言：猫：依次读诸文件而出之于标准出。
// 汉语：猫（cat）：依次读参数里的文件写到标准输出；打不开的文件报错到标准错误，最后以 1 退出。
#include <errno.h>
#include <stdio.h>
#include <string.h>
int main(int argc, char **argv) {
  int 状态 = 0;
  for (int i = 1; i < argc; i++) {
    FILE *f = fopen(argv[i], "rb");
    if (!f) { fprintf(stderr, "猫：无法打开 %s：%s\n", argv[i], strerror(errno)); 状态 = 1; continue; }
    char 缓[512]; size_t n;
    while ((n = fread(缓, 1, sizeof 缓, f)) > 0) fwrite(缓, 1, n, stdout);
    fclose(f);
  }
  return 状态;
}
