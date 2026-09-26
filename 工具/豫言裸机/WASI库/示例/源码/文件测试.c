// 文言：验文件之诸事：建目录、写读追加、二进制、寻、目录之列、改名、删、错误之码、未关闭之文件于退出时写回。
// 汉语：文件与目录综合测试（路径都在 /测试 下）：mkdir、fopen 的 w/r/a/r+/wb/rb、fseek/ftell、fwrite/fread 一万字节、opendir/readdir、rename、rmdir、unlink、stat、常见错误信息，以及不 fclose 的文件在退出时由执行器写回。
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

static int 比较串(const void *a, const void *b) { return strcmp(*(char *const *)a, *(char *const *)b); }
static void 列目录(const char *路径) {
  DIR *d = opendir(路径);
  if (!d) { printf("opendir %s 失败：%s\n", 路径, strerror(errno)); return; }
  char *名们[64]; int n = 0; struct dirent *e;
  while ((e = readdir(d)) && n < 64) if (strcmp(e->d_name, ".") && strcmp(e->d_name, "..")) 名们[n++] = strdup(e->d_name);
  closedir(d);
  qsort(名们, n, sizeof(char *), 比较串);
  printf("%s:", 路径);
  for (int i = 0; i < n; i++) { printf(" %s", 名们[i]); free(名们[i]); }
  printf("\n");
}
static void 显示大小(const char *路径) {
  struct stat s;
  if (stat(路径, &s) != 0) printf("stat %s 失败：%s\n", 路径, strerror(errno));
  else printf("stat %s：%s 大小 %ld\n", 路径, S_ISDIR(s.st_mode) ? "目录" : "文件", (long)s.st_size);
}
static void 报(const char *名, int 结果) {
  int 码 = errno;
  if (结果 == 0) printf("%s 成功\n", 名); else printf("%s 失败：%s\n", 名, strerror(码));
}
int main(void) {
  int r;
  r = mkdir("/测试", 0777); errno = 0; 报("mkdir /测试", r);
  r = mkdir("/测试", 0777); 报("mkdir 重复", r);
  FILE *f = fopen("/测试/甲.txt", "w");
  fprintf(f, "第一行\n第二行 %d\n", 42);
  fclose(f);
  f = fopen("/测试/甲.txt", "r");
  char 行[128];
  while (fgets(行, sizeof 行, f)) printf("读到：%s", 行);
  fclose(f);
  f = fopen("/测试/甲.txt", "a"); fputs("追加行\n", f); fclose(f);
  显示大小("/测试/甲.txt");
  f = fopen("/测试/甲.txt", "r+");
  fseek(f, 0, SEEK_END); printf("末尾 %ld\n", ftell(f));
  rewind(f); fputs("改", f);
  fseek(f, 0, SEEK_SET); fgets(行, sizeof 行, f); printf("首行：%s", 行);
  fclose(f);
  static unsigned char 缓[10000], 读[10000];
  for (int i = 0; i < 10000; i++) 缓[i] = (unsigned char)(i * 31 + i / 7);
  f = fopen("/测试/乙.bin", "wb"); size_t w = fwrite(缓, 1, sizeof 缓, f); fclose(f); printf("写 %zu\n", w);
  f = fopen("/测试/乙.bin", "rb"); size_t n = fread(读, 1, sizeof 读, f); fclose(f);
  printf("读 %zu %s\n", n, memcmp(缓, 读, sizeof 缓) == 0 ? "一致" : "不同");
  显示大小("/测试/乙.bin");
  r = mkdir("/测试/子", 0777); 报("mkdir 子", r);
  f = fopen("/测试/子/丙.txt", "w"); fputs("丙", f); fclose(f);
  列目录("/测试"); 列目录("/测试/子");
  r = rmdir("/测试/子"); 报("rmdir 非空", r);
  r = rename("/测试/子/丙.txt", "/测试/丁.txt"); 报("rename", r);
  r = rmdir("/测试/子"); 报("rmdir 空", r);
  列目录("/测试");
  r = unlink("/测试/丁.txt"); 报("unlink", r);
  r = unlink("/测试/丁.txt"); 报("unlink 无", r);
  f = fopen("/测试/无此.txt", "r");
  if (f) printf("fopen 无 成功\n"); else printf("fopen 无 失败：%s\n", strerror(errno));
  显示大小("/测试"); 显示大小("/测试/无此");
  f = fopen("/测试/未关.txt", "w"); fputs("未关闭也应写回\n", f); fflush(f);
  printf("结束\n");
  return 0;
}
