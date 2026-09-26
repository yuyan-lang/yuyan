// 文言：转储一目录之树：每文件之长与校验，目录缩进。
// 汉语：目录转储：递归列出参数（缺省 /）下的目录树，文件显示长度与 FNV-1a 校验，名字排序，用来核对文件确实写进了持久文件系统。
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
static int 比较串(const void *a, const void *b) { return strcmp(*(char *const *)a, *(char *const *)b); }
static void 转储(const char *路径, int 深) {
  DIR *d = opendir(路径);
  if (!d) { printf("%*s无法打开 %s\n", 深 * 2, "", 路径); return; }
  char *名们[256]; int n = 0; struct dirent *e;
  while ((e = readdir(d)) && n < 256) if (strcmp(e->d_name, ".") && strcmp(e->d_name, "..")) 名们[n++] = strdup(e->d_name);
  closedir(d);
  qsort(名们, n, sizeof(char *), 比较串);
  for (int i = 0; i < n; i++) {
    char 全[512]; snprintf(全, sizeof 全, "%s%s%s", 路径, strcmp(路径, "/") ? "/" : "", 名们[i]);
    struct stat s;
    if (stat(全, &s) != 0) { printf("%*s%s ?\n", 深 * 2, "", 名们[i]); free(名们[i]); continue; }
    if (S_ISDIR(s.st_mode)) { printf("%*s%s/\n", 深 * 2, "", 名们[i]); 转储(全, 深 + 1); }
    else {
      FILE *f = fopen(全, "rb"); unsigned h = 2166136261u; long 长 = 0; int c;
      if (f) { while ((c = fgetc(f)) != EOF) { h = (h ^ (unsigned)c) * 16777619u; 长++; } fclose(f); }
      printf("%*s%s 长%ld 校验%08x\n", 深 * 2, "", 名们[i], 长, h);
    }
    free(名们[i]);
  }
}
int main(int argc, char **argv) { 转储(argc > 1 ? argv[1] : "/", 0); return 0; }
