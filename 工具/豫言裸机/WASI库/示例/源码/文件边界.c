// 文言：验文件之边界：必新建、追加、预读预写、截断、稀疏、路径之规范、错误之码、覆盖之改名、相对路径。
// 汉语：文件边界情形：O_EXCL、O_APPEND、pread/pwrite、ftruncate、超出末尾再写（空洞为零）、读写目录、路径里的 ..、.、多余斜线、父路径是文件或不存在、rename 覆盖、chdir 后的相对路径。
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
static const char *结果(int 值) { static char 缓[64]; if (值 >= 0) return "成功"; snprintf(缓, sizeof 缓, "失败：%s", strerror(errno)); return 缓; }
int main(void) {
  char 缓[64]; int n, fd; struct stat s;
  mkdir("/边界", 0777);
  fd = open("/边界/独.txt", O_WRONLY | O_CREAT | O_EXCL, 0666); printf("excl 建 %s\n", 结果(fd));
  write(fd, "abc", 3); close(fd);
  fd = open("/边界/独.txt", O_WRONLY | O_CREAT | O_EXCL, 0666); printf("excl 再建 %s\n", 结果(fd));
  fd = open("/边界/独.txt", O_WRONLY | O_APPEND); write(fd, "def", 3); lseek(fd, 0, SEEK_SET); write(fd, "ghi", 3); close(fd);
  fd = open("/边界/独.txt", O_RDONLY); n = read(fd, 缓, 63); 缓[n] = 0; printf("内容 %s (%d)\n", 缓, n);
  n = pread(fd, 缓, 3, 2); 缓[n] = 0; printf("pread %s\n", 缓);
  n = read(fd, 缓, 1); printf("到尾再读 %d\n", n);
  n = write(fd, "x", 1); printf("只读写 %s\n", 结果(n));
  close(fd);
  fd = open("/边界/独.txt", O_RDWR); pwrite(fd, "ZZ", 2, 1); ftruncate(fd, 5);
  n = read(fd, 缓, 63); 缓[n] = 0; printf("截后 %s (%d)\n", 缓, n);
  ftruncate(fd, 8); lseek(fd, 0, SEEK_SET); n = read(fd, 缓, 63); printf("加长后 %d 末字节 %d\n", n, 缓[7]);
  lseek(fd, 20, SEEK_SET); write(fd, "E", 1); fstat(fd, &s); printf("稀疏后大小 %ld\n", (long)s.st_size);
  lseek(fd, 10, SEEK_SET); n = read(fd, 缓, 4); printf("空洞 %d %d %d %d %d\n", n, 缓[0], 缓[1], 缓[2], 缓[3]);
  printf("寻到负数 %s\n", 结果((int)lseek(fd, -5, SEEK_SET)));
  close(fd);
  fd = open("/边界/独.txt", O_WRONLY | O_TRUNC); close(fd); stat("/边界/独.txt", &s); printf("截断后大小 %ld\n", (long)s.st_size);
  fd = open("/边界", O_RDONLY); n = fd >= 0 ? (int)read(fd, 缓, 4) : -2; printf("读目录 %s\n", n >= 0 ? "成功" : (n == -2 ? "开败" : strerror(errno)));
  if (fd >= 0) close(fd);
  fd = open("/边界", O_WRONLY); printf("写开目录 %s\n", 结果(fd));
  fd = open("/边界/../边界/./独.txt", O_RDONLY); printf("规范路径 %s\n", 结果(fd)); if (fd >= 0) close(fd);
  fd = open("//边界///独.txt", O_RDONLY); printf("多斜线 %s\n", 结果(fd)); if (fd >= 0) close(fd);
  fd = open("/边界/独.txt/子", O_WRONLY | O_CREAT, 0666); printf("文件当目录 %s\n", 结果(fd));
  fd = open("/无此/子/文件", O_WRONLY | O_CREAT, 0666); printf("无父目录 %s\n", 结果(fd));
  n = mkdir("/无此/子", 0777); printf("mkdir 无父 %s\n", 结果(n));
  n = mkdir("/边界/独.txt/子", 0777); printf("mkdir 父是文件 %s\n", 结果(n));
  n = rmdir("/边界/独.txt"); printf("rmdir 文件 %s\n", 结果(n));
  n = unlink("/边界"); printf("unlink 目录 %s\n", 结果(n));
  fd = open("/边界/甲.txt", O_WRONLY | O_CREAT, 0666); write(fd, "甲内容", 9); close(fd);
  n = rename("/边界/甲.txt", "/边界/独.txt"); printf("rename 覆盖 %s\n", 结果(n));
  fd = open("/边界/独.txt", O_RDONLY); n = read(fd, 缓, 63); 缓[n] = 0; printf("覆盖后 %s\n", 缓); close(fd);
  n = access("/边界/甲.txt", F_OK); printf("旧名 %s\n", 结果(n));
  n = rename("/边界/无此", "/边界/新"); printf("rename 无 %s\n", 结果(n));
  n = rename("/边界/独.txt", "/边界"); printf("rename 文件到目录 %s\n", 结果(n));
  for (int i = 0; i < 5; i++) { char 名[32]; snprintf(名, sizeof 名, "/边界/多%d.txt", i); fd = open(名, O_WRONLY | O_CREAT, 0666); write(fd, 名, strlen(名)); close(fd); }
  n = chdir("/边界"); printf("chdir %s\n", 结果(n));
  fd = open("多2.txt", O_RDONLY); n = read(fd, 缓, 63); 缓[n] = 0; printf("相对路径 %s\n", 缓); close(fd);
  return 0;
}
