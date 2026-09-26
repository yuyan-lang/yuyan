// 文言：读标准输入，逐行易为大写而编号，至文末（行首 Ctrl-D）而止。
// 汉语：回声：按行读标准输入，转成大写并编号输出，读到文件结束（行首的 Ctrl-D）后打印统计。
#include <stdio.h>
#include <string.h>
#include <ctype.h>
int main(void) {
  char 行[256];
  int 序 = 0, 总 = 0;
  printf("回声就绪\n");
  while (fgets(行, sizeof 行, stdin)) {
    size_t 长 = strlen(行);
    序++;
    总 += (int)长;
    for (size_t i = 0; i < 长; i++) 行[i] = (char)toupper((unsigned char)行[i]);
    printf("%d: %s", 序, 行);
    if (长 == 0 || 行[长 - 1] != '\n') printf("\n");
  }
  printf("共 %d 行 %d 字节\n", 序, 总);
  return 0;
}
