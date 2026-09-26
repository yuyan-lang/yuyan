// 文言：大写：读标准入，易小写为大写而出。
// 汉语：大写（tr）：读标准输入，把 ASCII 小写字母转成大写写到标准输出，其余字节原样；用来验证管道。
#include <ctype.h>
#include <stdio.h>
int main(void) {
  int c;
  while ((c = getchar()) != EOF) putchar(toupper(c));
  return 0;
}
