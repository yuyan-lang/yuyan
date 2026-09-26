// 文言：数字：读标准入，数其行、词、字节。
// 汉语：字数（wc）：读标准输入到结尾，打印行数、词数与字节数；标准输入可以来自 < 重定向或管道。
#include <ctype.h>
#include <stdio.h>
int main(void) {
  long 行 = 0, 词 = 0, 字节 = 0; int c, 在词中 = 0;
  while ((c = getchar()) != EOF) {
    字节++;
    if (c == '\n') 行++;
    if (isspace((unsigned char)c)) 在词中 = 0;
    else if (!在词中) { 在词中 = 1; 词++; }
  }
  printf("行 %ld 词 %ld 字节 %ld\n", 行, 词, 字节);
  return 0;
}
