/* 文言：此为外来模块之试，四核各验实算。汉语：本 C 文件仅生成独立互操作输入，内核及机器后端仍由豫言实现。 */
__attribute__((import_module("平台"), import_name("核验"))) void 核验(int, int);
#ifndef 错误期望
#define 错误期望 0
#endif
static volatile int 计数 = 9;
__attribute__((noinline)) static int 递增(int 步) { 计数 += 步; return 计数; }
__attribute__((noinline)) static unsigned 公约数(unsigned 甲, unsigned 乙) {
  while (乙) { unsigned 余 = 甲 % 乙; 甲 = 乙; 乙 = 余; }
  return 甲;
}
__attribute__((export_name("启动"))) void 启动(void) {
  volatile int 数列[32];
  int 和 = 0;
  for (int 位 = 0; 位 < 32; ++位) 数列[位] = 位 * 位 + 3;
  for (int 位 = 0; 位 < 32; ++位) 和 += 数列[位];
  核验(和, 10512 + 错误期望);
  核验(公约数(1071, 462), 21);
  volatile unsigned 值 = 0x12345678u;
  核验((int)((值 << 5) | (值 >> 27)), (int)0x468acf02u);
  递增(3);
  核验(递增(-2), 10);
}
/* 文言：宿客同观导出之存。汉语：通过独立编译的导出函数核对宿主与客体访问同一线性内存。 */
__attribute__((export_name("取字节"))) int 取字节(unsigned 地址) {
  return *((volatile unsigned char *)(unsigned long)地址);
}
__attribute__((export_name("置字节"))) void 置字节(unsigned 地址, int 值) {
  *((volatile unsigned char *)(unsigned long)地址) = (unsigned char)值;
}
