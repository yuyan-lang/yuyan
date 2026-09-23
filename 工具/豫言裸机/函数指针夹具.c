/* 文言：此独立 C 夹具验函数指针，非客体之依赖。汉语：仅用宿主 C 编译器生成标准互操作输入，内核与执行器仍由豫言实现。 */
__attribute__((import_module("平台"), import_name("核验"))) void 核验(int, int);
__attribute__((noinline)) static int 加一(int 值) { return 值 + 1; }
__attribute__((noinline)) static int 倍增(int 值) { return 值 * 2; }
typedef int (*运算)(int);
static 运算 volatile 函数表[2] = {加一, 倍增};
__attribute__((export_name("指针验收"))) int 指针验收(void) {
  return 函数表[0](41) + 函数表[1](21);
}
__attribute__((export_name("启动"))) void 启动(void) {
  核验(函数表[0](41), 42);
  核验(函数表[1](21), 42);
  函数表[0] = 倍增;
  核验(函数表[0](7), 14);
}
