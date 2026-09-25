#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <stdint.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#include <pthread.h>
#include <assert.h>
#include <inttypes.h>
#include "值编解码.h"
#include "调试打印.h"


extern int 全局参数数量;
extern char** 全局参数值;

豫言值 单元转豫言值();

uint64_t C运行时内部错误();
uint64_t 报错并中止(char *错误消息);
char *豫言值转字符串(豫言值 参数);

uint64_t 获取同构列长度(const 豫言值 列);
豫言值* 获取同构列元素(const 豫言值 列);
豫言值 元组转豫言值(uint64_t 长度, const 豫言值 元素[]);
豫言值 爻转豫言值(bool 爻值);
豫言值 数组转同构列(uint64_t 长度, const 豫言值 元素[]);

int64_t 豫言值转整数(豫言值 参数);
double 豫言值转小数(豫言值 参数);
豫言值 *豫言值转元组(豫言值 参数);
uint64_t 获取豫言元组长度(豫言值 参数);
豫言值 整数转豫言值(int64_t 整数);
豫言值 小数转豫言值(double 小数);


void 初始化全局异常处理器();

豫言值 分配豫言元组(uint64_t 大小);
豫言值 分配豫言_字节串缓冲区(uint64_t 长度);


// 运行时配置。
int64_t 启动豫言运行时();

void 启动性能分析器();
extern bool 启用性能分析;




extern 豫言值* 豫言栈开始;
extern 豫言值* 豫言栈结束;
extern 豫言值* 豫言_栈指针;
extern 豫言函数指针类型 当前豫言函数;
extern pthread_mutex_t 豫言栈指针锁;
extern void 豫言_程序入口(豫言值 栈顶, 豫言值 当前分配参数);
void 调用豫言程序入口(豫言值 栈顶, 豫言值 当前分配参数);

void 执行垃圾回收();
void 验证垃圾回收结果();
void 初始化垃圾回收器();
extern bool 正在执行垃圾回收;
