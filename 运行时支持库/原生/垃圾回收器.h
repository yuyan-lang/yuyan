

#ifndef 豫言垃圾回收器_H
#define 豫言垃圾回收器_H

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "公共包含.h"

extern 豫言值 * 垃圾回收根[];
extern int 垃圾回收根数量;

extern 豫言值* 当前堆;
extern 豫言值* 新堆;
extern 豫言值 *新堆结束;
extern 豫言值 *豫言_当前分配指针;
extern 豫言值 *豫言_当前堆垃圾回收阈值;
extern 豫言值 *当前堆结束;
extern bool 应扩展堆;

void 初始化垃圾回收器();

void 豫言_登记垃圾回收根(豫言值* 指针);

void* 垃圾回收堆分配块(uint64_t 大小);

void* 垃圾回收堆重分配字节(void* 指针, uint64_t 原大小, uint64_t 新大小);
void 执行垃圾回收();

void 验证豫言值(豫言值 参数, bool 递归, int 深度);
void 验证新堆豫言值(豫言值 参数, bool 递归, int 深度);

bool 是旧堆指针(豫言值 参数);
bool 是新堆指针(豫言值 参数);

#endif /* 豫言垃圾回收器_H */
