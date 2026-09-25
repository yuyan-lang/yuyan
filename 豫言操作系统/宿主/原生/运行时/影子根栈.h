#ifndef 豫言影子根栈_H
#define 豫言影子根栈_H

#include "值编解码.h"
#include <stdint.h>

/* 文言：帧居真栈，唯根入链。汉语：帧及根槽由生成函数持有，不在堆上分配，不承载调用续延。 */
typedef struct 豫言影子根帧 {
    struct 豫言影子根帧 *前帧;
    uint64_t 根数;
    volatile 豫言值 *根槽;
} 豫言影子根帧;

/* 文言：今堆唯单执行者所治。汉语：沿用现有单执行线程 GC 协议；并行编译使用独立进程。 */
extern 豫言影子根帧 *豫言_影子根顶;
void 豫言_进入影子根帧(豫言影子根帧 *帧, volatile 豫言值 *槽, uint64_t 数);
void 豫言_离开影子根帧(豫言影子根帧 *帧);
void 豫言_恢复影子根栈(豫言影子根帧 *目标);
void 豫言_影子根安全点(void);
void 豫言_影子根慢回收(void) __attribute__((noinline, cold));
extern 豫言值 *豫言_当前分配指针;
extern 豫言值 *豫言_当前堆垃圾回收阈值;

/* 文言：常路唯验界，迁堆别置冷路。汉语：强制内联阈值比较，禁止完整 GC 被内联后拖累每次普通调用的寄存器保存。 */
static inline __attribute__((always_inline)) void 豫言_影子根检查(void) {
    if (__builtin_expect(豫言_当前分配指针 > 豫言_当前堆垃圾回收阈值, 0))
        豫言_影子根慢回收();
}

#endif
