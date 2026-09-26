#ifndef 豫言安全外壳信号名_H
#define 豫言安全外壳信号名_H

#include <signal.h>
#include <stddef.h>
#include <string.h>

typedef struct {
    const char *名称;
    size_t 长度;
    int 编号;
} 安全外壳信号项;

#define 标准信号项(名) {#名, sizeof(#名) - 1, SIG##名}

/* 文言：RFC 4254 诸常名之宿主号或异，惟依系统宏定之。汉语：按宿主平台的 SIG 宏映射 RFC 4254 标准信号名。 */
static const 安全外壳信号项 安全外壳信号表[] = {
    标准信号项(ABRT), 标准信号项(ALRM), 标准信号项(FPE),
    标准信号项(HUP), 标准信号项(ILL), 标准信号项(INT),
    标准信号项(KILL), 标准信号项(PIPE), 标准信号项(QUIT),
    标准信号项(SEGV), 标准信号项(TERM), 标准信号项(USR1),
    标准信号项(USR2)
};

#undef 标准信号项

static int 安全外壳信号编号(const unsigned char *名称, size_t 长度) {
    for (size_t 位 = 0; 位 < sizeof(安全外壳信号表) / sizeof(安全外壳信号表[0]); 位++) {
        if (长度 == 安全外壳信号表[位].长度 &&
            memcmp(名称, 安全外壳信号表[位].名称, 长度) == 0)
            return 安全外壳信号表[位].编号;
    }
    return 0;
}

static const char *安全外壳信号名称(int 编号) {
    for (size_t 位 = 0; 位 < sizeof(安全外壳信号表) / sizeof(安全外壳信号表[0]); 位++) {
        if (编号 == 安全外壳信号表[位].编号) return 安全外壳信号表[位].名称;
    }
    return NULL;
}

#endif
