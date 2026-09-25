#include "公共包含.h"

#include <ctype.h>
#ifndef __APPLE__
#include <iconv.h>
#endif

#ifdef __APPLE__
#include <dlfcn.h>

typedef const void *苹果字符串引用;
typedef void *苹果可变字符串引用;
typedef long 苹果序数;
typedef unsigned int 苹果字符串编码;
typedef unsigned char 苹果布尔;

typedef 苹果可变字符串引用 (*创建可变字符串函数)(const void *, 苹果序数);
typedef void (*追加字符串函数)(苹果可变字符串引用, const char *, 苹果字符串编码);
typedef 苹果布尔 (*转换字符串函数)(苹果可变字符串引用, void *, 苹果字符串引用, 苹果布尔);
typedef 苹果序数 (*获取字符串长度函数)(苹果字符串引用);
typedef 苹果序数 (*获取最大字节数函数)(苹果序数, 苹果字符串编码);
typedef 苹果布尔 (*取出字符串函数)(苹果字符串引用, char *, 苹果序数, 苹果字符串编码);
typedef void (*释放苹果对象函数)(const void *);

static char *苹果系统转拼音(const char *原文) {
    static void *框架 = NULL;
    static bool 已尝试加载 = false;
    if (!已尝试加载) {
        已尝试加载 = true;
        框架 = dlopen(
            "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation",
            RTLD_LAZY | RTLD_LOCAL
        );
    }
    if (框架 == NULL) {
        return NULL;
    }

    创建可变字符串函数 创建 = (创建可变字符串函数)dlsym(框架, "CFStringCreateMutable");
    追加字符串函数 追加 = (追加字符串函数)dlsym(框架, "CFStringAppendCString");
    转换字符串函数 转换 = (转换字符串函数)dlsym(框架, "CFStringTransform");
    获取字符串长度函数 取长度 = (获取字符串长度函数)dlsym(框架, "CFStringGetLength");
    获取最大字节数函数 取最大字节数 = (获取最大字节数函数)dlsym(框架, "CFStringGetMaximumSizeForEncoding");
    取出字符串函数 取出 = (取出字符串函数)dlsym(框架, "CFStringGetCString");
    释放苹果对象函数 释放 = (释放苹果对象函数)dlsym(框架, "CFRelease");
    const 苹果字符串引用 *转拉丁常量 = (const 苹果字符串引用 *)dlsym(框架, "kCFStringTransformToLatin");
    const 苹果字符串引用 *去组合符常量 = (const 苹果字符串引用 *)dlsym(框架, "kCFStringTransformStripCombiningMarks");

    if (创建 == NULL || 追加 == NULL || 转换 == NULL || 取长度 == NULL
        || 取最大字节数 == NULL || 取出 == NULL || 释放 == NULL
        || 转拉丁常量 == NULL || 去组合符常量 == NULL) {
        return NULL;
    }

    const 苹果字符串编码 八位统一码 = 0x08000100U;
    苹果可变字符串引用 文本 = 创建(NULL, 0);
    if (文本 == NULL) {
        return NULL;
    }
    追加(文本, 原文, 八位统一码);
    if (!转换(文本, NULL, *转拉丁常量, false)
        || !转换(文本, NULL, *去组合符常量, false)) {
        释放(文本);
        return NULL;
    }

    苹果序数 最大字节数 = 取最大字节数(取长度(文本), 八位统一码);
    if (最大字节数 < 0 || (unsigned long)最大字节数 > SIZE_MAX - 1) {
        释放(文本);
        return NULL;
    }
    char *结果 = malloc((size_t)最大字节数 + 1);
    if (结果 == NULL) {
        释放(文本);
        return NULL;
    }
    if (!取出(文本, 结果, 最大字节数 + 1, 八位统一码)) {
        free(结果);
        结果 = NULL;
    }
    释放(文本);
    return 结果;
}
#endif

static char 国标码首字母(unsigned int 编码) {
    static const unsigned int 起始码[] = {
        0xB0A1, 0xB0C5, 0xB2C1, 0xB4EE, 0xB6B2, 0xB7A2, 0xB8C1,
        0xB9FE, 0xBBF7, 0xBFA6, 0xC0AC, 0xC2E8, 0xC4C3, 0xC5B6,
        0xC5BE, 0xC6DA, 0xC8BB, 0xC8F6, 0xCBFA, 0xCDDA, 0xCEF4,
        0xD1B9, 0xD4D1,
    };
    static const char 字母[] = "ABCDEFGHJKLMNOPQRSTWXYZ";
    if (编码 < 起始码[0] || 编码 > 0xD7F9) {
        return '\0';
    }
    size_t 序数 = sizeof(起始码) / sizeof(起始码[0]);
    while (序数 > 0) {
        序数--;
        if (编码 >= 起始码[序数]) {
            return (char)tolower((unsigned char)字母[序数]);
        }
    }
    return '\0';
}

static size_t 八位统一码字符字节数(unsigned char 首字节) {
    if ((首字节 & 0x80U) == 0) return 1;
    if ((首字节 & 0xE0U) == 0xC0U) return 2;
    if ((首字节 & 0xF0U) == 0xE0U) return 3;
    if ((首字节 & 0xF8U) == 0xF0U) return 4;
    return 1;
}

static char *通用首字母拼写(const char *原文) {
    size_t 原文长度 = strlen(原文);
    char *结果 = malloc(原文长度 + 1);
    if (结果 == NULL) return NULL;
    size_t 输入序数 = 0;
    size_t 输出序数 = 0;
#ifndef __APPLE__
    iconv_t 转换器 = iconv_open("GB18030", "UTF-8");
#endif

    while (输入序数 < 原文长度) {
        unsigned char 当前 = (unsigned char)原文[输入序数];
        if (当前 < 0x80U) {
            if (isalnum(当前)) {
                结果[输出序数++] = (char)tolower(当前);
            }
            输入序数++;
            continue;
        }

        size_t 字符字节数 = 八位统一码字符字节数(当前);
        if (字符字节数 > 原文长度 - 输入序数) {
            输入序数 += 字符字节数;
            continue;
        }

#ifndef __APPLE__
        if (转换器 != (iconv_t)-1) {
        char *输入指针 = (char *)原文 + 输入序数;
        size_t 剩余输入 = 字符字节数;
        char 转换结果[8] = {0};
        char *输出指针 = 转换结果;
        size_t 剩余输出 = sizeof(转换结果);
        iconv(转换器, NULL, NULL, NULL, NULL);
        if (iconv(转换器, &输入指针, &剩余输入, &输出指针, &剩余输出) != (size_t)-1
            && (size_t)(输出指针 - 转换结果) == 2) {
            unsigned int 国标码 = ((unsigned char)转换结果[0] << 8)
                | (unsigned char)转换结果[1];
            char 首字母 = 国标码首字母(国标码);
            if (首字母 != '\0') {
                结果[输出序数++] = 首字母;
            }
        }
        }
#endif
        输入序数 += 字符字节数;
    }
#ifndef __APPLE__
    if (转换器 != (iconv_t)-1) iconv_close(转换器);
#endif
    结果[输出序数] = '\0';
    return 结果;
}

static char *规范拉丁拼写(const char *文本, bool 仅取首字母) {
    size_t 长度 = strlen(文本);
    char *结果 = malloc(长度 + 1);
    if (结果 == NULL) return NULL;
    size_t 输出序数 = 0;
    bool 新音节 = true;
    for (size_t 序数 = 0; 序数 < 长度; 序数++) {
        unsigned char 字节 = (unsigned char)文本[序数];
        if (isalnum(字节)) {
            if (!仅取首字母 || 新音节) {
                结果[输出序数++] = (char)tolower(字节);
            }
            新音节 = false;
        } else {
            新音节 = true;
        }
    }
    结果[输出序数] = '\0';
    return 结果;
}

static char *生成拼写(const char *原文, bool 仅取首字母) {
#ifdef __APPLE__
    char *拉丁文本 = 苹果系统转拼音(原文);
    if (拉丁文本 != NULL) {
        char *结果 = 规范拉丁拼写(拉丁文本, 仅取首字母);
        free(拉丁文本);
        if (结果 != NULL) return 结果;
    }
#endif
    return 通用首字母拼写(原文);
}

static 豫言值 拼写转豫言值(const char *原文, bool 仅取首字母) {
    char *拼写 = 生成拼写(原文, 仅取首字母);
    if (拼写 == NULL) {
        报错并中止("无法为拉丁拼写分配内存");
    }
    豫言值 结果 = 复制字节为豫言值(strlen(拼写), 拼写);
    free(拼写);
    return 结果;
}

豫言值 豫言_字符串转无调拼音(豫言值 文本) {
    return 拼写转豫言值(豫言值转字符串(文本), false);
}

豫言值 豫言_字符串转拼音首字母(豫言值 文本) {
    return 拼写转豫言值(豫言值转字符串(文本), true);
}
