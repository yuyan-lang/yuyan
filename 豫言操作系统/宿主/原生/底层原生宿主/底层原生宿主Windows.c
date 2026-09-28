/* 豫言操作系统底层接口在 Windows 上的原生宿主：用 Wasmtime C API 加载
 * 豫言操作系统底层原生后端 编出的同一份 Wasm 模块（与 macOS/Linux 共用，
 * 见 底层原生宿主.c），唯一的宿主导入 底宿主.call(术,块址) 在这里用 Win32
 * API 实现。参见同目录 说明.汉语.md。
 *
 * 「：文言：此文件未曾编译，亦未曾行——此机无 Windows，亦无随仓所带之
 * Windows 版 Wasmtime 库。逐术码依 Win32 之档核对写就，然终须于真
 * Windows 环境下编、行、验，方可信其无误。汉语：这份文件从未编译过，也
 * 从未运行过——本机没有 Windows，仓库也没带 Windows 版的 Wasmtime 库。
 * 每个术码都是对着 Win32 文档写的，但终究需要在真正的 Windows 环境里编译、
 * 运行、验证过，才能当作可信的实现。：」
 */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <bcrypt.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wasmtime.h>

#ifdef _MSC_VER
#pragma comment(lib, "bcrypt.lib")
#endif

/* ---- 状态码（须与 术码。豫 / 底层原生宿主.c 的编号一致） ---- */
enum {
    码_成功 = 0, 码_无权限 = 2, 码_地址已用 = 3, 码_暂不可用 = 6, 码_句柄无效 = 8,
    码_已存在 = 20, 码_参数无效 = 28, 码_输入输出 = 29, 码_是目录 = 31, 码_打开过多 = 33,
    码_名太长 = 37, 码_不存在 = 44, 码_未实现 = 52, 码_非目录 = 54, 码_目录非空 = 55,
    码_不支持 = 58, 码_溢出 = 61, 码_不可寻址 = 70, 码_跨设备 = 75, 码_无能力 = 76,
};

/* ---- 术码（须与 术码。豫 一致） ---- */
enum {
    术_读单调纳秒 = 1, 术_读墙钟纳秒 = 2, 术_取时钟精度 = 3, 术_取时区偏移 = 4,
    术_填随机字节 = 5, 术_取参数块 = 6, 术_取环境块 = 7, 术_退出 = 8,
    术_查询中断 = 9, 术_取处理器数 = 10,
    术_取初始句柄 = 11, 术_取句柄类别 = 12, 术_读 = 13, 术_写 = 14,
    术_定位读 = 15, 术_定位写 = 16, 术_寻址 = 17, 术_关闭 = 18, 术_同步 = 19,
    术_设长度 = 20, 术_设非阻塞 = 21, 术_建管道 = 22, 术_等待 = 23,
    术_打开 = 24, 术_取状态 = 25, 术_取路径状态 = 26, 术_读目录 = 27,
    术_建目录 = 28, 术_删文件 = 29, 术_删目录 = 30, 术_改名 = 31, 术_读链接 = 32,
    术_取平台 = 33,
};

/* ---- 句柄表：Windows 的 HANDLE 是指针大小、非小整数，须自建一张
 * 「小整数句柄 → HANDLE（加目录时之绝对路径）」的表，供 打开/取状态 等
 * 「相对某目录柄」的调用查其绝对路径后自行拼接（Win32 无 openat 一类
 * 「相对目录 HANDLE 直接开子路径」的简单 API，NtCreateFile 的
 * RootDirectory 用法过于底层，故取此更简单、可静态核对之法）。 */
#define 句柄表容量 256
typedef struct {
    int 占用;
    int 是目录;
    HANDLE 柄;
    wchar_t *目录路径; /* 仅「是目录」时有效；绝对路径，不带结尾反斜杠 */
} 句柄项;

static 句柄项 句柄表[句柄表容量];
static volatile LONG 中断标志 = 0;

static BOOL WINAPI 控制台处理(DWORD 类型) {
    if (类型 == CTRL_C_EVENT || 类型 == CTRL_BREAK_EVENT) {
        InterlockedExchange(&中断标志, 1);
        return TRUE;
    }
    return FALSE;
}

static int 分配句柄槽(void) {
    for (int i = 3; i < 句柄表容量; i++) if (!句柄表[i].占用) return i;
    return -1;
}

static void 释放句柄槽(int i) {
    if (i < 0 || i >= 句柄表容量 || !句柄表[i].占用) return;
    if (句柄表[i].目录路径) { free(句柄表[i].目录路径); 句柄表[i].目录路径 = NULL; }
    句柄表[i].占用 = 0;
}

/* ---- 客体内存访问：同 底层原生宿主.c 的 客内存，逻辑不因平台而异 ---- */
static uint8_t *客内存(wasmtime_caller_t *客, uint32_t 偏移, uint32_t 长) {
    wasmtime_extern_t 外部;
    if (!wasmtime_caller_export_get(客, "memory", 6, &外部) || 外部.kind != WASMTIME_EXTERN_MEMORY) {
        return NULL;
    }
    wasmtime_context_t *上下文 = wasmtime_caller_context(客);
    size_t 总长 = wasmtime_memory_data_size(上下文, &外部.of.memory);
    if ((size_t)偏移 > 总长 || (size_t)长 > 总长 - 偏移) return NULL;
    return wasmtime_memory_data(上下文, &外部.of.memory) + 偏移;
}

static uint32_t 读32(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return v; }
static void 写32(uint8_t *p, uint32_t v) { memcpy(p, &v, 4); }
static void 写64(uint8_t *p, uint64_t v) { memcpy(p, &v, 8); }
static uint64_t 读64(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return v; }
static uint32_t 块字(const uint8_t *块, int 序) { return 读32(块 + 序 * 4); }

/* UTF-8（guest 侧字符串编码）与 UTF-16（Win32 API 所需）互转 */
static wchar_t *窄转宽(const uint8_t *字, uint32_t 长) {
    if (长 == 0) { wchar_t *空 = malloc(sizeof(wchar_t)); if (空) 空[0] = 0; return 空; }
    int 需 = MultiByteToWideChar(CP_UTF8, 0, (const char *)字, (int)长, NULL, 0);
    if (需 <= 0) return NULL;
    wchar_t *出 = malloc(((size_t)需 + 1) * sizeof(wchar_t));
    if (!出) return NULL;
    MultiByteToWideChar(CP_UTF8, 0, (const char *)字, (int)长, 出, 需);
    出[需] = 0;
    return 出;
}

/* 文件时间（1601 年起百纳秒）→ Unix 纪元起纳秒 */
static uint64_t 文件时间转纳秒(const FILETIME *ft) {
    uint64_t 百纳秒 = ((uint64_t)ft->dwHighDateTime << 32) | ft->dwLowDateTime;
    const uint64_t 纪元差 = 116444736000000000ULL; /* 1601→1970，百纳秒计 */
    uint64_t 秒差百纳秒 = 百纳秒 > 纪元差 ? 百纳秒 - 纪元差 : 0;
    return 秒差百纳秒 * 100ULL;
}

static int32_t 错码(DWORD e) {
    switch (e) {
        case ERROR_SUCCESS: return 码_成功;
        case ERROR_ACCESS_DENIED: return 码_无权限;
        case ERROR_ADDRESS_ALREADY_ASSOCIATED: return 码_地址已用;
        case ERROR_INVALID_HANDLE: return 码_句柄无效;
        case ERROR_FILE_EXISTS: case ERROR_ALREADY_EXISTS: return 码_已存在;
        case ERROR_INVALID_PARAMETER: return 码_参数无效;
        case ERROR_TOO_MANY_OPEN_FILES: return 码_打开过多;
        case ERROR_FILENAME_EXCED_RANGE: case ERROR_BUFFER_OVERFLOW: return 码_名太长;
        case ERROR_FILE_NOT_FOUND: case ERROR_PATH_NOT_FOUND: return 码_不存在;
        case ERROR_CALL_NOT_IMPLEMENTED: return 码_未实现;
        case ERROR_DIRECTORY: return 码_非目录;
        case ERROR_DIR_NOT_EMPTY: return 码_目录非空;
        case ERROR_NOT_SUPPORTED: return 码_不支持;
        case ERROR_ARITHMETIC_OVERFLOW: case ERROR_DISK_FULL: return 码_溢出;
        case ERROR_SEEK: return 码_不可寻址;
        case ERROR_NOT_SAME_DEVICE: return 码_跨设备;
        default: return 码_输入输出;
    }
}

/* 打开标志：1 读 2 写 4 追加 8 创建 16 独占 32 截断 64 须为目录 128 不循环末段符号链接 */
static void 转开标志(int32_t 标志, DWORD *期望访问, DWORD *创建方式, DWORD *属性) {
    int 读 = 标志 & 1, 写 = 标志 & 2, 创 = 标志 & 8, 独 = 标志 & 16, 截 = 标志 & 32;
    *期望访问 = (读 ? GENERIC_READ : 0) | (写 ? GENERIC_WRITE : 0);
    if (标志 & 4) *期望访问 |= FILE_APPEND_DATA;
    if (创 && 独) *创建方式 = CREATE_NEW;
    else if (创 && 截) *创建方式 = CREATE_ALWAYS;
    else if (创) *创建方式 = OPEN_ALWAYS;
    else if (截) *创建方式 = TRUNCATE_EXISTING;
    else *创建方式 = OPEN_EXISTING;
    *属性 = FILE_ATTRIBUTE_NORMAL;
    if (标志 & 64) *属性 |= FILE_FLAG_BACKUP_SEMANTICS; /* 须为目录：允许以此打开目录本身 */
    if (标志 & 128) *属性 |= FILE_FLAG_OPEN_REPARSE_POINT; /* 不循环末段符号链接 */
}

/* 由目录柄的绝对路径与 guest 传来的相对路径（UTF-8），拼出可交 CreateFileW 用的宽字符路径 */
static wchar_t *解析相对路径(int 目录柄, const uint8_t *相对, uint32_t 相对长) {
    if (目录柄 < 0 || 目录柄 >= 句柄表容量 || !句柄表[目录柄].占用 || !句柄表[目录柄].是目录) return NULL;
    wchar_t *相对宽 = 窄转宽(相对, 相对长);
    if (!相对宽) return NULL;
    size_t 基长 = wcslen(句柄表[目录柄].目录路径);
    size_t 总长 = 基长 + 1 + wcslen(相对宽) + 1;
    wchar_t *出 = malloc(总长 * sizeof(wchar_t));
    if (!出) { free(相对宽); return NULL; }
    wcscpy(出, 句柄表[目录柄].目录路径);
    if (相对长 > 0) { wcscat(出, L"\\"); wcscat(出, 相对宽); }
    free(相对宽);
    return 出;
}

static void 填状态块(uint8_t *出, HANDLE 柄, const BY_HANDLE_FILE_INFORMATION *信息) {
    memset(出, 0, 64);
    写64(出 + 0, (uint64_t)信息->dwVolumeSerialNumber);
    写64(出 + 8, ((uint64_t)信息->nFileIndexHigh << 32) | (uint64_t)信息->nFileIndexLow);
    出[16] = (信息->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ? 3 : 4;
    写64(出 + 24, (uint64_t)信息->nNumberOfLinks);
    写64(出 + 32, (((uint64_t)信息->nFileSizeHigh << 32) | (uint64_t)信息->nFileSizeLow));
    写64(出 + 40, 文件时间转纳秒(&信息->ftLastAccessTime));
    写64(出 + 48, 文件时间转纳秒(&信息->ftLastWriteTime));
    写64(出 + 56, 文件时间转纳秒(&信息->ftCreationTime));
    (void)柄;
}

typedef struct { int 参数个数; wchar_t **参数们; } 宿主状态;

static int32_t 调度内部(宿主状态 *状态, wasmtime_caller_t *客, int32_t 术, uint32_t 块址) {
    uint8_t *块 = 客内存(客, 块址, 64);
    if (!块) return 码_参数无效;

    if (术 == 术_取平台) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 4);
        if (!出) return 码_参数无效;
        写32(出, 4); /* 4 = Windows，见 底层基础/规范 */
        return 码_成功;
    }
    if (术 == 术_读单调纳秒) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 8);
        if (!出) return 码_参数无效;
        LARGE_INTEGER 计数, 频率;
        QueryPerformanceCounter(&计数);
        QueryPerformanceFrequency(&频率);
        uint64_t 纳秒 = (uint64_t)((double)计数.QuadPart * 1000000000.0 / (double)频率.QuadPart);
        写64(出, 纳秒);
        return 码_成功;
    }
    if (术 == 术_读墙钟纳秒) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 8);
        if (!出) return 码_参数无效;
        FILETIME ft;
        GetSystemTimePreciseAsFileTime(&ft);
        写64(出, 文件时间转纳秒(&ft));
        return 码_成功;
    }
    if (术 == 术_取时钟精度) {
        int32_t 钟号 = (int32_t)块字(块, 0);
        uint8_t *出 = 客内存(客, 块字(块, 1), 8);
        if (!出) return 码_参数无效;
        if (钟号 != 0 && 钟号 != 1) return 码_参数无效;
        写64(出, 钟号 == 0 ? 1000ull : 1ull);
        return 码_成功;
    }
    if (术 == 术_取时区偏移) {
        uint8_t *出 = 客内存(客, 块字(块, 2), 4);
        if (!出) return 码_参数无效;
        TIME_ZONE_INFORMATION 时区;
        DWORD 状态码 = GetTimeZoneInformation(&时区);
        long 偏 = -(long)时区.Bias * 60;
        if (状态码 == TIME_ZONE_ID_DAYLIGHT) 偏 -= (long)时区.DaylightBias * 60;
        else if (状态码 == TIME_ZONE_ID_STANDARD) 偏 -= (long)时区.StandardBias * 60;
        写32(出, (uint32_t)(int32_t)偏);
        return 码_成功;
    }
    if (术 == 术_填随机字节) {
        uint32_t 长 = 块字(块, 1);
        uint8_t *址 = 客内存(客, 块字(块, 0), 长);
        if (!址) return 码_参数无效;
        NTSTATUS 状态码 = BCryptGenRandom(NULL, 址, 长, BCRYPT_USE_SYSTEM_PREFERRED_RNG);
        return 状态码 == 0 ? 码_成功 : 码_输入输出;
    }
    if (术 == 术_取参数块 || 术 == 术_取环境块) {
        uint32_t 缓址 = 块字(块, 0), 缓长 = 块字(块, 1);
        uint8_t *出 = 客内存(客, 块字(块, 2), 8);
        uint8_t *缓 = 客内存(客, 缓址, 缓长);
        if (!出 || (缓长 && !缓)) return 码_参数无效;
        int 个数 = 0; uint32_t 写位 = 0; int 溢出 = 0;
        wchar_t **表宽 = 术 == 术_取参数块 ? 状态->参数们 : _wenviron;
        for (int i = 0; 表宽[i]; i++) {
            int 窄长 = WideCharToMultiByte(CP_UTF8, 0, 表宽[i], -1, NULL, 0, NULL, NULL);
            if (窄长 <= 0) continue;
            if (!溢出) {
                if (写位 + (uint32_t)窄长 <= 缓长) {
                    WideCharToMultiByte(CP_UTF8, 0, 表宽[i], -1, (char *)(缓 + 写位), 窄长, NULL, NULL);
                } else 溢出 = 1;
            }
            写位 += (uint32_t)窄长; 个数++;
        }
        if (溢出) { 写32(出, 写位); return 码_溢出; }
        写32(出, 写位); 写32(出 + 4, (uint32_t)个数);
        return 码_成功;
    }
    if (术 == 术_退出) {
        int32_t 码 = (int32_t)块字(块, 0);
        fflush(NULL);
        ExitProcess((UINT)码);
    }
    if (术 == 术_查询中断) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 4);
        if (!出) return 码_参数无效;
        写32(出, InterlockedExchange(&中断标志, 0) ? 1 : 0);
        return 码_成功;
    }
    if (术 == 术_取处理器数) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 4);
        if (!出) return 码_参数无效;
        DWORD 数 = GetActiveProcessorCount(ALL_PROCESSOR_GROUPS);
        写32(出, 数 > 0 ? (uint32_t)数 : 1);
        return 码_成功;
    }
    if (术 == 术_取初始句柄) {
        int32_t 序号 = (int32_t)块字(块, 0);
        if (序号 != 0) return 码_不存在;
        uint8_t *类别出 = 客内存(客, 块字(块, 1), 4);
        uint8_t *句柄出 = 客内存(客, 块字(块, 2), 4);
        uint8_t *名总长出 = 客内存(客, 块字(块, 5), 4);
        uint32_t 名缓长 = 块字(块, 4);
        uint8_t *名缓 = 客内存(客, 块字(块, 3), 名缓长);
        if (!类别出 || !句柄出 || !名总长出) return 码_参数无效;
        写32(名总长出, 1);
        if (名缓长 < 1) return 码_溢出;
        if (!名缓) return 码_参数无效;
        wchar_t 当前目录[MAX_PATH];
        DWORD 长 = GetCurrentDirectoryW(MAX_PATH, 当前目录);
        if (长 == 0 || 长 >= MAX_PATH) return 码_输入输出;
        int 槽 = 分配句柄槽();
        if (槽 < 0) return 码_打开过多;
        句柄表[槽].目录路径 = _wcsdup(当前目录);
        if (!句柄表[槽].目录路径) { 释放句柄槽(槽); return 码_输入输出; }
        句柄表[槽].占用 = 1; 句柄表[槽].是目录 = 1; 句柄表[槽].柄 = INVALID_HANDLE_VALUE;
        名缓[0] = '.';
        写32(类别出, 2); 写32(句柄出, (uint32_t)槽);
        return 码_成功;
    }
    if (术 == 术_取句柄类别) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        uint8_t *出 = 客内存(客, 块字(块, 1), 4);
        if (!出) return 码_参数无效;
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用) return 码_句柄无效;
        if (句柄表[柄号].是目录) { 写32(出, 2); return 码_成功; }
        DWORD 类型 = GetFileType(句柄表[柄号].柄);
        uint32_t 类;
        switch (类型) {
            case FILE_TYPE_DISK: 类 = 1; break;
            case FILE_TYPE_CHAR: 类 = 3; break;
            case FILE_TYPE_PIPE: 类 = 5; break;
            default: 类 = 4; break;
        }
        写32(出, 类);
        return 码_成功;
    }
    if (术 == 术_读) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        uint32_t 长 = 块字(块, 2);
        uint8_t *缓 = 客内存(客, 块字(块, 1), 长);
        uint8_t *出 = 客内存(客, 块字(块, 3), 4);
        if (!出 || (长 && !缓)) return 码_参数无效;
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用 || 句柄表[柄号].是目录) return 码_句柄无效;
        DWORD 得 = 0;
        if (!ReadFile(句柄表[柄号].柄, 缓, 长, &得, NULL)) return 错码(GetLastError());
        写32(出, (uint32_t)得);
        return 码_成功;
    }
    if (术 == 术_写) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        uint32_t 长 = 块字(块, 2);
        uint8_t *址 = 客内存(客, 块字(块, 1), 长);
        uint8_t *出 = 客内存(客, 块字(块, 3), 4);
        if (!出 || (长 && !址)) return 码_参数无效;
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用 || 句柄表[柄号].是目录) return 码_句柄无效;
        DWORD 得 = 0;
        if (!WriteFile(句柄表[柄号].柄, 址, 长, &得, NULL)) return 错码(GetLastError());
        写32(出, (uint32_t)得);
        return 码_成功;
    }
    if (术 == 术_定位读 || 术 == 术_定位写) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        uint32_t 长 = 块字(块, 2);
        uint64_t 偏 = ((uint64_t)块字(块, 4) << 32) | (uint64_t)块字(块, 3);
        uint8_t *出 = 客内存(客, 块字(块, 5), 4);
        uint8_t *缓 = 客内存(客, 块字(块, 1), 长);
        if (!出 || (长 && !缓)) return 码_参数无效;
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用 || 句柄表[柄号].是目录) return 码_句柄无效;
        OVERLAPPED 重叠 = {0};
        重叠.Offset = (DWORD)(偏 & 0xFFFFFFFFu);
        重叠.OffsetHigh = (DWORD)(偏 >> 32);
        DWORD 得 = 0;
        BOOL 成;
        if (术 == 术_定位读) 成 = ReadFile(句柄表[柄号].柄, 缓, 长, &得, &重叠);
        else 成 = WriteFile(句柄表[柄号].柄, 缓, 长, &得, &重叠);
        if (!成) {
            DWORD 错 = GetLastError();
            if (术 == 术_定位读 && 错 == ERROR_HANDLE_EOF) { 写32(出, 0); return 码_成功; }
            return 错码(错);
        }
        写32(出, (uint32_t)得);
        return 码_成功;
    }
    if (术 == 术_寻址) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        uint64_t 偏 = ((uint64_t)块字(块, 2) << 32) | (uint64_t)块字(块, 1);
        int32_t 起点 = (int32_t)块字(块, 3);
        uint8_t *出 = 客内存(客, 块字(块, 4), 8);
        if (!出) return 码_参数无效;
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用 || 句柄表[柄号].是目录) return 码_句柄无效;
        DWORD 起 = 起点 == 0 ? FILE_BEGIN : 起点 == 1 ? FILE_CURRENT : FILE_END;
        LARGE_INTEGER 移距, 新位;
        移距.QuadPart = (LONGLONG)偏;
        if (!SetFilePointerEx(句柄表[柄号].柄, 移距, &新位, 起)) return 错码(GetLastError());
        写64(出, (uint64_t)新位.QuadPart);
        return 码_成功;
    }
    if (术 == 术_关闭) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用) return 码_句柄无效;
        BOOL 成 = 句柄表[柄号].是目录 ? TRUE : CloseHandle(句柄表[柄号].柄);
        释放句柄槽(柄号);
        return 成 ? 码_成功 : 错码(GetLastError());
    }
    if (术 == 术_同步) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用 || 句柄表[柄号].是目录) return 码_句柄无效;
        return FlushFileBuffers(句柄表[柄号].柄) ? 码_成功 : 错码(GetLastError());
    }
    if (术 == 术_设长度) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        uint64_t 长 = ((uint64_t)块字(块, 2) << 32) | (uint64_t)块字(块, 1);
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用 || 句柄表[柄号].是目录) return 码_句柄无效;
        HANDLE 柄 = 句柄表[柄号].柄;
        LARGE_INTEGER 原位, 零 = {0}, 目标;
        if (!SetFilePointerEx(柄, 零, &原位, FILE_CURRENT)) return 错码(GetLastError());
        目标.QuadPart = (LONGLONG)长;
        if (!SetFilePointerEx(柄, 目标, NULL, FILE_BEGIN)) return 错码(GetLastError());
        BOOL 成 = SetEndOfFile(柄);
        DWORD 错 = 成 ? 0 : GetLastError();
        SetFilePointerEx(柄, 原位, NULL, FILE_BEGIN);
        return 成 ? 码_成功 : 错码(错);
    }
    if (术 == 术_设非阻塞) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        int32_t 开关 = (int32_t)块字(块, 1);
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用 || 句柄表[柄号].是目录) return 码_句柄无效;
        DWORD 模式 = 开关 ? PIPE_NOWAIT : PIPE_WAIT;
        return SetNamedPipeHandleState(句柄表[柄号].柄, &模式, NULL, NULL) ? 码_成功 : 码_不支持;
    }
    if (术 == 术_建管道) {
        uint8_t *读端出 = 客内存(客, 块字(块, 0), 4);
        uint8_t *写端出 = 客内存(客, 块字(块, 1), 4);
        if (!读端出 || !写端出) return 码_参数无效;
        HANDLE 读柄, 写柄;
        SECURITY_ATTRIBUTES 安全 = { sizeof(安全), NULL, FALSE };
        if (!CreatePipe(&读柄, &写柄, &安全, 0)) return 错码(GetLastError());
        int 读槽 = 分配句柄槽(), 写槽 = -1;
        if (读槽 >= 0) 写槽 = 分配句柄槽();
        if (读槽 < 0 || 写槽 < 0) {
            CloseHandle(读柄); CloseHandle(写柄);
            if (读槽 >= 0) 释放句柄槽(读槽);
            return 码_打开过多;
        }
        句柄表[读槽].占用 = 1; 句柄表[读槽].是目录 = 0; 句柄表[读槽].柄 = 读柄;
        句柄表[写槽].占用 = 1; 句柄表[写槽].是目录 = 0; 句柄表[写槽].柄 = 写柄;
        写32(读端出, (uint32_t)读槽); 写32(写端出, (uint32_t)写槽);
        return 码_成功;
    }
    if (术 == 术_等待) {
        uint32_t 订址 = 块字(块, 0);
        int32_t 订数 = (int32_t)块字(块, 1);
        uint32_t 事址 = 块字(块, 2);
        uint8_t *出 = 客内存(客, 块字(块, 3), 4);
        if (订数 <= 0) return 码_参数无效;
        uint8_t *订 = 客内存(客, 订址, (uint32_t)订数 * 16);
        uint8_t *事 = 客内存(客, 事址, (uint32_t)订数 * 16);
        if (!订 || !事 || !出) return 码_参数无效;
        /* 文言：磁盘之柄恒视为已就；管道读端以 PeekNamedPipe 探之，控制台等其余类别亦视为已就，
         * 免于牵涉 WaitForMultipleObjects 之复杂重叠状态。汉语：磁盘文件恒视为就绪；管道读端用
         * PeekNamedPipe 探测是否有数据；其余类别（控制台等）简化为恒就绪，避免引入
         * WaitForMultipleObjects 与重叠 I/O 的复杂状态机。 */
        int64_t 超时毫秒 = -1;
        for (int i = 0; i < 订数; i++) {
            uint32_t 类 = 读32(订 + i * 16);
            if (类 == 1) {
                uint64_t 纳秒 = 读64(订 + i * 16 + 8);
                int64_t 毫 = (int64_t)(纳秒 / 1000000ull);
                if (超时毫秒 < 0 || 毫 < 超时毫秒) 超时毫秒 = 毫;
            }
        }
        DWORD 起始 = GetTickCount();
        int32_t 写位 = 0;
        for (;;) {
            写位 = 0;
            for (int i = 0; i < 订数; i++) {
                uint32_t 类 = 读32(订 + i * 16);
                int 就绪 = 0;
                if (类 == 1) 就绪 = 1;
                else if (类 == 2 || 类 == 3) {
                    int32_t 柄号 = (int32_t)读32(订 + i * 16 + 4);
                    if (柄号 >= 0 && 柄号 < 句柄表容量 && 句柄表[柄号].占用 && !句柄表[柄号].是目录) {
                        DWORD 类型 = GetFileType(句柄表[柄号].柄);
                        if (类 == 3 || 类型 != FILE_TYPE_PIPE) 就绪 = 1;
                        else {
                            DWORD 可读 = 0;
                            if (PeekNamedPipe(句柄表[柄号].柄, NULL, 0, NULL, &可读, NULL) && 可读 > 0) 就绪 = 1;
                        }
                    }
                }
                if (就绪) {
                    uint8_t *e = 事 + (uint32_t)写位 * 16;
                    写32(e, (uint32_t)i); 写32(e + 4, 0); 写32(e + 8, 类); 写32(e + 12, 0);
                    写位++;
                }
            }
            if (写位 > 0 || 超时毫秒 == 0) break;
            if (超时毫秒 > 0 && (int64_t)(GetTickCount() - 起始) >= 超时毫秒) break;
            Sleep(1);
        }
        写32(出, (uint32_t)写位);
        return 码_成功;
    }
    if (术 == 术_打开) {
        int32_t 目录柄 = (int32_t)块字(块, 0);
        uint32_t 路径长 = 块字(块, 2);
        uint8_t *路径 = 客内存(客, 块字(块, 1), 路径长);
        int32_t 标志 = (int32_t)块字(块, 3);
        uint8_t *出 = 客内存(客, 块字(块, 4), 4);
        if (!路径 || !出) return 码_参数无效;
        wchar_t *全路径 = 解析相对路径(目录柄, 路径, 路径长);
        if (!全路径) return 码_句柄无效;
        DWORD 期望访问, 创建方式, 属性;
        转开标志(标志, &期望访问, &创建方式, &属性);
        HANDLE 柄 = CreateFileW(全路径, 期望访问, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                                创建方式, 属性, NULL);
        free(全路径);
        if (柄 == INVALID_HANDLE_VALUE) return 错码(GetLastError());
        int 槽 = 分配句柄槽();
        if (槽 < 0) { CloseHandle(柄); return 码_打开过多; }
        BY_HANDLE_FILE_INFORMATION 信息;
        int 是目录 = GetFileInformationByHandle(柄, &信息) && (信息.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
        句柄表[槽].占用 = 1; 句柄表[槽].柄 = 柄;
        if (是目录) {
            wchar_t 全路径缓[MAX_PATH];
            DWORD 长 = GetFinalPathNameByHandleW(柄, 全路径缓, MAX_PATH, FILE_NAME_NORMALIZED);
            句柄表[槽].是目录 = 1;
            句柄表[槽].目录路径 = (长 > 0 && 长 < MAX_PATH) ? _wcsdup(全路径缓) : _wcsdup(L".");
        } else {
            句柄表[槽].是目录 = 0; 句柄表[槽].目录路径 = NULL;
        }
        写32(出, (uint32_t)槽);
        return 码_成功;
    }
    if (术 == 术_取状态) {
        int32_t 柄号 = (int32_t)块字(块, 0);
        uint8_t *出 = 客内存(客, 块字(块, 1), 64);
        if (!出) return 码_参数无效;
        if (柄号 < 0 || 柄号 >= 句柄表容量 || !句柄表[柄号].占用) return 码_句柄无效;
        HANDLE 柄 = 句柄表[柄号].是目录
            ? CreateFileW(句柄表[柄号].目录路径, 0, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                          OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL)
            : 句柄表[柄号].柄;
        BY_HANDLE_FILE_INFORMATION 信息;
        BOOL 成 = GetFileInformationByHandle(柄, &信息);
        DWORD 错 = 成 ? 0 : GetLastError();
        if (句柄表[柄号].是目录 && 柄 != INVALID_HANDLE_VALUE) CloseHandle(柄);
        if (!成) return 错码(错);
        填状态块(出, 柄, &信息);
        return 码_成功;
    }
    if (术 == 术_取路径状态) {
        int32_t 目录柄 = (int32_t)块字(块, 0);
        uint32_t 路径长 = 块字(块, 2);
        uint8_t *路径 = 客内存(客, 块字(块, 1), 路径长);
        int32_t 标志 = (int32_t)块字(块, 3);
        uint8_t *出 = 客内存(客, 块字(块, 4), 64);
        if (!路径 || !出) return 码_参数无效;
        wchar_t *全路径 = 解析相对路径(目录柄, 路径, 路径长);
        if (!全路径) return 码_句柄无效;
        DWORD 属性 = FILE_FLAG_BACKUP_SEMANTICS;
        if (!(标志 & 1)) 属性 |= FILE_FLAG_OPEN_REPARSE_POINT; /* 标志&1：跟随末段符号链接 */
        HANDLE 柄 = CreateFileW(全路径, 0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                                NULL, OPEN_EXISTING, 属性, NULL);
        free(全路径);
        if (柄 == INVALID_HANDLE_VALUE) return 错码(GetLastError());
        BY_HANDLE_FILE_INFORMATION 信息;
        BOOL 成 = GetFileInformationByHandle(柄, &信息);
        DWORD 错 = 成 ? 0 : GetLastError();
        CloseHandle(柄);
        if (!成) return 错码(错);
        填状态块(出, NULL, &信息);
        return 码_成功;
    }
    if (术 == 术_读目录) {
        /* 文言：与 macOS/Linux 之宿主同：首版未接实，留待后续。汉语：与 macOS/Linux 宿主一样，
         * 首版未实现（需要宿主维护"句柄→查找流"的状态，用 FindFirstFileW/FindNextFileW），
         * 留待后续接上。 */
        return 码_未实现;
    }
    if (术 == 术_建目录) {
        int32_t 目录柄 = (int32_t)块字(块, 0);
        uint32_t 路径长 = 块字(块, 2);
        uint8_t *路径 = 客内存(客, 块字(块, 1), 路径长);
        if (!路径) return 码_参数无效;
        wchar_t *全路径 = 解析相对路径(目录柄, 路径, 路径长);
        if (!全路径) return 码_句柄无效;
        BOOL 成 = CreateDirectoryW(全路径, NULL);
        DWORD 错 = 成 ? 0 : GetLastError();
        free(全路径);
        return 成 ? 码_成功 : 错码(错);
    }
    if (术 == 术_删文件 || 术 == 术_删目录) {
        int32_t 目录柄 = (int32_t)块字(块, 0);
        uint32_t 路径长 = 块字(块, 2);
        uint8_t *路径 = 客内存(客, 块字(块, 1), 路径长);
        if (!路径) return 码_参数无效;
        wchar_t *全路径 = 解析相对路径(目录柄, 路径, 路径长);
        if (!全路径) return 码_句柄无效;
        BOOL 成 = 术 == 术_删目录 ? RemoveDirectoryW(全路径) : DeleteFileW(全路径);
        DWORD 错 = 成 ? 0 : GetLastError();
        free(全路径);
        return 成 ? 码_成功 : 错码(错);
    }
    if (术 == 术_改名) {
        int32_t 旧目录 = (int32_t)块字(块, 0), 新目录 = (int32_t)块字(块, 3);
        uint32_t 旧长 = 块字(块, 2), 新长 = 块字(块, 5);
        uint8_t *旧路径 = 客内存(客, 块字(块, 1), 旧长);
        uint8_t *新路径 = 客内存(客, 块字(块, 4), 新长);
        if (!旧路径 || !新路径) return 码_参数无效;
        wchar_t *旧全 = 解析相对路径(旧目录, 旧路径, 旧长);
        wchar_t *新全 = 旧全 ? 解析相对路径(新目录, 新路径, 新长) : NULL;
        if (!旧全 || !新全) { free(旧全); free(新全); return 码_句柄无效; }
        BOOL 成 = MoveFileExW(旧全, 新全, MOVEFILE_REPLACE_EXISTING);
        DWORD 错 = 成 ? 0 : GetLastError();
        free(旧全); free(新全);
        return 成 ? 码_成功 : 错码(错);
    }
    if (术 == 术_读链接) {
        /* 文言：Windows 之符号链接须经 FSCTL_GET_REPARSE_POINT 解析，颇繁而未曾验，
         * 姑返"不支持"，留待后续与真机核对后再接。汉语：Windows 的符号链接需要经
         * FSCTL_GET_REPARSE_POINT 解析，实现较繁且完全未经验证，先返回"不支持"，
         * 留给后续在真实 Windows 环境核对后再接上，避免带着未验证的复杂代码。 */
        return 码_不支持;
    }
    return 码_未实现;
}

static int32_t 调度(宿主状态 *状态, wasmtime_caller_t *客, int32_t 术, uint32_t 块址) {
    int32_t 果 = 调度内部(状态, 客, 术, 块址);
    if (getenv("YY_底层宿主_跟踪")) fprintf(stderr, "[跟踪] 术=%d 块址=%u 果=%d\n", 术, 块址, 果);
    return 果;
}

static wasm_trap_t *宿主回调(void *数据, wasmtime_caller_t *客, const wasmtime_val_t *参数, size_t 数, wasmtime_val_t *结果, size_t 结果数) {
    (void)数; (void)结果数;
    结果[0].kind = WASMTIME_I32;
    结果[0].of.i32 = 调度(数据, 客, 参数[0].of.i32, (uint32_t)参数[1].of.i32);
    return NULL;
}

static int 打印错误(wasmtime_error_t *错误, wasm_trap_t *陷阱) {
    wasm_name_t 文本;
    if (错误) {
        wasmtime_error_message(错误, &文本);
        fprintf(stderr, "宿主错误：%.*s\n", (int)文本.size, 文本.data);
        wasm_name_delete(&文本);
        wasmtime_error_delete(错误);
    }
    if (陷阱) {
        wasm_trap_message(陷阱, &文本);
        fprintf(stderr, "网页汇编陷阱：%.*s\n", (int)文本.size, 文本.data);
        wasm_name_delete(&文本);
        wasm_trap_delete(陷阱);
    }
    return 1;
}

int wmain(int argc, wchar_t **argv) {
    if (argc < 2) { fwprintf(stderr, L"用法：%s 模块.wasm [参数…]\n", argv[0]); return 2; }
    SetConsoleCtrlHandler(控制台处理, TRUE);

    句柄表[0].占用 = 1; 句柄表[0].是目录 = 0; 句柄表[0].柄 = GetStdHandle(STD_INPUT_HANDLE);
    句柄表[1].占用 = 1; 句柄表[1].是目录 = 0; 句柄表[1].柄 = GetStdHandle(STD_OUTPUT_HANDLE);
    句柄表[2].占用 = 1; 句柄表[2].是目录 = 0; 句柄表[2].柄 = GetStdHandle(STD_ERROR_HANDLE);

    FILE *文件 = _wfopen(argv[1], L"rb");
    if (!文件) { fwprintf(stderr, L"打开模块失败：%s\n", argv[1]); return 2; }
    fseek(文件, 0, SEEK_END);
    long 长 = ftell(文件);
    rewind(文件);
    if (长 <= 0) { fclose(文件); return 2; }
    uint8_t *字节 = malloc((size_t)长);
    if (!字节 || fread(字节, 1, (size_t)长, 文件) != (size_t)长) { fclose(文件); free(字节); return 2; }
    fclose(文件);

    wasm_engine_t *引擎 = wasm_engine_new();
    wasmtime_module_t *模块 = NULL;
    wasmtime_error_t *错误 = wasmtime_module_new(引擎, 字节, (size_t)长, &模块);
    free(字节);
    if (错误) { int 状态 = 打印错误(错误, NULL); wasm_engine_delete(引擎); return 状态; }

    wasmtime_store_t *存储 = wasmtime_store_new(引擎, NULL, NULL);
    wasmtime_context_t *上下文 = wasmtime_store_context(存储);
    wasmtime_linker_t *链接器 = wasmtime_linker_new(引擎);

    宿主状态 状态 = {argc - 1, argv + 1};

    wasm_valtype_t *入[2] = {wasm_valtype_new_i32(), wasm_valtype_new_i32()};
    wasm_valtype_t *出[1] = {wasm_valtype_new_i32()};
    wasm_valtype_vec_t 入组, 出组;
    wasm_valtype_vec_new(&入组, 2, 入);
    wasm_valtype_vec_new(&出组, 1, 出);
    wasm_functype_t *类型 = wasm_functype_new(&入组, &出组);
    const char *接口 = "底宿主";
    错误 = wasmtime_linker_define_func(链接器, 接口, strlen(接口), "call", 4, 类型, 宿主回调, &状态, NULL);
    wasm_functype_delete(类型);

    wasmtime_instance_t 实例;
    wasm_trap_t *陷阱 = NULL;
    int 退出状态 = 0;
    if (!错误) 错误 = wasmtime_linker_instantiate(链接器, 上下文, 模块, &实例, &陷阱);
    if (!错误 && !陷阱) {
        wasmtime_extern_t 启动;
        if (!wasmtime_instance_export_get(上下文, &实例, "启动", strlen("启动"), &启动) || 启动.kind != WASMTIME_EXTERN_FUNC) {
            fprintf(stderr, "模块没有导出“启动”函数\n");
            退出状态 = 2;
        } else {
            wasmtime_val_t 实参 = {.kind = WASMTIME_I32, .of = {.i32 = 0}};
            wasmtime_val_t 实果;
            错误 = wasmtime_func_call(上下文, &启动.of.func, &实参, 1, &实果, 1, &陷阱);
        }
    }
    if (错误 || 陷阱) 退出状态 = 打印错误(错误, 陷阱);

    wasmtime_linker_delete(链接器);
    wasmtime_store_delete(存储);
    wasmtime_module_delete(模块);
    wasm_engine_delete(引擎);
    return 退出状态;
}
