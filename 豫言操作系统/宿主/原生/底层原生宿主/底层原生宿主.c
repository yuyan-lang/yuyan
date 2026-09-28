/* 豫言操作系统底层接口在 macOS／Linux 上的原生宿主：用 Wasmtime C API 加载
 * 豫言操作系统底层原生后端 编出的 Wasm 模块，唯一的宿主导入 底宿主.call(术,块址)
 * 在这里用真实的 POSIX 系统调用实现。参见同目录 说明.汉语.md。 */
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <wasmtime.h>

extern char **environ;

/* ---- 状态码（须与 裸机底层状态码。豫 / 底层接口方案 的编号一致） ---- */
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

static volatile sig_atomic_t 中断标志 = 0;
static void 收到中断(int 信号) { (void)信号; 中断标志 = 1; }

typedef struct { int 参数个数; char **参数们; } 宿主状态;

/* ---- 客体内存访问：返回 guest 偏移处、长度合法的宿主指针，越界返回 NULL ---- */
static uint8_t *客内存(wasmtime_caller_t *客, uint32_t 偏移, uint32_t 长) {
    wasmtime_extern_t 外部;
    if (!wasmtime_caller_export_get(客, "memory", 6, &外部) || 外部.kind != WASMTIME_EXTERN_MEMORY) {
        if (getenv("YY_底层宿主_跟踪")) fprintf(stderr, "[跟踪] 找不到内存导出\n");
        return NULL;
    }
    wasmtime_context_t *上下文 = wasmtime_caller_context(客);
    size_t 总长 = wasmtime_memory_data_size(上下文, &外部.of.memory);
    if ((size_t)偏移 > 总长 || (size_t)长 > 总长 - 偏移) {
        if (getenv("YY_底层宿主_跟踪")) fprintf(stderr, "[跟踪] 内存越界 偏移=%u 长=%u 总长=%zu\n", 偏移, 长, 总长);
        return NULL;
    }
    return wasmtime_memory_data(上下文, &外部.of.memory) + 偏移;
}

static uint32_t 读32(const uint8_t *p) { uint32_t v; memcpy(&v, p, 4); return v; }
static void 写32(uint8_t *p, uint32_t v) { memcpy(p, &v, 4); }
static void 写64(uint8_t *p, uint64_t v) { memcpy(p, &v, 8); }
static uint64_t 读64(const uint8_t *p) { uint64_t v; memcpy(&v, p, 8); return v; }

static int32_t 错码(int e) {
    switch (e) {
        case 0: return 码_成功;
        case EACCES: return 码_无权限;
        case EADDRINUSE: return 码_地址已用;
        case EAGAIN: return 码_暂不可用;
        case EBADF: return 码_句柄无效;
        case EEXIST: return 码_已存在;
        case EINVAL: return 码_参数无效;
        case EIO: return 码_输入输出;
        case EISDIR: return 码_是目录;
        case EMFILE: case ENFILE: return 码_打开过多;
        case ENAMETOOLONG: return 码_名太长;
        case ENOENT: return 码_不存在;
        case ENOSYS: return 码_未实现;
        case ENOTDIR: return 码_非目录;
        case ENOTEMPTY: return 码_目录非空;
        case ENOTSUP: return 码_不支持;
        case EOVERFLOW: return 码_溢出;
        case ESPIPE: return 码_不可寻址;
        case EXDEV: return 码_跨设备;
        default: return 码_输入输出;
    }
}

/* 打开标志：1 读 2 写 4 追加 8 创建 16 独占 32 截断 64 须为目录 128 不循环末段符号链接 */
static int 转开标志(int32_t 标志) {
    int f = 0;
    int 读 = 标志 & 1, 写 = 标志 & 2;
    if (读 && 写) f |= O_RDWR; else if (写) f |= O_WRONLY; else f |= O_RDONLY;
    if (标志 & 4) f |= O_APPEND;
    if (标志 & 8) f |= O_CREAT;
    if (标志 & 16) f |= O_EXCL;
    if (标志 & 32) f |= O_TRUNC;
#ifdef O_DIRECTORY
    if (标志 & 64) f |= O_DIRECTORY;
#endif
#ifdef O_NOFOLLOW
    if (标志 & 128) f |= O_NOFOLLOW;
#endif
    return f;
}

static void 填状态块(uint8_t *出, const struct stat *st) {
    memset(出, 0, 64);
    写64(出 + 0, (uint64_t)st->st_dev);
    写64(出 + 8, (uint64_t)st->st_ino);
    int 类型 = 4;
    if (S_ISDIR(st->st_mode)) 类型 = 3;
    else if (S_ISREG(st->st_mode)) 类型 = 4;
    else if (S_ISLNK(st->st_mode)) 类型 = 7;
    else if (S_ISSOCK(st->st_mode)) 类型 = 6;
    else if (S_ISCHR(st->st_mode) || S_ISBLK(st->st_mode)) 类型 = st->st_mode & S_IFBLK ? 1 : 2;
    出[16] = (uint8_t)类型;
    写64(出 + 24, (uint64_t)st->st_nlink);
    写64(出 + 32, (uint64_t)st->st_size);
#if defined(__APPLE__)
    写64(出 + 40, (uint64_t)st->st_atimespec.tv_sec * 1000000000ull + (uint64_t)st->st_atimespec.tv_nsec);
    写64(出 + 48, (uint64_t)st->st_mtimespec.tv_sec * 1000000000ull + (uint64_t)st->st_mtimespec.tv_nsec);
    写64(出 + 56, (uint64_t)st->st_ctimespec.tv_sec * 1000000000ull + (uint64_t)st->st_ctimespec.tv_nsec);
#else
    写64(出 + 40, (uint64_t)st->st_atim.tv_sec * 1000000000ull + (uint64_t)st->st_atim.tv_nsec);
    写64(出 + 48, (uint64_t)st->st_mtim.tv_sec * 1000000000ull + (uint64_t)st->st_mtim.tv_nsec);
    写64(出 + 56, (uint64_t)st->st_ctim.tv_sec * 1000000000ull + (uint64_t)st->st_ctim.tv_nsec);
#endif
}

/* 取块中第 序 个 4 字节字（从 0 记）。 */
static uint32_t 块字(const uint8_t *块, int 序) { return 读32(块 + 序 * 4); }

static int32_t 调度内部(宿主状态 *状态, wasmtime_caller_t *客, int32_t 术, uint32_t 块址) {
    uint8_t *块 = 客内存(客, 块址, 64);
    if (!块) return 码_参数无效;

    if (术 == 术_取平台) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 4);
        if (!出) return 码_参数无效;
#if defined(__APPLE__)
        写32(出, 3);
#elif defined(__linux__)
        写32(出, 2);
#else
        写32(出, 0);
#endif
        return 码_成功;
    }
    if (术 == 术_读单调纳秒 || 术 == 术_读墙钟纳秒) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 8);
        if (!出) return 码_参数无效;
        struct timespec ts;
        clock_gettime(术 == 术_读单调纳秒 ? CLOCK_MONOTONIC : CLOCK_REALTIME, &ts);
        写64(出, (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec);
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
        uint64_t 秒 = ((uint64_t)块字(块, 1) << 32) | (uint64_t)块字(块, 0);
        uint8_t *出 = 客内存(客, 块字(块, 2), 4);
        if (!出) return 码_参数无效;
        time_t t = (time_t)秒;
        struct tm lt;
        if (!localtime_r(&t, &lt)) { 写32(出, 0); return 码_成功; }
        long 偏 = 0;
#if defined(__APPLE__) || defined(__linux__)
        偏 = lt.tm_gmtoff;
#endif
        写32(出, (uint32_t)(int32_t)偏);
        return 码_成功;
    }
    if (术 == 术_填随机字节) {
        uint32_t 长 = 块字(块, 1);
        uint8_t *址 = 客内存(客, 块字(块, 0), 长);
        if (!址) return 码_参数无效;
#if defined(__APPLE__)
        arc4random_buf(址, 长);
        return 码_成功;
#elif defined(__linux__)
        FILE *f = fopen("/dev/urandom", "rb");
        if (!f) return 码_不支持;
        size_t 得 = fread(址, 1, 长, f);
        fclose(f);
        return 得 == 长 ? 码_成功 : 码_输入输出;
#else
        return 码_不支持;
#endif
    }
    if (术 == 术_取参数块 || 术 == 术_取环境块) {
        uint32_t 缓址 = 块字(块, 0), 缓长 = 块字(块, 1);
        uint8_t *出 = 客内存(客, 块字(块, 2), 8);
        uint8_t *缓 = 客内存(客, 缓址, 缓长);
        if (!出 || (缓长 && !缓)) return 码_参数无效;
        int 个数 = 0; uint32_t 写位 = 0; int 溢出 = 0;
        char **表 = 术 == 术_取参数块 ? 状态->参数们 : environ;
        for (int i = 0; 表[i]; i++) {
            size_t 长 = strlen(表[i]) + 1;
            if (!溢出) {
                if (写位 + 长 <= 缓长) { memcpy(缓 + 写位, 表[i], 长); }
                else 溢出 = 1;
            }
            写位 += (uint32_t)长; 个数++;
        }
        if (溢出) { 写32(出, 写位); return 码_溢出; }
        写32(出, 写位); 写32(出 + 4, (uint32_t)个数);
        return 码_成功;
    }
    if (术 == 术_退出) {
        int32_t 码 = (int32_t)块字(块, 0);
        fflush(NULL);
        _exit(码);
    }
    if (术 == 术_查询中断) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 4);
        if (!出) return 码_参数无效;
        写32(出, 中断标志 ? 1 : 0); 中断标志 = 0;
        return 码_成功;
    }
    if (术 == 术_取处理器数) {
        uint8_t *出 = 客内存(客, 块字(块, 0), 4);
        if (!出) return 码_参数无效;
        long 数 = sysconf(_SC_NPROCESSORS_ONLN);
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
        名缓[0] = '.';
        写32(类别出, 2); 写32(句柄出, 3);
        return 码_成功;
    }
    if (术 == 术_取句柄类别) {
        int32_t 柄 = (int32_t)块字(块, 0);
        uint8_t *出 = 客内存(客, 块字(块, 1), 4);
        if (!出) return 码_参数无效;
        struct stat st;
        if (fstat(柄, &st) != 0) return 错码(errno);
        uint32_t 类;
        if (isatty(柄)) 类 = 3;
        else if (S_ISDIR(st.st_mode)) 类 = 2;
        else if (S_ISREG(st.st_mode)) 类 = 1;
        else if (S_ISFIFO(st.st_mode)) 类 = 5;
        else if (S_ISSOCK(st.st_mode)) 类 = 6;
        else 类 = 4;
        写32(出, 类);
        return 码_成功;
    }
    if (术 == 术_读) {
        int32_t 柄 = (int32_t)块字(块, 0);
        uint32_t 长 = 块字(块, 2);
        uint8_t *缓 = 客内存(客, 块字(块, 1), 长);
        uint8_t *出 = 客内存(客, 块字(块, 3), 4);
        if (!出 || (长 && !缓)) return 码_参数无效;
        ssize_t 得;
        do { 得 = read(柄, 缓, 长); } while (得 < 0 && errno == EINTR);
        if (得 < 0) return 错码(errno);
        写32(出, (uint32_t)得);
        return 码_成功;
    }
    if (术 == 术_写) {
        int32_t 柄 = (int32_t)块字(块, 0);
        uint32_t 长 = 块字(块, 2);
        uint8_t *址 = 客内存(客, 块字(块, 1), 长);
        uint8_t *出 = 客内存(客, 块字(块, 3), 4);
        if (!出 || (长 && !址)) return 码_参数无效;
        ssize_t 得;
        do { 得 = write(柄, 址, 长); } while (得 < 0 && errno == EINTR);
        if (得 < 0) return 错码(errno);
        写32(出, (uint32_t)得);
        return 码_成功;
    }
    if (术 == 术_定位读 || 术 == 术_定位写) {
        int32_t 柄 = (int32_t)块字(块, 0);
        uint32_t 长 = 块字(块, 2);
        uint64_t 偏 = ((uint64_t)块字(块, 4) << 32) | (uint64_t)块字(块, 3);
        uint8_t *出 = 客内存(客, 块字(块, 5), 4);
        uint8_t *缓 = 客内存(客, 块字(块, 1), 长);
        if (!出 || (长 && !缓)) return 码_参数无效;
        ssize_t 得;
        if (术 == 术_定位读) do { 得 = pread(柄, 缓, 长, (off_t)偏); } while (得 < 0 && errno == EINTR);
        else do { 得 = pwrite(柄, 缓, 长, (off_t)偏); } while (得 < 0 && errno == EINTR);
        if (得 < 0) return 错码(errno);
        写32(出, (uint32_t)得);
        return 码_成功;
    }
    if (术 == 术_寻址) {
        int32_t 柄 = (int32_t)块字(块, 0);
        uint64_t 偏 = ((uint64_t)块字(块, 2) << 32) | (uint64_t)块字(块, 1);
        int32_t 起点 = (int32_t)块字(块, 3);
        uint8_t *出 = 客内存(客, 块字(块, 4), 8);
        if (!出) return 码_参数无效;
        int 起 = 起点 == 0 ? SEEK_SET : 起点 == 1 ? SEEK_CUR : SEEK_END;
        off_t 得 = lseek(柄, (off_t)(int64_t)偏, 起);
        if (得 < 0) return 错码(errno);
        写64(出, (uint64_t)得);
        return 码_成功;
    }
    if (术 == 术_关闭) {
        int32_t 柄 = (int32_t)块字(块, 0);
        return close(柄) == 0 ? 码_成功 : 错码(errno);
    }
    if (术 == 术_同步) {
        int32_t 柄 = (int32_t)块字(块, 0);
        return fsync(柄) == 0 ? 码_成功 : 错码(errno);
    }
    if (术 == 术_设长度) {
        int32_t 柄 = (int32_t)块字(块, 0);
        uint64_t 长 = ((uint64_t)块字(块, 2) << 32) | (uint64_t)块字(块, 1);
        return ftruncate(柄, (off_t)长) == 0 ? 码_成功 : 错码(errno);
    }
    if (术 == 术_设非阻塞) {
        int32_t 柄 = (int32_t)块字(块, 0);
        int32_t 开关 = (int32_t)块字(块, 1);
        int 旧 = fcntl(柄, F_GETFL);
        if (旧 < 0) return 错码(errno);
        int 新 = 开关 ? (旧 | O_NONBLOCK) : (旧 & ~O_NONBLOCK);
        return fcntl(柄, F_SETFL, 新) == 0 ? 码_成功 : 错码(errno);
    }
    if (术 == 术_建管道) {
        uint8_t *读端出 = 客内存(客, 块字(块, 0), 4);
        uint8_t *写端出 = 客内存(客, 块字(块, 1), 4);
        if (!读端出 || !写端出) return 码_参数无效;
        int fds[2];
        if (pipe(fds) != 0) return 错码(errno);
        写32(读端出, fds[0]); 写32(写端出, fds[1]);
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
        struct pollfd 轮询[64];
        int 轮询序[64];
        int 轮询数 = 0;
        int64_t 超时毫秒 = -1;
        for (int i = 0; i < 订数 && i < 64; i++) {
            uint32_t 类 = 读32(订 + i * 16);
            if (类 == 1) {
                uint64_t 纳秒 = 读64(订 + i * 16 + 8);
                int64_t 毫 = (int64_t)(纳秒 / 1000000ull);
                if (超时毫秒 < 0 || 毫 < 超时毫秒) 超时毫秒 = 毫;
            } else if (类 == 2 || 类 == 3) {
                轮询[轮询数].fd = (int)读32(订 + i * 16 + 4);
                轮询[轮询数].events = (short)(类 == 2 ? POLLIN : POLLOUT);
                轮询序[轮询数] = i;
                轮询数++;
            }
        }
        if (轮询数 > 0) poll(轮询, (nfds_t)轮询数, 超时毫秒 < 0 ? -1 : (int)超时毫秒);
        else if (超时毫秒 > 0) { struct timespec ts = {超时毫秒 / 1000, (超时毫秒 % 1000) * 1000000}; nanosleep(&ts, NULL); }
        int32_t 写位 = 0;
        for (int i = 0; i < 订数; i++) {
            uint32_t 类 = 读32(订 + i * 16);
            int 就绪 = 类 == 1;
            short revents = 0;
            for (int j = 0; j < 轮询数; j++) if (轮询序[j] == i) revents = 轮询[j].revents;
            if ((类 == 2 && (revents & (POLLIN | POLLHUP | POLLERR))) ||
                (类 == 3 && (revents & (POLLOUT | POLLERR)))) 就绪 = 1;
            if (就绪) {
                uint8_t *e = 事 + (uint32_t)写位 * 16;
                写32(e, (uint32_t)i); 写32(e + 4, 0); 写32(e + 8, 类); 写32(e + 12, 0);
                写位++;
            }
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
        if (!路径 || !出 || 路径长 >= 4096) return 码_参数无效;
        char 路径c[4096]; memcpy(路径c, 路径, 路径长); 路径c[路径长] = 0;
        int fd = openat(目录柄, 路径c, 转开标志(标志), 0644);
        if (fd < 0) return 错码(errno);
        写32(出, fd);
        return 码_成功;
    }
    if (术 == 术_取状态) {
        int32_t 柄 = (int32_t)块字(块, 0);
        uint8_t *出 = 客内存(客, 块字(块, 1), 64);
        if (!出) return 码_参数无效;
        struct stat st;
        if (fstat(柄, &st) != 0) return 错码(errno);
        填状态块(出, &st);
        return 码_成功;
    }
    if (术 == 术_取路径状态) {
        int32_t 目录柄 = (int32_t)块字(块, 0);
        uint32_t 路径长 = 块字(块, 2);
        uint8_t *路径 = 客内存(客, 块字(块, 1), 路径长);
        int32_t 标志 = (int32_t)块字(块, 3);
        uint8_t *出 = 客内存(客, 块字(块, 4), 64);
        if (!路径 || !出 || 路径长 >= 4096) return 码_参数无效;
        char 路径c[4096]; memcpy(路径c, 路径, 路径长); 路径c[路径长] = 0;
        struct stat st;
        int flags = (标志 & 1) ? 0 : AT_SYMLINK_NOFOLLOW;
        if (fstatat(目录柄, 路径c, &st, flags) != 0) return 错码(errno);
        填状态块(出, &st);
        return 码_成功;
    }
    if (术 == 术_读目录) {
        /* 文言：首版未接实：目录之流须宿主持之，涉句柄表，留待后续。汉语：首版未实现：目录流需要宿主维护状态（句柄到 DIR* 的映射），留待后续接上。 */
        return 码_未实现;
    }
    if (术 == 术_建目录 || 术 == 术_删文件 || 术 == 术_删目录) {
        int32_t 目录柄 = (int32_t)块字(块, 0);
        uint32_t 路径长 = 块字(块, 2);
        uint8_t *路径 = 客内存(客, 块字(块, 1), 路径长);
        if (!路径 || 路径长 >= 4096) return 码_参数无效;
        char 路径c[4096]; memcpy(路径c, 路径, 路径长); 路径c[路径长] = 0;
        int 果;
        if (术 == 术_建目录) 果 = mkdirat(目录柄, 路径c, 0755);
        else if (术 == 术_删目录) 果 = unlinkat(目录柄, 路径c, AT_REMOVEDIR);
        else 果 = unlinkat(目录柄, 路径c, 0);
        return 果 == 0 ? 码_成功 : 错码(errno);
    }
    if (术 == 术_改名) {
        int32_t 旧目录 = (int32_t)块字(块, 0), 新目录 = (int32_t)块字(块, 3);
        uint32_t 旧长 = 块字(块, 2), 新长 = 块字(块, 5);
        uint8_t *旧路径 = 客内存(客, 块字(块, 1), 旧长);
        uint8_t *新路径 = 客内存(客, 块字(块, 4), 新长);
        if (!旧路径 || !新路径 || 旧长 >= 4096 || 新长 >= 4096) return 码_参数无效;
        char 旧c[4096], 新c[4096];
        memcpy(旧c, 旧路径, 旧长); 旧c[旧长] = 0;
        memcpy(新c, 新路径, 新长); 新c[新长] = 0;
        return renameat(旧目录, 旧c, 新目录, 新c) == 0 ? 码_成功 : 错码(errno);
    }
    if (术 == 术_读链接) {
        int32_t 目录柄 = (int32_t)块字(块, 0);
        uint32_t 路径长 = 块字(块, 2), 缓长 = 块字(块, 4);
        uint8_t *路径 = 客内存(客, 块字(块, 1), 路径长);
        uint8_t *缓 = 客内存(客, 块字(块, 3), 缓长);
        uint8_t *出 = 客内存(客, 块字(块, 5), 4);
        if (!路径 || !出 || 路径长 >= 4096) return 码_参数无效;
        char 路径c[4096]; memcpy(路径c, 路径, 路径长); 路径c[路径长] = 0;
        char 目标[4096];
        ssize_t 得 = readlinkat(目录柄, 路径c, 目标, sizeof 目标);
        if (得 < 0) return 错码(errno);
        写32(出, (uint32_t)得);
        if ((uint32_t)得 > 缓长) return 码_溢出;
        if (缓长 && !缓) return 码_参数无效;
        if (得) memcpy(缓, 目标, (size_t)得);
        return 码_成功;
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

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "用法：%s 模块.wasm [参数…]\n", argv[0]); return 2; }
    signal(SIGINT, 收到中断);

    FILE *文件 = fopen(argv[1], "rb");
    if (!文件) { perror("打开模块"); return 2; }
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
            /* 文言：豫言「有→有」之导出，未加签名后缀，故 Wasm 签之为一 i32 进一 i32 出（值恒零）。汉语：豫言「有→有」的导出，没加签名后缀，Wasm 签名是 1 个 i32 参数、1 个 i32 结果（值恒为 0）。 */
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
