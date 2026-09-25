#include "公共包含.h"
#include "宿主协议.h"
#include <wasmtime.h>
#include <spawn.h>
#include <signal.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>

extern char **environ;
extern 豫言值 豫言_存放包上下文(豫言值);

/* 文言：句柄各归其客，客终则收其子。汉语：不向 guest 暴露宿主指针或文件描述符，句柄单调分配，销毁实例时清理进程。 */
typedef struct 进程记录 {
    int32_t 句柄;
    pid_t 编号;
    int 输出文件, 错误文件, 完成, 状态;
    char *输出, *错误;
    uint32_t 输出长, 错误长;
    struct 进程记录 *下一个;
} 进程记录;
typedef struct {
    进程记录 *诸进程;
    int32_t 下一句柄;
    size_t 存活数;
    const char *运行器, *模块;
    bool 预编译;
    豫言值 客参数;
    size_t 参数起;
} 宿主状态;

static int 临时输出(void) {
    char 路径[] = "/tmp/yy宿主输出.XXXXXX";
    int 文件 = mkstemp(路径);
    if (文件 >= 0) { unlink(路径); fcntl(文件, F_SETFD, FD_CLOEXEC); }
    return 文件;
}
static int 读完文件(int 文件, char **内容, uint32_t *长度) {
    if (文件 < 0) { *内容 = strdup(""); *长度 = 0; return *内容 ? 0 : ENOMEM; }
    struct stat 信息;
    if (fstat(文件, &信息) != 0) return errno;
    if (信息.st_size < 0 || 信息.st_size > 256 * 1024 * 1024) return EFBIG;
    *长度 = (uint32_t)信息.st_size;
    *内容 = malloc((size_t)*长度 + 1);
    if (!*内容) return ENOMEM;
    size_t 已读 = 0;
    while (已读 < *长度) {
        ssize_t 数 = pread(文件, *内容 + 已读, *长度 - 已读, (off_t)已读);
        if (数 < 0 && errno == EINTR) continue;
        if (数 <= 0) { free(*内容); *内容 = NULL; return 数 < 0 ? errno : EIO; }
        已读 += (size_t)数;
    }
    (*内容)[*长度] = 0; return 0;
}
static 进程记录 *找进程(宿主状态 *状态, int32_t 句柄) {
    for (进程记录 *项 = 状态->诸进程; 项; 项 = 项->下一个) if (项->句柄 == 句柄) return 项;
    return NULL;
}
static 进程记录 *新记录(宿主状态 *状态) {
    if (状态->存活数 >= 4096 || 状态->下一句柄 == INT32_MAX) return NULL;
    进程记录 *项 = calloc(1, sizeof(*项));
    if (!项) return NULL;
    项->输出文件 = 项->错误文件 = -1;
    项->句柄 = ++状态->下一句柄;
    项->下一个 = 状态->诸进程; 状态->诸进程 = 项; 状态->存活数++;
    return 项;
}
static void 释记录(宿主状态 *状态, 进程记录 *项) {
    if (项->编号 > 0 && !项->完成) {
        kill(项->编号, SIGKILL);
        while (waitpid(项->编号, NULL, 0) < 0 && errno == EINTR) {}
    }
    if (项->输出文件 >= 0) close(项->输出文件);
    if (项->错误文件 >= 0) close(项->错误文件);
    进程记录 **位置 = &状态->诸进程;
    while (*位置 != 项) 位置 = &(*位置)->下一个;
    *位置 = 项->下一个; 状态->存活数--;
    free(项->输出); free(项->错误); free(项);
}
static int 检查完成(进程记录 *项) {
    if (项->完成) return 1;
    int 状态;
    pid_t 所得;
    do { 所得 = waitpid(项->编号, &状态, WNOHANG); } while (所得 < 0 && errno == EINTR);
    if (所得 < 0) return -errno;
    if (!所得) return 0;
    项->完成 = 1;
    项->状态 = WIFEXITED(状态) ? WEXITSTATUS(状态) : 128 + WTERMSIG(状态);
    int 错误 = 读完文件(项->输出文件, &项->输出, &项->输出长);
    if (!错误) 错误 = 读完文件(项->错误文件, &项->错误, &项->错误长);
    if (错误) {
        free(项->输出); free(项->错误);
        项->输出 = strdup(""); 项->输出长 = 0;
        项->错误 = strdup(strerror(错误)); 项->错误长 = (uint32_t)strlen(strerror(错误));
        项->状态 = 125;
        if (!项->输出 || !项->错误) return -ENOMEM;
    }
    return 1;
}
static uint64_t 毫秒(void) {
    struct timespec 时; clock_gettime(CLOCK_MONOTONIC, &时);
    return (uint64_t)时.tv_sec * 1000 + 时.tv_nsec / 1000000;
}
static int 等待(进程记录 *项, int32_t 超时) {
    if (超时 < -1) return -EINVAL;
    uint64_t 始 = 毫秒();
    for (;;) {
        int 结果 = 检查完成(项);
        if (结果 || (超时 >= 0 && 毫秒() - 始 >= (uint32_t)超时)) return 结果;
        struct timespec 暂停 = {0, 1000000}; nanosleep(&暂停, NULL);
    }
}
static bool 是模块(const char *路径) {
    unsigned char 头[4]; int 文件 = open(路径, O_RDONLY | O_CLOEXEC);
    if (文件 < 0) return false;
    ssize_t 数 = read(文件, 头, sizeof 头); close(文件);
    return 数 == 4 && memcmp(头, "\0asm", 4) == 0;
}
static uint32_t 读数(const unsigned char *位置) {
    return (uint32_t)位置[0] | (uint32_t)位置[1] << 8 | (uint32_t)位置[2] << 16 | (uint32_t)位置[3] << 24;
}
static void 写数(unsigned char *位置, uint32_t 数) {
    for (int 序 = 0; 序 < 4; 序++) 位置[序] = (unsigned char)(数 >> (序 * 8));
}
static int32_t 派生(宿主状态 *状态, const unsigned char *请求, uint32_t 长度, int 模式) {
    if (长度 < 4 || 模式 < 0 || 模式 > 3) return -EINVAL;
    uint32_t 数 = 读数(请求), 序 = 0;
    if (!数 || 数 > 65536) return -E2BIG;
    char **参数 = calloc((size_t)数 + 4, sizeof(char *));
    if (!参数) return -ENOMEM;
    size_t 偏移 = 4; int 错误 = EINVAL;
    for (; 序 < 数; 序++) {
        if (长度 - 偏移 < 4) goto 释放参数;
        uint32_t 长 = 读数(请求 + 偏移); 偏移 += 4;
        if (长 > 长度 - 偏移 || memchr(请求 + 偏移, 0, 长)) goto 释放参数;
        参数[序] = malloc((size_t)长 + 1);
        if (!参数[序]) { 错误 = ENOMEM; goto 释放参数; }
        memcpy(参数[序], 请求 + 偏移, 长); 参数[序][长] = 0; 偏移 += 长;
    }
    if (偏移 != 长度 || !参数[0][0]) goto 释放参数;
    进程记录 *项 = 新记录(状态);
    if (!项) { 错误 = ENOMEM; goto 释放参数; }
    posix_spawn_file_actions_t 操作;
    错误 = posix_spawn_file_actions_init(&操作);
    if (错误) { 释记录(状态, 项); goto 释放参数; }
    if (模式 != 宿主继承标准流) 错误 = posix_spawn_file_actions_addopen(&操作, 0, "/dev/null", O_RDONLY, 0);
    if (!错误 && 模式 == 宿主捕获) {
        项->输出文件 = 临时输出(); 项->错误文件 = 临时输出();
        if (项->输出文件 < 0 || 项->错误文件 < 0) 错误 = errno;
        if (!错误) 错误 = posix_spawn_file_actions_adddup2(&操作, 项->输出文件, 1);
        if (!错误) 错误 = posix_spawn_file_actions_adddup2(&操作, 项->错误文件, 2);
    } else if (!错误 && 模式 == 宿主弃流) {
        错误 = posix_spawn_file_actions_addopen(&操作, 1, "/dev/null", O_WRONLY, 0);
        if (!错误) 错误 = posix_spawn_file_actions_addopen(&操作, 2, "/dev/null", O_WRONLY, 0);
    }
    /* 文言：客器再召己，仍由宿主载之。汉语：编译器 worker 的 Wasm 自调用自动转为同一宿主启动；原生工具保持原参数边界。 */
    char **运行参数 = 参数;
    bool 复用预编译 = 状态->预编译 && strcmp(参数[0], 状态->模块) == 0;
    if (是模块(参数[0]) || 复用预编译) {
        运行参数 = calloc((size_t)数 + 3, sizeof(char *));
        if (!运行参数) 错误 = ENOMEM;
        else {
            运行参数[0] = (char *)状态->运行器;
            size_t 始 = 1;
            if (复用预编译) 运行参数[始++] = "--运行预编译";
            for (uint32_t 次 = 0; 次 < 数; 次++) 运行参数[始 + 次] = 参数[次];
        }
    }
    if (!错误) 错误 = posix_spawnp(&项->编号, 运行参数[0], &操作, NULL, 运行参数, environ);
    if (运行参数 != 参数) free(运行参数);
    posix_spawn_file_actions_destroy(&操作);
    if (错误) { 项->编号 = 0; 释记录(状态, 项); }
    else 错误 = -项->句柄;
释放参数:
    for (uint32_t 次 = 0; 次 < 数; 次++) free(参数[次]);
    free(参数); return -错误;
}

static unsigned char *客内范围(wasmtime_caller_t *客, uint32_t 偏移, uint32_t 长) {
    wasmtime_extern_t 外部;
    if (!wasmtime_caller_export_get(客, "memory", 6, &外部) || 外部.kind != WASMTIME_EXTERN_MEMORY) return NULL;
    wasmtime_context_t *上下文 = wasmtime_caller_context(客);
    size_t 总长 = wasmtime_memory_data_size(上下文, &外部.of.memory);
    if ((size_t)偏移 > 总长 || (size_t)长 > 总长 - 偏移) return NULL;
    return wasmtime_memory_data(上下文, &外部.of.memory) + 偏移;
}
static int32_t 调度宿主(宿主状态 *状态, wasmtime_caller_t *客, int32_t 术, int32_t 甲, int32_t 乙, int32_t 丙, int32_t 丁) {
    if (术 == 宿主核数) { long 数 = sysconf(_SC_NPROCESSORS_ONLN); return 数 > 0 && 数 < INT32_MAX ? (int32_t)数 : 1; }
    if (术 == 宿主启动 || 术 == 宿主存上下文 || 术 == 宿主真实路径) {
        if (乙 < 0 || 乙 > 64 * 1024 * 1024) return -E2BIG;
        unsigned char *请求 = 客内范围(客, (uint32_t)甲, (uint32_t)乙);
        if (!请求) return -EFAULT;
        if (术 == 宿主启动) return 派生(状态, 请求, (uint32_t)乙, 丙);
        if (memchr(请求, 0, (size_t)乙)) return -EINVAL;
        char *副本 = malloc((size_t)乙 + 1);
        if (!副本) return -ENOMEM;
        memcpy(副本, 请求, (size_t)乙); 副本[乙] = 0;
        char *真实 = NULL;
        if (术 == 宿主真实路径) 真实 = realpath(副本, NULL);
        else {
            豫言值 路径 = 豫言_存放包上下文(静态字符串转豫言值(副本));
            真实 = strdup(豫言值转字符串(路径));
        }
        int 错误 = errno; free(副本);
        if (!真实) return -错误;
        进程记录 *项 = 新记录(状态);
        if (!项) { free(真实); return -ENOMEM; }
        项->完成 = 1; 项->输出 = 真实; 项->错误 = strdup("");
        if (!项->输出 || !项->错误) { 释记录(状态, 项); return -ENOMEM; }
        项->输出长 = (uint32_t)strlen(项->输出); return 项->句柄;
    }
    if (术 == 宿主多路等待) {
        if (乙 < 0 || 乙 > 65535 || 丙 < -1) return -EINVAL;
        unsigned char *请求 = 客内范围(客, (uint32_t)甲, ((uint32_t)乙 * 2 + 1) * 4);
        unsigned char *答 = 客内范围(客, (uint32_t)丁, (uint32_t)乙 * 4);
        if (!请求 || !答 || 读数(请求) != (uint32_t)乙) return -EFAULT;
        uint64_t 始 = 毫秒();
        for (;;) {
            bool 就绪 = false;
            for (int32_t 序 = 0; 序 < 乙; 序++) {
                uint32_t 关注 = 读数(请求 + 8 + 序 * 8);
                if (关注 != 1) return -EINVAL;
                进程记录 *项 = 找进程(状态, (int32_t)读数(请求 + 4 + 序 * 8));
                if (!项) return -EBADF;
                int 结果 = 检查完成(项); if (结果 < 0) return 结果;
                写数(答 + 序 * 4, 结果 ? 1 : 0); 就绪 |= 结果 != 0;
            }
            if (就绪 || (丙 >= 0 && 毫秒() - 始 >= (uint32_t)丙)) return 0;
            struct timespec 暂停 = {0, 1000000}; nanosleep(&暂停, NULL);
        }
    }
    进程记录 *项 = 找进程(状态, 甲);
    if (!项) return -EBADF;
    if (术 == 宿主等待) return 等待(项, 乙);
    if (术 == 宿主释放) { 释记录(状态, 项); return 0; }
    if (!项->完成) return -EAGAIN;
    if (术 == 宿主结果信息) {
        unsigned char *答 = 客内范围(客, (uint32_t)乙, 16);
        if (!答 || 丙 != 16) return -EFAULT;
        写数(答, (uint32_t)项->编号); 写数(答 + 4, (uint32_t)项->状态);
        写数(答 + 8, 项->输出长); 写数(答 + 12, 项->错误长); return 0;
    }
    if (术 == 宿主读输出) {
        if (丁 != 0 && 丁 != 1) return -EINVAL;
        uint32_t 长 = 丁 ? 项->错误长 : 项->输出长;
        unsigned char *答 = 客内范围(客, (uint32_t)乙, 长);
        if (!答 || 丙 < 0 || (uint32_t)丙 < 长) return -EFAULT;
        const char *内容 = 丁 ? 项->错误 : 项->输出;
        if (长 && !内容) return -ENOMEM;
        if (长) memcpy(答, 内容, 长);
        return (int32_t)长;
    }
    return -ENOSYS;
}
static wasm_trap_t *宿主回调(void *数据, wasmtime_caller_t *客, const wasmtime_val_t *参数, size_t 数, wasmtime_val_t *结果, size_t 结果数) {
    (void)数; (void)结果数;
    结果[0].kind = WASMTIME_I32;
    结果[0].of.i32 = 调度宿主(数据, 客, 参数[0].of.i32, 参数[1].of.i32, 参数[2].of.i32, 参数[3].of.i32, 参数[4].of.i32);
    return NULL;
}

#include "回收桥接.h"

static int 打印引擎错误(wasmtime_error_t *错误, wasm_trap_t *陷阱) {
    int 状态 = 1; wasm_name_t 文本;
    if (错误) {
        if (!wasmtime_error_exit_status(错误, &状态)) {
            wasmtime_error_message(错误, &文本); fprintf(stderr, "宿主错误：%.*s\n", (int)文本.size, 文本.data); wasm_name_delete(&文本);
        }
        wasmtime_error_delete(错误);
    }
    if (陷阱) { wasm_trap_message(陷阱, &文本); fprintf(stderr, "网页汇编陷阱：%.*s\n", (int)文本.size, 文本.data); wasm_name_delete(&文本); wasm_trap_delete(陷阱); }
    return 状态;
}

static 豫言值 运行网页汇编内核(豫言值 自己, 豫言值 参数组) {
    uint64_t 数 = 获取同构列长度(参数组); 豫言值 *值 = 获取同构列元素(参数组);
    if (!数 || strcmp(豫言值转字符串(值[0]), "--帮助") == 0) {
        puts("用法：yy网页汇编宿主 模块.wasm [参数…]\n      yy网页汇编宿主 --预编译 模块.wasm 本机.cwasm\n      yy网页汇编宿主 --运行预编译 本机.cwasm [参数…]\n此编译宿主授予进程执行及当前目录读写能力，仅加载可信构建程序。");
        return 整数转豫言值(数 ? 0 : 2);
    }
    bool 组装 = strcmp(豫言值转字符串(值[0]), "--组装") == 0;
    bool 预编译 = strcmp(豫言值转字符串(值[0]), "--预编译") == 0;
    bool 载预编译 = strcmp(豫言值转字符串(值[0]), "--运行预编译") == 0;
    size_t 始 = 预编译 || 载预编译 || 组装 ? 1 : 0;
    if (数 <= 始 || ((预编译 || 组装) && 数 != 3)) return 整数转豫言值(2);
    char *模块路径 = realpath(豫言值转字符串(值[始]), NULL), *自己路径 = realpath(豫言值转字符串(自己), NULL);
    if (!模块路径 || !自己路径) { perror("模块或宿主路径"); free(模块路径); free(自己路径); return 整数转豫言值(2); }
    FILE *文件 = fopen(模块路径, "rb");
    if (!文件) { free(模块路径); free(自己路径); return 整数转豫言值(2); }
    fseek(文件, 0, SEEK_END); long 长 = ftell(文件); rewind(文件);
    if (长 <= 0 || 长 > 1024L * 1024 * 1024) { fclose(文件); free(模块路径); free(自己路径); return 整数转豫言值(2); }
    /* 文言：预编译之器直接映卷，勿每客复抄。汉语：AOT 使用引擎的文件映射入口，避免每个 worker 先读取整份机器码再复制到可执行映射。 */
    unsigned char *字节 = NULL;
    if (!载预编译) {
        字节 = malloc((size_t)长);
        if (!字节 || fread(字节, 1, (size_t)长, 文件) != (size_t)长) { fclose(文件); free(字节); free(模块路径); free(自己路径); return 整数转豫言值(2); }
    }
    fclose(文件);
    if (组装) {
        wasm_byte_vec_t 产物;
        wasmtime_error_t *错 = wasmtime_wat2wasm((const char *)字节, (size_t)长, &产物);
        int 状态 = 0;
        if (错) 状态 = 打印引擎错误(错, NULL);
        else {
            FILE *输出 = fopen(豫言值转字符串(值[2]), "wb");
            if (!输出) 状态 = 1;
            else { if (fwrite(产物.data, 1, 产物.size, 输出) != 产物.size) 状态 = 1; if (fclose(输出)) 状态 = 1; }
            wasm_byte_vec_delete(&产物);
        }
        free(字节); free(模块路径); free(自己路径); return 整数转豫言值(状态);
    }
    /* 文言：以所设造引擎，勿旁取默认。汉语：必须使用配置构造入口，才能使复制收集器的初始堆和内联配置实际生效。 */
    wasm_config_t *引擎配置 = wasm_config_new();
    /* 文言：常调用用真栈，给足而留宿主之余。汉语：Wasm 使用 64 MiB，执行线程另留 64 MiB 给桥接与引擎。 */
    /* 汉语：栈限额由复制宿主的 Rust 配置同时设置，避免超过引擎的 async_stack_size。 */
    wasm_engine_t *引擎 = wasm_engine_new_with_config(引擎配置); wasmtime_module_t *模块 = NULL;
    wasmtime_error_t *错误 = NULL;
    if (!载预编译 && ((size_t)长 < 4 || memcmp(字节, "\0asm", 4) != 0)) {
        /* 文言：文本亦先校而成模块。汉语：WAT 仅经过标准解析，预编译数据仍须显式选择。 */
        wasm_byte_vec_t 二进制;
        错误 = wasmtime_wat2wasm((const char *)字节, (size_t)长, &二进制);
        if (!错误) { 错误 = wasmtime_module_new(引擎, (const uint8_t *)二进制.data, 二进制.size, &模块); wasm_byte_vec_delete(&二进制); }
    } else 错误 = 载预编译 ? wasmtime_module_deserialize_file(引擎, 模块路径, &模块) : wasmtime_module_new(引擎, 字节, (size_t)长, &模块);
    free(字节); int 状态 = 0;
    if (错误) { 状态 = 打印引擎错误(错误, NULL); goto 释引擎; }
    if (预编译) {
        wasm_byte_vec_t 产物; 错误 = wasmtime_module_serialize(模块, &产物);
        if (错误) 状态 = 打印引擎错误(错误, NULL);
        else {
            文件 = fopen(豫言值转字符串(值[2]), "wb");
            if (!文件) 状态 = 1;
            else { if (fwrite(产物.data, 1, 产物.size, 文件) != 产物.size) 状态 = 1; if (fclose(文件)) 状态 = 1; }
            wasm_byte_vec_delete(&产物);
        }
        goto 释模块;
    }
    宿主状态 宿主 = {.运行器 = 自己路径, .模块 = 模块路径, .预编译 = 载预编译, .客参数 = 参数组, .参数起 = 始 + 1};
    wasmtime_store_t *存储 = wasmtime_store_new(引擎, NULL, NULL);
    wasmtime_context_t *上下文 = wasmtime_store_context(存储);
    wasi_config_t *配置 = wasi_config_new();
    char 目录[PATH_MAX];
    if (!getcwd(目录, sizeof 目录)) { 状态 = 1; wasi_config_delete(配置); goto 释存储; }
    setenv("YY_WASM_HOST_CWD", 目录, 1); setenv("YY_WASM_HOST_BRIDGE", "1", 1);
    wasi_config_inherit_env(配置); wasi_config_inherit_stdin(配置); wasi_config_inherit_stdout(配置); wasi_config_inherit_stderr(配置);
    const char **参数 = calloc((size_t)数 - 始, sizeof(char *));
    if (!参数) { 状态 = 1; wasi_config_delete(配置); goto 释存储; }
    参数[0] = 模块路径;
    for (size_t 序 = 始 + 1; 序 < 数; 序++) 参数[序 - 始] = 豫言值转字符串(值[序]);
    bool 配好 = wasi_config_set_argv(配置, (size_t)数 - 始, 参数) &&
        wasi_config_preopen_dir(配置, 目录, 目录, true) && wasi_config_preopen_dir(配置, 目录, ".", true);
    free(参数);
    if (!配好) { 状态 = 1; wasi_config_delete(配置); goto 释存储; }
    错误 = wasmtime_context_set_wasi(上下文, 配置);
    if (错误) { 状态 = 打印引擎错误(错误, NULL); goto 释存储; }
    wasmtime_linker_t *链接器 = wasmtime_linker_new(引擎);
    错误 = wasmtime_linker_define_wasi(链接器);
    if (!错误) {
        wasm_valtype_t *入[5]; for (size_t 序 = 0; 序 < 5; 序++) 入[序] = wasm_valtype_new_i32();
        wasm_valtype_t *出[1] = {wasm_valtype_new_i32()};
        wasm_valtype_vec_t 入组, 出组; wasm_valtype_vec_new(&入组, 5, 入); wasm_valtype_vec_new(&出组, 1, 出);
        wasm_functype_t *类型 = wasm_functype_new(&入组, &出组);
        const char *接口 = "yuyan:build-host/v1";
        错误 = wasmtime_linker_define_func(链接器, 接口, strlen(接口), "call", 4, 类型, 宿主回调, &宿主, NULL);
        wasm_functype_delete(类型);
    }
    if (!错误) 错误 = 定义回收宿主(引擎, 链接器, &宿主);
    wasm_trap_t *陷阱 = NULL; wasmtime_instance_t 实例;
    if (!错误) 错误 = wasmtime_linker_instantiate(链接器, 上下文, 模块, &实例, &陷阱);
    if (!错误 && !陷阱) 错误 = 预留回收堆(上下文, &实例);
    if (!错误 && !陷阱) {
        wasmtime_extern_t 启动;
        if (!wasmtime_instance_export_get(上下文, &实例, "_start", 6, &启动) || 启动.kind != WASMTIME_EXTERN_FUNC) 状态 = 2;
        else 错误 = wasmtime_func_call(上下文, &启动.of.func, NULL, 0, NULL, 0, &陷阱);
    }
    if (错误 || 陷阱) 状态 = 打印引擎错误(错误, 陷阱);
    while (宿主.诸进程) 释记录(&宿主, 宿主.诸进程);
    wasmtime_linker_delete(链接器);
释存储:
    wasmtime_store_delete(存储);
释模块:
    wasmtime_module_delete(模块);
释引擎:
    wasm_engine_delete(引擎); free(模块路径); free(自己路径); return 整数转豫言值(状态);
}

/* 文言：主线程候之，不并行用原生堆。汉语：独立线程提供明确的本机栈容量；调用方等待其结束，原生运行时不会并发执行。 */
typedef struct { 豫言值 自己, 参数, 结果; } 宿主线程参数;
static void *执行宿主线程(void *指针) {
    宿主线程参数 *参数 = 指针;
    参数->结果 = 运行网页汇编内核(参数->自己, 参数->参数);
    return NULL;
}
豫言值 豫言_运行网页汇编宿主(豫言值 自己, 豫言值 参数组) {
    宿主线程参数 参数 = {自己, 参数组, 整数转豫言值(1)};
    pthread_attr_t 属性;
    int 错 = pthread_attr_init(&属性);
    if (错) { fprintf(stderr, "宿主线程属性：%s\n", strerror(错)); return 参数.结果; }
    错 = pthread_attr_setstacksize(&属性, 128u * 1024u * 1024u);
    pthread_t 线程;
    if (!错) 错 = pthread_create(&线程, &属性, 执行宿主线程, &参数);
    pthread_attr_destroy(&属性);
    if (错) { fprintf(stderr, "宿主执行线程：%s\n", strerror(错)); return 参数.结果; }
    错 = pthread_join(线程, NULL);
    if (错) { fprintf(stderr, "等待宿主线程：%s\n", strerror(错)); abort(); }
    return 参数.结果;
}
