#include "公共包含.h"
#include "调试打印.h"

/* WASI 无原生终端。原始终端实现仅用于原生目标。 */
#ifndef __wasi__
#include <poll.h>
#include <termios.h>
#include <sys/ioctl.h>

static struct termios 原终端设置;
static bool 已进入原始输入模式 = false;

static void 恢复终端输入模式(void) {
    if (已进入原始输入模式) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &原终端设置);
        已进入原始输入模式 = false;
    }
}

#endif

/* 文言：屏仅司绘，事数属豫言。汉语：这里只提供终端检测、裁剪和区域重绘，不持有构建状态。 */
static int 面板旧行数 = 0;
static unsigned short 面板旧宽 = 0, 面板旧高 = 0;

豫言值 豫言_可绘监视面板(void) {
#ifndef __wasi__
    struct winsize 尺寸;
    const char *类型 = getenv("TERM");
    return 爻转豫言值(isatty(STDERR_FILENO) && 类型 && strcmp(类型, "dumb") &&
        ioctl(STDERR_FILENO, TIOCGWINSZ, &尺寸) == 0 && 尺寸.ws_row >= 6 && 尺寸.ws_col >= 20);
#else
    return 爻转豫言值(false);
#endif
}

豫言值 豫言_绘监视面板(豫言值 文本值) {
#ifndef __wasi__
    struct winsize 尺寸;
    bool 有尺寸 = ioctl(STDERR_FILENO, TIOCGWINSZ, &尺寸) == 0;
    /* 文言：窗改则另起，勿误删旧辞。汉语：缩放可能触发终端自动折行；此时保留旧快照，避免按旧行数擦除历史输出。 */
    if (面板旧行数 && (!有尺寸 || 尺寸.ws_col != 面板旧宽 || 尺寸.ws_row != 面板旧高)) {
        fputc('\n', stderr);
        面板旧行数 = 0;
    }
    if (面板旧行数) {
        fprintf(stderr, "\r\033[%dA\033[J", 面板旧行数);
        面板旧行数 = 0;
    }
    const unsigned char *文 = (const unsigned char *)豫言值转字符串(文本值);
    if (!*文 || !有尺寸 || 尺寸.ws_row < 6 || 尺寸.ws_col < 20) {
        fflush(stderr);
        return 单元转豫言值();
    }
    面板旧宽 = 尺寸.ws_col;
    面板旧高 = 尺寸.ws_row;
    int 限行 = 尺寸.ws_row - 2;
    while (*文 && 面板旧行数 < 限行) {
        if (面板旧行数 == 限行 - 1 && strchr((const char *)文, '\n')) {
            fputs("...\n", stderr);
            ++面板旧行数;
            break;
        }
        int 宽 = 0;
        while (*文 && *文 != '\n') {
            int 长 = *文 < 0x80 ? 1 : (*文 < 0xe0 ? 2 : (*文 < 0xf0 ? 3 : 4));
            int 字宽 = *文 < 0x80 ? 1 : 2;
            int 有效长 = 1;
            while (有效长 < 长 && 文[有效长] && (文[有效长] & 0xc0) == 0x80) ++有效长;
            /* 文言：制符不入屏，宽留一格。汉语：过滤路径中的控制符，保守估计非 ASCII 宽度，避免折行。 */
            if (宽 + 字宽 < 尺寸.ws_col && *文 >= 0x20 && *文 != 0x7f)
                fwrite(文, 1, 有效长, stderr);
            宽 += 字宽;
            文 += 有效长;
        }
        if (*文 == '\n') ++文;
        fputc('\n', stderr);
        ++面板旧行数;
    }
    fflush(stderr);
#endif
    return 单元转豫言值();
}

豫言值 豫言_打印行(豫言值 字符串) {
    fprintf(stdout,"%s\n", 豫言值转字符串(字符串));
    fflush(stdout);
    return 单元转豫言值();
}

豫言值 豫言_标准错误打印行(豫言值 字符串) {
    fprintf(stderr,"%s\n", 豫言值转字符串(字符串));
    fflush(stderr);
    return 单元转豫言值();
}

豫言值 豫言_打印字符串(豫言值 字符串) {
    fprintf(stdout,"%s", 豫言值转字符串(字符串));
    fflush(stdout);
    return 单元转豫言值();
}

豫言值 豫言_读取全部标准输入() {
    // 分配初始缓冲区。
    size_t 缓冲区大小 = 1024;
    char* 缓冲区 = (char*)malloc(缓冲区大小);
    if (缓冲区 == NULL) {
        报错并中止("无法分配内存");
        return 单元转豫言值();
    }

    size_t 总大小 = 0;
    size_t 已读字节数;

    // 读取标准输入直到文件末尾。
    while ((已读字节数 = fread(缓冲区 + 总大小, sizeof(char), 缓冲区大小 - 总大小, stdin)) > 0) {
        总大小 += 已读字节数;

        // 必要时扩展缓冲区。
        if (总大小 == 缓冲区大小) {
            缓冲区大小 *= 2;
            char* 新缓冲区 = (char*)realloc(缓冲区, 缓冲区大小);
            if (新缓冲区 == NULL) {
                free(缓冲区);
                报错并中止("无法重新分配内存");
            }
            缓冲区 = 新缓冲区;
        }
    }

    // 以空字符结束字符串。
    缓冲区[总大小] = '\0';

    char *结果 = malloc(总大小 + 1);
    memcpy(结果, 缓冲区, 总大小 + 1);

    豫言值 返回值 = 复制字符串为豫言值(总大小 + 1, 结果);
    free(结果);
    free(缓冲区);
    return 返回值;
}

豫言值 豫言_读取标准输入行() {
    char* 行 = NULL;
    size_t 缓冲区大小 = 0;
    ssize_t 已读字节数 = getline(&行, &缓冲区大小, stdin);
    if (已读字节数 == -1) {
        free(行);
        报错并中止("无法从标准输入读取一行");
        return 单元转豫言值();
    }

    豫言值 返回值;
    // 若末尾存在换行符则将其删除。
    if (已读字节数 > 0 && 行[已读字节数 - 1] == '\n') {
        行[已读字节数 - 1] = '\0';
        返回值 = 复制字符串为豫言值(已读字节数, 行);
    } else {
        行[已读字节数] = '\0';
        返回值 = 复制字符串为豫言值(已读字节数 + 1, 行);
    }

    free(行);
    return 返回值;
}

豫言值 豫言_尝试读取标准输入行() {
    char *行 = NULL;
    size_t 缓冲区大小 = 0;
    errno = 0;
    ssize_t 已读字节数 = getline(&行, &缓冲区大小, stdin);
    if (已读字节数 < 0) {
        free(行);
        豫言值 值组[] = {
            爻转豫言值(false),
            复制字节为豫言值(0, ""),
        };
        return 元组转豫言值(2, 值组);
    }

    // 行结尾同时兼容换行和回车换行。
    size_t 内容长度 = (size_t)已读字节数;
    if (内容长度 > 0 && 行[内容长度 - 1] == '\n') {
        内容长度--;
    }
    if (内容长度 > 0 && 行[内容长度 - 1] == '\r') {
        内容长度--;
    }

    豫言值 值组[] = {
        爻转豫言值(true),
        复制字节为豫言值(内容长度, 行),
    };
    free(行);
    return 元组转豫言值(2, 值组);
}

豫言值 豫言_标准输入是终端() {
    return 爻转豫言值(isatty(STDIN_FILENO));
}

豫言值 豫言_标准输出是终端() {
    return 爻转豫言值(isatty(STDOUT_FILENO));
}

#ifndef __wasi__
豫言值 豫言_进入终端原始输入模式() {
    if (已进入原始输入模式) {
        return 爻转豫言值(true);
    }
    if (!isatty(STDIN_FILENO) || tcgetattr(STDIN_FILENO, &原终端设置) != 0) {
        return 爻转豫言值(false);
    }

    struct termios 原始设置 = 原终端设置;
    原始设置.c_iflag &= (tcflag_t)~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    原始设置.c_cflag |= CS8;
    原始设置.c_lflag &= (tcflag_t)~(ECHO | ICANON | IEXTEN | ISIG);
    原始设置.c_cc[VMIN] = 1;
    原始设置.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &原始设置) != 0) {
        return 爻转豫言值(false);
    }

    已进入原始输入模式 = true;
    static bool 已登记退出恢复 = false;
    if (!已登记退出恢复) {
        atexit(恢复终端输入模式);
        已登记退出恢复 = true;
    }
    return 爻转豫言值(true);
}

豫言值 豫言_退出终端原始输入模式() {
    恢复终端输入模式();
    return 单元转豫言值();
}

static ssize_t 读取按键字节(unsigned char *缓冲区, size_t 需要字节数) {
    size_t 已读 = 0;
    while (已读 < 需要字节数) {
        ssize_t 本次 = read(STDIN_FILENO, 缓冲区 + 已读, 需要字节数 - 已读);
        if (本次 > 0) {
            已读 += (size_t)本次;
            continue;
        }
        if (本次 < 0 && errno == EINTR) continue;
        return 已读 == 0 ? 本次 : (ssize_t)已读;
    }
    return (ssize_t)已读;
}

豫言值 豫言_读取终端按键() {
    unsigned char 按键[16] = {0};
    ssize_t 已读 = 读取按键字节(按键, 1);
    if (已读 <= 0) {
        豫言值 值组[] = {爻转豫言值(false), 复制字节为豫言值(0, "")};
        return 元组转豫言值(2, 值组);
    }

    size_t 目标长度 = 1;
    if ((按键[0] & 0xE0U) == 0xC0U) 目标长度 = 2;
    else if ((按键[0] & 0xF0U) == 0xE0U) 目标长度 = 3;
    else if ((按键[0] & 0xF8U) == 0xF0U) 目标长度 = 4;

    if (目标长度 > 1) {
        ssize_t 后续 = 读取按键字节(按键 + 1, 目标长度 - 1);
        if (后续 > 0) 已读 += 后续;
    } else if (按键[0] == 0x1BU) {
        // 把方向键等短转义序列合并为一次按键，避免其中的 '[' 被写入命令行。
        while ((size_t)已读 < sizeof(按键)) {
            struct pollfd 描述符 = {.fd = STDIN_FILENO, .events = POLLIN};
            int 可读 = poll(&描述符, 1, 5);
            if (可读 <= 0 || (描述符.revents & POLLIN) == 0) break;
            ssize_t 本次 = read(STDIN_FILENO, 按键 + 已读, sizeof(按键) - (size_t)已读);
            if (本次 <= 0) break;
            已读 += 本次;
        }
    }

    豫言值 值组[] = {
        爻转豫言值(true),
        复制字节为豫言值((size_t)已读, (const char *)按键),
    };
    return 元组转豫言值(2, 值组);
}

#endif

豫言值 豫言_打印通用值(豫言值 消息, 豫言值 对象) {
    fprintf(stderr, "[豫言通用值打印] %s: ", 豫言值转字符串(消息));
    打印豫言值(对象, 0);
    fprintf(stderr, "\n");
    return 单元转豫言值();
}
