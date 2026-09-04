#include "公共包含.h"
#include "调试打印.h"

#include <poll.h>
#include <termios.h>

static struct termios 原终端设置;
static bool 已进入原始输入模式 = false;

static void 恢复终端输入模式(void) {
    if (已进入原始输入模式) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &原终端设置);
        已进入原始输入模式 = false;
    }
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

豫言值 豫言_打印通用值(豫言值 消息, 豫言值 对象) {
    fprintf(stderr, "[豫言通用值打印] %s: ", 豫言值转字符串(消息));
    打印豫言值(对象, 0);
    fprintf(stderr, "\n");
    return 单元转豫言值();
}
