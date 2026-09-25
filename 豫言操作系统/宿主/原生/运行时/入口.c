
#include "公共包含.h"

extern void 可选入口初始化();

int 全局参数数量 = 0;
char** 全局参数值 = NULL;


extern void 处理豫言参数(int, char **);

int main(int 参数数量, char* 参数值[]) {

    // 保存全局命令行参数。
    全局参数数量 = 参数数量;
    全局参数值 = 参数值;

    处理豫言参数(参数数量, 参数值);
    可选入口初始化();

    // 初始化全局异常处理器。
    初始化全局异常处理器();

    // 初始化随机种子。
    srand ( time ( NULL));

    int 返回值 = 启动豫言运行时();
    return 返回值;
}
