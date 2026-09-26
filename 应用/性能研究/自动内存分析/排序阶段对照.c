/* 文言：同源内核，分时观其费。汉语：复用既有 C 对照算法，只新增分阶段观测入口，不改排序实现。 */
#define main 旧对照入口
#include "../运行内核/原生内核.c"
#undef main
__attribute__((export_name("分配"))) int64_t *阶段分配(int64_t 长度) {
    return malloc((size_t)长度 * sizeof(int64_t));
}
__attribute__((export_name("填充"))) void 阶段填充(int64_t *数组, int64_t 长度) {
    for (int64_t 序数 = 0; 序数 < 长度; 序数++) 数组[序数] = 长度 - 序数;
}
__attribute__((export_name("排序"))) void 阶段排序(int64_t *数组, int64_t 长度) {
    快速排序(数组, 0, 长度 - 1);
}
__attribute__((export_name("校验"))) int64_t 阶段校验(int64_t *数组, int64_t 长度) {
    return 数组[0] + 数组[长度 / 2] + 数组[长度 - 1];
}
__attribute__((export_name("释放"))) void 阶段释放(int64_t *数组) { free(数组); }
