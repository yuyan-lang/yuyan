import java.util.Arrays;

final class 性能基准 {
    private static long 斐波那契(long 数) {
        if (数 < 2) {
            return 数;
        }
        return 斐波那契(数 - 1) + 斐波那契(数 - 2);
    }

    private static long 素数筛(int 上限) {
        boolean[] 是否质数 = new boolean[上限 + 1];
        Arrays.fill(是否质数, true);
        是否质数[0] = false;
        是否质数[1] = false;
        for (int 质数 = 2; (long) 质数 * 质数 <= 上限; 质数++) {
            if (是否质数[质数]) {
                for (int 倍数 = 质数 * 质数; 倍数 <= 上限; 倍数 += 质数) {
                    是否质数[倍数] = false;
                }
            }
        }
        long 个数 = 0;
        for (int 数值 = 2; 数值 <= 上限; 数值++) {
            if (是否质数[数值]) {
                个数++;
            }
        }
        return 个数;
    }

    private static long 矩阵乘法(int 边长) {
        int 总长 = 边长 * 边长;
        long[] 矩阵甲 = new long[总长];
        long[] 矩阵乙 = new long[总长];
        long[] 矩阵丙 = new long[总长];
        for (int 序数 = 0; 序数 < 总长; 序数++) {
            int 行 = 序数 / 边长;
            int 列 = 序数 - 行 * 边长;
            矩阵甲[序数] = 行 + 列 + 1L;
            矩阵乙[序数] = 行 * 2L + 列 + 1L;
        }
        for (int 行 = 0; 行 < 边长; 行++) {
            for (int 列 = 0; 列 < 边长; 列++) {
                long 总和 = 0;
                for (int 中间序数 = 0; 中间序数 < 边长; 中间序数++) {
                    总和 += 矩阵甲[行 * 边长 + 中间序数] * 矩阵乙[中间序数 * 边长 + 列];
                }
                矩阵丙[行 * 边长 + 列] = 总和;
            }
        }
        return 矩阵丙[0] + 矩阵丙[总长 - 1];
    }

    private static void 交换(long[] 数组, int 左序, int 右序) {
        long 数值 = 数组[左序];
        数组[左序] = 数组[右序];
        数组[右序] = 数值;
    }

    private static int 分区(long[] 数组, int 下界, int 上界) {
        int 中点 = 下界 + (上界 - 下界) / 2;
        交换(数组, 中点, 上界);
        long 枢轴 = 数组[上界];
        int 存放序数 = 下界;
        for (int 序数 = 下界; 序数 < 上界; 序数++) {
            if (数组[序数] < 枢轴) {
                交换(数组, 序数, 存放序数);
                存放序数++;
            }
        }
        交换(数组, 存放序数, 上界);
        return 存放序数;
    }

    private static void 快速排序(long[] 数组, int 下界, int 上界) {
        if (下界 < 上界) {
            int 分区序数 = 分区(数组, 下界, 上界);
            快速排序(数组, 下界, 分区序数 - 1);
            快速排序(数组, 分区序数 + 1, 上界);
        }
    }

    private static long 快速排序基准(int 长度) {
        long[] 数组 = new long[长度];
        for (int 序数 = 0; 序数 < 长度; 序数++) {
            数组[序数] = 长度 - 序数;
        }
        快速排序(数组, 0, 长度 - 1);
        return 数组[0] + 数组[长度 / 2] + 数组[长度 - 1];
    }

    public static void main(String[] 参数) {
        if (参数.length != 2) {
            throw new IllegalArgumentException("用法：性能基准 <斐波那契|素数筛|矩阵乘法|快速排序> <规模>");
        }
        String 算法 = 参数[0];
        int 规模 = Integer.parseInt(参数[1]);
        long 结果 = switch (算法) {
            case "斐波那契" -> 斐波那契(规模);
            case "素数筛" -> 素数筛(规模);
            case "矩阵乘法" -> 矩阵乘法(规模);
            case "快速排序" -> 快速排序基准(规模);
            default -> throw new IllegalArgumentException("未知算法：" + 算法);
        };
        System.out.println(结果);
    }
}
