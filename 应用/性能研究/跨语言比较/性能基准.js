function 斐波那契(数) {
    if (数 < 2) {
        return 数;
    }
    return 斐波那契(数 - 1) + 斐波那契(数 - 2);
}

function 素数筛(上限) {
    const 是否质数 = new Uint8Array(上限 + 1);
    是否质数.fill(1);
    是否质数[0] = 0;
    是否质数[1] = 0;
    for (let 质数 = 2; 质数 * 质数 <= 上限; 质数++) {
        if (是否质数[质数]) {
            for (let 倍数 = 质数 * 质数; 倍数 <= 上限; 倍数 += 质数) {
                是否质数[倍数] = 0;
            }
        }
    }
    let 个数 = 0;
    for (let 数值 = 2; 数值 <= 上限; 数值++) {
        if (是否质数[数值]) {
            个数++;
        }
    }
    return 个数;
}

function 矩阵乘法(边长) {
    const 总长 = 边长 * 边长;
    const 矩阵甲 = new Float64Array(总长);
    const 矩阵乙 = new Float64Array(总长);
    const 矩阵丙 = new Float64Array(总长);
    for (let 序数 = 0; 序数 < 总长; 序数++) {
        const 行 = Math.floor(序数 / 边长);
        const 列 = 序数 - 行 * 边长;
        矩阵甲[序数] = 行 + 列 + 1;
        矩阵乙[序数] = 行 * 2 + 列 + 1;
    }
    for (let 行 = 0; 行 < 边长; 行++) {
        for (let 列 = 0; 列 < 边长; 列++) {
            let 总和 = 0;
            for (let 中间序数 = 0; 中间序数 < 边长; 中间序数++) {
                总和 += 矩阵甲[行 * 边长 + 中间序数] * 矩阵乙[中间序数 * 边长 + 列];
            }
            矩阵丙[行 * 边长 + 列] = 总和;
        }
    }
    return 矩阵丙[0] + 矩阵丙[总长 - 1];
}

function 交换(数组, 左序, 右序) {
    const 数值 = 数组[左序];
    数组[左序] = 数组[右序];
    数组[右序] = 数值;
}

function 分区(数组, 下界, 上界) {
    const 中点 = 下界 + Math.floor((上界 - 下界) / 2);
    交换(数组, 中点, 上界);
    const 枢轴 = 数组[上界];
    let 存放序数 = 下界;
    for (let 序数 = 下界; 序数 < 上界; 序数++) {
        if (数组[序数] < 枢轴) {
            交换(数组, 序数, 存放序数);
            存放序数++;
        }
    }
    交换(数组, 存放序数, 上界);
    return 存放序数;
}

function 快速排序(数组, 下界, 上界) {
    if (下界 < 上界) {
        const 分区序数 = 分区(数组, 下界, 上界);
        快速排序(数组, 下界, 分区序数 - 1);
        快速排序(数组, 分区序数 + 1, 上界);
    }
}

function 快速排序基准(长度) {
    const 数组 = new Float64Array(长度);
    for (let 序数 = 0; 序数 < 长度; 序数++) {
        数组[序数] = 长度 - 序数;
    }
    快速排序(数组, 0, 长度 - 1);
    return 数组[0] + 数组[Math.floor(长度 / 2)] + 数组[长度 - 1];
}

if (process.argv.length !== 4) {
    console.error(`用法：node ${process.argv[1]} <斐波那契|素数筛|矩阵乘法|快速排序> <规模>`);
    process.exit(1);
}

const 算法 = process.argv[2];
const 规模 = Number.parseInt(process.argv[3], 10);
const 函数表 = {
    斐波那契,
    素数筛,
    矩阵乘法,
    快速排序: 快速排序基准,
};
if (!(算法 in 函数表)) {
    console.error(`未知算法：${算法}`);
    process.exit(1);
}
console.log(函数表[算法](规模));
