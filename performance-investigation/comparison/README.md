# 豫言跨语言性能比较

这个目录用四种不同算法比较豫言、C、Python、OCaml、Java 和 JavaScript：

- 递归 Fibonacci：递归调用与整数运算
- Eratosthenes 素数筛：数组访问、分支与循环
- 整数矩阵乘法：密集算术与内存访问
- 原地快速排序：递归、交换与不规则分支

运行：

```sh
python3 performance-investigation/comparison/run.py
```

默认每项预热 1 次、正式测量 5 次。可用 `--warmups` 和 `--runs` 修改次数。脚本会：

1. 从提交 `40aa7a16` 提取与 `yy_bs_stable` 配套的标准库和运行时到 `.yybuild.nosync`。
2. 只用 `./yy_bs_stable` 编译豫言实现，并开启 `--optimize --whole-program-opt`。
3. 用 `-O3` 编译 C 和 OCaml，编译 Java，并使用本机 Python 与 Node.js。
4. 在计时前校验六种实现的结果完全一致。
5. 生成 `results.md` 和包含原始样本的 `results.json`。

编译时间不计入，进程启动时间计入。每次正式测量都启动新进程，因此 Java 和 JavaScript 的结果包含单次运行中的 JIT 预热成本。该套件反映这些具体算法与实现，不应被解释成语言的单一总分。
