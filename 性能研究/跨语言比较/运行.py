#!/usr/bin/env python3

import argparse
import io
import json
import os
import platform
import shutil
import statistics
import subprocess
import sys
import tarfile
import time
from dataclasses import dataclass
from pathlib import Path


稳定工具链版本 = "40aa7a16"


@dataclass(frozen=True)
class 算法说明:
    名称: str
    显示名称: str
    规模: int
    期望结果: int


算法列表 = (
    算法说明("斐波那契", "递归斐波那契", 40, 102334155),
    算法说明("素数筛", "埃拉托斯特尼素数筛", 2_000_000, 148933),
    算法说明("矩阵乘法", "整数矩阵乘法", 300, 103320050),
    算法说明("快速排序", "原地快速排序", 1_000_000, 1500002),
)


class 中文参数解析器(argparse.ArgumentParser):
    def format_usage(self):
        return super().format_usage().replace("usage:", "用法：", 1)

    def format_help(self):
        return (
            super()
            .format_help()
            .replace("usage:", "用法：", 1)
            .replace("options:", "选项：", 1)
        )

    def error(self, 消息):
        self.print_usage(sys.stderr)
        self.exit(2, f"{self.prog}：错误：{消息}\n")


def 运行并检查(命令, *, 工作目录=None):
    完成结果 = subprocess.run(
        [str(部分) for 部分 in 命令],
        cwd=工作目录,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    if 完成结果.returncode != 0:
        命令文本 = " ".join(str(部分) for 部分 in 命令)
        raise RuntimeError(f"命令失败（{命令文本}）：\n{完成结果.stdout}")
    return 完成结果.stdout.strip()


def 查找工具(名称, 首选路径=()):
    for 候选路径 in 首选路径:
        路径 = Path(候选路径)
        if 路径.is_file() and os.access(路径, os.X_OK):
            return 路径
    搜索结果 = shutil.which(名称)
    if 搜索结果:
        return Path(搜索结果)
    raise RuntimeError(f"找不到所需工具：{名称}")


def 准备稳定工具链(仓库根目录, 工具链目录):
    标记文件 = 工具链目录 / ".来源版本"
    if 标记文件.is_file() and 标记文件.read_text().strip() == 稳定工具链版本:
        return

    工具链目录.mkdir(parents=True, exist_ok=True)
    归档结果 = subprocess.run(
        [
            "git",
            "archive",
            稳定工具链版本,
            "Makefile",
            "藏书阁",
            "运行时支持库",
            "豫言编译器/编译辅助工具/命令行/版本管理。豫",
        ],
        cwd=仓库根目录,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if 归档结果.returncode != 0:
        raise RuntimeError(归档结果.stderr.decode(errors="replace"))
    with tarfile.open(fileobj=io.BytesIO(归档结果.stdout), mode="r:") as 归档包:
        归档包.extractall(工具链目录, filter="data")
    标记文件.write_text(稳定工具链版本 + "\n")


def 编译程序(仓库根目录, 比较目录, 构建目录):
    构建目录.mkdir(parents=True, exist_ok=True)
    工具链目录 = 构建目录 / f"稳定工具链-{稳定工具链版本}"
    准备稳定工具链(仓库根目录, 工具链目录)

    稳定编译器 = 仓库根目录 / "yy_bs_stable"
    if not 稳定编译器.is_file():
        raise RuntimeError(f"找不到稳定编译器：{稳定编译器}")

    C编译器 = 查找工具(
        "clang",
        ("/opt/homebrew/opt/llvm/bin/clang", "/usr/local/opt/llvm/bin/clang"),
    )
    OCaml编译器 = 查找工具("ocamlopt")
    Java编译器 = 查找工具(
        "javac",
        ("/opt/homebrew/opt/openjdk/bin/javac", "/usr/local/opt/openjdk/bin/javac"),
    )
    Java运行器 = 查找工具(
        "java",
        ("/opt/homebrew/opt/openjdk/bin/java", "/usr/local/opt/openjdk/bin/java"),
    )
    Python解释器 = 查找工具("python3")
    Node运行器 = 查找工具("node")

    print("编译 C、OCaml、Java……", flush=True)
    运行并检查(
        [C编译器, "-O3", "-DNDEBUG", 比较目录 / "性能基准.c", "-o", 构建目录 / "性能基准-C"]
    )
    shutil.copy2(比较目录 / "性能基准.ml", 构建目录 / "性能基准.ml")
    运行并检查(
        [OCaml编译器, "-O3", "-o", "性能基准-OCaml", "性能基准.ml"],
        工作目录=构建目录,
    )
    运行并检查([Java编译器, "-d", 构建目录, 比较目录 / "性能基准.java"])

    豫言程序 = {}
    print("用 yy_bs_stable 编译四个豫言基准……", flush=True)
    for 算法 in 算法列表:
        输出路径 = 构建目录 / f"性能基准-豫言-{算法.名称}"
        运行并检查(
            [
                稳定编译器,
                "-c",
                "--optimize",
                "--whole-program-opt",
                "--do-not-load-cache",
                "--do-not-save-cache",
                "--library-root",
                "藏书阁",
                比较目录 / f"{算法.名称}。豫",
                "-o",
                输出路径,
                "--no-debug-print",
            ],
            工作目录=工具链目录,
        )
        豫言程序[算法.名称] = [str(输出路径)]

    通用命令 = {
        "C": [str(构建目录 / "性能基准-C")],
        "Python": [str(Python解释器), str(比较目录 / "性能基准.py")],
        "OCaml": [str(构建目录 / "性能基准-OCaml")],
        "Java": [str(Java运行器), "-cp", str(构建目录), "性能基准"],
        "JavaScript": [str(Node运行器), str(比较目录 / "性能基准.js")],
    }
    命令表 = {语言: {} for 语言 in ("豫言", *通用命令.keys())}
    for 算法 in 算法列表:
        命令表["豫言"][算法.名称] = 豫言程序[算法.名称] + [str(算法.规模)]
        for 语言, 命令前缀 in 通用命令.items():
            命令表[语言][算法.名称] = 命令前缀 + [算法.名称, str(算法.规模)]

    版本表 = {
        "豫言": 运行并检查([稳定编译器, "--version"]).splitlines()[0],
        "C": 运行并检查([C编译器, "--version"]).splitlines()[0],
        "Python": 运行并检查([Python解释器, "--version"]).splitlines()[0],
        "OCaml": "OCaml " + 运行并检查([OCaml编译器, "-version"]).splitlines()[0],
        "Java": 运行并检查([Java运行器, "-version"]).splitlines()[0],
        "JavaScript": "Node.js " + 运行并检查([Node运行器, "--version"]).splitlines()[0],
    }
    return 命令表, 版本表


def 执行一次(命令, 期望结果):
    开始时间 = time.perf_counter_ns()
    完成结果 = subprocess.run(
        命令,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    耗时 = (time.perf_counter_ns() - 开始时间) / 1_000_000_000
    if 完成结果.returncode != 0:
        raise RuntimeError(
            f"程序失败（{' '.join(命令)}）：\n{完成结果.stdout}{完成结果.stderr}"
        )
    输出文本 = 完成结果.stdout.strip()
    if 输出文本 != str(期望结果):
        raise RuntimeError(
            f"结果校验失败（{' '.join(命令)}）：期望 {期望结果}，实际 {输出文本!r}"
        )
    return 耗时


def 测量性能(命令表, 测量次数, 预热次数):
    语言列表 = tuple(命令表)
    计时数据 = {算法.名称: {语言: [] for 语言 in 语言列表} for 算法 in 算法列表}
    for 算法 in 算法列表:
        print(f"\n{算法.显示名称}（输入 {算法.规模}）", flush=True)
        for 语言 in 语言列表:
            for _ in range(预热次数):
                执行一次(命令表[语言][算法.名称], 算法.期望结果)
        for 轮次 in range(测量次数):
            运行顺序 = 语言列表[轮次 % len(语言列表) :] + 语言列表[: 轮次 % len(语言列表)]
            for 语言 in 运行顺序:
                耗时 = 执行一次(命令表[语言][算法.名称], 算法.期望结果)
                计时数据[算法.名称][语言].append(耗时)
                print(f"  第 {轮次 + 1}/{测量次数} 轮 {语言:<10} {耗时:.6f} 秒", flush=True)
    return 计时数据


def 获取机器描述():
    描述片段 = [platform.platform(), platform.machine()]
    系统查询工具 = shutil.which("sysctl")
    if 系统查询工具:
        for 查询项 in ("machdep.cpu.brand_string", "hw.model"):
            完成结果 = subprocess.run(
                [系统查询工具, "-n", 查询项], text=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL
            )
            查询结果 = 完成结果.stdout.strip()
            if 完成结果.returncode == 0 and 查询结果:
                描述片段.append(查询结果)
                break
    return " / ".join(dict.fromkeys(描述片段))


def 汇总结果(计时数据):
    汇总 = {}
    for 算法 in 算法列表:
        各语言数据 = {}
        for 语言, 样本 in 计时数据[算法.名称].items():
            各语言数据[语言] = {
                "中位数": statistics.median(样本),
                "平均数": statistics.mean(样本),
                "最小值": min(样本),
                "最大值": max(样本),
                "样本": 样本,
            }
        C中位数 = 各语言数据["C"]["中位数"]
        for 数据 in 各语言数据.values():
            数据["相对C"] = 数据["中位数"] / C中位数
        汇总[算法.名称] = 各语言数据
    return 汇总


def 生成报告(汇总, 版本表, 测量次数, 预热次数, 机器):
    行列表 = [
        "# 豫言跨语言性能比较",
        "",
        f"- 机器：{机器}",
        f"- 每项：{预热次数} 次预热，{测量次数} 次独立进程测量，表中使用中位数",
        "- 编译时间不计入；进程启动与运行时启动计入",
        f"- 豫言：只使用 `yy_bs_stable`，配套工具链快照 `{稳定工具链版本}`，开启优化与全程序优化",
        "- 所有实现均先校验整数结果完全一致",
        "",
        "## 工具版本",
        "",
    ]
    for 语言, 版本 in 版本表.items():
        行列表.append(f"- {语言}：{版本}")
    for 算法 in 算法列表:
        行列表.extend(
            [
                "",
                f"## {算法.显示名称}",
                "",
                f"输入：`{算法.规模}`；校验结果：`{算法.期望结果}`。",
                "",
                "| 语言 | 中位数（秒） | 最快 | 最慢 | 相对 C |",
                "|---|---:|---:|---:|---:|",
            ]
        )
        排序后数据 = sorted(汇总[算法.名称].items(), key=lambda 项: 项[1]["中位数"])
        for 语言, 数据 in 排序后数据:
            行列表.append(
                f"| {语言} | {数据['中位数']:.6f} | {数据['最小值']:.6f} | "
                f"{数据['最大值']:.6f} | {数据['相对C']:.2f}× |"
            )
    行列表.extend(
        [
            "",
            "## 解读限制",
            "",
            "这是四个确定性算法的端到端墙钟时间，不是语言的单一总分。不同语言的数组表示、垃圾回收、即时编译与进程启动策略不同；结果适合观察当前实现的性能特征，不宜外推到所有工作负载。",
            "",
        ]
    )
    return "\n".join(行列表)


def 主程序():
    参数解析器 = 中文参数解析器(
        description="比较豫言、C、Python、OCaml、Java 和 JavaScript",
        add_help=False,
    )
    参数解析器.add_argument("-h", "--帮助", action="help", help="显示此帮助信息并退出")
    参数解析器.add_argument("--次数", type=int, default=5, help="每项正式测量次数（默认：5）")
    参数解析器.add_argument("--预热次数", type=int, default=1, help="每项预热次数（默认：1）")
    参数 = 参数解析器.parse_args()
    if 参数.次数 < 1 or 参数.预热次数 < 0:
        参数解析器.error("--次数 必须大于零，--预热次数 不能小于零")

    比较目录 = Path(__file__).resolve().parent
    仓库根目录 = 比较目录.parents[1]
    构建目录 = 仓库根目录 / ".yybuild.nosync" / "性能比较"
    命令表, 版本表 = 编译程序(仓库根目录, 比较目录, 构建目录)
    计时数据 = 测量性能(命令表, 参数.次数, 参数.预热次数)
    汇总 = 汇总结果(计时数据)
    机器 = 获取机器描述()

    JSON内容 = {
        "机器": 机器,
        "测量次数": 参数.次数,
        "预热次数": 参数.预热次数,
        "稳定工具链版本": 稳定工具链版本,
        "工具版本": 版本表,
        "算法": {算法.名称: 算法.__dict__ for 算法 in 算法列表},
        "结果": 汇总,
    }
    (比较目录 / "结果.json").write_text(
        json.dumps(JSON内容, ensure_ascii=False, indent=2) + "\n"
    )
    报告文本 = 生成报告(汇总, 版本表, 参数.次数, 参数.预热次数, 机器)
    (比较目录 / "结果.md").write_text(报告文本)
    print("\n" + 报告文本)


if __name__ == "__main__":
    主程序()
