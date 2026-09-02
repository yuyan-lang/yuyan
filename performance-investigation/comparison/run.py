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


STABLE_TOOLCHAIN_REF = "40aa7a16"


@dataclass(frozen=True)
class Algorithm:
    name: str
    display_name: str
    size: int
    expected: int


ALGORITHMS = (
    Algorithm("fib", "递归 Fibonacci", 40, 102334155),
    Algorithm("sieve", "Eratosthenes 素数筛", 2_000_000, 148933),
    Algorithm("matrix", "整数矩阵乘法", 300, 103320050),
    Algorithm("quicksort", "原地快速排序", 1_000_000, 1500002),
)


def run_checked(command, *, cwd=None):
    completed = subprocess.run(
        [str(part) for part in command],
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    if completed.returncode != 0:
        rendered = " ".join(str(part) for part in command)
        raise RuntimeError(f"命令失败（{rendered}）：\n{completed.stdout}")
    return completed.stdout.strip()


def find_tool(name, preferred=()):
    for candidate in preferred:
        path = Path(candidate)
        if path.is_file() and os.access(path, os.X_OK):
            return path
    found = shutil.which(name)
    if found:
        return Path(found)
    raise RuntimeError(f"找不到所需工具：{name}")


def prepare_stable_toolchain(root, toolchain_dir):
    marker = toolchain_dir / ".source-ref"
    if marker.is_file() and marker.read_text().strip() == STABLE_TOOLCHAIN_REF:
        return

    toolchain_dir.mkdir(parents=True, exist_ok=True)
    archive = subprocess.run(
        [
            "git",
            "archive",
            STABLE_TOOLCHAIN_REF,
            "Makefile",
            "藏书阁",
            "运行时支持库",
            "豫言编译器/编译辅助工具/命令行/版本管理。豫",
        ],
        cwd=root,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if archive.returncode != 0:
        raise RuntimeError(archive.stderr.decode(errors="replace"))
    with tarfile.open(fileobj=io.BytesIO(archive.stdout), mode="r:") as bundle:
        bundle.extractall(toolchain_dir, filter="data")
    marker.write_text(STABLE_TOOLCHAIN_REF + "\n")


def build_programs(root, comparison_dir, build_dir):
    build_dir.mkdir(parents=True, exist_ok=True)
    toolchain_dir = build_dir / f"stable-toolchain-{STABLE_TOOLCHAIN_REF}"
    prepare_stable_toolchain(root, toolchain_dir)

    stable_compiler = root / "yy_bs_stable"
    if not stable_compiler.is_file():
        raise RuntimeError(f"找不到稳定编译器：{stable_compiler}")

    clang = find_tool(
        "clang",
        ("/opt/homebrew/opt/llvm/bin/clang", "/usr/local/opt/llvm/bin/clang"),
    )
    ocamlopt = find_tool("ocamlopt")
    javac = find_tool(
        "javac",
        ("/opt/homebrew/opt/openjdk/bin/javac", "/usr/local/opt/openjdk/bin/javac"),
    )
    java = find_tool(
        "java",
        ("/opt/homebrew/opt/openjdk/bin/java", "/usr/local/opt/openjdk/bin/java"),
    )
    python = find_tool("python3")
    node = find_tool("node")

    print("编译 C、OCaml、Java……", flush=True)
    run_checked(
        [clang, "-O3", "-DNDEBUG", comparison_dir / "benchmark.c", "-o", build_dir / "benchmark-c"]
    )
    shutil.copy2(comparison_dir / "benchmark.ml", build_dir / "benchmark.ml")
    run_checked(
        [ocamlopt, "-O3", "-o", "benchmark-ocaml", "benchmark.ml"],
        cwd=build_dir,
    )
    run_checked([javac, "-d", build_dir, comparison_dir / "Benchmark.java"])

    yuyan_programs = {}
    print("用 yy_bs_stable 编译四个豫言基准……", flush=True)
    for algorithm in ALGORITHMS:
        output = build_dir / f"benchmark-yuyan-{algorithm.name}"
        run_checked(
            [
                stable_compiler,
                "-c",
                "--optimize",
                "--whole-program-opt",
                "--do-not-load-cache",
                "--do-not-save-cache",
                "--library-root",
                "藏书阁",
                comparison_dir / f"{algorithm.name}.yuyan",
                "-o",
                output,
                "--no-debug-print",
            ],
            cwd=toolchain_dir,
        )
        yuyan_programs[algorithm.name] = [str(output)]

    common = {
        "C": [str(build_dir / "benchmark-c")],
        "Python": [str(python), str(comparison_dir / "benchmark.py")],
        "OCaml": [str(build_dir / "benchmark-ocaml")],
        "Java": [str(java), "-cp", str(build_dir), "Benchmark"],
        "JavaScript": [str(node), str(comparison_dir / "benchmark.js")],
    }
    commands = {language: {} for language in ("豫言", *common.keys())}
    for algorithm in ALGORITHMS:
        commands["豫言"][algorithm.name] = yuyan_programs[algorithm.name] + [str(algorithm.size)]
        for language, prefix in common.items():
            commands[language][algorithm.name] = prefix + [algorithm.name, str(algorithm.size)]

    versions = {
        "豫言": run_checked([stable_compiler, "--version"]).splitlines()[0],
        "C": run_checked([clang, "--version"]).splitlines()[0],
        "Python": run_checked([python, "--version"]).splitlines()[0],
        "OCaml": "OCaml " + run_checked([ocamlopt, "-version"]).splitlines()[0],
        "Java": run_checked([java, "-version"]).splitlines()[0],
        "JavaScript": "Node.js " + run_checked([node, "--version"]).splitlines()[0],
    }
    return commands, versions


def execute(command, expected):
    started = time.perf_counter_ns()
    completed = subprocess.run(
        command,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    elapsed = (time.perf_counter_ns() - started) / 1_000_000_000
    if completed.returncode != 0:
        raise RuntimeError(
            f"程序失败（{' '.join(command)}）：\n{completed.stdout}{completed.stderr}"
        )
    output = completed.stdout.strip()
    if output != str(expected):
        raise RuntimeError(
            f"结果校验失败（{' '.join(command)}）：期望 {expected}，实际 {output!r}"
        )
    return elapsed


def benchmark(commands, runs, warmups):
    languages = tuple(commands)
    timings = {algorithm.name: {language: [] for language in languages} for algorithm in ALGORITHMS}
    for algorithm in ALGORITHMS:
        print(f"\n{algorithm.display_name}（输入 {algorithm.size}）", flush=True)
        for language in languages:
            for _ in range(warmups):
                execute(commands[language][algorithm.name], algorithm.expected)
        for round_index in range(runs):
            order = languages[round_index % len(languages) :] + languages[: round_index % len(languages)]
            for language in order:
                elapsed = execute(commands[language][algorithm.name], algorithm.expected)
                timings[algorithm.name][language].append(elapsed)
                print(f"  第 {round_index + 1}/{runs} 轮 {language:<10} {elapsed:.6f} s", flush=True)
    return timings


def machine_description():
    parts = [platform.platform(), platform.machine()]
    sysctl = shutil.which("sysctl")
    if sysctl:
        for key in ("machdep.cpu.brand_string", "hw.model"):
            completed = subprocess.run(
                [sysctl, "-n", key], text=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL
            )
            value = completed.stdout.strip()
            if completed.returncode == 0 and value:
                parts.append(value)
                break
    return " / ".join(dict.fromkeys(parts))


def summarize(timings):
    summary = {}
    for algorithm in ALGORITHMS:
        rows = {}
        for language, values in timings[algorithm.name].items():
            rows[language] = {
                "median": statistics.median(values),
                "mean": statistics.mean(values),
                "min": min(values),
                "max": max(values),
                "samples": values,
            }
        c_median = rows["C"]["median"]
        for row in rows.values():
            row["relative_to_c"] = row["median"] / c_median
        summary[algorithm.name] = rows
    return summary


def render_markdown(summary, versions, runs, warmups, machine):
    lines = [
        "# 豫言跨语言性能比较",
        "",
        f"- 机器：{machine}",
        f"- 每项：{warmups} 次预热，{runs} 次独立进程测量，表中使用中位数",
        "- 编译时间不计入；进程启动与运行时启动计入",
        f"- 豫言：只使用 `yy_bs_stable`，配套工具链快照 `{STABLE_TOOLCHAIN_REF}`，开启 `--optimize --whole-program-opt`",
        "- 所有实现均先校验整数结果完全一致",
        "",
        "## 工具版本",
        "",
    ]
    for language, version in versions.items():
        lines.append(f"- {language}: {version}")
    for algorithm in ALGORITHMS:
        lines.extend(
            [
                "",
                f"## {algorithm.display_name}",
                "",
                f"输入：`{algorithm.size}`；校验结果：`{algorithm.expected}`。",
                "",
                "| 语言 | 中位数（秒） | 最快 | 最慢 | 相对 C |",
                "|---|---:|---:|---:|---:|",
            ]
        )
        ordered = sorted(summary[algorithm.name].items(), key=lambda item: item[1]["median"])
        for language, row in ordered:
            lines.append(
                f"| {language} | {row['median']:.6f} | {row['min']:.6f} | "
                f"{row['max']:.6f} | {row['relative_to_c']:.2f}× |"
            )
    lines.extend(
        [
            "",
            "## 解读限制",
            "",
            "这是四个确定性算法的端到端墙钟时间，不是语言的单一总分。不同语言的数组表示、垃圾回收、JIT 与进程启动策略不同；结果适合观察当前实现的性能特征，不宜外推到所有工作负载。",
            "",
        ]
    )
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(description="比较豫言、C、Python、OCaml、Java 和 JavaScript")
    parser.add_argument("--runs", type=int, default=5, help="每项正式测量次数（默认：5）")
    parser.add_argument("--warmups", type=int, default=1, help="每项预热次数（默认：1）")
    args = parser.parse_args()
    if args.runs < 1 or args.warmups < 0:
        parser.error("--runs 必须大于零，--warmups 不能小于零")

    comparison_dir = Path(__file__).resolve().parent
    root = comparison_dir.parents[1]
    build_dir = root / ".yybuild.nosync" / "performance-comparison"
    commands, versions = build_programs(root, comparison_dir, build_dir)
    timings = benchmark(commands, args.runs, args.warmups)
    summary = summarize(timings)
    machine = machine_description()

    payload = {
        "machine": machine,
        "runs": args.runs,
        "warmups": args.warmups,
        "stable_toolchain_ref": STABLE_TOOLCHAIN_REF,
        "versions": versions,
        "algorithms": {algorithm.name: algorithm.__dict__ for algorithm in ALGORITHMS},
        "results": summary,
    }
    (comparison_dir / "results.json").write_text(
        json.dumps(payload, ensure_ascii=False, indent=2) + "\n"
    )
    markdown = render_markdown(summary, versions, args.runs, args.warmups, machine)
    (comparison_dir / "results.md").write_text(markdown)
    print("\n" + markdown)


if __name__ == "__main__":
    main()
