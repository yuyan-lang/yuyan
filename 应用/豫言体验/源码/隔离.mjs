import { spawn } from "node:child_process";
import { mkdtemp, mkdir, writeFile, rm, cp } from "node:fs/promises";
import { join } from "node:path";
import { StringDecoder } from "node:string_decoder";

// 古曰：编与行皆入别室；无网，无钥，无前客之迹。
// 今释：Bubblewrap 同时隔离编译期和执行期的网络、PID、文件系统与环境。
// 隔离失败直接报错，绝不退回宿主执行。进程组超时/超输出后整体终止。
export function 执行受限(命令, 参数, 时限, 输出上限 = 65536, 报告 = () => {}) {
  return new Promise((完成, 拒绝) => {
    const 子 = spawn(命令, 参数, { detached: true, stdio: ["ignore", "pipe", "pipe"],
      env: { PATH: "/usr/bin:/bin", LANG: "C.UTF-8" } });
    const 输出 = [], 错误 = []; let 总长 = 0, 原因 = "";
    const 杀止 = () => { try { process.kill(-子.pid, "SIGKILL"); } catch {} };
    const 计时 = setTimeout(() => { 原因 = "超过运行时限"; 杀止(); }, 时限);
    const 解码们 = { stdout: new StringDecoder("utf8"), stderr: new StringDecoder("utf8") };
    const 收 = (目标, 流, 块) => {
      const 余量 = Math.max(0, 输出上限 - 总长);
      总长 += 块.length;
      if (余量) {
        const 保留 = 块.subarray(0, 余量); 目标.push(保留);
        const 文 = 解码们[流].write(保留);
        if (文) 报告({ type: "output", stream: 流, text: 文 });
      }
      if (总长 > 输出上限) { 原因 = "输出超过 64 KB"; 杀止(); }
    };
    子.stdout.on("data", 块 => 收(输出, "stdout", 块)); 子.stderr.on("data", 块 => 收(错误, "stderr", 块));
    子.once("error", 错 => { clearTimeout(计时); 拒绝(错); });
    子.once("close", (码, 信号) => {
      clearTimeout(计时); 杀止();
      for (const [流, 解码] of Object.entries(解码们)) {
        const 文 = 解码.end(); if (文) 报告({ type: "output", stream: 流, text: 文 });
      }
      完成({ ok: 码 === 0 && !原因, stdout: Buffer.concat(输出).toString("utf8"),
        stderr: Buffer.concat(错误).toString("utf8"), exitCode: 码, signal: 信号, error: 原因 || undefined });
    });
  });
}

export function 隔离参数(目录, 命令, 参数, 编译) {
  return ["--as=2147483648", `--cpu=${编译 ? 20 : 3}`, "--nproc=32", "--nofile=64", "--fsize=33554432", "--core=0", "--",
    "/usr/bin/bwrap", "--unshare-all", "--die-with-parent", "--new-session", "--cap-drop", "ALL", "--clearenv",
    "--ro-bind", "/usr", "/usr", "--symlink", "usr/bin", "/bin", "--symlink", "usr/lib", "/lib", "--symlink", "usr/lib64", "/lib64",
    "--dir", "/proc", "--dev", "/dev", "--tmpfs", "/tmp", "--dir", "/work",
    "--ro-bind", "/opt/yuyan", "/opt/yuyan", "--bind", 目录, "/work",
    "--ro-bind", "/opt/yuyan/运行时支持库", "/work/运行时支持库",
    "--ro-bind", "/opt/yuyan/库", "/work/库",
    "--ro-bind", "/opt/yuyan/yy", "/work/yy",
    "--setenv", "PATH", "/usr/bin:/bin", "--setenv", "LANG", "C.UTF-8", "--setenv", "HOME", "/tmp",
    "--setenv", "YY_GC_INITIAL_STACK_SIZE_MB", "2", "--setenv", "YY_GC_INITIAL_HEAP_SIZE_MB", "4",
    "--setenv", "YY_GC_MAX_HEAP_SIZE_MB", "256", "--chdir", "/work", "--", 命令, ...参数];
}

export async function 编译运行(源码, 仅编译 = false, 报告 = () => {}) {
  const 目录 = await mkdtemp("/tmp/yy-job-");
  try {
    报告({ type: "stage", phase: "compile", label: "准备隔离空间与标准库缓存" });
    // 古曰：官书先备，客稿各藏；不取前客之余墨。
    // 今释：只复制镜像构建时的可信标准库缓存，每个请求独占副本，避免重新编译标准库与跨访客污染。
    await cp("/opt/yuyan/预热缓存", join(目录, ".yybuild"), { recursive: true, preserveTimestamps: true });
    // 古曰：客稿亦有籍，与诸包同制。今释：每次请求创建独立软件包，入口与依赖明确声明。
    await mkdir(join(目录, "用户程序"), { recursive: true });
    await writeFile(join(目录, "用户程序/用户程序。包。豫"), "「依赖」者「列」【『标准库』】也。「入口」者『入口』也。", { mode: 0o600 });
    await writeFile(join(目录, "用户程序/入口。豫"), 源码, { mode: 0o600 });
    报告({ type: "stage", phase: "compile", label: "正在编译（最多 30 秒）" });
    const 编译 = await 执行受限("/usr/bin/prlimit", 隔离参数(目录, "/opt/yuyan/yy豫构", [
      "构建", "用户程序", "--编译器", "/work/yy", "--输出", "/work/yy程序", "-j", "1", "--", "--compile-only", "--no-debug-print"
    ], true), 30000, 65536, 事件 => 报告({ ...事件, phase: "compile" }));
    if (!编译.ok) return { ...编译, phase: "compile" };
    if (仅编译) return { ...编译, phase: "compile" };
    报告({ type: "stage", phase: "run", label: "编译通过，正在运行（最多 5 秒）" });
    return { ...await 执行受限("/usr/bin/prlimit", 隔离参数(目录, "/work/yy程序", [], false), 5000, 65536,
      事件 => 报告({ ...事件, phase: "run" })), phase: "run", compilation: 编译 };
  } finally { await rm(目录, { recursive: true, force: true }); }
}

export async function 验隔离() {
  const 目录 = await mkdtemp("/tmp/yy-check-");
  try {
    const 结果 = await 执行受限("/usr/bin/prlimit", 隔离参数(目录, "/usr/bin/true", [], false), 5000);
    if (!结果.ok) throw new Error(`无法建立隔离空间：${结果.stderr}`);
  } finally { await rm(目录, { recursive: true, force: true }); }
}
