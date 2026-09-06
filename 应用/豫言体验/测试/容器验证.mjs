import { test } from "node:test";
import assert from "node:assert/strict";
import { createServer } from "node:http";
import { mkdtemp, rm, readFile } from "node:fs/promises";
import { 编译运行, 隔离参数, 执行受限 } from "../源码/隔离.mjs";

// 古曰：恶客之试止于器中，不及宿主。
// 今释：这些测试只允许在显式指定的 Linux 测试容器中运行。
const 跳过 = process.platform !== "linux" || process.env.YY_SANDBOX_TEST !== "1";
test("隔离空间看不到服务源码、父进程环境或本地模型服务", { skip: 跳过 }, async () => {
  const 服务 = createServer((求, 应) => 应.end("不应被用户程序读到"));
  await new Promise(成 => 服务.listen(18081, "127.0.0.1", 成));
  const 目录 = await mkdtemp("/tmp/yy-boundary-");
  try {
    const 结果 = await 执行受限("/usr/bin/prlimit", 隔离参数(目录, "/bin/bash", ["-c",
      'test ! -e /app/源码/服务.mjs && test ! -e /proc/1/environ && test -z "$YY_TEST_SECRET" && ! (echo x > /dev/tcp/127.0.0.1/18081)'
    ], false), 5000);
    assert.equal(结果.ok, true, JSON.stringify(结果));
  } finally { 服务.close(); await rm(目录, { recursive: true, force: true }); }
});
test("真实编译器在隔离空间输出你好及算术结果", { skip: 跳过 }, async () => {
  for (const [源码, 期望] of [
    ["寻观「标准库」之书。\n「打印行」于『你好，豫言！』。", "你好，豫言！"],
    ["寻观「标准库」之书。\n「答案」者「加」于「三」于「五」也。\n「打印行」于（「整数表示」于「答案」）。", "8"]
  ]) {
    const 结果 = await 编译运行(源码);
    assert.equal(结果.ok, true, JSON.stringify(结果));
    assert.equal(结果.stdout.trim(), 期望);
  }
});
test("非法源码返回编译失败", { skip: 跳过 }, async () => {
  const 结果 = await 编译运行("此非合法豫言程序");
  assert.equal(结果.ok, false); assert.equal(结果.phase, "compile");
});

// 古曰：所授之例，皆以真炉验之。今释：提示中的每个示例都经过 Linux 隔离编译与运行。
test("助写示例全部通过真实编译运行", { skip: 跳过 }, async () => {
  for (const [名, 期望] of [["函数与分支", "3"], ["列表与模式匹配", "6"], ["递归与求和", "55"], ["嵌套递归与乘法表", "9×9=81"]]) {
    const 源码 = await readFile(`/app/知识/示例/${名}。豫`, "utf8");
    const 结果 = await 编译运行(源码);
    assert.equal(结果.ok, true, JSON.stringify(结果));
    assert.ok(结果.stdout.trim().endsWith(期望), 结果.stdout);
  }
});
