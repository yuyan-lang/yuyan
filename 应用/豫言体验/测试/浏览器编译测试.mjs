import { test } from "node:test";
import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import binaryen from "binaryen";
import { 创建编译器 } from "../网页/编译/编译核心.mjs";
import { 创建组装器 } from "../网页/编译/组装.mjs";
import { gunzipSync } from "node:zlib";
import { createHash } from "node:crypto";

// 文言：用真编器验其往复。汉语：测试使用发布所用 WasmGC 编译器及标准库，不以模拟编译结果代替。
const 根 = new URL("../../../", import.meta.url);
const 组装 = 创建组装器(binaryen);
const 编译模块 = await WebAssembly.compile(await readFile(new URL("../网页/编译/资源/编译器.wasm", import.meta.url)));
const 桥模块 = await WebAssembly.compile(await readFile(new URL("../网页/编译/资源/值桥接.wasm", import.meta.url)));
const 资料 = JSON.parse(gunzipSync(await readFile(new URL("../网页/编译/资源/标准库.json.gz", import.meta.url))));
const 清单 = JSON.parse(await readFile(new URL("../网页/编译/资源/清单.json", import.meta.url)));
for (const [名, 项] of Object.entries(清单)) {
  const 字节 = await readFile(new URL("../网页/编译/资源/" + 名, import.meta.url));
  assert.equal(字节.length, 项.bytes); assert.equal(createHash("sha256").update(字节).digest("hex"), 项.sha256);
}
async function 读目录(径) {
  for (const 项 of await readdir(new URL(径, 根), { withFileTypes: true })) {
    const 名 = 径 + 项.name;
    if (项.isDirectory()) await 读目录(名 + "/");
    else if (名.endsWith("。豫")) assert.equal(资料["/" + 名], await readFile(new URL(名, 根), "utf8"), "标准库资源须重新生成：" + 名);
  }
}
await 读目录("库/标准库/");
assert.equal(资料["/工具/网页汇编直生/运行时.wat"], await readFile(new URL("工具/网页汇编直生/运行时.wat", 根), "utf8"));
const 编译 = 创建编译器({ 编译模块, 桥模块, 资料, 组装 });
test("真实 WasmGC 编译及运行你好程序", async () => {
  const 果 = await 编译("寻观「标准库」之书。\n「打印行」于『你好，豫言！』。", false, 事 => { if (process.env.YY_BROWSER_TRACE) console.error(事); });
  assert.equal(果.ok, true, JSON.stringify(果)); assert.equal(果.stdout, "你好，豫言！\n");
});
test("语法错误返回编译诊断", async () => {
  const 果 = await 编译("「坏源码」。"); assert.equal(果.ok, false); assert.equal(果.phase, "compile");
  assert.match(果.error ?? 果.stderr, /坏源码/); assert.doesNotMatch(果.error ?? "", /illegal cast|Maximum call stack/);
});
for (const 名 of await readdir(new URL("../知识/示例/", import.meta.url))) {
  const 源码 = await readFile(new URL("../知识/示例/" + 名, import.meta.url), "utf8");
  test("页面示例：" + 名, async () => {
    const 果 = await 编译(源码);
    assert.equal(果.ok, true, JSON.stringify(果)); assert.ok(果.stdout.length > 0);
    if (名.includes("乘法表")) { assert.equal(果.stdout.trim().split("\n").length, 9); assert.match(果.stdout, /81/); }
  });
}
test("仅编译也完成 WAT 组装，且不运行打印", async () => {
  const 果 = await 编译("寻观「标准库」之书。「打印行」于『不应打印』。", true);
  assert.equal(果.ok, true, JSON.stringify(果)); assert.equal(果.phase, "compile"); assert.equal(果.stdout, "");
});
test("长尾递归经过分支合流后仍使用固定调用栈", async () => {
  const 果 = await 编译("寻观「标准库」之书。「计数」乃化「整数」而「整数」也。「计数」者会「数」而若「等于」于「数」于「零」则「零」否则「计数」于（「减」于「数」于「一」）也。「打印行」于（「整数表示」于（「计数」于「五零零零零」））。");
  assert.equal(果.ok, true, JSON.stringify(果)); assert.equal(果.stdout, "0\n");
});
test("用户实例读不到编译器包上下文", async () => {
  const 果 = await 编译("寻观「标准库」之书。「打印行」于（「文件系统」之「读文件」于『/包上下文』）。");
  assert.equal(果.ok, false); assert.equal(果.phase, "run"); assert.match(果.error, /文件不存在/);
});
