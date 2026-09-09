import { test } from "node:test";
import assert from "node:assert/strict";
import { Worker as 节点线程 } from "node:worker_threads";

// 文言：以真线程验可止之器。汉语：仅将浏览器的消息接口和静态资源读取适配到 Node；工作线程、客户端、编译器与组装器均使用实际发布代码和默认栈。
const 线程集 = new Set();
class 浏览器线程 {
  constructor(入口) {
    const 源 = `import { parentPort } from 'node:worker_threads';
import { readFile } from 'node:fs/promises';
globalThis.self = globalThis;
globalThis.postMessage = data => parentPort.postMessage(data);
globalThis.fetch = async url => new Response(await readFile(url));
await import(${JSON.stringify(入口.href)});
parentPort.on('message', data => self.onmessage({ data }));`;
    this.线程 = new 节点线程(new URL("data:text/javascript," + encodeURIComponent(源)), { execArgv: [] });
    线程集.add(this.线程); this.线程.on("exit", () => 线程集.delete(this.线程));
    this.线程.on("message", data => this.onmessage?.({ data }));
    this.线程.on("error", 错 => this.onerror?.(错));
  }
  postMessage(data) { this.线程.postMessage(data); }
  terminate() { return this.线程.terminate(); }
}
test("发布线程能加载资源；主窗可停止死循环且超时后可以重新运行", { timeout: 45000 }, async t => {
  globalThis.Worker = 浏览器线程;
  const { 浏览器编译, 停止编译 } = await import("../网页/编译/客户端.mjs");
  t.after(async () => { 停止编译(); await Promise.all([...线程集].map(线程 => 线程.terminate())); delete globalThis.Worker; });
  const 正常 = "寻观「标准库」之书。「打印行」于『线程成功』。";
  const 死循环 = "寻观「标准库」之书。「循环」乃化「整数」而「整数」也。「循环」者会「数」而「循环」于「数」也。「循环」于「零」。";
  let 果 = await 浏览器编译(正常); assert.equal(果.ok, true, JSON.stringify(果)); assert.equal(果.stdout, "线程成功\n");
  果 = await 浏览器编译(死循环, false, 事 => { if (事.type === "stage" && 事.phase === "run") 停止编译(); });
  assert.equal(果.ok, false); assert.equal(果.error, "已停止");
  果 = await 浏览器编译(死循环); assert.equal(果.ok, false); assert.match(果.error, /五秒时限/);
  果 = await 浏览器编译(正常); assert.equal(果.ok, true, JSON.stringify(果)); assert.equal(果.stdout, "线程成功\n");
});
