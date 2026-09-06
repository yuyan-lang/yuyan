import { test } from "node:test";
import assert from "node:assert/strict";
import { 读取日志流 } from "../网页/日志流.mjs";
import { 助写 } from "../源码/助写.mjs";
import { 执行受限 } from "../源码/隔离.mjs";
import { 开启事件流 } from "../源码/事件流.mjs";
import { EventEmitter } from "node:events";

test("中文与记录跨字节分块仍能实时解析；最终结果之前已有进度", async () => {
  let 推送;
  const 流 = new ReadableStream({ start(控制) { 推送 = 控制; } });
  const 事件们 = [];
  const 读取 = 读取日志流(new Response(流, { headers: { "Content-Type": "application/x-ndjson" } }), 事 => 事件们.push(事));
  const 字节 = new TextEncoder().encode(JSON.stringify({ type: "stage", label: "正在编译豫言" }) + "\n");
  for (const 字 of 字节) 推送.enqueue(new Uint8Array([字]));
  await new Promise(成 => setImmediate(成));
  assert.equal(事件们[0].label, "正在编译豫言");
  推送.enqueue(new TextEncoder().encode('{"type":"result","data":{"ok":true}}\n')); 推送.close();
  assert.deepEqual(await 读取, { ok: true });
});
test("断流报错并保留已接收日志", async () => {
  const 事件们 = [];
  const 回 = new Response('{"type":"output","text":"已有输出"}\n', { headers: { "Content-Type": "application/x-ndjson" } });
  await assert.rejects(读取日志流(回, 事 => 事件们.push(事)), /连接中断/);
  assert.equal(事件们[0].text, "已有输出");
});
test("六轮修复失败保留全部发送接收、具体诊断与待修复稿", async () => {
  const 事件们 = []; let 次 = 0;
  const 源码 = "「打印行」于『你好』。";
  const 结果 = await 助写({ prompt: "打印你好", code: "" }, "完整语法指南", async 消息 => {
    assert.equal(事件们.at(-1).type, "model_request");
    return ++次 === 1 ? { allow: true } : { refuse: false, code: 源码 };
  }, async (文, 仅编译, 报告) => {
    报告({ type: "output", phase: "compile", stream: "stderr", text: "未定义标识符：打印行" });
    return { ok: false, stderr: "未定义标识符：打印行", stdout: "" };
  }, 事 => 事件们.push(structuredClone(事)));
  assert.equal(事件们.filter(事 => 事.type === "model_request").length, 7);
  assert.equal(事件们.filter(事 => 事.type === "model_response").length, 7);
  const 请求们 = 事件们.filter(事 => 事.type === "model_request");
  assert.equal(请求们[1].data.messages.length, 2);
  assert.equal(请求们[2].data.messages.length, 4);
  assert.match(请求们[2].data.messages.at(-1).content, /未定义标识符/);
  assert.equal(结果.diagnostic, "未定义标识符：打印行");
  assert.equal(结果.draft, 源码); assert.equal(结果.code, undefined);
});
test("子进程完成前即报告输出，中文跨分块无乱码", async () => {
  let 完 = false; const 事件们 = [];
  const 结果 = await 执行受限(process.execPath, ["-e", "const b=Buffer.from('豫言');process.stdout.write(b.subarray(0,2));setTimeout(()=>{process.stdout.write(b.subarray(2));setTimeout(()=>{},100)},30)"], 2000, 65536, 事 => {
    assert.equal(完, false); 事件们.push(事);
  });
  完 = true;
  assert.equal(结果.ok, true);
  assert.equal(事件们.map(事 => 事.text).join(""), "豫言");
});
test("事件流处理慢连接并保留最终结果，每个响应独立", () => {
  const 制回应 = () => Object.assign(new EventEmitter(), { writableLength: 0, text: "", writeHead() {}, flushHeaders() {}, write(文) { this.text += 文; }, end() { this.writableEnded = true; this.emit("close"); } });
  const 甲 = 制回应(), 乙 = 制回应(), 甲流 = 开启事件流(甲), 乙流 = 开启事件流(乙);
  甲流.报告({ type: "stage", label: "甲的请求" });
  甲.writableLength = 2 * 1024 * 1024; 甲流.报告({ type: "output", text: "过慢" });
  甲流.完成({ ok: false }); 乙流.完成({ ok: true });
  assert.match(甲.text, /部分实时日志已省略/); assert.match(甲.text, /"type":"result"/);
  assert.doesNotMatch(乙.text, /甲的请求/);
});
