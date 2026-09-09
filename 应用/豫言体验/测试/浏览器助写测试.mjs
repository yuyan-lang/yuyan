import { test } from "node:test";
import assert from "node:assert/strict";
import { 建立助写会话, 验浏览器结果 } from "../源码/浏览器助写.mjs";

// 文言：往复有凭，乱序与越界皆拒。汉语：验证真实助写循环和浏览器编译回传协议，不调用付费模型。
class 测试连接 extends EventTarget {
  constructor() { super(); this.消息 = []; this.结束 = new Promise(成 => { this.成 = 成; }); }
  收(值) { this.dispatchEvent(new MessageEvent("message", { data: JSON.stringify(值) })); }
  send(文) { const 事 = JSON.parse(文); this.消息.push(事); queueMicrotask(() => this.发送后?.(事)); }
  close() { this.成(); }
}
const 源码 = "寻观「标准库」之书。「打印行」于『你好』。";
test("AI 的最终稿在浏览器完整编译后才返回，额度只扣一次", async () => {
  const 连接 = new 测试连接(); let 次 = 0, 扣次 = 0;
  连接.发送后 = 事 => {
    if (事.type === "compile_request") {
      assert.equal(事.code, 源码); assert.equal(事.compileOnly, true);
      连接.收({ type: "compile_result", id: 事.id, data: { ok: true, phase: "compile", exitCode: 0 } });
    }
  };
  建立助写会话(连接, { 知识: "指南", 扣额度: async () => { 扣次++; },
    调模型: async () => ++次 === 1 ? { allow: true } : { refuse: false, code: 源码 } });
  连接.收({ type: "start", data: { prompt: "打印你好", code: "" } });
  await 连接.结束;
  assert.equal(扣次, 1); assert.equal(次, 2);
  assert.deepEqual(连接.消息.at(-1).data, { code: 源码, verified: true });
});
test("过期或伪造的编译请求编号终止会话", async () => {
  const 连接 = new 测试连接(); let 次 = 0;
  连接.发送后 = 事 => { if (事.type === "compile_request") 连接.收({ type: "compile_result", id: 事.id + 1, data: { ok: true, phase: "compile" } }); };
  建立助写会话(连接, { 知识: "指南", 扣额度: async () => {}, 调模型: async () => ++次 === 1 ? { allow: true } : { refuse: false, code: 源码 } });
  连接.收({ type: "start", data: { prompt: "打印你好", code: "" } });
  await 连接.结束; assert.match(连接.消息.at(-1).data.error, /不属于当前请求/);
});
test("非法初始请求不能提交自定义模型消息", async () => {
  const 连接 = new 测试连接();
  建立助写会话(连接, { 知识: "指南", 扣额度: async () => assert.fail(), 调模型: async () => assert.fail() });
  连接.收({ type: "start", data: { prompt: "打印你好", code: "", messages: [] } });
  await 连接.结束; assert.match(连接.消息.at(-1).data.error, /字段/);
});
test("回传结果限长且只传递诊断字段", () => {
  assert.throws(() => 验浏览器结果({ ok: true, phase: "run", stdout: "x".repeat(65537) }));
  assert.throws(() => 验浏览器结果({ ok: "true", phase: "run" }));
  assert.equal(验浏览器结果({ ok: true, phase: "run", messages: ["注入"] }).messages, undefined);
});
