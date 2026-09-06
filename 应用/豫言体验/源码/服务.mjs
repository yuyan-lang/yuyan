import { createServer } from "node:http";
import { readFile, readdir } from "node:fs/promises";
import { 验请求, 请求上限 } from "./约束.mjs";
import { 编译运行, 验隔离 } from "./隔离.mjs";
import { 助写 } from "./助写.mjs";
import { 开启事件流 } from "./事件流.mjs";
import { 构建知识 } from "./知识.mjs";

// 古曰：先验其室，而后纳客；一事既毕，乃受其次。
// 今释：启动时验证隔离能力，以忙碌锁保护共享容器，避免并发耗尽编译资源。
await 验隔离();
const 指南 = await readFile("/app/知识/助写指南.汉语.md", "utf8");
const 目录 = JSON.parse(await readFile("/app/知识/标准库签名.json", "utf8"));
const 示例 = Object.fromEntries(await Promise.all((await readdir("/app/知识/示例")).filter(名 => 名.endsWith("。豫")).sort().map(async 名 => [名, await readFile(`/app/知识/示例/${名}`, "utf8")])));
const 知识 = 构建知识(指南, 目录, 示例);
let 忙 = false;
async function 调模型(messages, 报告, 选项 = {}) {
  const 回 = await fetch("http://model.internal/completion", { method: "POST",
    headers: { "Content-Type": "application/json" }, body: JSON.stringify({ messages, mode: 选项.mode }), signal: AbortSignal.timeout(选项.timeoutMs ?? 125000) });
  const 原文 = await 回.text();
  if (Buffer.byteLength(原文) > 500000) throw new Error("模型响应过大");
  let 值;
  try { 值 = JSON.parse(原文); } catch { 报告?.({ status: 回.status, body: 原文 }); throw new Error("模型响应不是有效 JSON"); }
  报告?.({ status: 回.status, body: 值 });
  if (!回.ok) throw new Error(`模型服务返回 HTTP ${回.status}，请查看接收日志`);
  if (!["stop", "tool_calls"].includes(值.choices?.[0]?.finish_reason)) throw new Error(`模型未完成输出：${值.choices?.[0]?.finish_reason ?? "未知原因"}`);
  return { message: 值.choices[0].message, usage: 值.usage };
}
const 服务 = createServer(async (请求, 回应) => {
  const 答 = (状态, 值) => { 回应.writeHead(状态, { "Content-Type": "application/json; charset=utf-8", "Cache-Control": "no-store" }); 回应.end(JSON.stringify(值)); };
  if (请求.url === "/health" && 请求.method === "GET") return 答(200, { ok: true });
  if (请求.method !== "POST" || !["/api/run", "/api/assist"].includes(请求.url)) return 答(404, { error: "接口不存在" });
  if (忙) return 答(503, { error: "编译室正忙，请稍后重试" });
  忙 = true;
  let 事件流;
  try {
    let 长 = 0; const 块们 = [];
    for await (const 块 of 请求) { 长 += 块.length; if (长 > 请求上限) { 答(413, { error: "请求过大" }); return; } 块们.push(块); }
    let 值;
    try { 值 = 验请求(JSON.parse(Buffer.concat(块们).toString("utf8")), 请求.url); }
    catch { return 答(400, { error: "请求格式无效" }); }
    if (请求.headers.accept?.includes("application/x-ndjson")) 事件流 = 开启事件流(回应);
    const 报告 = 事件流?.报告 ?? (() => {});
    报告({ type: "stage", label: 请求.url === "/api/run" ? "已收到运行请求" : "已收到助写请求" });
    if (请求.url === "/api/assist") 报告({ type: "stage", label: `已加载 ${知识.模块数} 个标准库模块、${知识.签名数} 条公开签名和 ${Object.keys(示例).length} 个已验证示例` });
    const 结果 = 请求.url === "/api/run" ? await 编译运行(值.code, false, 报告) : await 助写(值, 知识, 调模型, 编译运行, 报告);
    if (事件流) 事件流.完成(结果); else 答(200, 结果);
  } catch (错) {
    const 结果 = { error: 错.message || "服务暂不可用，请稍后重试" };
    if (事件流) { 事件流.报告({ type: "error", label: 结果.error }); 事件流.完成(结果); }
    else 答(503, 结果);
  }
  finally { 忙 = false; }
});
服务.requestTimeout = 10000;
服务.headersTimeout = 10000;
服务.listen(8080, "0.0.0.0");
