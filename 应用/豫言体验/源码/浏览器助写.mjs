import { 助写 } from "./助写.mjs";
import { 验请求 } from "./约束.mjs";
import { 助写时限 } from "./模型协议.mjs";

// 文言：谋在云端，编在客室；往复有号，用度有界。汉语：服务端掌握模型提示、轮数和额度；浏览器只返回当前编译请求的结果。
export function 验浏览器结果(值) {
  if (!值 || typeof 值.ok !== "boolean" || !["compile", "run"].includes(值.phase)) throw Error("编译结果格式无效");
  const 果 = { ok: 值.ok, phase: 值.phase };
  for (const 键 of ["stdout", "stderr", "error"]) {
    if (值[键] !== undefined && (typeof 值[键] !== "string" || 值[键].length > 65536)) throw Error("编译结果过大");
    果[键] = 值[键] ?? "";
  }
  if (值.exitCode !== undefined && 值.exitCode !== null && !Number.isSafeInteger(值.exitCode)) throw Error("退出状态无效");
  果.exitCode = 值.exitCode ?? null;
  return 果;
}
export function 建立助写会话(连接, { 知识, 调模型, 扣额度 }) {
  const 中止 = new AbortController();
  let 已开始 = false, 已结束 = false, 序号 = 0, 等待;
  const 发送 = 事 => { if (!已结束) 连接.send(JSON.stringify({ ...事, time: Date.now() })); };
  const 结束 = () => {
    if (已结束) return;
    已结束 = true; clearTimeout(时钟); 中止.abort();
    等待?.拒绝(Error("助写连接已关闭")); 等待 = undefined;
    try { 连接.close(1000, "助写结束"); } catch {}
  };
  const 时钟 = setTimeout(() => { 发送({ type: "result", data: { error: "助写超过八分钟时限" } }); 结束(); }, 助写时限);
  连接.addEventListener("close", 结束); 连接.addEventListener("error", 结束);
  连接.addEventListener("message", async 事件 => {
    try {
      if (已结束) return;
      if (typeof 事件.data !== "string" || new TextEncoder().encode(事件.data).length > 400000) throw Error("助写消息过大");
      const 值 = JSON.parse(事件.data);
      if (!已开始) {
        if (值.type !== "start") throw Error("缺少助写请求");
        const 请求 = 验请求(值.data, "/api/assist"); 已开始 = true;
        await 扣额度();
        if (中止.signal.aborted) return;
        const 编译 = (code, 仅编译) => new Promise((完成, 拒绝) => {
          if (中止.signal.aborted) { 拒绝(Error("连接已关闭")); return; }
          const id = ++序号;
          const 计时 = setTimeout(() => { 等待 = undefined; 拒绝(Error("浏览器编译响应超时")); }, 90000);
          等待 = { id, 完成: 结果 => { clearTimeout(计时); 完成(结果); }, 拒绝: 错 => { clearTimeout(计时); 拒绝(错); } };
          发送({ type: "compile_request", id, code, compileOnly: 仅编译 });
        });
        const 果 = await 助写(请求, 知识, (消息, 报告, 选项) => {
          if (中止.signal.aborted) throw Error("连接已关闭");
          return 调模型(消息, 报告, { ...选项, signal: 中止.signal });
        }, 编译, 发送);
        发送({ type: "result", data: 果 }); 结束();
      } else {
        if (值.type !== "compile_result" || !等待 || 值.id !== 等待.id) throw Error("编译结果不属于当前请求");
        const 果 = 验浏览器结果(值.data), 当前 = 等待; 等待 = undefined; 当前.完成(果);
      }
    } catch (错) { 发送({ type: "result", data: { error: 错.message } }); 结束(); }
  });
  return 结束;
}
