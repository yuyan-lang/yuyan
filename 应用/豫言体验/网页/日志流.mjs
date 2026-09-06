// 古曰：字可分至，句成乃读。今释：兼容 UTF-8 与 JSON 记录跨网络分块，断流须明确报错。
export async function 读取日志流(回, 收事件) {
  if (!回.headers.get("Content-Type")?.includes("application/x-ndjson")) {
    const 值 = await 回.json(); if (!回.ok) throw new Error(值.error ?? "服务暂不可用"); return 值;
  }
  const 读取器 = 回.body.getReader(), 解码 = new TextDecoder();
  let 缓存 = "", 结果, 已完成 = false;
  const 读行 = 行 => {
    if (!行.trim()) return;
    const 事件 = JSON.parse(行);
    if (事件.type === "result") { 结果 = 事件.data; 已完成 = true; }
    else if (事件.type !== "heartbeat") 收事件(事件);
  };
  try {
    while (true) {
      const { done, value } = await 读取器.read();
      缓存 += done ? 解码.decode() : 解码.decode(value, { stream: true });
      let 尾;
      while ((尾 = 缓存.indexOf("\n")) >= 0) { 读行(缓存.slice(0, 尾)); 缓存 = 缓存.slice(尾 + 1); }
      if (done) { if (缓存.trim()) 读行(缓存); break; }
    }
  } finally { 读取器.releaseLock(); }
  if (!已完成) throw new Error("连接中断：尚未收到最终结果，已收到的日志保留在下方。");
  return 结果;
}
