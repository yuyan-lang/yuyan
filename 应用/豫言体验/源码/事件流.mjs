// 古曰：事行则报，不待终篇；慢客之积亦须有度。
// 今释：请求专属 NDJSON 流实时传回进度；慢连接积压时明确标记日志截断，避免无限占用内存。
export function 开启事件流(回应) {
  回应.writeHead(200, { "Content-Type": "application/x-ndjson; charset=utf-8", "Cache-Control": "no-store, no-transform", "X-Content-Type-Options": "nosniff" });
  回应.flushHeaders();
  let 截断 = false;
  const 报告 = 事件 => {
    if (回应.destroyed || 回应.writableEnded) return;
    if (回应.writableLength > 1024 * 1024 && 事件.type !== "result") { 截断 = true; return; }
    if (截断) {
      回应.write(JSON.stringify({ type: "error", label: "连接过慢，部分实时日志已省略。" }) + "\n");
      截断 = false;
    }
    回应.write(JSON.stringify({ time: new Date().toISOString(), ...事件 }) + "\n");
  };
  const 心跳 = setInterval(() => 报告({ type: "heartbeat" }), 10000);
  回应.once("close", () => clearInterval(心跳));
  return { 报告, 完成(结果) { clearInterval(心跳); 报告({ type: "result", data: 结果 }); 回应.end(); } };
}
