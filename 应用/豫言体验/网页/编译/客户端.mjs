let 工作线程, 当前, 序号 = 0, 线程就绪 = false;
// 文言：久而不毕则止，无须候客自返。汉语：计时器在主线程，超时直接销毁 Worker，包括死循环和输出超限后的执行状态。
export function 停止编译() {
  工作线程?.terminate(); 工作线程 = undefined;
  if (当前) { const 旧 = 当前; 旧.完成({ ok: false, phase: 旧.阶段, error: "已停止" }); }
}
export function 浏览器编译(code, compileOnly = false, 报告 = () => {}) {
  if (当前) return Promise.reject(Error("已有编译任务正在运行"));
  return new Promise(完成 => {
    const id = ++序号;
    const 收尾 = 果 => { if (当前?.id !== id) return; clearTimeout(当前.时钟); 当前 = undefined; 完成(果); };
    const 设时限 = (毫秒, 阶段) => {
      clearTimeout(当前.时钟); 当前.阶段 = 阶段;
      当前.时钟 = setTimeout(() => {
        工作线程?.terminate(); 工作线程 = undefined;
        收尾({ ok: false, phase: 阶段, error: 阶段 === "run" ? "运行超过五秒时限" : "加载或编译超过九十秒时限" });
      }, 毫秒);
    };
    当前 = { id, 完成: 收尾, 阶段: "compile" }; 设时限(90000, "compile");
    try {
      if (!工作线程) { 线程就绪 = false; 工作线程 = new Worker(new URL("./工作线程.mjs", import.meta.url), { type: "module" }); }
      工作线程.onerror = () => {
        工作线程?.terminate(); 工作线程 = undefined;
        收尾({ ok: false, phase: 当前?.阶段 ?? "compile", error: "编译线程启动或执行失败，请使用支持 WasmGC 的新版浏览器" });
      };
      工作线程.onmessage = ({ data }) => {
        // 文言：器报可受信，乃授首稿。汉语：显式就绪握手覆盖模块异步初始化，真实浏览器和测试使用相同协议。
        if (data.ready && !线程就绪 && 当前?.id === id) {
          线程就绪 = true; 工作线程.postMessage({ id, code, compileOnly }); return;
        }
        if (data.id !== id || 当前?.id !== id) return;
        if (data.event) {
          if (data.event.type === "stage" && data.event.phase === "run") 设时限(5000, "run");
          报告(data.event);
        } else if (data.result) 收尾(data.result);
      };
      if (线程就绪) 工作线程.postMessage({ id, code, compileOnly });
    } catch (错) { 工作线程?.terminate(); 工作线程 = undefined; 收尾({ ok: false, phase: "compile", error: 错.message }); }
  });
}
export function 浏览器助写(data, 报告 = () => {}) {
  return new Promise((完成, 拒绝) => {
    const 地址 = new URL("/api/assist", location.href); 地址.protocol = location.protocol === "https:" ? "wss:" : "ws:";
    const 连接 = new WebSocket(地址);
    let 已结束 = false;
    const 收尾 = (果, 错) => {
      if (已结束) return; 已结束 = true; clearTimeout(计时); 连接.close();
      if (错) { 停止编译(); 拒绝(错); } else 完成(果);
    };
    const 计时 = setTimeout(() => 收尾(null, Error("助写超过八分钟时限")), 480000);
    连接.onopen = () => 连接.send(JSON.stringify({ type: "start", data }));
    连接.onerror = () => 收尾(null, Error("无法连接助写服务"));
    连接.onclose = () => { if (!已结束) 收尾(null, Error("助写连接已中断")); };
    连接.onmessage = async ({ data }) => {
      try {
        const 事 = JSON.parse(data);
        if (事.type === "compile_request") {
          const 果 = await 浏览器编译(事.code, 事.compileOnly, 报告);
          if (!已结束) 连接.send(JSON.stringify({ type: "compile_result", id: 事.id, data: 果 }));
        } else if (事.type === "result") 收尾(事.data);
        else 报告(事);
      } catch (错) { 收尾(null, 错); }
    };
  });
}
