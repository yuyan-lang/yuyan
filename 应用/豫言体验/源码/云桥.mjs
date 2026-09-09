import { 组模型请求 } from "./模型协议.mjs";
import { 读限量正文 } from "./约束.mjs";
import { 建立助写会话 } from "./浏览器助写.mjs";
import { 构建知识 } from "./知识.mjs";
import 指南 from "../知识/助写指南.汉语.md";
import 目录 from "../知识/标准库签名.json";
import 列表 from "../知识/示例/列表与模式匹配。豫";
import 求和 from "../知识/示例/递归与求和。豫";
import 分支 from "../知识/示例/函数与分支。豫";
import 乘法表 from "../知识/示例/嵌套递归与乘法表。豫";
export { Quota } from "./额度.mjs";

// 文言：旧籍暂存以备复旧，不复役其器。汉语：保留旧 Durable Object 类名与数据，取消所有容器调用；首次上线不做不可逆的类删除迁移。
export class YuyanContainer {
  async fetch() { return Response.json({ error: "编译运行已移至浏览器" }, { status: 410 }); }
}

const 知识 = 构建知识(指南, 目录, { "列表与模式匹配。豫": 列表, "递归与求和。豫": 求和, "函数与分支。豫": 分支, "嵌套递归与乘法表。豫": 乘法表 });
function 回应(值, 状态 = 200) {
  return Response.json(值, { status: 状态, headers: { "Cache-Control": "no-store", "X-Content-Type-Options": "nosniff" } });
}
export default {
  async fetch(请求, 环境) {
    const 网址 = new URL(请求.url);
    if (!网址.pathname.startsWith("/api/")) {
      if (网址.pathname === "/") 网址.pathname = "/首页.html";
      const 静态 = await 环境.ASSETS.fetch(new Request(网址, 请求));
      const 标头 = new Headers(静态.headers);
      // 文言：只许同源之器，客程不得通网。汉语：允许 Wasm 编译和本站 Worker；Wasm 程序本身只获得内存宿主原语。
      标头.set("Content-Security-Policy", "default-src 'self'; script-src 'self' 'wasm-unsafe-eval'; worker-src 'self'; style-src 'self'; connect-src 'self'; img-src 'self' data:; frame-ancestors 'none'; base-uri 'none'; form-action 'self'");
      标头.set("X-Content-Type-Options", "nosniff"); 标头.set("Referrer-Policy", "no-referrer");
      return new Response(静态.body, { status: 静态.status, headers: 标头 });
    }
    if (网址.pathname === "/api/status" && 请求.method === "GET")
      return 回应({ enabled: true, backend: "wasmgc", ai: 环境.ENABLED === "true" && Boolean(环境.DEEPSEEK_API_KEY), model: 环境.MODEL });
    if (网址.pathname === "/api/run") return 回应({ error: "编译运行已移至浏览器，请刷新页面" }, 410);
    if (网址.pathname !== "/api/assist") return 回应({ error: "接口不存在" }, 404);
    if (请求.method !== "GET" || 请求.headers.get("Upgrade")?.toLowerCase() !== "websocket") return 回应({ error: "请使用新版浏览器助写连接" }, 426);
    if (环境.ENABLED !== "true" || !环境.DEEPSEEK_API_KEY) return 回应({ error: "AI 助写尚未开放" }, 503);
    if (请求.headers.get("Origin") !== 网址.origin) return 回应({ error: "仅接受本站请求" }, 403);
    const 地址 = 请求.headers.get("CF-Connecting-IP");
    if (!地址) return 回应({ error: "无法识别访客" }, 403);
    const 摘要 = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(地址));
    const 客名 = Array.from(new Uint8Array(摘要), 字节 => 字节.toString(16).padStart(2, "0")).join("");
    const [客户端, 服务端] = Object.values(new WebSocketPair()); 服务端.accept();
    建立助写会话(服务端, {
      知识,
      扣额度: async () => {
        const 回 = await 环境.QUOTA.getByName("全站额度").fetch(new Request("https://quota/", {
          method: "POST", body: JSON.stringify({ ip: 客名, action: "assist" })
        }));
        if (!回.ok) throw Error((await 回.json()).error ?? "助写额度不可用");
      },
      调模型: async (messages, 报告, 选项) => {
        // 文言：钥与模型皆由主定，不纳客之命令。汉语：完整代理循环留在服务端，客户端不能提交任意模型请求。
        const 回 = await fetch("https://api.deepseek.com/chat/completions", {
          method: "POST", signal: AbortSignal.any([选项.signal, AbortSignal.timeout(选项.timeoutMs)]),
          headers: { "Content-Type": "application/json", Authorization: `Bearer ${环境.DEEPSEEK_API_KEY}` },
          body: JSON.stringify(组模型请求({ messages, mode: 选项.mode }, 环境.MODEL))
        });
        const 文 = (await 读限量正文(回, 500000)).split(环境.DEEPSEEK_API_KEY).join("[密钥已隐藏]");
        let 值;
        try { 值 = JSON.parse(文); } catch { 报告({ status: 回.status, body: 文 }); throw Error("模型响应不是有效 JSON"); }
        报告({ status: 回.status, body: 值 });
        if (!回.ok) throw Error(`模型服务返回 HTTP ${回.status}`);
        if (!["stop", "tool_calls"].includes(值.choices?.[0]?.finish_reason)) throw Error("模型未完成输出");
        return { message: 值.choices[0].message, usage: 值.usage };
      }
    });
    return new Response(null, { status: 101, webSocket: 客户端 });
  }
};
