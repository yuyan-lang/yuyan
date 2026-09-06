import { Container } from "@cloudflare/containers";
import { 验请求, 读限量正文 } from "./约束.mjs";
import { 组模型请求 } from "./模型协议.mjs";
export { Quota } from "./额度.mjs";
export { ContainerProxy } from "@cloudflare/containers";

export class YuyanContainer extends Container {
  defaultPort = 8080;
  requiredPorts = [8080];
  sleepAfter = "1m";
  enableInternet = false;
}

// 古曰：钥藏于桥，不入客室；出门惟此一途。
// 今释：模型密钥只存在 Worker secret 中，容器仅能访问固定的模型代理。
YuyanContainer.outboundByHost = {
  "model.internal": async (请求, 环境) => {
    if (请求.method !== "POST" || new URL(请求.url).pathname !== "/completion") return new Response(null, { status: 404 });
    if (!环境.DEEPSEEK_API_KEY) return new Response(null, { status: 503 });
    let 内容;
    try { 内容 = 组模型请求(JSON.parse(await 读限量正文(请求, 1500000)), 环境.MODEL); } catch { return Response.json({ error: "模型请求超过上下文限制或格式无效" }, { status: 400 }); }
    // 古曰：型号与用度出于主，不听于客。
    // 今释：代理不接受自定义目标、模型、工具、token 上限或任意 API 路径。
    const 上游 = await fetch("https://api.deepseek.com/chat/completions", {
      method: "POST", signal: AbortSignal.timeout(120000),
      headers: { "Content-Type": "application/json", Authorization: `Bearer ${环境.DEEPSEEK_API_KEY}` },
      body: JSON.stringify(内容)
    });
    // 古曰：往来可示，钥不可示。今释：即使上游错误回显密钥，也在离开 Worker 前抹去。
    const 正文 = (await 上游.text()).split(环境.DEEPSEEK_API_KEY).join("[密钥已隐藏]");
    return new Response(正文, { status: 上游.status, headers: { "Content-Type": "application/json" } });
  }
};

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
      标头.set("Content-Security-Policy", "default-src 'self'; script-src 'self'; style-src 'self'; connect-src 'self'; img-src 'self' data:; frame-ancestors 'none'; base-uri 'none'; form-action 'self'");
      标头.set("X-Content-Type-Options", "nosniff");
      标头.set("Referrer-Policy", "no-referrer");
      return new Response(静态.body, { status: 静态.status, headers: 标头 });
    }
    if (网址.pathname === "/api/status" && 请求.method === "GET")
      return 回应({ enabled: 环境.ENABLED === "true", ai: Boolean(环境.DEEPSEEK_API_KEY), model: 环境.MODEL });
    if (!["/api/run", "/api/assist"].includes(网址.pathname)) return 回应({ error: "接口不存在" }, 404);
    if (请求.method !== "POST") return 回应({ error: "仅支持 POST" }, 405);
    if (环境.ENABLED !== "true") return 回应({ error: "编译服务尚未开放" }, 503);
    if (请求.headers.get("Origin") !== 网址.origin) return 回应({ error: "仅接受本站请求" }, 403);
    if (!请求.headers.get("Content-Type")?.startsWith("application/json")) return 回应({ error: "请提交 JSON" }, 415);
    if (网址.pathname === "/api/assist" && !环境.DEEPSEEK_API_KEY) return 回应({ error: "AI 密钥尚未配置" }, 503);
    let 内容;
    try { 内容 = 验请求(JSON.parse(await 读限量正文(请求)), 网址.pathname); }
    catch (错误) { return 回应({ error: 错误.message }, 400); }
    try {
      const 地址 = 请求.headers.get("CF-Connecting-IP");
      if (!地址) return 回应({ error: "无法识别访客" }, 403);
      const 摘要 = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(地址));
      const 客名 = Array.from(new Uint8Array(摘要), 字节 => 字节.toString(16).padStart(2, "0")).join("");
      const 配额 = await 环境.QUOTA.getByName("全站额度").fetch(new Request("http://quota/", {
        method: "POST", body: JSON.stringify({ ip: 客名, action: 网址.pathname === "/api/assist" ? "assist" : "run" })
      }));
      if (!配额.ok) return 配额;
      // 古曰：室有二，事各专之；一室有事，后来者暂辞。
      // 今释：固定容器池配合容器端忙碌锁，限制并发与费用；每次运行再建内部隔离空间。
      const 槽 = new Uint8Array(摘要)[0] % 2;
      const 结果 = await 环境.RUNNER.getByName(`试写-${环境.RUNNER_RELEASE ?? "初版"}-${槽}`).fetch(new Request(`http://container${网址.pathname}`, {
        method: "POST", headers: { "Content-Type": "application/json", Accept: 请求.headers.get("Accept")?.includes("application/x-ndjson") ? "application/x-ndjson" : "application/json" }, body: JSON.stringify(内容)
      }));
      const 类型 = 结果.headers.get("Content-Type") ?? "";
      if (!类型.includes("application/json") && !类型.includes("application/x-ndjson"))
        return 回应({ error: "编译室正在启动或暂不可用，请稍后重试" }, 503);
      return new Response(结果.body, { status: 结果.status, headers: { "Content-Type": 类型, "Cache-Control": "no-store, no-transform", "X-Content-Type-Options": "nosniff" } });
    } catch { return 回应({ error: "服务暂不可用，请稍后重试" }, 503); }
  }
};
