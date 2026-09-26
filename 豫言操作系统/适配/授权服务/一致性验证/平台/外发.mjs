// 外发拦截（真 workerd 内）：应用 Worker 的全局 fetch 都到这里，模拟公网服务商。
const 观察 = {请求: [], 取消: []};
const 编 = new TextEncoder();
const 睡 = ms => new Promise(r => setTimeout(r, ms));
export default {
  async fetch(请求) {
    const 网址 = new URL(请求.url);
    if (网址.hostname === 'o' && 网址.pathname === '/__obs') return Response.json(观察);
    const 记 = {url: 请求.url, 方法: 请求.method, 头: Object.fromEntries(请求.headers), 转址: 请求.redirect};
    if (请求.method === 'POST') { const 体 = new Uint8Array(await 请求.arrayBuffer()); 记.正文文 = new TextDecoder().decode(体); }
    观察.请求.push(记);
    switch (网址.pathname) {
      case '/sse': { const 块们 = ['data: {"choices":[{"delta":{"content":"甲"}}]}\n\n', 'data: {"choices":[{"delta":{"content":"乙"}}]}\n\n', 'data: [DONE]\n\n']; let i = 0;
        return new Response(new ReadableStream({ async pull(c) { await 睡(10); if (i < 块们.length) c.enqueue(编.encode(块们[i++])); else c.close(); }, cancel(r) { 观察.取消.push(String(r?.name ?? r)); } }, {highWaterMark: 0}), {headers: {'content-type': 'text/event-stream'}}); }
      case '/redirect': return new Response('moved', {status: 302, headers: {location: 'https://elsewhere.example.com/'}});
      case '/hang': await 睡(120000); return new Response('never');
      case '/slow': return new Response(new ReadableStream({ pull() { return 睡(120000); }, cancel(r) { 观察.取消.push(String(r?.name ?? r)); } }), {status: 200});
      default: return Response.json({ok: true, 网址: 请求.url});
    }
  }
};
