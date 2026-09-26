// 持久对象桩（真 workerd 内）：SSE 事件、上传流、回声。
import {DurableObject} from 'cloudflare:workers';
const 编 = new TextEncoder();
const 睡 = ms => new Promise(r => setTimeout(r, ms));
export class 桩对象 extends DurableObject {
  async fetch(请求) {
    const 网址 = new URL(请求.url);
    const 路径 = 网址.pathname;
    if (路径 === '/events') {
      let i = 0;
      return new Response(new ReadableStream({
        async pull(c) { await 睡(80); if (i < 4) c.enqueue(编.encode(`data: ${i++}\n\n`)); else c.close(); }
      }, {highWaterMark: 0}), {status: 207, headers: {'content-type': 'text/event-stream', 'x-yuyan-revision': '12'}});
    }
    if (路径 === '/events200') {
      let i = 0;
      return new Response(new ReadableStream({
        async pull(c) { await 睡(80); if (i < 4) c.enqueue(编.encode(`data: ${i++}\n\n`)); else c.close(); }
      }, {highWaterMark: 0}), {status: 200, headers: {'content-type': 'text/event-stream', 'x-yuyan-revision': '15'}});
    }
    if (路径 === '/slowhead') { await 睡(800); return new Response('迟到'); }
    if (路径 === '/upload') {
      const 读 = 请求.body.getReader(); let 块数 = 0, 总 = 0; const 时刻 = [];
      for (;;) { const {done, value} = await 读.read(); if (done) break; 块数++; 总 += value.length; 时刻.push(Date.now()); }
      return Response.json({块数, 总, 时刻});
    }
    if (路径 === '/echo') { const 体 = new Uint8Array(await 请求.arrayBuffer()); return Response.json({方法: 请求.method, 正文长: 体.length, 头: Object.fromEntries(请求.headers), 网址: 请求.url, 转址: 请求.redirect}); }
    if (路径 === '/read') { const n = Number(网址.searchParams.get('n') ?? 0); return new Response('a'.repeat(n), {headers: {'content-type': 'text/plain', 'set-cookie': 's=1', 'x-yuyan-revision': '9', 'x-secret': 'do-not-leak'}}); }
    return new Response('未知路径', {status: 404});
  }
}
export default {fetch: () => new Response('do worker')};
