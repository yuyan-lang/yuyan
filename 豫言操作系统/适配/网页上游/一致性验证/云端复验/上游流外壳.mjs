// 文言：上游流三适配云端复验之外壳。与产物之 入口.mjs 同目录。凡出于应用之请——全局 fetch（网页上游）、服务绑定 SVC（授权服务）、持久对象 DOS（命名持久服务）——皆转于测试所起之本机假服务，附原址、转址之式、有无信号、对象之名诸观察之头；假服务之基址由测试以 /__yy/fake?base= 告之。
// 文言：外发、服务绑定、持久对象皆为平台之物，豫言之云工应用不能代之受请，故暂以此 JavaScript 外壳接之；行为与记录皆在豫言之测试中。/upload 惟此外壳自读其流，以记分块之时（假服务唯收全请乃分派，不能观其流）。
// 汉语：上游流三个适配云端复验的 Worker 外壳，与构建产物的 入口.mjs 放在同一目录。应用发出的请求——全局 fetch（网页上游）、服务绑定 SVC（授权服务）、持久对象 DOS（命名持久服务）——都转给测试起的本机假服务，并附上原网址、转址方式、有无信号、对象名等观察用的标头；假服务基址由测试经 /__yy/fake?base= 告知。
// 汉语：外发、服务绑定与持久对象都是平台对象，豫言云工应用无法代替它们接收请求，所以暂用这个 JavaScript 外壳接线；行为与记录都在豫言测试里。只有 /upload 由外壳自己读请求流并记下各块到达时刻（假服务收齐整个请求才分派，看不到流）。
import 宿主 from './入口.mjs';
import {DurableObject, WorkerEntrypoint} from 'cloudflare:workers';
export {豫言持久对象, 豫言服务入口} from './入口.mjs';

const 原取 = globalThis.fetch;
let 假基址 = '';

async function 转假(前缀, 请求, 附加) {
  const 网址 = new URL(请求.url);
  const 头 = new Headers(请求.headers);
  for (const [名, 值] of Object.entries(附加)) 头.set(名, 值);
  const 初 = {method: 请求.method, headers: 头, redirect: 'manual', signal: 请求.signal};
  if (请求.body !== null) 初.body = await 请求.arrayBuffer();
  return 原取(假基址 + 前缀 + 网址.pathname + 网址.search, 初);
}

// 文言：全局 fetch 之外发转于假服务；其头与体照录，不另增 Content-Type；x-yy-url 存应用所给之原址，不经规整。汉语：全局 fetch 的外发转给假服务；标头与正文照原样转，不另加 Content-Type；x-yy-url 存应用给出的原网址，不经 URL 规范化。
globalThis.fetch = async (输入, 初 = {}) => {
  const 是请求 = 输入 instanceof Request;
  const 址 = new URL(是请求 ? 输入.url : String(输入));
  if (假基址 === '' || 址.hostname === '127.0.0.1') return 原取(输入, 初);
  const 头 = new Headers(初.headers ?? (是请求 ? 输入.headers : undefined));
  头.set('x-yy-url', 是请求 ? 输入.url : String(输入));
  头.set('x-yy-redirect', 初.redirect ?? (是请求 ? 输入.redirect : 'follow'));
  头.set('x-yy-signal', String((初.signal ?? (是请求 ? 输入.signal : undefined)) instanceof AbortSignal));
  const 原体 = 初.body ?? (是请求 ? 输入.body : null);
  const 体 = 原体 == null ? null : await new Response(原体).arrayBuffer();
  return 原取(假基址 + '/out/' + 址.host + 址.pathname + 址.search, {method: 初.method ?? (是请求 ? 输入.method : 'GET'), headers: 头, body: 体, redirect: 'manual', signal: 初.signal});
};

export class 假服务 extends WorkerEntrypoint {
  fetch(请求) {
    return 转假('/svc', 请求, {'x-yy-url': 请求.url, 'x-yy-redirect': 请求.redirect, 'x-yy-signal': String(请求.signal instanceof AbortSignal)});
  }
}

export class 假对象 extends DurableObject {
  async fetch(请求) {
    const 网址 = new URL(请求.url);
    if (网址.pathname === '/upload' && 请求.body !== null) {
      const 读 = 请求.body.getReader();
      let 块数 = 0, 总 = 0;
      const 时刻 = [];
      for (;;) { const {done, value} = await 读.read(); if (done) break; 块数++; 总 += value.length; 时刻.push(Date.now()); }
      return Response.json({块数, 总, 首末间隔: 时刻.length ? 时刻.at(-1) - 时刻[0] : 0, 对象: this.ctx.id.name ?? ''});
    }
    return 转假('/do', 请求, {'x-yy-url': 请求.url, 'x-yy-redirect': 请求.redirect, 'x-yy-signal': String(请求.signal instanceof AbortSignal), 'x-yy-object': this.ctx.id.name ?? ''});
  }
}

export default {
  fetch(请求, 环境, 上下文) {
    const 网址 = new URL(请求.url);
    if (网址.pathname === '/__yy/fake') { 假基址 = 网址.searchParams.get('base') ?? ''; return new Response('ok'); }
    return 宿主.fetch(请求, 环境, 上下文);
  }
};
