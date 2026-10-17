// 服务绑定桩（真 workerd 内的另一个 Worker）：按路径给出流式答复，并记录取消与请求形态。
const 观察 = {取消: {}, 请求: []};
const 编 = new TextEncoder();
const 睡 = ms => new Promise(r => setTimeout(r, ms));
const 造流 = (块们, {间隔 = 5, 末尾 = 'close', 名 = ''} = {}) => {
  let i = 0;
  return new ReadableStream({
    async pull(c) {
      if (i < 块们.length) { await 睡(间隔); c.enqueue(typeof 块们[i] === 'string' ? 编.encode(块们[i]) : 块们[i]); i++; return; }
      if (末尾 === 'close') c.close(); else if (末尾 === 'error') c.error(new Error('上游炸了')); else await 睡(120000);
    },
    cancel(r) { (观察.取消[名] ??= []).push(String(r?.name ?? r)); }
  }, {highWaterMark: 0});
};
const 切 = (字节, 宽) => { const 果 = []; for (let i = 0; i < 字节.length; i += 宽) 果.push(字节.slice(i, i + 宽)); return 果; };
const sse = ['你', '好', '，', '世界'].map(字 => 'data: ' + JSON.stringify({choices: [{delta: {content: 字}}]}) + '\n\n').join('') + 'data: [DONE]\n\n';
export default {
  async fetch(请求) {
    const 网址 = new URL(请求.url);
    const 路径 = 网址.pathname;
    if (路径 === '/__obs') return Response.json(观察);
    if (路径 === '/__clear') { 观察.取消 = {}; 观察.请求 = []; 观察.拉取 = {}; 观察.信号中止 = []; return new Response('ok'); }
    const 记 = {方法: 请求.method, 路径, 头: Object.fromEntries(请求.headers), 转址: 请求.redirect, 有信号: !!请求.signal};
    if (请求.method === 'POST') { const 体 = new Uint8Array(await 请求.arrayBuffer()); 记.正文长 = 体.length; 记.正文文 = new TextDecoder().decode(体); }
    观察.请求.push(记);
    switch (路径) {
      case '/sse5': return new Response(造流(切(编.encode(sse), 7), {名: 路径}), {headers: {'content-type': 'text/event-stream', 'x-yuyan-model': 'm1'}});
      case '/utf8': { const 字节 = Uint8Array.of(0xEF, 0xBB, 0xBF, 0xE7, 0x94, 0xB2, 0xF0, 0x9F, 0x98, 0x80, 0xFF, 0x80, 0x78, 0xE8, 0xB1); return new Response(造流([字节.slice(0, 2), 字节.slice(2, 4), 字节.slice(4, 7), 字节.slice(7, 8), 字节.slice(8, 10), 字节.slice(10, 11), 字节.slice(11, 13), 字节.slice(13, 14), 字节.slice(14)], {名: 路径})); }
      case '/slow': return new Response(造流(['甲'], {末尾: 'hang', 名: 路径}));
      case '/hang': return new Response(造流([], {末尾: 'hang', 名: 路径}));
      case '/errmid': return new Response(造流(['丙'], {末尾: 'error', 名: 路径}));
      case '/cl': return new Response(造流(['0123456789'], {名: 路径}), {headers: {'content-length': '5000'}});
      case '/nolen': return new Response(造流(Array.from({length: 20}, (_, i) => String.fromCharCode(97 + i).repeat(500)), {名: 路径}));
      case '/json': return new Response(JSON.stringify({答: '豫言'.repeat(50)}), {headers: {'content-type': 'application/json'}});
      case '/echo': return new Response(JSON.stringify({方法: 记.方法, 正文长: 记.正文长, 头: 记.头, 转址: 记.转址, 有信号: 记.有信号}), {headers: {'content-type': 'application/json'}});
      case '/late': await 睡(800); return new Response('迟到');
      case '/ticker': { let n = 0; 观察.拉取 ??= {}; 观察.信号中止 ??= []; 请求.signal.addEventListener('abort', () => 观察.信号中止.push('/ticker:' + String(请求.signal.reason?.name ?? 请求.signal.reason))); return new Response(new ReadableStream({ async pull(c) { await 睡(30); 观察.拉取['/ticker'] = (观察.拉取['/ticker'] ?? 0) + 1; c.enqueue(编.encode('滴' + (n++))); }, cancel(r) { (观察.取消['/ticker'] ??= []).push(String(r?.name ?? r)); } }, {highWaterMark: 0})); }
      case '/nobody': return new Response(null, {status: 204});
      default: return new Response('未知路径', {status: 404});
    }
  }
};
