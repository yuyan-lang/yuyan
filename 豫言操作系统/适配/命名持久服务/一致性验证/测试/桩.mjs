// 上游流一致性测试的公共桩：慢速分块流、服务绑定桩、持久对象桩、宿主装配。
import {readFile} from 'node:fs/promises';
import {pathToFileURL} from 'node:url';
import path from 'node:path';

export const 编 = new TextEncoder();
export const 解 = new TextDecoder();
export const 睡 = ms => new Promise(r => setTimeout(r, ms));

// 产物目录：由环境变量 产物目录 指定（私有暂存里的 dist/上游流一致性）。
const 产物 = path.resolve(process.env.产物目录 ?? './dist/上游流一致性') + path.sep;
const {创建云工宿主} = await import(pathToFileURL(产物 + '宿主.mjs').href);
const 程序模块 = await WebAssembly.compile(await readFile(产物 + '程序.wasm'));
const 值桥模块 = await WebAssembly.compile(await readFile(产物 + '值桥.wasm'));

export const 默认许可 = {SERVICE: ['SVC'], DO: ['DOS'], OUTBOUND_ORIGINS: ['https://api.example.com', 'https://*']};
export const 造宿主 = (额外 = {}) => 创建云工宿主({程序模块, 值桥模块, 许可: 默认许可, 时限毫秒: 60000, ...额外});

// 慢速流：每块前等待 间隔 毫秒；末尾可为 close、error、hang；观察记录拉取、取消原因与已送字节。
export function 造流(块们, {间隔 = 5, 末尾 = 'close', 观察 = {}} = {}) {
  let i = 0;
  观察.取消 ??= []; 观察.拉取 = 0; 观察.送出字节 = 0; 观察.块数 = 0;
  return new ReadableStream({
    async pull(c) {
      观察.拉取++;
      if (i < 块们.length) {
        if (间隔 > 0) await 睡(间隔);
        const 块 = typeof 块们[i] === 'string' ? 编.encode(块们[i]) : 块们[i];
        i++; 观察.送出字节 += 块.length; 观察.块数++;
        c.enqueue(块);
        return;
      }
      if (末尾 === 'close') c.close();
      else if (末尾 === 'error') c.error(new Error('上游炸了'));
      else await new Promise(() => {});
    },
    cancel(r) { 观察.取消.push(r instanceof Error || r?.name ? String(r.name) : String(r)); }
  }, {highWaterMark: 0});
}
export const 切块 = (字节, 宽) => { const 块们 = []; for (let i = 0; i < 字节.length; i += 宽) 块们.push(字节.slice(i, i + 宽)); return 块们; };
export const 拼接 = 块们 => { const 总 = 块们.reduce((n, 块) => n + 块.length, 0); const 果 = new Uint8Array(总); let 位 = 0; for (const 块 of 块们) { 果.set(块, 位); 位 += 块.length; } return 果; };

// 服务绑定桩：行为(路径, 请求, 桩, 记录)返回 Response 或抛错。必须是类实例（宿主桥只对非纯对象发句柄）。
export class 服务桩 {
  constructor(行为) { this.行为 = 行为; this.请求们 = []; this.观察 = {}; this.滴答 = []; this.等滴答 = new Map(); }
  async fetch(请求) {
    const 网址 = new URL(请求.url);
    const 记录 = {方法: 请求.method, 网址: 请求.url, 路径: 网址.pathname, 查询: 网址.search, 头: Object.fromEntries(请求.headers), 有信号: 请求.signal instanceof AbortSignal, 转址: 请求.redirect, 正文: null};
    if (请求.method !== 'GET' && 请求.method !== 'HEAD' && 网址.pathname !== '/pass' && 网址.pathname !== '/consume' && 请求.body) 记录.正文 = new Uint8Array(await 请求.arrayBuffer());
    this.请求们.push(记录);
    if (网址.pathname === '/tick') { this.滴答.push(Number(网址.searchParams.get('n'))); const 等 = this.等滴答.get(Number(网址.searchParams.get('n'))); 等?.(); return new Response('ok'); }
    return this.行为(网址.pathname, 请求, this, 记录);
  }
  等滴答到(n) { return this.滴答.includes(n) ? Promise.resolve() : new Promise(完成 => this.等滴答.set(n, 完成)); }
}
// 持久对象命名空间桩：getByName(名) 返回对象桩（类实例）。
export class 对象桩 {
  constructor(行为, 名) { this.行为 = 行为; this.名 = 名; this.请求们 = []; this.观察 = {}; }
  async fetch(请求) {
    const 网址 = new URL(请求.url);
    const 记录 = {对象: this.名, 方法: 请求.method, 网址: 请求.url, 路径: 网址.pathname, 查询: 网址.search, 头: Object.fromEntries(请求.headers), 有信号: 请求.signal instanceof AbortSignal, 转址: 请求.redirect, 正文: null};
    this.请求们.push(记录);
    return this.行为(网址.pathname, 请求, this, 记录);
  }
}
export class 命名空间桩 {
  constructor(行为) { this.行为 = 行为; this.对象们 = new Map(); this.取名们 = []; }
  getByName(名) { this.取名们.push(名); if (!this.对象们.has(名)) this.对象们.set(名, new 对象桩(this.行为, 名)); return this.对象们.get(名); }
}

export async function 跑(述, 环境, 宿主 = 造宿主()) {
  const 回 = await 宿主.fetch(new Request('https://x.test/run', {method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify(述)}), 环境);
  return {状态: 回.status, 文: await 回.text()};
}
