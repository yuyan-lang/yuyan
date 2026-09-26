// 基线：纯 JS 应用 Worker 对服务绑定流的取消传播（对照豫言适配）。
import {readFileSync} from 'node:fs';
import {pathToFileURL} from 'node:url';
const 迷 = '/Users/zc/repos/yuyan-worktrees/yuyan-cloud/工具/包管理服务/node_modules/miniflare/dist/src/index.js';
const {Miniflare, convertV4MiniflareOptions} = await import(pathToFileURL(迷).href);
const 睡 = ms => new Promise(r => setTimeout(r, ms));
const 应用 = `
export default { async fetch(请求, env) {
  const 路径 = new URL(请求.url).pathname;
  if (路径 === '/cancel') { const r = await env.SVC.fetch('https://svc/ticker'); const rd = r.body.getReader(); await rd.read(); await rd.cancel('js取消'); return new Response('ok'); }
  if (路径 === '/abort') { const c = new AbortController(); const r = await env.SVC.fetch(new Request('https://svc/ticker', {signal: c.signal})); const rd = r.body.getReader(); await rd.read(); c.abort(); try { await rd.read(); } catch (e) {} return new Response('ok'); }
  if (路径 === '/timeout') { const r = await env.SVC.fetch(new Request('https://svc/ticker', {signal: AbortSignal.timeout(300)})); const rd = r.body.getReader(); try { for (;;) { const x = await rd.read(); if (x.done) break; } } catch (e) {} return new Response('ok'); }
  if (路径 === '/pipe') { const c = new AbortController(); const r = await env.SVC.fetch('https://svc/ticker'); const out = r.body.pipeThrough(new TransformStream(), {signal: c.signal}); const rd = out.getReader(); await rd.read(); c.abort(); try { await rd.read(); } catch (e) {} return new Response('ok'); }
  if (路径 === '/pipecancel') { const r = await env.SVC.fetch('https://svc/ticker'); const out = r.body.pipeThrough(new TransformStream()); const rd = out.getReader(); await rd.read(); await rd.cancel('管道取消'); return new Response('ok'); }
  return new Response('?', {status: 404});
} }`;
const mf = new Miniflare(convertV4MiniflareOptions({workers: [
  {name: 'app', compatibilityDate: '2026-09-09', modules: true, script: 应用, serviceBindings: {SVC: 'svc'}},
  {name: 'svc', compatibilityDate: '2026-09-09', modules: true, script: readFileSync(new URL('./服务.mjs', import.meta.url), 'utf8')}
]}));
const 观察 = async () => (await (await mf.getWorker('svc')).fetch('https://o/__obs')).json();
for (const 路径 of ['/cancel', '/abort', '/timeout', '/pipe', '/pipecancel']) {
  await (await mf.getWorker('svc')).fetch('https://o/__clear');
  const r = await mf.dispatchFetch('https://x.test' + 路径);
  await r.text(); await 睡(700);
  const o = await 观察();
  console.log(路径.padEnd(12), '取消:', JSON.stringify(o.取消), '请求信号中止:', JSON.stringify(o.信号中止), '拉取:', JSON.stringify(o.拉取));
}
await mf.dispose();
