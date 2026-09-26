// 在真 workerd（Miniflare）里运行豫言 Wasm 产物，验证平台行为：流、信号、取消、服务绑定、持久对象、外发拦截。
import {readFileSync, readdirSync} from 'node:fs';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
const 迷 = process.env.迷你路径 ?? '/Users/zc/repos/yuyan-worktrees/yuyan-cloud/工具/包管理服务/node_modules/miniflare/dist/src/index.js';
const {Miniflare, convertV4MiniflareOptions} = await import(pathToFileURL(迷).href);
const 产物 = path.resolve(process.env.产物目录);
const 全部 = readdirSync(产物).filter(f => /\.(mjs|wasm|json)$/.test(f));
const 模块 = [...全部.filter(f => f === '入口.mjs'), ...全部.filter(f => f !== '入口.mjs')].map(f => {
  const 文 = f.endsWith('.wasm') ? undefined : readFileSync(path.join(产物, f), 'utf8');
  return {type: f.endsWith('.wasm') ? 'CompiledWasm' : 'ESModule', path: path.join(产物, f), contents: f.endsWith('.wasm') ? readFileSync(path.join(产物, f)) : f.endsWith('.json') ? 'export default ' + 文 + ';' : 文};
});
const 服务脚本 = readFileSync(new URL('./服务.mjs', import.meta.url), 'utf8');
const 对象脚本 = readFileSync(new URL('./对象.mjs', import.meta.url), 'utf8');
const 外发脚本 = readFileSync(new URL('./外发.mjs', import.meta.url), 'utf8');
const 共用 = {compatibilityDate: '2026-09-09'};
const mf = new Miniflare(convertV4MiniflareOptions({
  workers: [
    {...共用, name: 'app', modules: 模块, modulesRoot: 产物,
      serviceBindings: {SVC: 'svc'}, durableObjects: {DOS: {className: '桩对象', scriptName: 'do'}}, outboundService: 'net'},
    {...共用, name: 'svc', modules: true, script: 服务脚本},
    {...共用, name: 'do', modules: true, script: 对象脚本, durableObjects: {DOS: '桩对象'}},
    {...共用, name: 'net', modules: true, script: 外发脚本}
  ]
}));
export {mf};
export const 跑 = async 述 => { const r = await mf.dispatchFetch('https://x.test/run', {method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify(述)}); return {状态: r.status, 文: await r.text()}; };
export const 观察 = async (名 = 'svc') => (await (await mf.getWorker(名)).fetch('https://o/__obs')).json();
