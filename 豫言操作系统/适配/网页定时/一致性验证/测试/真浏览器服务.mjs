// 文言：真浏览器验收之小服务：静态供探针产物与测试页，另设 /api/* 供请求与事件源之验，及假编译客户端与假子应用。
// 汉语：用法 node 真浏览器服务.mjs <探针产物目录> [端口]（环境变量 测试页目录 可指向另一个一致性验证目录，以便用它的测试页）；打开 http://127.0.0.1:端口/测试页.html。只监听 127.0.0.1，不访问外网。
//       路由：/api/json、/api/echo（POST）、/api/slow?d=毫秒、/api/status/N、/api/badutf8、/api/big?n=字节、/api/redirect-out、
//             /api/sse?n=事件数&gap=间隔毫秒&close=首连后关闭毫秒&retry=毫秒（SSE，带 Last-Event-ID 续发）、/编译/客户端.mjs（假客户端；设环境变量 编译目录 则改供真客户端与资源）、/子应用/入口.mjs（假子应用）、/api/cookie。
import http from 'node:http';
import {readFile, stat} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';

const 产物目录 = path.resolve(process.argv[2] ?? '.');
const 页面目录 = process.env.测试页目录 ? path.resolve(process.env.测试页目录, '测试') : path.dirname(fileURLToPath(import.meta.url));
const 端口 = Number(process.argv[3] ?? 0);
const 类型表 = {'.mjs': 'text/javascript; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.wasm': 'application/wasm', '.json': 'application/json; charset=utf-8',
  '.html': 'text/html; charset=utf-8', '.css': 'text/css; charset=utf-8'};

const 假编译客户端 = `
let 收尾 = null;
export function 浏览器编译(输入, 仅编译, 报告) {
  return new Promise(完成 => {
    收尾 = 完成;
    报告({type: 'stage', phase: 'load', label: '假客户端：加载'});
    let i = 0;
    const 钟 = setInterval(() => {
      if (i < 30) { 报告({type: 'output', stream: i % 2 ? 'stdout' : 'stderr', text: '正在编译：' + Object.keys(输入.files)[0] + ' #' + i + '\\n'}); i++; return; }
      clearInterval(钟);
      完成({ok: true, stdout: '编译通过', stderr: '', artifact: {sha256: 'b'.repeat(64)}});
    }, 10);
    收尾.钟 = 钟;
  });
}
export function 停止编译() { if (收尾) { clearInterval(收尾.钟); 收尾({ok: false, phase: 'compile', error: '已停止'}); } }
`;
const 假子应用 = `
export async function 启动豫言浏览器应用() {
  const 根 = document.getElementById('子根');
  根.textContent = '子应用已启动';
  根.addEventListener('click', () => { 根.textContent = '子应用响应了点击'; });
  return {就绪: Promise.resolve(), 完成: new Promise(() => {}), 关闭() { 根.textContent = '子应用已关闭'; }};
}
`;

const 帧 = (号, 数据, 事件名) => (号 ? 'id: ' + 号 + '\n' : '') + (事件名 ? 'event: ' + 事件名 + '\n' : '') + 'data: ' + 数据 + '\n\n';

const 服务 = http.createServer(async (请求, 响应) => {
  const 址 = new URL(请求.url, 'http://x');
  const 路 = decodeURIComponent(址.pathname);
  if (路.includes('..')) { 响应.writeHead(400); 响应.end('路径不合规'); return; }
  const 头 = (状态, 类型, 额外 = {}) => 响应.writeHead(状态, {'Content-Type': 类型, 'Cache-Control': 'no-store', ...额外});
  try {
    if (路 === '/api/json') { 头(200, 'application/json; charset=utf-8', {'X-Custom': 'a'}); 响应.end(JSON.stringify({用户: '张三', 表情: '😀', 时刻: Date.now()})); return; }
    if (路 === '/api/echo') {
      const 块 = []; for await (const 项 of 请求) 块.push(项);
      头(200, 'application/json; charset=utf-8');
      响应.end(JSON.stringify({方法: 请求.method, 类型: 请求.headers['content-type'] ?? '', 缓存: 请求.headers['cache-control'] ?? '', 体: Buffer.concat(块).toString('utf8'), 饼: 请求.headers.cookie ?? ''}));
      return;
    }
    if (路 === '/api/slow') { setTimeout(() => { 头(200, 'text/plain; charset=utf-8'); 响应.end('慢的结果 ' + 址.searchParams.get('d')); }, Number(址.searchParams.get('d') ?? 200)); return; }
    if (路.startsWith('/api/status/')) { 头(Number(路.split('/').pop()), 'application/json; charset=utf-8'); 响应.end('{"error":"状态测试"}'); return; }
    if (路 === '/api/badutf8') { 头(200, 'text/plain'); 响应.end(Buffer.from([0x61, 0xff, 0xfe, 0x62])); return; }
    if (路 === '/api/big') { 头(200, 'text/plain'); 响应.end('x'.repeat(Number(址.searchParams.get('n') ?? 1000))); return; }
    if (路 === '/api/hang') return;
    if (路 === '/api/redirect-out') { 响应.writeHead(302, {Location: 'https://example.com/'}); 响应.end(); return; }
    if (路 === '/api/cookie') { 响应.writeHead(200, {'Set-Cookie': ['yuyan_lang=wen; Path=/', 'plain=abc%20d; Path=/', 'secret=hidden; Path=/; HttpOnly'], 'Content-Type': 'text/plain'}); 响应.end('ok'); return; }
    if (路 === '/api/sse') {
      const 总 = Number(址.searchParams.get('n') ?? 5), 间隔 = Number(址.searchParams.get('gap') ?? 50), 关闭毫秒 = Number(址.searchParams.get('close') ?? 0), 重试 = 址.searchParams.get('retry');
      const 起 = Number(请求.headers['last-event-id'] ?? 0);
      响应.writeHead(200, {'Content-Type': 'text/event-stream; charset=utf-8', 'Cache-Control': 'no-cache'});
      if (重试) 响应.write('retry: ' + 重试 + '\n\n');
      let 号 = 起;
      const 发 = () => { if (号 >= 总) { if (!关闭毫秒) 响应.end(); return; } 号++; 响应.write(帧(号, JSON.stringify({序: 号, 文: '事件' + 号 + '😀'}))); if (间隔 > 0) 钟 = setTimeout(发, 间隔); else setImmediate(发); };
      let 钟 = setTimeout(发, 0);
      if (关闭毫秒) setTimeout(() => 响应.end(), 关闭毫秒);
      响应.on('close', () => clearTimeout(钟));
      响应.on('error', () => {});
      return;
    }
    if (路 === '/编译/客户端.mjs' && !process.env.编译目录) { 头(200, 类型表['.mjs']); 响应.end(假编译客户端); return; }
    if (路.startsWith('/编译/') && process.env.编译目录) {
      // 文言：用真编译客户端与编译资源（须先由构建工具生成，如云仓 应用/豫言体验/网页/编译）。汉语：环境变量 编译目录 指向含 客户端.mjs、工作线程.mjs、资源/ 的目录时，用它代替假客户端。
      try {
        const 文件 = path.join(path.resolve(process.env.编译目录), 路.slice('/编译/'.length));
        头(200, 类型表[path.extname(文件)] ?? 'application/octet-stream');
        响应.end(await readFile(文件));
        return;
      } catch { 头(404, 'text/plain'); 响应.end('没有 ' + 路); return; }
    }
    if (路 === '/子应用/入口.mjs') { 头(200, 类型表['.mjs']); 响应.end(假子应用); return; }
    // 静态：先测试目录（测试页.html），再产物目录
    const 候选 = [path.join(页面目录, '..', 路 === '/' ? '测试页.html' : 路), path.join(产物目录, 路)];
    for (const 文件 of 候选) {
      try {
        if (!(await stat(文件)).isFile()) continue;
        头(200, 类型表[path.extname(文件)] ?? 'application/octet-stream');
        响应.end(await readFile(文件));
        return;
      } catch { /* 试下一个 */ }
    }
    头(404, 'text/plain; charset=utf-8'); 响应.end('没有 ' + 路);
  } catch (错) { try { 头(500, 'text/plain; charset=utf-8'); 响应.end(String(错)); } catch { /* 忽略 */ } }
});
服务.listen(端口, '127.0.0.1', () => console.log('http://127.0.0.1:' + 服务.address().port + '/测试页.html'));
