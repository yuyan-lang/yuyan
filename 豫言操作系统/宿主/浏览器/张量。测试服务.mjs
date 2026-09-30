// 文言：张量探针之本机服务：静态供产物与测试页；带 --隔离 则诸应皆附 COOP、COEP 之头，令页跨源隔离而得共享之存与多线；另设 /api/slow 迟应，以供无头 Chrome 候探针之毕。惟听 127.0.0.1。
// 汉语：张量探针测试页的本机静态服务。用法：node 张量。测试服务.mjs <产物目录> [端口] [--隔离]。先在产物目录找文件，找不到再到本文件所在目录找（测试页 张量。测试页.html 在这里）；
//       带 --隔离 时每个响应都带 Cross-Origin-Opener-Policy: same-origin 与 Cross-Origin-Embedder-Policy: require-corp，页面因而跨源隔离，
//       中央张量宿主可用共享内存与 Web Worker 多线程。/api/slow?d=毫秒 延迟响应，供无头 Chrome 的 --dump-dom 等探针跑完。只监听 127.0.0.1，不访问外网。
import http from 'node:http';
import {readFile, stat} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';

const 参数们 = process.argv.slice(2).filter(项 => !项.startsWith('--'));
const 隔离 = process.argv.includes('--隔离');
const 产物目录 = path.resolve(参数们[0] ?? '.');
const 页面目录 = path.dirname(fileURLToPath(import.meta.url));
const 端口 = Number(参数们[1] ?? 0);
const 类型表 = {'.mjs': 'text/javascript; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.wasm': 'application/wasm',
  '.json': 'application/json; charset=utf-8', '.html': 'text/html; charset=utf-8'};
const 隔离头 = 隔离 ? {'Cross-Origin-Opener-Policy': 'same-origin', 'Cross-Origin-Embedder-Policy': 'require-corp'} : {};

const 服务 = http.createServer(async (请求, 响应) => {
  const 址 = new URL(请求.url, 'http://x');
  const 路 = decodeURIComponent(址.pathname);
  const 头 = (状态, 类型) => 响应.writeHead(状态, {'Content-Type': 类型, 'Cache-Control': 'no-store', ...隔离头});
  if (路.includes('..')) { 头(400, 'text/plain; charset=utf-8'); 响应.end('路径不合规'); return; }
  if (路 === '/api/slow') {
    setTimeout(() => { 头(200, 'text/plain; charset=utf-8'); 响应.end('慢'); }, Number(址.searchParams.get('d') ?? 200));
    return;
  }
  for (const 文件 of [path.join(产物目录, 路), path.join(页面目录, 路)]) {
    try {
      if (!(await stat(文件)).isFile()) continue;
      头(200, 类型表[path.extname(文件)] ?? 'application/octet-stream');
      响应.end(await readFile(文件));
      return;
    } catch { /* 试下一个 */ }
  }
  头(404, 'text/plain; charset=utf-8');
  响应.end('没有 ' + 路);
});
服务.listen(端口, '127.0.0.1', () => console.log('http://127.0.0.1:' + 服务.address().port + '/张量。测试页.html' + (隔离 ? '（跨源隔离）' : '')));
