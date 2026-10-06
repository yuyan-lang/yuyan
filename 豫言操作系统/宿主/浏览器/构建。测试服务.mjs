// 文言：浏览器构建测试之本机服务：以宿主目为根而静供之，另供一资料目（快照与工具链）；诸应皆附 COOP、COEP 之头，令页跨源隔离。惟听 127.0.0.1。
// 汉语：浏览器构建测试页的本机静态服务。用法：node 构建。测试服务.mjs <资料目录> [端口]。
//   路径 /资料/… 取资料目录里的文件（源码快照与工具链的 .tar.gz），其余以 豫言操作系统/宿主/ 为根（测试页在 /浏览器/构建。测试页.html，
//   共用胶水在 /网页汇编/边界.mjs）。每个响应都带 Cross-Origin-Opener-Policy: same-origin 与 Cross-Origin-Embedder-Policy: require-corp，
//   页面因而跨源隔离，可用 SharedArrayBuffer。只监听 127.0.0.1，不访问外网。
import http from 'node:http';
import {readFile, stat} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';

const 资料目录 = path.resolve(process.argv[2] ?? '.');
const 宿主目录 = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const 端口 = Number(process.argv[3] ?? 0);
const 类型表 = {'.mjs': 'text/javascript; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.wasm': 'application/wasm',
  '.json': 'application/json; charset=utf-8', '.html': 'text/html; charset=utf-8', '.gz': 'application/gzip'};

const 服务 = http.createServer(async (请求, 响应) => {
  const 路 = decodeURIComponent(new URL(请求.url, 'http://x').pathname);
  const 头 = (状态, 类型) => 响应.writeHead(状态, {'Content-Type': 类型, 'Cache-Control': 'no-store',
    'Cross-Origin-Opener-Policy': 'same-origin', 'Cross-Origin-Embedder-Policy': 'require-corp'});
  if (路.includes('..')) { 头(400, 'text/plain; charset=utf-8'); 响应.end('路径不合规'); return; }
  const 文件 = 路.startsWith('/资料/') ? path.join(资料目录, 路.slice('/资料/'.length)) : path.join(宿主目录, 路);
  try {
    if (!(await stat(文件)).isFile()) throw Error();
    头(200, 类型表[path.extname(文件)] ?? 'application/octet-stream');
    响应.end(await readFile(文件));
  } catch {
    头(404, 'text/plain; charset=utf-8');
    响应.end('没有 ' + 路);
  }
});
服务.listen(端口, '127.0.0.1', () => console.log('http://127.0.0.1:' + 服务.address().port + '/浏览器/构建。测试页.html'));
