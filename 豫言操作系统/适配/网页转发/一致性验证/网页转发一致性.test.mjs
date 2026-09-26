// 网页转发 0.4.0 一致性测试：真实 Wasm + Node 宿主 + 模拟的 SERVICE 与 ASSETS 绑定。
// 复跑：在私有暂存目录（含 dist/）里 `node --test <本文件>`；产物位置可用环境变量 产物根 指定（默认 <当前目录>/dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createHash, randomBytes} from 'node:crypto';
import {pathToFileURL} from 'node:url';
import path from 'node:path';

const 产物 = path.resolve(process.env.产物根 ?? path.join(process.cwd(), 'dist'), '网页转发一致性');
const {创建云工宿主} = await import(pathToFileURL(path.join(产物, '宿主.mjs')).href);
const 程序模块 = await WebAssembly.compile(await readFile(path.join(产物, '程序.wasm')));
const 值桥模块 = await WebAssembly.compile(await readFile(path.join(产物, '值桥.wasm')));
const 许可 = JSON.parse(await readFile(path.join(产物, '许可.json'), 'utf8'));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可});
const 上下文 = {waitUntil(承诺) { Promise.resolve(承诺).catch(() => {}); }};
const MiB = 1024 * 1024;
const 摘 = 字节 => createHash('sha256').update(字节).digest('hex');
const 限时 = (承诺, 毫秒 = 5000, 说明 = '') => {
  let 计时;
  return Promise.race([承诺, new Promise((_, 拒) => { 计时 = setTimeout(() => 拒(new Error('超时（疑似缓冲或死锁）' + 说明)), 毫秒); })]).finally(() => clearTimeout(计时));
};

class 服务 {
  constructor(处理) { this.处理 = 处理 ?? (async (请, 记) => new Response('答:' + 请.method, {status: 202, headers: {'X-Up': 'yes', 'Set-Cookie': 's=1'}})); this.记录 = []; }
  async fetch(请求) {
    const 记 = {方法: 请求.method, 网址: 请求.url, 头: Object.fromEntries([...请求.headers]), 重定向: 请求.redirect, 有体: 请求.body !== null, 正文: null};
    this.记录.push(记);
    return this.处理(请求, 记);
  }
}
class 资产 {
  constructor(表 = {}) { this.表 = 表; this.记录 = []; }
  async fetch(输入) {
    const 网址 = typeof 输入 === 'string' ? 输入 : 输入.url;
    this.记录.push({网址, 类型: typeof 输入});
    const 项 = this.表[new URL(网址).pathname];
    if (!项) return new Response('无此资源', {status: 404});
    return typeof 项 === 'function' ? 项() : new Response(项.体 ?? null, {status: 项.状态 ?? 200, headers: 项.头 ?? {}});
  }
}
const 查询 = (对象) => Object.entries(对象).map(([k, v]) => k + '=' + encodeURIComponent(typeof v === 'string' ? v : JSON.stringify(v))).join('&');
async function 调(参数, {方法 = 'POST', 体, 头 = {}, 环境 = {}, 请求} = {}) {
  let 请 = 请求;
  if (!请) {
    const 初 = {method: 方法, headers: 头};
    if (体 !== undefined) { 初.body = 体; if (体 instanceof ReadableStream) 初.duplex = 'half'; }
    请 = new Request('https://x.test/orig?z=1&' + 查询(参数), 初);
  }
  const 回 = await 宿主.fetch(请, 环境, 上下文);
  return 回;
}
async function 简(参数, 选项) {
  const 回 = await 调(参数, 选项);
  return {状态: 回.status, 头: [...回.headers], 饼: 回.headers.getSetCookie(), 文: await 回.text()};
}
function 错(果) {
  assert.equal(果.状态, 500, '应得到可捕获的失败，实得 ' + 果.状态 + ' ' + 果.文.slice(0, 120));
  assert.ok(果.文.startsWith('err|'));
  return Buffer.from(果.文.slice(4), 'hex').toString('utf8');
}
const 筛 = (svc, 径, keep, fixed) => ({op: 'filter', svc, path: 径, keep: JSON.stringify(keep), fixed: JSON.stringify(fixed)});

test('筛头转发：任意方法、路径含查询、manual 重定向、只带列出的头', async () => {
  for (const 方法 of ['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS']) {
    const 甲 = new 服务();
    const 带体 = ['POST', 'PUT', 'PATCH', 'DELETE'].includes(方法);
    const 果 = await 简(筛('API', '/x/y?q=1&r=%20', ['Authorization', 'X-Keep', 'X-Missing'], [['Cookie', 'a=b'], ['X-Fixed', 'f']]), {方法, 体: 带体 ? '内容' : undefined, 头: {Authorization: 'Bearer t', 'X-Keep': 'k', 'X-Drop': 'd', Cookie: 'inbound=1', 'Content-Type': 'text/plain'}, 环境: {API: 甲}});
    assert.equal(果.状态, 202, 方法);
    assert.equal(果.文, '答:' + 方法);
    assert.deepEqual(果.饼, ['s=1']);
    assert.ok(果.头.some(项 => 项[0] === 'x-up' && 项[1] === 'yes'));
    assert.equal(甲.记录.length, 1);
    const 记 = 甲.记录[0];
    assert.equal(记.方法, 方法);
    assert.equal(记.网址, 'https://x.test/x/y?q=1&r=%20');
    assert.equal(记.重定向, 'manual');
    assert.deepEqual(记.头, {authorization: 'Bearer t', cookie: 'a=b', 'x-fixed': 'f', 'x-keep': 'k'}, '只复制列出的头，固定头覆盖同名头：' + 方法);
    assert.equal(记.有体, 带体, 方法);
  }
});

test('筛头转发：请求体流原样转发且不缓冲', async () => {
  let 放行第二块;
  const 第二块 = new Promise(完成 => { 放行第二块 = 完成; });
  const 块 = [Buffer.from('第一块'), Buffer.from('第二块')];
  let 序 = 0;
  const 入 = new ReadableStream({async pull(c) {
    if (序 === 0) c.enqueue(块[序++]);
    else if (序 === 1) { await 第二块; c.enqueue(块[序++]); }
    else c.close();
  }}, {highWaterMark: 0});
  const 甲 = new 服务(async (请, 记) => {
    const 读 = 请.body.getReader();
    const 首 = await 读.read();
    记.首块 = Buffer.from(首.value).toString();
    放行第二块();
    const 余 = [];
    for (;;) { const r = await 读.read(); if (r.done) break; 余.push(Buffer.from(r.value)); }
    记.余 = Buffer.concat(余).toString();
    return new Response('ok');
  });
  const 果 = await 限时(简(筛('API', '/up', [], []), {体: 入, 环境: {API: 甲}}), 5000, '请求体');
  assert.equal(果.文, 'ok');
  assert.equal(甲.记录[0].首块, '第一块');
  assert.equal(甲.记录[0].余, '第二块');
  const 大 = randomBytes(8 * MiB);
  const 乙 = new 服务(async (请, 记) => { const 体 = Buffer.from(await 请.arrayBuffer()); 记.摘 = 摘(体); 记.长 = 体.length; return new Response('done'); });
  assert.equal((await 简(筛('API', '/big', [], []), {体: new Blob([大]).stream(), 环境: {API: 乙}})).文, 'done');
  assert.equal(乙.记录[0].长, 8 * MiB);
  assert.equal(乙.记录[0].摘, 摘(大));
});

test('筛头转发：响应流原样交回且不缓冲，3xx 不跟随', async () => {
  let 放行;
  const 等 = new Promise(完成 => { 放行 = 完成; });
  const 甲 = new 服务(async () => new Response(new ReadableStream({
    async start(c) { c.enqueue(Buffer.from('首')); await 等; c.enqueue(Buffer.from('尾')); c.close(); }
  }), {status: 200, headers: {'X-Stream': '1'}}));
  const 回 = await 限时(调(筛('API', '/s', [], []), {方法: 'GET', 环境: {API: 甲}}), 5000, '响应头');
  assert.equal(回.headers.get('x-stream'), '1');
  const 读 = 回.body.getReader();
  const 首 = await 限时(读.read(), 3000, '首块');
  assert.equal(Buffer.from(首.value).toString(), '首');
  放行();
  const 尾 = await 读.read();
  assert.equal(Buffer.from(尾.value).toString(), '尾');
  const 乙 = new 服务(async () => new Response('moved', {status: 302, headers: {Location: '/elsewhere'}}));
  const 跳 = await 简(筛('API', '/r', [], []), {方法: 'GET', 环境: {API: 乙}});
  assert.equal(跳.状态, 302);
  assert.deepEqual(跳.头.filter(项 => 项[0] === 'location'), [['location', '/elsewhere']]);
  assert.equal(乙.记录.length, 1, '不得跟随重定向');
  for (const 状态 of [201, 404, 500, 503]) {
    const 丙 = new 服务(async () => new Response('s' + 状态, {status: 状态}));
    assert.equal((await 简(筛('API', '/x', [], []), {方法: 'GET', 环境: {API: 丙}})).状态, 状态);
  }
});

test('筛头转发：保留名与固定头的校验', async () => {
  const 甲 = new 服务();
  const 试 = (keep, fixed, 选项 = {}) => 简(筛('API', '/x', keep, fixed), {方法: 'GET', 环境: {API: 甲}, ...选项});
  assert.equal((await 试(['Content-Length', 'content-type'], [['Content-Type', 'application/json']], {方法: 'POST', 体: 'abc', 头: {'content-length': '3', 'content-type': 'text/plain'}})).状态, 202);
  assert.equal(甲.记录.at(-1).头['content-length'], '3', '可显式保留 Content-Length');
  assert.equal(甲.记录.at(-1).头['content-type'], 'application/json', '固定头覆盖');
  for (const 名 of ['Host', 'Connection', 'Keep-Alive', 'Proxy-Connection', 'Proxy-Authorization', 'TE', 'Trailer', 'Transfer-Encoding', 'Upgrade', 'Expect']) for (const 形 of [名, 名.toLowerCase(), 名.toUpperCase()]) {
    assert.match(错(await 试([形], [])), /由宿主管理，不得保留/, 形);
    assert.match(错(await 试([], [[形, 'x']])), /由宿主管理，不得固定/, 形);
  }
  for (const 名 of ['Content-Length', 'content-length', 'Set-Cookie', 'SET-COOKIE']) assert.match(错(await 试([], [[名, '1']])), /由宿主管理，不得固定/, 名);
  for (const 坏 of ['', 'a b', 'a:b', 'a\r\nb', '名', 'a'.repeat(65)]) {
    assert.match(错(await 试([坏], [])), /转发保留标头名称无效/, JSON.stringify(坏));
    assert.match(错(await 试([], [[坏, 'v']])), /转发固定标头名称无效/, JSON.stringify(坏));
  }
  for (const 值 of ['a\rb', 'a\nb', 'a\r\nX: y', 'a\u0000b', 'a\u0001', 'a\u007f', '甲', 'a'.repeat(8193)]) assert.match(错(await 试([], [['X-V', 值]])), /转发固定标头值无效/, JSON.stringify(值).slice(0, 40));
  assert.match(错(await 试(['X-A', 'x-a'], [])), /转发保留标头名称重复/);
  assert.match(错(await 试([], [['X-A', '1'], ['x-a', '2']])), /转发固定标头名称重复/);
  assert.match(错(await 试([], [['Cookie', 'a=1'], ['cookie', 'b=2']])), /转发固定标头名称重复/, '单个 Cookie');
  assert.equal((await 试([], [['Cookie', 'a=b; c=d'], ['Authorization', 'Bearer x'], ['X-Empty', '']])).状态, 202);
  assert.deepEqual(甲.记录.at(-1).头.cookie, 'a=b; c=d');
  const 十六 = Array.from({length: 16}, (_, i) => 'X-K' + i);
  assert.equal((await 试(十六, Array.from({length: 16}, (_, i) => ['X-F' + i, 'v']))).状态, 202);
  assert.match(错(await 试(十六.concat(['X-Extra']), [])), /转发保留标头过多/);
  assert.match(错(await 试([], Array.from({length: 17}, (_, i) => ['X-F' + i, 'v']))), /转发固定标头过多/);
  assert.equal((await 试(['X-Absent'], [])).状态, 202);
  assert.equal(Object.keys(甲.记录.at(-1).头).length, 0);
});

test('筛头转发：入站保留头含控制字符时拒绝转发', async () => {
  const 甲 = new 服务();
  const 头 = new Headers();
  try { 头.set('X-Bad', 'a\u0001b'); } catch { return; }
  const 请 = new Request('https://x.test/orig?' + 查询(筛('API', '/x', ['X-Bad'], [])), {headers: 头});
  const 回 = await 宿主.fetch(请, {API: 甲}, 上下文);
  const 文 = await 回.text();
  assert.match(错({状态: 回.status, 文}), /入站标头值含控制字符/);
  assert.equal(甲.记录.length, 0);
});

test('转发路径校验：单斜起首，无控字符、反斜线、片段，至多 8192 字节', async () => {
  const 甲 = new 服务();
  for (const 径 of ['/', '/x', '/x?y=1', '/a//b', '/a?b=//c', '/中文/路径?键=值', '/a%2Fb', '/' + 'a'.repeat(8191)]) {
    const 果 = await 简(筛('API', 径, [], []), {方法: 'GET', 环境: {API: 甲}});
    assert.equal(果.状态, 202, JSON.stringify(径).slice(0, 40) + ' ' + 果.文.slice(0, 60));
    assert.equal(new URL(甲.记录.at(-1).网址).origin, 'https://x.test');
    assert.equal(甲.记录.at(-1).网址, new URL(径, 'https://x.test').href, JSON.stringify(径).slice(0, 40));
  }
  for (const 径 of ['', 'x', './x', '../x', '?q=1', '//evil.example', '//evil.example/x', '/\\evil.example', '/a\\b', '/a#b', '/a\r\nb', '/a\nb', '/a\u0000b', '/a\u0001b', '/a\u007fb', '/' + 'a'.repeat(8192)]) {
    assert.match(错(await 简(筛('API', 径, [], []), {方法: 'GET', 环境: {API: 甲}})), /转发路径无效/, JSON.stringify(径).slice(0, 40));
  }
});

test('转发绑定：空名、未授权名与上游失败', async () => {
  const 甲 = new 服务();
  assert.match(错(await 简(筛('', '/x', [], []), {方法: 'GET', 环境: {API: 甲}})), /转发绑定名为空/);
  await assert.rejects(简(筛('NOPE', '/x', [], []), {方法: 'GET', 环境: {NOPE: 甲}}), /未授权的SERVICE绑定：NOPE/);
  await assert.rejects(简(筛('OTHER', '/x', [], []), {方法: 'GET', 环境: {}}), /绑定不存在：OTHER/);
  const 坏 = new 服务(async () => { throw new TypeError('上游连接被拒绝'); });
  assert.match(错(await 简(筛('API', '/x', [], []), {方法: 'GET', 环境: {API: 坏}})), /转发到服务失败：TypeError: 上游连接被拒绝/);
});

test('转发绑定：已读取的请求体不可转发（可捕获失败）', async () => {
  const 甲 = new 服务();
  const 已用 = new Request('https://x.test/orig?' + 查询(筛('API', '/x', [], [])), {method: 'POST', body: 'abc'});
  await 已用.arrayBuffer();
  const 回 = await 宿主.fetch(已用, {API: 甲}, 上下文);
  const 文 = await 回.text();
  assert.match(错({状态: 回.status, 文}), /入站请求正文已被读取，无法转发/);
  assert.equal(甲.记录.length, 0);
});

const 静 = (svc, 径, 状态, hdrs) => ({op: 'assets', svc, path: 径, status: String(状态), hdrs: JSON.stringify(hdrs)});

test('静态资源：沿用状态与标头，覆盖并追加附加标头，正文原样', async () => {
  const 乙 = new 资产({
    '/a.html': {体: '<html>甲</html>', 头: {'Content-Type': 'text/html', 'Cache-Control': 'max-age=60', ETag: '"1"', 'Set-Cookie': 'keep=1', 'X-Asset': 'yes'}},
    '/gone.html': {体: '没了', 状态: 410, 头: {'Content-Type': 'text/plain'}}
  });
  const 甲 = await 简(静('ASSETS', '/a.html', 0, []), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(甲.状态, 200);
  assert.equal(甲.文, '<html>甲</html>');
  assert.deepEqual(甲.头, [['cache-control', 'max-age=60'], ['content-type', 'text/html'], ['etag', '"1"'], ['set-cookie', 'keep=1'], ['x-asset', 'yes']]);
  assert.deepEqual(乙.记录.at(-1), {网址: 'https://x.test/a.html', 类型: 'string'});
  const 覆 = await 简(静('ASSETS', '/a.html', 203, [['Cache-Control', 'no-store'], ['X-New', '1'], ['x-new', '2'], ['Set-Cookie', 'a=1'], ['Set-Cookie', 'b=2']]), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(覆.状态, 203);
  assert.equal(覆.文, '<html>甲</html>');
  assert.deepEqual(覆.头, [['cache-control', 'no-store'], ['content-type', 'text/html'], ['etag', '"1"'], ['set-cookie', 'a=1'], ['set-cookie', 'b=2'], ['x-asset', 'yes'], ['x-new', '1, 2']]);
  const 旧状态 = await 简(静('ASSETS', '/gone.html', 0, []), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(旧状态.状态, 410);
  const 改 = await 简(静('ASSETS', '/gone.html', 200, []), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(改.状态, 200);
  assert.equal(改.文, '没了');
  const 缺 = await 简(静('ASSETS', '/missing.html', 0, []), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(缺.状态, 404);
  const 缺改 = await 简(静('ASSETS', '/missing.html', 200, [['X-N', '1']]), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(缺改.状态, 200);
});

test('静态资源：HEAD 不带正文并取消资源正文流；正文原样流出', async () => {
  const 取消 = [];
  const 造 = () => new Response(new ReadableStream({start(c) { c.enqueue(Buffer.from('资源体')); c.close(); }, cancel(原因) { 取消.push(原因); }}), {headers: {'Content-Type': 'text/plain', 'Content-Length': '9'}});
  const 乙 = new 资产({'/s.txt': 造});
  const 头 = await 简(静('ASSETS', '/s.txt', 0, [['X-A', '1']]), {方法: 'HEAD', 环境: {ASSETS: 乙}});
  assert.equal(头.状态, 200);
  assert.equal(头.文, '');
  assert.deepEqual(头.头, [['content-length', '9'], ['content-type', 'text/plain'], ['x-a', '1']]);
  assert.equal(取消.length, 1, 'HEAD 须取消资源正文流');
  const 取 = await 简(静('ASSETS', '/s.txt', 0, []), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(取.文, '资源体');
  let 放行;
  const 等 = new Promise(完成 => { 放行 = 完成; });
  const 流资产 = new 资产({'/big': () => new Response(new ReadableStream({async start(c) { c.enqueue(Buffer.from('首')); await 等; c.enqueue(Buffer.from('尾')); c.close(); }}))});
  const 回 = await 限时(调(静('ASSETS', '/big', 0, []), {方法: 'GET', 环境: {ASSETS: 流资产}}), 5000, '静态资源响应头');
  const 读 = 回.body.getReader();
  assert.equal(Buffer.from((await 限时(读.read(), 3000, '静态资源首块')).value).toString(), '首');
  放行();
  assert.equal(Buffer.from((await 读.read()).value).toString(), '尾');
});

test('静态资源：状态与正文的兼容性，继承的无正文状态', async () => {
  const 乙 = new 资产({'/a.html': {体: 'x', 头: {'Content-Type': 'text/html'}}, '/nm': {状态: 304, 头: {ETag: '"1"'}}, '/moved': {状态: 307, 头: {Location: '/a.html'}}});
  for (const 状态 of [204, 205, 304]) {
    assert.match(错(await 简(静('ASSETS', '/a.html', 状态, []), {方法: 'GET', 环境: {ASSETS: 乙}})), /该 HTTP 状态不得带静态资源正文/, String(状态));
    const 头 = await 简(静('ASSETS', '/a.html', 状态, []), {方法: 'HEAD', 环境: {ASSETS: 乙}});
    assert.equal(头.状态, 状态, 'HEAD 不带体，故允许');
  }
  const 继承 = await 简(静('ASSETS', '/nm', 0, []), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(继承.状态, 304);
  assert.equal(继承.文, '');
  const 跳 = await 简(静('ASSETS', '/moved', 0, []), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(跳.状态, 307);
  assert.deepEqual(跳.头.filter(项 => 项[0] === 'location'), [['location', '/a.html']]);
  for (const 状态 of [-1, 1, 99, 100, 199, 600, 99999]) assert.match(错(await 简(静('ASSETS', '/a.html', 状态, []), {方法: 'GET', 环境: {ASSETS: 乙}})), /静态资源状态须为零或 200 至 599/, String(状态));
});

test('静态资源：路径、绑定名与附加标头校验；资源自带的内容类型不可覆盖', async () => {
  const 乙 = new 资产({'/a.html': {体: 'x', 头: {'Content-Type': 'text/html'}}});
  const 试 = (径, hdrs = [], svc = 'ASSETS') => 简(静(svc, 径, 0, hdrs), {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.match(错(await 试('/a.html', [], '')), /静态资源绑定名为空/);
  for (const 径 of ['', 'a.html', '//evil.example/a.html', '/a\\b', '/a#b', '/a\r\nb', '/a\u0000', '/' + 'a'.repeat(8192)]) assert.match(错(await 试(径)), /静态资源路径无效/, JSON.stringify(径).slice(0, 30));
  for (const 名 of ['Content-Length', 'Transfer-Encoding', 'Connection', 'Keep-Alive', 'Upgrade', 'TE', 'Trailer', 'Content-Type', 'Content-Encoding']) for (const 形 of [名, 名.toLowerCase()]) assert.match(错(await 试('/a.html', [[形, 'x']])), /由宿主或资源管理，不得设置/, 形);
  for (const 名 of ['', 'a b', 'a\r\nb', '名', 'a'.repeat(65)]) assert.match(错(await 试('/a.html', [[名, 'v']])), /响应标头名称无效/, JSON.stringify(名));
  for (const 值 of ['a\rb', 'a\nb', 'a\u0000b', 'a\u0001', '甲', 'a'.repeat(8193)]) assert.match(错(await 试('/a.html', [['X-V', 值]])), /响应标头值无效/, JSON.stringify(值).slice(0, 30));
  assert.match(错(await 试('/a.html', Array.from({length: 65}, (_, i) => ['X-H' + i, 'v']))), /响应标头超过 64 项/);
  assert.equal((await 试('/a.html', Array.from({length: 64}, (_, i) => ['X-H' + i, 'v']))).状态, 200);
  assert.match(错(await 试('/a.html', [['Set-Cookie', 'a=' + 'b'.repeat(4095)]])), /Set-Cookie 值超过 4096 字节/);
  const 果 = await 试('/a.html', [['X-Ok', '1']]);
  assert.deepEqual(果.头.find(项 => 项[0] === 'content-type'), ['content-type', 'text/html']);
});

test('静态资源：绑定授权与上游失败', async () => {
  const 乙 = new 资产({'/a.html': {体: 'x'}});
  await assert.rejects(简(静('NOPE', '/a.html', 0, []), {方法: 'GET', 环境: {NOPE: 乙}}), /未授权的ASSETS绑定：NOPE/);
  await assert.rejects(简(静('ASSETS', '/a.html', 0, []), {方法: 'GET', 环境: {}}), /绑定不存在：ASSETS/);
  class 坏资产 { async fetch() { throw new Error('资源读取炸了'); } }
  assert.match(错(await 简(静('ASSETS', '/a.html', 0, []), {方法: 'GET', 环境: {ASSETS: new 坏资产()}})), /读取静态资源失败：Error: 资源读取炸了/);
});

test('旧函数回归：转发入站请求、并加标头、重定向入站请求', async () => {
  const 甲 = new 服务();
  const 果 = await 简({op: 'old', svc: 'API', path: '/new/path'}, {方法: 'POST', 体: 'body', 头: {'X-In': 'v', Authorization: 'a'}, 环境: {API: 甲}});
  assert.equal(果.状态, 202);
  assert.equal(甲.记录[0].方法, 'POST');
  assert.equal(new URL(甲.记录[0].网址).pathname, '/new/path');
  assert.equal(甲.记录[0].头['x-in'], 'v');
  assert.equal(甲.记录[0].头.authorization, 'a');
  const 乙 = new 资产({'/a.html': {体: 'asset'}});
  const 资 = await 简({op: 'old', svc: 'ASSETS', path: ''}, {方法: 'GET', 环境: {ASSETS: 乙}});
  assert.equal(乙.记录.at(-1).类型, 'object', '旧函数以 Request 访问 ASSETS');
  const 丙 = new 服务(async () => new Response('页面', {headers: {'Content-Type': 'text/plain', 'Set-Cookie': 'x=1', 'X-Keep': '1'}}));
  const 加 = await 简({op: 'oldhdr', svc: 'API', path: '', hdrs: [['Content-Security-Policy', "default-src 'none'"], ['X-Content-Type-Options', 'nosniff']]}, {方法: 'GET', 环境: {API: 丙}});
  assert.deepEqual(加.头.filter(项 => ['content-security-policy', 'x-content-type-options', 'set-cookie', 'x-keep'].includes(项[0])).sort(), [['content-security-policy', "default-src 'none'"], ['set-cookie', 'x=1'], ['x-content-type-options', 'nosniff'], ['x-keep', '1']]);
  assert.match(错(await 简({op: 'oldhdr', svc: 'API', path: '', hdrs: [['Set-Cookie', 'x=2']]}, {方法: 'GET', 环境: {API: 丙}})), /响应标头名称不允许覆盖/);
  const 跳 = await 简({op: 'redir', path: '/to'}, {方法: 'GET', 环境: {}});
  assert.equal(跳.状态, 308);
  assert.deepEqual(跳.头.filter(项 => 项[0] === 'location'), [['location', 'https://x.test/to?z=1&op=redir&path=%2Fto']]);
  assert.match(错(await 简({op: 'redir', path: '//x'}, {方法: 'GET', 环境: {}})), /重定向路径无效/);
});

// 确定性伪随机（mulberry32），便于复现。
function 随机器(种子) { let a = 种子 >>> 0; return () => { a = (a + 0x6D2B79F5) >>> 0; let t = a; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; }; }

const 令牌正则 = /^[!#$%&'*+.^_`|~0-9A-Za-z-]+$/;
const 路径可用 = 径 => {
  const 长 = Buffer.byteLength(径);
  if (长 < 1 || 长 > 8192 || 径[0] !== '/' || 径[1] === '/' || 径[1] === '\\') return false;
  if (/\/(?:\.|%2e){1,2}(?=[\/?]|$)/i.test(径)) return false;
  return ![...径].some(字 => { const 码 = 字.codePointAt(0); return 码 < 32 || 码 === 127 || 字 === '\\' || 字 === '#'; });
};
// 文言：点段（含百分号编码者）能使路径出前缀，一律拒之；形似而非点段者不误拒。汉语：/public/../secret 这类路径经 URL 解析会越出前缀，转发与静态资源都须拒绝 . 与 .. 段（%2e 任意大小写）。
test('点段路径被拒而形似点段者放行', async () => {
  const 甲 = new 服务();
  const 乙 = new 资产({});
  for (const 径 of ['/public/../secret/s.txt', '/a/%2e%2e/b', '/a/%2E./b', '/a/.%2e/b', '/a/%2e%2E?x=1', '/a/..?x=1', '/a/.', '/a/..', '/a/./b', '/%2e/x']) {
    const 果 = await 简(筛('API', 径, [], []), {方法: 'GET', 环境: {API: 甲}});
    assert.match(错(果), /转发路径无效/, '转发 ' + 径);
    const 资 = await 简(静('ASSETS', 径, 0, []), {方法: 'GET', 环境: {ASSETS: 乙}});
    assert.match(错(资), /静态资源路径无效/, '静态 ' + 径);
  }
  for (const 径 of ['/a/.../b', '/a/..b', '/a/.b/c', '/a/b.', '/a/%2ex/b', '/a/x%2e/b', '/a/%2e%2e%2e', '/.hidden', '/a/b..c']) {
    const 果 = await 简(筛('API', 径, [], []), {方法: 'GET', 环境: {API: 甲}});
    assert.equal(果.状态, 202, '转发 ' + 径 + ' ' + 果.文.slice(0, 60));
  }
});
test('差分模糊：随机转发路径与静态资源路径的接受与拒绝与参考实现一致', async () => {
  const 随 = 随机器(20260928);
  const 片 = ['/', 'a', 'b', '?', '&', '=', '%', '.', '..', '#', '\\', ' ', '\t', '\u0001', '\u007f', '\u0000', 'é', '你', '🙂', '//', '/x/', 'q=1'];
  const 甲 = new 服务();
  const 乙 = new 资产({});
  let 接受 = 0, 拒绝 = 0;
  for (let i = 0; i < 2500; i++) {
    let 径 = '/';
    const 段数 = Math.floor(随() * 7);
    for (let j = 0; j < 段数; j++) 径 += 片[Math.floor(随() * 片.length)];
    if (i % 6 === 0) 径 = 径.slice(1);
    if (i % 19 === 0) 径 = '/' + 'a'.repeat([8190, 8191, 8192][Math.floor(随() * 3)]);
    const 果 = await 简(筛('API', 径, [], []), {方法: 'GET', 环境: {API: 甲}});
    const 资 = await 简(静('ASSETS', 径, 0, []), {方法: 'GET', 环境: {ASSETS: 乙}});
    if (路径可用(径)) {
      assert.equal(果.状态, 202, '转发 ' + JSON.stringify(径).slice(0, 60) + ' ' + 果.文.slice(0, 60));
      assert.equal(资.状态, 404, '静态 ' + JSON.stringify(径).slice(0, 60) + ' ' + 资.文.slice(0, 60));
      接受++;
    } else {
      assert.match(错(果), /转发路径无效/, JSON.stringify(径).slice(0, 60));
      assert.match(错(资), /静态资源路径无效/, JSON.stringify(径).slice(0, 60));
      拒绝++;
    }
  }
  assert.ok(接受 > 100 && 拒绝 > 100, `样本应两类都足够：接受 ${接受}，拒绝 ${拒绝}`);
});

test('差分模糊：随机保留名与固定头的接受与拒绝与参考实现一致', async () => {
  const 随 = 随机器(20260929);
  const 池 = Array.from("abcXYZ019-_.~!#$%&'*+^`|", c => c).concat([' ', '\t', ':', ';', '"', '\\', '/', 'é', '\u0001']);
  const 随文 = (最长) => { const 长 = Math.floor(随() * 随() * 最长); let s = ''; for (let i = 0; i < 长; i++) s += 池[Math.floor(随() * 池.length)]; return s; };
  const 禁请求 = new Set(['host', 'connection', 'keep-alive', 'proxy-connection', 'proxy-authorization', 'te', 'trailer', 'transfer-encoding', 'upgrade', 'expect']);
  const 甲 = new 服务();
  let 接受 = 0, 拒绝 = 0;
  for (let i = 0; i < 2000; i++) {
    let 名 = 随文(10);
    if (i % 5 === 0) 名 = ['Host', 'TE', 'x-a', 'X-A', 'Content-Length', 'Set-Cookie', 'Cookie', 'Authorization'][Math.floor(随() * 8)];
    if (i % 23 === 0) 名 = 'n'.repeat([63, 64, 65][Math.floor(随() * 3)]);
    const 合法名 = Buffer.byteLength(名) >= 1 && Buffer.byteLength(名) <= 64 && 令牌正则.test(名);
    const 小 = 名.toLowerCase();
    // 保留名
    let 期 = !合法名 ? '转发保留标头名称无效' : 禁请求.has(小) ? '由宿主管理，不得保留' : null;
    let 果 = await 简(筛('API', '/x', [名], []), {方法: 'GET', 环境: {API: 甲}});
    if (期 === null) { assert.equal(果.状态, 202, '保留 ' + JSON.stringify(名) + ' ' + 果.文.slice(0, 60)); 接受++; } else { assert.match(错(果), new RegExp(期), '保留 ' + JSON.stringify(名)); 拒绝++; }
    // 固定头
    let 值 = 随文(20);
    if (i % 29 === 0) 值 = 'v'.repeat([8191, 8192, 8193][Math.floor(随() * 3)]);
    const 合法值 = /^[\t\x20-\x7e]*$/.test(值) && Buffer.byteLength(值) <= 8192;
    期 = !合法名 ? '转发固定标头名称无效' : (禁请求.has(小) || 小 === 'content-length' || 小 === 'set-cookie') ? '由宿主管理，不得固定' : !合法值 ? '转发固定标头值无效' : null;
    果 = await 简(筛('API', '/x', [], [[名, 值]]), {方法: 'GET', 环境: {API: 甲}});
    if (期 === null) { assert.equal(果.状态, 202, '固定 ' + JSON.stringify([名, 值]).slice(0, 60) + ' ' + 果.文.slice(0, 60)); 接受++; } else { assert.match(错(果), new RegExp(期), '固定 ' + JSON.stringify([名, 值]).slice(0, 60)); 拒绝++; }
  }
  assert.ok(接受 > 200 && 拒绝 > 200, `样本应两类都足够：接受 ${接受}，拒绝 ${拒绝}`);
});
