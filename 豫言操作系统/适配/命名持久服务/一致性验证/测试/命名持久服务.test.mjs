import {test, after} from 'node:test';
import assert from 'node:assert/strict';
import {编, 解, 睡, 造流, 切块, 拼接, 命名空间桩, 服务桩, 造宿主, 跑} from './桩.mjs';

const 对 = o => Object.entries(o);
const 转义 = 文 => 文.replace(/[\u0080-\uffff]/g, c => '\\u' + c.charCodeAt(0).toString(16).padStart(4, '0'));
// 保活：让期限计时器（AbortSignal.timeout 的定时器不占事件循环）在等待期间不致令进程提前退出。
const 保活 = setInterval(() => {}, 1000);
after(() => clearInterval(保活));
const 块们于 = 文 => [...文.matchAll(/〔([^〕]*)〕/g)].map(m => m[1]);
const 终于 = 文 => { const m = 文.match(/‖状态(\d+):([^‖]*)$/s); return m ? [Number(m[1]), m[2]] : null; };
const 事件 = ['一', '二', '三'].map((字, i) => `id: ${i + 1}\nevent: 进度\ndata: ${JSON.stringify({步: 字})}\n\n`);
const sse全文 = 事件.join('');
const 读全 = async 请求 => { if (!请求.body) return new Uint8Array(); return new Uint8Array(await 请求.arrayBuffer()); };

const 行为 = async (路径, 请求, 对象, 记录) => {
  const 观察 = (记录.观察 = {});
  switch (路径) {
    case '/events': return new Response(造流(切块(编.encode(sse全文), 11), {观察}), {status: 200, headers: {'content-type': 'text/event-stream', 'cache-control': 'no-store', 'x-yuyan-revision': '7'}});
    case '/slow': return new Response(造流(['甲'], {末尾: 'hang', 观察}), {status: 200});
    case '/errmid': return new Response(造流(['丙'], {末尾: 'error', 观察}), {status: 200});
    case '/hangfetch': return new Promise((_, 拒) => 请求.signal.addEventListener('abort', () => 拒(请求.signal.reason)));
    case '/throws': throw new Error('对象不可用');
    case '/echo': { const 体 = await 读全(请求); 记录.正文 = 体; return new Response(JSON.stringify({方法: 请求.method, 正文长: 体.length}), {status: 200, headers: {'content-type': 'application/json; charset=utf-8'}}); }
    case '/read': { const n = Number(new URL(请求.url).searchParams.get('n') ?? 0); const 文 = 'a'.repeat(n); return new Response(文, {status: 200, headers: {'content-type': 'text/plain', location: '/l', 'set-cookie': 's=1', 'x-yuyan-revision': '9', 'x-secret': 'do-not-leak', 'content-length': String(n)}}); }
    case '/readnolen': { const n = Number(new URL(请求.url).searchParams.get('n') ?? 0); return new Response(造流(切块(编.encode('b'.repeat(n)), 65536), {间隔: 0, 观察}), {status: 200}); }
    case '/bad8': return new Response(Uint8Array.of(0x61, 0xFF, 0xE8, 0xB1), {status: 200});
    case '/bom': return new Response(Uint8Array.of(0xEF, 0xBB, 0xBF, 0x78), {status: 200});
    case '/head': return new Response(null, {status: 204, headers: {'x-yuyan-revision': '3'}});
    case '/fail': return new Response('坏了', {status: 503, headers: {'content-type': 'text/plain; charset=utf-8'}});
    case '/upload': {
      const 读 = 请求.body.getReader(); let 块数 = 0, 总 = 0;
      for (;;) { const {done, value} = await 读.read(); if (done) break; 块数++; 总 += value.length; 对象.首块?.(); }
      return new Response(JSON.stringify({块数, 总}), {status: 200});
    }
    case '/sse-slow': {
      const 观察2 = 记录.观察;
      let i = 0;
      return new Response(new ReadableStream({
        async pull(c) { await 睡(80); if (i < 4) { 观察2.发出时刻 = [...(观察2.发出时刻 ?? []), Date.now()]; c.enqueue(编.encode(`data: ${i++}\n\n`)); } else c.close(); },
        cancel(r) { 观察2.取消 = String(r?.name ?? r); }
      }, {highWaterMark: 0}), {status: 207, headers: {'content-type': 'text/event-stream', 'x-yuyan-revision': '12'}});
    }
    default: return new Response('未知路径', {status: 404});
  }
};
const 造 = () => new 命名空间桩(行为);
const 流 = (述, 空间, 宿主) => 跑({op: 'do', timeout: 5000, ...述}, {DOS: 空间}, 宿主);
const 文字 = (述, 空间, 宿主) => 跑({op: 'dotext', ...述}, {DOS: 空间}, 宿主);

test('流：增量读取 SSE（事件 11 字节切块），状态、类型与版本头可读', async () => {
  const 空间 = 造();
  const 果 = await 流({url: 'https://internal/events', read: 'sse', header: 'x-yuyan-revision'}, 空间);
  assert.equal(果.状态, 200, 果.文);
  assert.match(果.文, /^状态码=200‖头=7‖文=/);
  const 记 = 空间.对象们.get('room1').请求们[0];
  assert.equal(记.方法, 'GET'); assert.equal(记.路径, '/events'); assert.equal(记.有信号, true); assert.equal(记.转址, 'manual');
  assert.deepEqual(空间.取名们, ['room1']);
  const 分块 = await 流({url: 'https://internal/events', read: 'chunks', max: 65536}, 造());
  assert.equal(块们于(分块.文).join(''), sse全文.replaceAll('\n', '<LF>'));
  assert.deepEqual(终于(分块.文), [1, '']);
});

test('流：查询由 URLSearchParams 编码；标头与 POST 正文（任意字符）逐字节送达', async () => {
  const 空间 = 造();
  const 正文 = '甲"乙\\丙\n丁\t戊😀';
  const 果 = await 流({url: 'https://asset/echo', method: 'POST', query: 对({a: '1 2', b: '中文&=', c: ''}), headers: 对({'Origin': 'https://x.test', 'Content-Type': 'application/json', 'Cookie': 'sid=abc'}), body: 正文, read: 'chunks'}, 空间);
  assert.equal(果.状态, 200, 果.文);
  const 记 = 空间.对象们.get('room1').请求们[0];
  assert.equal(记.网址, 'https://asset/echo?a=1+2&b=%E4%B8%AD%E6%96%87%26%3D&c=');
  assert.deepEqual(记.头, {origin: 'https://x.test', 'content-type': 'application/json', cookie: 'sid=abc'});
  assert.equal(解.decode(记.正文), 正文);
  assert.deepEqual(块们于(果.文).join(''), '{"方法":"POST","正文长":' + 编.encode(正文).length + '}');
});

test('流：参数违规抛事故且不发请求', async () => {
  const 空间 = 造();
  const 试 = async (述, 期) => { const 果 = await 流({url: 'https://internal/events', read: 'none', ...述}, 空间); assert.equal(果.状态, 400, JSON.stringify(述).slice(0, 80)); assert.match(果.文, 期, JSON.stringify(述).slice(0, 80)); };
  await 试({object: ''}, /持久服务对象名无效/); await 试({object: 'a b'}, /持久服务对象名无效/); await 试({object: 'a'.repeat(129)}, /持久服务对象名无效/);
  await 试({object: '房间'}, /持久服务对象名无效/); await 试({object: '../x'}, /持久服务对象名无效/);
  await 试({url: 'https://evil.example.com/x'}, /持久服务内部网址无效/); await 试({url: 'http://internal/x'}, /持久服务内部网址无效/); await 试({url: 'https://internal.evil.com/x'}, /持久服务内部网址无效/);
  await 试({url: 'https://internal//x'}, /持久服务路径无效/); await 试({url: 'https://internal/x?y=1'}, /持久服务路径无效/); await 试({url: 'https://internal/x#f'}, /持久服务路径无效/);
  await 试({url: 'https://internal/x\\y'}, /持久服务路径无效/); await 试({url: 'https://internal/x\ty'}, /持久服务路径无效/);
  await 试({method: 'PUT'}, /持久服务方法无效/); await 试({method: 'get'}, /持久服务方法无效/);
  await 试({query: Array.from({length: 17}, (_, i) => ['k' + i, 'v'])}, /持久服务查询参数过多/);
  await 试({headers: 对({Authorization: 'x'})}, /持久服务请求头不允许/); await 试({headers: 对({origin: 'x'})}, /持久服务请求头不允许/);
  await 试({headers: [['Origin', 'a'], ['Origin', 'b']]}, /持久服务请求头重复/);
  await 试({headers: 对({Origin: 'a\nb'})}, /持久服务请求头值无效/); await 试({headers: 对({Cookie: 'x'.repeat(8193)})}, /持久服务请求头值无效/);
  await 试({method: 'GET', body: 'x'}, /GET\/HEAD 不得带正文/); await 试({method: 'HEAD', body: 'x'}, /GET\/HEAD 不得带正文/);
  await 试({method: 'POST', bodyPow: 23, bodyExtra: 'b'}, /持久服务正文超过 8 MiB/);
  await 试({timeout: 0}, /持久服务超时毫秒须在 1 至 900000/); await 试({timeout: 900001}, /持久服务超时毫秒须在 1 至 900000/);
  assert.equal(空间.对象们.size, 0, '违规不得触及对象');
  const 重复 = await 跑({op: 'do', url: 'https://internal/x', read: 'none', headers: 对({Origin: 'a'})}, {DOS: 空间}); assert.equal(重复.状态, 200);
});

test('流：未授权的持久对象绑定报部署错误', async () => {
  await assert.rejects(跑({op: 'do', binding: 'OTHER', url: 'https://internal/events', read: 'none'}, {DOS: 造()}), /未授权的DO绑定：OTHER/);
});

test('流：超时（读停滞、等响应头）、错误、取消、有限读取', async () => {
  const 空间 = 造();
  const 甲 = await 流({url: 'https://internal/slow', read: 'chunks', timeout: 300}, 空间);
  assert.deepEqual(块们于(甲.文), ['甲']); assert.deepEqual(终于(甲.文), [3, '超时']);
  await 睡(30); assert.deepEqual(空间.对象们.get('room1').请求们.at(-1).观察.取消, ['TimeoutError']);
  const 乙 = await 流({url: 'https://internal/hangfetch', read: 'chunks', timeout: 200}, 空间);
  assert.match(乙.文, /^请求失败:持久服务请求失败或超时：.*TimeoutError/);
  const 丙 = await 流({url: 'https://internal/errmid', read: 'chunks'}, 空间);
  assert.deepEqual(块们于(丙.文), ['丙']); assert.deepEqual(终于(丙.文), [2, 'Error: 上游炸了']);
  const 丁 = await 流({url: 'https://internal/slow', read: 'cancel'}, 空间);
  assert.equal(丁.文, '状态码=200‖头=‖首块状态0:甲‖再读状态3:已取消‖又读状态3:已取消');
  await 睡(30); assert.deepEqual(空间.对象们.get('room1').请求们.at(-1).观察.取消, ['已取消']);
  const 戊 = await 流({url: 'https://internal/events', read: 'limited', limit: 20}, 空间);
  assert.match(戊.文, /‖有限状态1:字节0:$/);
  const 己 = await 流({url: 'https://internal/fail', read: 'limited', limit: 100}, 空间);
  assert.equal(己.文, '状态码=503‖头=text/plain; charset=utf-8‖有限状态0:字节' + 编.encode('坏了').length + ':坏了');
  const 庚 = await 流({url: 'https://internal/throws', read: 'none'}, 空间);
  assert.match(庚.文, /^请求失败:持久服务请求失败或超时：Error: 对象不可用/);
});

test('文字：状态、正文与四个允许的响应头；其余响应头不外泄；HTTP 错误仍是成功响应', async () => {
  const 空间 = 造();
  const 甲 = await 文字({url: 'https://internal/read', query: 对({n: '5'})}, 空间);
  assert.equal(甲.文, '状态=200‖体字节=5‖体首=aaaaa‖类型=text/plain‖位置=/l‖饼=s=1‖版=9');
  const 乙 = await 文字({url: 'https://internal/fail'}, 空间);
  assert.match(乙.文, /^状态=503‖体字节=6‖体首=坏了‖类型=text\/plain; charset=utf-8‖位置=‖饼=‖版=$/);
  const 丙 = await 文字({url: 'https://internal/head', method: 'HEAD'}, 空间);
  assert.equal(丙.文, '状态=204‖体字节=0‖体首=‖类型=‖位置=‖饼=‖版=3');
  const 丁 = await 文字({url: 'https://internal/bad8'}, 空间);
  assert.match(丁.文, /体字节=7‖体首=a��/, '非法序列按 UTF-8 宽松解码成 U+FFFD，与 Response.text() 一致');
  const 戊 = await 文字({url: 'https://internal/bom'}, 空间);
  assert.match(戊.文, /体字节=1‖体首=x/, 'BOM 去掉，与 Response.text() 一致');
});

test('文字：上限提高到 8 MiB——请求正文与响应各至 8 MiB 通过，超一字节抛事故', async () => {
  const 空间 = 造();
  const 大读 = await 文字({url: 'https://internal/read', query: 对({n: '5000000'})}, 空间);
  assert.match(大读.文, /^状态=200‖体字节=5000000‖/);
  const 满 = await 文字({url: 'https://internal/read', query: 对({n: '8388608'})}, 空间);
  assert.match(满.文, /^状态=200‖体字节=8388608‖/);
  const 超 = await 文字({url: 'https://internal/read', query: 对({n: '8388609'})}, 空间);
  assert.equal(超.状态, 400); assert.match(超.文, /持久服务响应超过 8 MiB/);
  const 无长超 = await 文字({url: 'https://internal/readnolen', query: 对({n: '8388609'})}, 空间);
  assert.equal(无长超.状态, 400); assert.match(无长超.文, /持久服务响应超过 8 MiB/);
  const 大写 = await 文字({url: 'https://internal/echo', method: 'POST', bodyPow: 23}, 空间);
  assert.equal(大写.状态, 200, 大写.文.slice(0, 200));
  assert.equal(空间.对象们.get('room1').请求们.at(-1).正文.length, 8388608);
  const 超写 = await 文字({url: 'https://internal/echo', method: 'POST', bodyPow: 23, bodyExtra: 'b'}, 空间);
  assert.equal(超写.状态, 400); assert.match(超写.文, /持久服务正文超过 8 MiB/);
});

test('文字：对象抛错转为可捕获的事故（不再是宿主崩溃）；旧的校验依旧', async () => {
  const 空间 = 造();
  const 果 = await 文字({url: 'https://internal/throws'}, 空间);
  assert.equal(果.状态, 400); assert.match(果.文, /持久服务请求失败：Error: 对象不可用/);
  const 甲 = await 文字({object: 'a b', url: 'https://internal/read'}, 空间);
  assert.match(甲.文, /持久服务对象名无效/);
  const 乙 = await 文字({url: 'https://internal/read', headers: 对({Authorization: 'x'})}, 空间);
  assert.match(乙.文, /持久服务请求头不允许/);
});

// —— 转发入站请求 ——
const 转 = (cfg, 初始 = {}) => ({method: 'GET', ...初始, headers: {'x-cfg': 转义(JSON.stringify(cfg)), ...(初始.headers ?? {})}});
const 发 = (宿主, 空间, 初始, cfg) => 宿主.fetch(new Request('https://x.test/fwd', 转(cfg, 初始)), {DOS: 空间, SVC: new 服务桩(async (路径, 请求) => { await 请求.arrayBuffer(); return new Response('c'); })});
const 基 = {binding: 'DOS', object: 'room1', url: 'https://internal/events'};

test('转发：DO 的 SSE 响应原样直通，逐事件到达（不缓冲）；状态与头保留', async () => {
  const 空间 = 造(); const 宿主 = 造宿主();
  const 回 = await 发宿主(宿主, 空间, {}, {...基, url: 'https://internal/sse-slow'});
  assert.equal(回.status, 207); assert.equal(回.headers.get('content-type'), 'text/event-stream'); assert.equal(回.headers.get('x-yuyan-revision'), '12');
  const 读 = 回.body.getReader(); const 到 = [];
  for (;;) { const {done, value} = await 读.read(); if (done) break; 到.push([Date.now(), 解.decode(value)]); }
  assert.deepEqual(到.map(x => x[1]), ['data: 0\n\n', 'data: 1\n\n', 'data: 2\n\n', 'data: 3\n\n']);
  assert.ok(到[3][0] - 到[0][0] >= 150, '事件应随 DO 发出逐个到达，而不是一次到齐：' + (到[3][0] - 到[0][0]));
});
const 发宿主 = 发;

test('转发：只转发所列且在白名单内的入站标头，另加固定标头；其余（Host、X-Cfg、任意他头）不转', async () => {
  const 空间 = 造(); const 宿主 = 造宿主();
  const 回 = await 发(宿主, 空间, {method: 'POST', body: '{"k":1}', headers: {cookie: 'sid=1', origin: 'https://x.test', 'content-type': 'application/json', authorization: 'Bearer t', 'x-drop-me': '1', 'accept-language': 'zh', 'last-event-id': '42', 'x-yuyan-session': 's9'}}, {...基, url: 'https://internal/echo', keep: ['Cookie', 'Origin', 'content-type', 'Last-Event-ID', 'X-Yuyan-Session', 'If-None-Match'], fixed: {'X-Yuyan-Route': 'r1', Accept: 'application/json'}});
  assert.equal(回.status, 200);
  assert.deepEqual(await 回.json(), {方法: 'POST', 正文长: 7});
  const 记 = 空间.对象们.get('room1').请求们[0];
  assert.deepEqual(记.头, {cookie: 'sid=1', origin: 'https://x.test', 'content-type': 'application/json', 'last-event-id': '42', 'x-yuyan-session': 's9', 'x-yuyan-route': 'r1', accept: 'application/json'});
  assert.equal(记.转址, 'manual'); assert.equal(记.方法, 'POST'); assert.equal(记.网址, 'https://internal/echo');
});

test('转发：查询参数编码；GET/HEAD 不带正文；方法原样', async () => {
  const 空间 = 造(); const 宿主 = 造宿主();
  await 发(宿主, 空间, {method: 'GET'}, {...基, url: 'https://asset/echo', query: {path: '/a b', 页: '商品'}});
  await 发(宿主, 空间, {method: 'HEAD'}, {...基, url: 'https://asset/echo'});
  const 记们 = 空间.对象们.get('room1').请求们;
  assert.equal(记们[0].方法, 'GET'); assert.equal(记们[0].网址, 'https://asset/echo?path=%2Fa+b&%E9%A1%B5=%E5%95%86%E5%93%81');
  assert.equal(记们[1].方法, 'HEAD'); assert.equal(记们[1].网址, 'https://asset/echo');
});

test('转发：请求正文是流——DO 收到首块时客户端尚未发完（缓冲整个正文会死锁）', async () => {
  const 空间 = 造(); const 宿主 = 造宿主();
  const 对象 = 空间.getByName('room1');
  const 首块到 = new Promise(r => { 对象.首块 = r; });
  let 放行; const 门 = new Promise(r => { 放行 = r; }); let 状态 = 0;
  const 体 = new ReadableStream({ async pull(c) { if (状态 === 0) { 状态 = 1; c.enqueue(编.encode('第一块')); } else { await 门; c.enqueue(编.encode('第二块')); c.close(); } } }, {highWaterMark: 0});
  const 回P = 宿主.fetch(new Request('https://x.test/fwd', {method: 'POST', duplex: 'half', body: 体, headers: {'x-cfg': 转义(JSON.stringify({...基, url: 'https://internal/upload', keep: ['Content-Type']})), 'content-type': 'text/plain'}}), {DOS: 空间});
  await Promise.race([首块到, 睡(3000).then(() => { throw new Error('DO 长久收不到首块：入站正文被缓冲了'); })]);
  放行();
  const 回 = await 回P;
  assert.deepEqual(await 回.json(), {块数: 2, 总: 编.encode('第一块第二块').length});
});

test('转发：违规抛事故（方法、保留名、固定标头、对象名、网址、已读取的正文），不触及对象', async () => {
  const 空间 = 造(); const 宿主 = 造宿主();
  const 试 = async (初始, cfg, 期) => { const 回 = await 发(宿主, 空间, 初始, {...基, ...cfg}); const 文 = await 回.text(); assert.equal(回.status, 400, 文); assert.match(文, 期, JSON.stringify(cfg)); };
  await 试({method: 'DELETE'}, {}, /持久服务方法无效/);
  await 试({method: 'PUT', body: 'x'}, {}, /持久服务方法无效/);
  await 试({}, {keep: ['Host']}, /持久服务转发保留标头不允许/);
  await 试({}, {keep: ['Content-Length']}, /持久服务转发保留标头不允许/);
  await 试({}, {keep: ['Transfer-Encoding']}, /持久服务转发保留标头不允许/);
  await 试({}, {keep: ['X-Forwarded-For']}, /持久服务转发保留标头不允许/);
  await 试({}, {keep: ['Bad Name']}, /持久服务转发保留标头名无效/);
  await 试({}, {keep: ['Cookie', 'cookie']}, /持久服务转发保留标头重复/);
  await 试({}, {keep: Array.from({length: 17}, (_, i) => 'X-Yuyan-' + i)}, /持久服务转发保留标头过多/);
  await 试({}, {fixed: {Cookie: 'x'}}, /持久服务转发固定标头不允许/);
  await 试({}, {keep: ['Origin'], fixed: {Origin: 'https://x.test'}}, /持久服务转发固定标头重复/);
  await 试({}, {fixed: {Accept: 'a', accept: 'b'}}, /持久服务转发固定标头重复/);
  await 试({}, {fixed: {'X-Yuyan-': 'v'}}, /持久服务转发固定标头不允许/);
  await 试({}, {fixed: {Authorization: 'x'}}, /持久服务转发固定标头不允许/);
  await 试({}, {fixed: {Accept: 'a\nb'}}, /持久服务转发固定标头值无效/);
  await 试({}, {fixed: Object.fromEntries(Array.from({length: 9}, (_, i) => ['X-Yuyan-' + i, 'v']))}, /持久服务转发固定标头过多/);
  await 试({}, {object: 'a b'}, /持久服务对象名无效/);
  await 试({}, {url: 'https://evil.example.com/x'}, /持久服务内部网址无效/);
  await 试({}, {url: 'https://internal/x?y'}, /持久服务路径无效/);
  await 试({}, {query: Object.fromEntries(Array.from({length: 17}, (_, i) => ['k' + i, 'v']))}, /持久服务查询参数过多/);
  assert.equal(空间.对象们.size, 0);
  await 试({method: 'POST', body: 'x'}, {preconsume: true}, /入站正文已被读取|入站正文已上锁/);
  const 回 = await 发(宿主, 空间, {}, {...基, url: 'https://internal/throws'});
  assert.equal(回.status, 400); assert.match(await 回.text(), /持久服务转发失败：Error: 对象不可用/);
});

// —— 以流式响应作最终响应（先请后验）——
test('先请后作原答：读得状态 207 后把对象的响应原样作终答，事件逐个到达；状态非 200 则应用自行回 502', async () => {
  const 空间 = 造(); const 宿主 = 造宿主();
  const 回 = await 宿主.fetch(new Request('https://x.test/dopass', {headers: {'x-cfg': 转义(JSON.stringify({url: 'https://internal/sse-slow'}))}}), {DOS: 空间});
  assert.equal(回.status, 502); assert.match(await 回.text(), /对象状态异常：207/);
  const 空间二 = 造();
  空间二.行为 = async (路径, 请求, 对象, 记录) => new Response(造流(['事件甲\n\n', '事件乙\n\n'], {间隔: 40, 观察: {}}), {status: 200, headers: {'content-type': 'text/event-stream', 'x-yuyan-revision': '5'}});
  const 回二 = await 宿主.fetch(new Request('https://x.test/dopass', {headers: {'x-cfg': 转义(JSON.stringify({url: 'https://internal/events'}))}}), {DOS: 空间二});
  assert.equal(回二.status, 200); assert.equal(回二.headers.get('content-type'), 'text/event-stream'); assert.equal(回二.headers.get('x-yuyan-revision'), '5');
  const 读 = 回二.body.getReader(); const 到 = [];
  for (;;) { const {done, value} = await 读.read(); if (done) break; 到.push([Date.now(), 解.decode(value)]); }
  assert.equal(到.map(x => x[1]).join(''), '事件甲\n\n事件乙\n\n');
  assert.ok(到.length >= 2 && 到.at(-1)[0] - 到[0][0] >= 30, '事件应逐个到达');
  const 记 = 空间二.对象们.get('room1').请求们[0];
  assert.equal(记.方法, 'GET'); assert.equal(记.路径, '/events'); assert.equal(记.转址, 'manual');
});

test('先读后作原答：已开始增量读取的柄抛可捕获的事故；作终答后再读得状态 3', async () => {
  const 空间 = 造(); const 宿主 = 造宿主();
  空间.行为 = async () => new Response(造流(['甲', '乙'], {间隔: 5, 观察: {}}), {status: 200});
  const 回 = await 宿主.fetch(new Request('https://x.test/dopass', {headers: {'x-cfg': 转义(JSON.stringify({url: 'https://internal/events', readfirst: true}))}}), {DOS: 空间});
  assert.equal(回.status, 400); assert.match(await 回.text(), /持久服务响应正文已被增量读取，不可再作原答/);
});

test('对象名可含斜线（0.3.0）：账户编号/项目编号按原名定位（文字、流、转发三处）；点、空格、反斜线、非 ASCII、冒号、空串、过长仍抛事故', async () => {
  const 空间 = 造();
  const 名 = '7/3f2a9c1e-4b5d-4e6f-8a7b-9c0d1e2f3a4b';
  const 甲 = await 流({object: 名, url: 'https://internal/events', read: 'none'}, 空间); assert.equal(甲.状态, 200);
  const 乙 = await 文字({object: 名, url: 'https://internal/read', method: 'GET'}, 空间); assert.equal(乙.状态, 200);
  const 丙 = await 发宿主(造宿主(), 空间, {}, {...基, object: 名, url: 'https://internal/events'});
  assert.equal(丙.status, 200); await 丙.arrayBuffer();
  assert.deepEqual(空间.取名们, [名, 名, 名], '宿主以原名 getByName');
  assert.deepEqual([...空间.对象们.keys()], [名]);
  for (const 坏 of ['a b', 'a.b', 'a\\b', '中文', '', 'a'.repeat(129), '7/项目', 'a:b']) {
    const 果 = await 流({object: 坏, url: 'https://internal/events', read: 'none'}, 空间);
    assert.equal(果.状态, 400, JSON.stringify(坏)); assert.match(果.文, /持久服务对象名无效/, JSON.stringify(坏));
  }
  assert.equal(空间.对象们.size, 1, '违规名不得触及对象');
});
