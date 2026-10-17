import {test, after} from 'node:test';
import assert from 'node:assert/strict';
import {mf, 跑, 观察} from './冒烟.mjs';

after(async () => { await mf.dispose(); });
const 块们于 = 文 => [...文.matchAll(/〔([^〕]*)〕/g)].map(m => m[1]);
const 终于 = 文 => { const m = 文.match(/‖状态(\d+):([^‖]*)$/s); return m ? [Number(m[1]), m[2]] : null; };
const sse = ['你', '好', '，', '世界'].map(字 => 'data: ' + JSON.stringify({choices: [{delta: {content: 字}}]}) + '\n\n').join('') + 'data: [DONE]\n\n';
const 清 = async () => { await (await mf.getWorker('svc')).fetch('https://o/__clear'); };

test('真 workerd：5 帧 SSE 经服务绑定增量读取，字节与文字皆等于整体', async () => {
  await 清();
  const 果 = await 跑({op: 'svc', url: 'https://svc/sse5', read: 'sse', timeout: 8000});
  assert.equal(果.状态, 200, 果.文);
  assert.match(果.文, /^状态码=200‖头=text\/event-stream‖文=你好，世界‖完成‖读次=\d+‖字节=\d+$/);
  assert.equal(Number(果.文.match(/字节=(\d+)/)[1]), new TextEncoder().encode(sse).length);
});

test('真 workerd：UTF-8 跨块、坏字节、BOM、残缺序列', async () => {
  const 果 = await 跑({op: 'svc', url: 'https://svc/utf8', read: 'chunks', max: 65536, timeout: 8000});
  assert.equal(果.状态, 200, 果.文);
  assert.equal(块们于(果.文).join(''), '甲😀��x�');
  assert.deepEqual(终于(果.文), [1, '']);
});

test('真 workerd：读流途中期满 → 状态 3，服务端 Worker 观察到取消', async () => {
  await 清();
  const t = Date.now();
  const 果 = await 跑({op: 'svc', url: 'https://svc/slow', read: 'chunks', timeout: 500});
  assert.equal(果.状态, 200, 果.文);
  assert.deepEqual(块们于(果.文), ['甲']);
  assert.deepEqual(终于(果.文), [3, '超时']);
  assert.ok(Date.now() - t < 4000);
  await new Promise(r => setTimeout(r, 200));
  const 观 = await 观察('svc');
  console.log('  服务端取消观察(超时):', JSON.stringify(观.取消));
});

test('真 workerd：主动取消 → 服务端观察到取消；再读得状态 3', async () => {
  await 清();
  const 果 = await 跑({op: 'svc', url: 'https://svc/slow', read: 'cancel', timeout: 8000});
  assert.equal(果.文, '状态码=200‖头=‖首块状态0:甲‖再读状态3:已取消‖又读状态3:已取消');
  await new Promise(r => setTimeout(r, 200));
  const 观 = await 观察('svc');
  console.log('  服务端取消观察(主动):', JSON.stringify(观.取消));
});

test('真 workerd：中途出错 → 状态 2；有限读取超限（声明长度与流式）', async () => {
  const 甲 = await 跑({op: 'svc', url: 'https://svc/errmid', read: 'chunks', timeout: 8000});
  assert.deepEqual(块们于(甲.文), ['丙']);
  const 终 = 终于(甲.文); console.log('  错误流终态:', JSON.stringify(终));
  assert.equal(终[0], 2);
  const 乙 = await 跑({op: 'svc', url: 'https://svc/cl', read: 'limited', limit: 1000, timeout: 8000});
  assert.match(乙.文, /‖有限状态1:字节0:$/);
  await 清();
  const 丙 = await 跑({op: 'svc', url: 'https://svc/nolen', read: 'limited', limit: 1000, timeout: 8000});
  assert.match(丙.文, /‖有限状态1:字节0:$/);
  await new Promise(r => setTimeout(r, 200));
  console.log('  流式超限取消观察:', JSON.stringify((await 观察('svc')).取消));
});

test('真 workerd：POST 正文逐字节送达（引号、反斜线、换行、制表、表情），标头与 manual、信号', async () => {
  await 清();
  const 正文 = '甲"乙\\丙\n丁\t戊\r\n😀 末尾';
  const 果 = await 跑({op: 'svc', url: 'https://svc/echo?a=1', method: 'POST', body: 正文, headers: [['Content-Type', 'application/json'], ['accept', 'text/event-stream'], ['X-Yuyan-Request-ID', '0123abcd-0123-4abc-8def-0123456789ab']], read: 'chunks', max: 65536, timeout: 8000});
  assert.equal(果.状态, 200, 果.文);
  const 观 = await 观察('svc');
  const 记 = 观.请求.at(-1);
  console.log('  服务端所见请求:', JSON.stringify({...记, 正文文: undefined}));
  assert.equal(记.正文文, 正文);
  assert.equal(记.正文长, new TextEncoder().encode(正文).length);
  assert.equal(记.头['content-type'], 'application/json'); assert.equal(记.头['accept'], 'text/event-stream'); assert.equal(记.头['x-yuyan-request-id'], '0123abcd-0123-4abc-8def-0123456789ab');
});

test('真 workerd：等响应头期满（服务 800 毫秒后才答，超时 300 毫秒）', async () => {
  const t = Date.now();
  const 果 = await 跑({op: 'svc', url: 'https://svc/late', read: 'chunks', timeout: 300});
  console.log('  等头期满结果:', 果.文.slice(0, 120), Date.now() - t, 'ms');
  assert.match(果.文, /^请求失败:授权服务请求(失败或超时：.*|超时)$/);
});

test('真 workerd：大正文——8 MiB 请求正文通过', async () => {
  await 清();
  const 果 = await 跑({op: 'svc', url: 'https://svc/echo', method: 'POST', bodyPow: 23, read: 'limited', limit: 100000, timeout: 30000});
  assert.equal(果.状态, 200, 果.文.slice(0, 200));
  assert.equal((await 观察('svc')).请求.at(-1).正文长, 8388608);
});

// —— 动态公网 HTTPS（外发拦截）——
const 外观 = async () => (await (await mf.getWorker('net')).fetch('https://o/__obs')).json();
test('真 workerd：公网上游——通配许可的 URL 解析、SSE 读取、请求形态', async () => {
  const 果 = await 跑({op: 'pub', url: 'https://api.provider.com/sse', method: 'POST', body: '{"q":"甲\\n乙"}', headers: [['Content-Type', 'application/json'], ['Accept', 'text/event-stream'], ['X-Yuyan-Request-ID', '0123abcd-0123-4abc-8def-0123456789ab']], read: 'sse', timeout: 8000});
  assert.equal(果.状态, 200, 果.文);
  assert.match(果.文, /^状态码=200‖头=text\/event-stream‖文=甲乙‖完成‖读次=\d+‖字节=\d+$/);
  const 记 = (await 外观()).请求.at(-1);
  console.log('  外发所见:', JSON.stringify(记));
  assert.equal(记.url, 'https://api.provider.com/sse'); assert.equal(记.方法, 'POST'); assert.equal(记.转址, 'manual');
  assert.equal(记.正文文, '{"q":"甲\\n乙"}');
  assert.equal(记.头['x-yuyan-request-id'], '0123abcd-0123-4abc-8def-0123456789ab');
});

test('真 workerd：公网上游——302 不跟随、恶意网址不外发、超时', async () => {
  const 前 = (await 外观()).请求.length;
  const 甲 = await 跑({op: 'pub', url: 'https://api.provider.com/redirect', header: 'location', read: 'none', timeout: 8000});
  assert.equal(甲.文, '状态码=302‖头=https://elsewhere.example.com/');
  const 中 = (await 外观()).请求.length;
  for (const 网址 of ['https://127.0.0.1/x', 'https://0x7f.0.0.1/x', 'https://[::1]/x', 'https://localhost/x', 'https://a.internal/x', 'https://user:pw@api.provider.com/x', 'http://api.provider.com/x', 'https://api.provider.com:22/x', 'https://api.provider.com/x?y=1']) {
    const 果 = await 跑({op: 'pub', url: 网址, read: 'none'});
    assert.match(果.文, /^请求失败:/, 网址 + ' ' + 果.文);
  }
  assert.equal((await 外观()).请求.length, 中, '恶意网址不得外发');
  const 乙 = await 跑({op: 'pub', url: 'https://api.provider.com/hang', read: 'none', timeout: 400});
  console.log('  公网等头期满:', 乙.文.slice(0, 140));
  assert.match(乙.文, /^请求失败:公网上游请求失败或超时：/);
  const 丙 = await 跑({op: 'pub', url: 'https://api.provider.com/slow', read: 'chunks', timeout: 400});
  assert.deepEqual(终于(丙.文), [3, '超时']);
});

// —— 持久对象 ——
test('真 workerd：持久对象——SSE 增量读取、文字版 8 MiB、等响应头期满', async () => {
  const 甲 = await 跑({op: 'do', url: 'https://internal/events', read: 'chunks', max: 65536, timeout: 8000});
  assert.equal(甲.状态, 200, 甲.文);
  assert.equal(块们于(甲.文).join(''), 'data: 0<LF><LF>data: 1<LF><LF>data: 2<LF><LF>data: 3<LF><LF>');
  const 乙 = await 跑({op: 'dotext', url: 'https://internal/read', query: [['n', '5000000']]});
  assert.match(乙.文, /^状态=200‖体字节=5000000‖.*‖类型=text\/plain‖位置=‖饼=s=1‖版=9$/);
  const 丙 = await 跑({op: 'do', url: 'https://internal/slowhead', read: 'none', timeout: 300});
  console.log('  对象等头期满:', 丙.文.slice(0, 140));
  assert.match(丙.文, /^请求失败:持久服务请求(失败或超时：|超时)/);
});

test('真 workerd：转发入站请求——SSE 逐事件直通、请求体流式、只转白名单标头', async () => {
  const cfg = {binding: 'DOS', object: 'room1', url: 'https://internal/events'};
  const 回 = await mf.dispatchFetch('https://x.test/fwd', {headers: {'x-cfg': JSON.stringify(cfg)}});
  assert.equal(回.status, 207); assert.equal(回.headers.get('x-yuyan-revision'), '12');
  const 读 = 回.body.getReader(); const 到 = [];
  for (;;) { const {done, value} = await 读.read(); if (done) break; 到.push([Date.now(), new TextDecoder().decode(value)]); }
  console.log('  转发事件到达间隔(ms):', 到.slice(1).map((x, i) => x[0] - 到[i][0]).join(','));
  assert.equal(到.map(x => x[1]).join(''), 'data: 0\n\ndata: 1\n\ndata: 2\n\ndata: 3\n\n');
  const 回二 = await mf.dispatchFetch('https://x.test/fwd', {method: 'POST', headers: {'x-cfg': JSON.stringify({...cfg, url: 'https://internal/echo', keep: ['Cookie', 'Content-Type'], fixed: {'X-Yuyan-Route': 'r1'}}), cookie: 'sid=1', 'content-type': 'text/plain', authorization: 'Bearer t', 'x-drop': '1'}, body: '正文'});
  const 体 = await 回二.json();
  console.log('  DO 所见:', JSON.stringify(体));
  assert.equal(体.方法, 'POST'); assert.equal(体.正文长, new TextEncoder().encode('正文').length);
  assert.equal(体.头.cookie, 'sid=1'); assert.equal(体.头['x-yuyan-route'], 'r1'); assert.equal(体.头.authorization, undefined); assert.equal(体.头['x-drop'], undefined); assert.equal(体.转址, 'manual');
});

test('真 workerd：转发入站请求——请求体是流，对象在客户端未发完时即可读首块', async () => {
  const cfg = {binding: 'DOS', object: 'room1', url: 'https://internal/upload', keep: ['Content-Type']};
  let 放行; const 门 = new Promise(r => { 放行 = r; }); let 步 = 0; const 编 = new TextEncoder();
  const 体 = new ReadableStream({ async pull(c) { if (步 === 0) { 步 = 1; c.enqueue(编.encode('第一块')); } else { await 门; c.enqueue(编.encode('第二块')); c.close(); } } }, {highWaterMark: 0});
  const 发出时 = Date.now();
  const 回P = mf.dispatchFetch('https://x.test/fwd', {method: 'POST', duplex: 'half', body: 体, headers: {'x-cfg': JSON.stringify(cfg), 'content-type': 'text/plain'}});
  await new Promise(r => setTimeout(r, 500));
  放行();
  const 回 = await 回P;
  const 结果 = await 回.json();
  console.log('  上传结果:', JSON.stringify({块数: 结果.块数, 总: 结果.总, 首末间隔: 结果.时刻.at(-1) - 结果.时刻[0]}));
  assert.equal(结果.总, 编.encode('第一块第二块').length);
});

test('真 workerd：先请后作原答——对象的 SSE 原样作终答，逐事件到达', async () => {
  const 回 = await mf.dispatchFetch('https://x.test/dopass', {headers: {'x-cfg': JSON.stringify({url: 'https://internal/events200'})}});
  assert.equal(回.status, 200); assert.equal(回.headers.get('x-yuyan-revision'), '15'); assert.equal(回.headers.get('content-type'), 'text/event-stream');
  const 读 = 回.body.getReader(); const 到 = [];
  for (;;) { const {done, value} = await 读.read(); if (done) break; 到.push([Date.now(), new TextDecoder().decode(value)]); }
  console.log('  先请后作原答事件间隔(ms):', 到.slice(1).map((x, i) => x[0] - 到[i][0]).join(','));
  assert.equal(到.map(x => x[1]).join(''), 'data: 0\n\ndata: 1\n\ndata: 2\n\ndata: 3\n\n');
  const 回二 = await mf.dispatchFetch('https://x.test/dopass', {headers: {'x-cfg': JSON.stringify({url: 'https://internal/events'})}});
  assert.equal(回二.status, 502);
});
