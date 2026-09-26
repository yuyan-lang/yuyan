import {test, after} from 'node:test';
import assert from 'node:assert/strict';
import {编, 解, 睡, 造流, 切块, 造宿主, 跑, 默认许可} from './桩.mjs';

// 保活：让期限计时器（AbortSignal.timeout 的定时器不占事件循环）在等待期间不致令进程提前退出。
const 保活 = setInterval(() => {}, 1000);
after(() => clearInterval(保活));
const 块们于 = 文 => [...文.matchAll(/〔([^〕]*)〕/g)].map(m => m[1]);
const 终于 = 文 => { const m = 文.match(/‖状态(\d+):([^‖]*)$/s); return m ? [Number(m[1]), m[2]] : null; };
const 头对象 = h => h instanceof Headers ? Object.fromEntries(h) : Object.fromEntries(Object.entries(h ?? {}).map(([k, v]) => [k.toLowerCase(), v]));
const 正文文 = b => b == null ? null : typeof b === 'string' ? b : 解.decode(b);
const sse = ['你', '好', '世界'].map(字 => 'data: ' + JSON.stringify({choices: [{delta: {content: 字}}]}) + '\n\n').join('') + 'data: [DONE]\n\n';

const 行为 = (路径, init, 记) => {
  const 观察 = (记.观察 = {});
  switch (路径) {
    case '/sse': return new Response(造流(切块(编.encode(sse), 9), {观察}), {status: 200, headers: {'content-type': 'text/event-stream'}});
    case '/json': return new Response(JSON.stringify({ok: true, 名: '豫言'}), {status: 200, headers: {'content-type': 'application/json'}});
    case '/slow': return new Response(造流(['甲'], {末尾: 'hang', 观察}), {status: 200});
    case '/errmid': return new Response(造流(['丙'], {末尾: 'error', 观察}), {status: 200});
    case '/redirect': return new Response('moved', {status: 302, headers: {location: 'https://elsewhere.example.com/'}});
    case '/late': return 睡(500).then(() => new Response('迟到'));
    case '/hangfetch': return new Promise((_, 拒) => { init.signal.addEventListener('abort', () => 拒(init.signal.reason)); });
    default: return new Response('未知', {status: 404});
  }
};
const 造网络 = () => { const 调用们 = []; const 网络 = async (url, init) => { const 记 = {url: String(url), init}; 调用们.push(记); return 行为(new URL(url).pathname, init, 记); }; 网络.调用们 = 调用们; return 网络; };
const 造全局 = () => { const 调用们 = []; const 全局 = Object.create(globalThis, {fetch: {value: async (url, init) => { const 记 = {url: String(url), init}; 调用们.push(记); return 行为(new URL(url).pathname, init, 记); }}}); return {全局, 调用们}; };

// —— 静态来源（OUTBOUND_ORIGINS）——
test('静态上游：增量读取 SSE，并遵守 redirect:error、超时信号与新的请求编号标头', async () => {
  const 网络 = 造网络();
  const 果 = await 跑({op: 'up', url: 'https://api.example.com/sse', method: 'POST', body: '{"model":"m"}', headers: [['Content-Type', 'application/json'], ['Accept', 'text/event-stream'], ['X-Yuyan-Request-ID', '0123abcd-0123-4abc-8def-0123456789ab']], read: 'sse', timeout: 5000}, {}, 造宿主({网络}));
  assert.equal(果.状态, 200, 果.文);
  assert.match(果.文, /^状态码=200‖头=text\/event-stream‖文=你好世界‖完成‖读次=\d+‖字节=\d+$/);
  assert.equal(Number(果.文.match(/字节=(\d+)/)[1]), 编.encode(sse).length);
  const 记 = 网络.调用们[0];
  assert.equal(记.init.redirect, 'error'); assert.equal(记.init.method, 'POST'); assert.ok(记.init.signal instanceof AbortSignal);
  assert.equal(记.init.body, '{"model":"m"}');
  assert.deepEqual(头对象(记.init.headers), {'content-type': 'application/json', accept: 'text/event-stream', 'x-yuyan-request-id': '0123abcd-0123-4abc-8def-0123456789ab'});
});

test('静态上游：请求编号值须为 36 位小写十六进制与连字符；其余标头值须可见 ASCII', async () => {
  const 网络 = 造网络();
  const 试 = async (头, 期) => { const 果 = await 跑({op: 'up', url: 'https://api.example.com/json', headers: 头, read: 'none'}, {}, 造宿主({网络})); assert.equal(果.状态, 400, JSON.stringify(头)); assert.match(果.文, 期); };
  await 试([['X-Yuyan-Request-ID', '0123ABCD-0123-4abc-8def-0123456789ab']], /请求标头值无效/);
  await 试([['X-Yuyan-Request-ID', '0123abcd-0123-4abc-8def-0123456789a']], /请求标头值无效/);
  await 试([['X-Yuyan-Request-ID', '0123abcd-0123-4abc-8def-0123456789az']], /请求标头值无效/);
  await 试([['X-Yuyan-Request-ID', '../../../etc/passwd/0123456789abcdef0123']], /请求标头值无效/);
  await 试([['X-Yuyan-Request-ID', '']], /请求标头值无效/);
  await 试([['Authorization', 'Bearer a\nb']], /请求标头值无效/);
  await 试([['Accept', '非ASCII']], /请求标头值无效/);
  await 试([['Authorization', 'x'.repeat(8193)]], /请求标头值无效/);
  await 试([['X-Yuyan-Model', 'v']], /请求标头不允许/);
  await 试([['Accept', 'a'], ['Accept', 'b']], /请求标头重复/);
  assert.equal(网络.调用们.length, 0);
  const 好 = await 跑({op: 'up', url: 'https://api.example.com/json', headers: [['Authorization', 'Bearer ' + 'x'.repeat(2000)]], read: 'none'}, {}, 造宿主({网络}));
  assert.equal(好.状态, 200);
});

test('静态上游：未授权来源返回阴而不发请求；旧函数读状态与整读 JSON 仍可用', async () => {
  const 网络 = 造网络();
  const 甲 = await 跑({op: 'up', url: 'https://evil.example.com/json', read: 'none'}, {}, 造宿主({网络}));
  assert.equal(甲.文, '请求失败:上游网址未获授权'); assert.equal(网络.调用们.length, 0);
  const 乙 = await 跑({op: 'upjson', url: 'https://api.example.com/json'}, {}, 造宿主({网络}));
  assert.equal(乙.文, '状态=200‖json={"ok":true,"名":"豫言"}');
});

test('静态上游：超时、读取错误、取消、有限读取与柄校验', async () => {
  const 网络 = 造网络();
  const 宿主 = 造宿主({网络});
  const 甲 = await 跑({op: 'up', url: 'https://api.example.com/slow', read: 'chunks', timeout: 300}, {}, 宿主);
  assert.deepEqual(块们于(甲.文), ['甲']); assert.deepEqual(终于(甲.文), [3, '超时']);
  await 睡(30); assert.deepEqual(网络.调用们.at(-1).观察.取消, ['TimeoutError']);
  const 乙 = await 跑({op: 'up', url: 'https://api.example.com/errmid', read: 'chunks'}, {}, 宿主);
  assert.deepEqual(终于(乙.文), [2, 'Error: 上游炸了']);
  const 丙 = await 跑({op: 'up', url: 'https://api.example.com/slow', read: 'cancel'}, {}, 宿主);
  assert.equal(丙.文, '状态码=200‖头=‖首块状态0:甲‖再读状态3:已取消‖又读状态3:已取消');
  await 睡(30); assert.deepEqual(网络.调用们.at(-1).观察.取消, ['已取消']);
  const 丁 = await 跑({op: 'up', url: 'https://api.example.com/sse', read: 'limited', limit: 30}, {}, 宿主);
  assert.match(丁.文, /‖有限状态1:字节0:$/);
  const 戊 = await 跑({op: 'up', url: 'https://api.example.com/hangfetch', read: 'chunks', timeout: 200}, {}, 宿主);
  assert.match(戊.文, /^请求失败:.*TimeoutError/);
});

// —— 动态公网 HTTPS ——
const 公 = (url, 额外 = {}) => ({op: 'pub', url, read: 'none', ...额外});
test('公网上游：合规网址成功；请求经全局 fetch，redirect:manual、允许的标头、字节正文、超时信号', async () => {
  const {全局, 调用们} = 造全局();
  const 果 = await 跑(公('https://api.provider.com/sse', {method: 'POST', body: '{"q":"甲\\n乙"}', headers: [['Content-Type', 'application/json'], ['Accept', 'text/event-stream'], ['x-yuyan-request-id', '0123abcd-0123-4abc-8def-0123456789ab']], read: 'sse', timeout: 5000}), {}, 造宿主({全局}));
  assert.equal(果.状态, 200, 果.文);
  assert.match(果.文, /^状态码=200‖头=text\/event-stream‖文=你好世界‖完成‖读次=\d+‖字节=\d+$/);
  assert.equal(调用们.length, 1);
  const {url, init} = 调用们[0];
  assert.equal(url, 'https://api.provider.com/sse');
  assert.equal(init.method, 'POST'); assert.equal(init.redirect, 'manual'); assert.ok(init.signal instanceof AbortSignal);
  assert.equal(正文文(init.body), '{"q":"甲\\n乙"}');
  assert.deepEqual(头对象(init.headers), {'content-type': 'application/json', accept: 'text/event-stream', 'x-yuyan-request-id': '0123abcd-0123-4abc-8def-0123456789ab'});
});

test('公网上游：路径任意，网址经规范化后才发出（大小写、默认端口、点段、编码）', async () => {
  const {全局, 调用们} = 造全局();
  const 宿主 = 造宿主({全局});
  const 期 = [
    ['https://API.Provider.COM/json', 'https://api.provider.com/json'],
    ['https://api.provider.com:443/json', 'https://api.provider.com/json'],
    ['https://api.provider.com/a/../json', 'https://api.provider.com/json'],
    ['https://api.provider.com/json/', 'https://api.provider.com/json/'],
    ['https://xn--fsqu00a.xn--fiqs8s/json', 'https://xn--fsqu00a.xn--fiqs8s/json'],
    ['https://api.provider.com:8443/json', 'https://api.provider.com:8443/json'],
    ['https://a.b.c.provider.com:11434/json', 'https://a.b.c.provider.com:11434/json'],
    ['https://api.provider.com:8000/json', 'https://api.provider.com:8000/json'],
    ['https://api.provider.com:9999/json', 'https://api.provider.com:9999/json'],
    ['https://api.provider.com:2053/json', 'https://api.provider.com:2053/json'],
    ['https://api.provider.com/%E4%B8%AD/json', 'https://api.provider.com/%E4%B8%AD/json']
  ];
  for (const [入, 出] of 期) {
    const 果 = await 跑(公(入), {}, 宿主);
    assert.equal(果.状态, 200, 入 + ' → ' + 果.文);
    assert.match(果.文, /^状态码=(200|404)‖头=/, 入);   // 桩对已知路径回 200、未知路径回 404，说明请求已发出
    assert.equal(调用们.at(-1).url, 出, 入);
  }
});

const 恶意 = [
  ['https://127.0.0.1/x', /主机名不合规则/], ['https://0x7f.0.0.1/x', /主机名不合规则/], ['https://2130706433/x', /主机名不合规则/],
  ['https://0177.0.0.1/x', /主机名不合规则/], ['https://127.1/x', /主机名不合规则/], ['https://0.0.0.0/x', /主机名不合规则/],
  ['https://10.0.0.1/x', /主机名不合规则/], ['https://192.168.1.1/x', /主机名不合规则/], ['https://169.254.169.254/latest/meta-data', /主机名不合规则/],
  ['https://[::1]/x', /主机名不合规则/], ['https://[::ffff:127.0.0.1]/x', /主机名不合规则/], ['https://[2001:db8::1]/x', /主机名不合规则/],
  ['https://localhost/x', /主机名不合规则/], ['https://LOCALHOST/x', /主机名不合规则/], ['https://localhost./x', /主机名不合规则/],
  ['https://a.localhost/x', /主机名不合规则/], ['https://printer.local/x', /主机名不合规则/], ['https://db.internal/x', /主机名不合规则/],
  ['https://host.localdomain/x', /主机名不合规则/], ['https://router.home.arpa/x', /主机名不合规则/], ['https://1.0.0.127.in-addr.arpa/x', /主机名不合规则/],
  ['https://nas.lan/x', /主机名不合规则/], ['https://x.home/x', /主机名不合规则/], ['https://x.corp/x', /主机名不合规则/], ['https://x.intranet/x', /主机名不合规则/], ['https://x.private/x', /主机名不合规则/],
  ['https://provider/x', /主机名不合规则/], ['https://api.provider.com./x', /主机名不合规则/], ['https://.api.provider.com/x', /网址无效|主机名不合规则/],
  ['https://api..provider.com/x', /主机名不合规则/], ['https://-a.provider.com/x', /主机名不合规则/], ['https://a-.provider.com/x', /主机名不合规则/],
  ['https://' + 'a'.repeat(64) + '.provider.com/x', /主机名不合规则/], ['https://' + Array(40).fill('abcdef').join('.') + '.com/x', /主机名不合规则/],
  ['https://user:pw@api.provider.com/x', /用户信息/], ['https://user@api.provider.com/x', /用户信息/], ['https://@api.provider.com/x', /用户信息/],
  ['https://api.provider.com@evil.com/x', /用户信息/], ['https://api.provider.com\\@evil.com/x', /含空白、控制字符、反斜线/],
  ['http://api.provider.com/x', /须以 https:\/\/ 开头/], ['HTTPS://api.provider.com/x', /须以 https:\/\/ 开头/], ['ftp://api.provider.com/x', /须以 https:\/\/ 开头/],
  ['//api.provider.com/x', /须以 https:\/\/ 开头/], ['api.provider.com/x', /须以 https:\/\/ 开头/], ['', /长度须在 1 至 2048/],
  ['https://api.provider.com/x?y=1', /查询或片段标记/], ['https://api.provider.com/x#f', /查询或片段标记/], ['https://api.provider.com/x?', /查询或片段标记/],
  ['https://api.provider.com/x y', /含空白/], ['https://api.provider.com/x\ty', /含空白/], ['https://api.provider.com/\u0000', /含空白/],
  ['https://例子.中国/x', /含空白、控制字符/], ['https://api.provider.com/中文', /含空白、控制字符/],
  ['https://api.provider.com/' + 'a'.repeat(2050), /长度须在 1 至 2048/],
  ['https://api.provider.com:80/x', /端口不在允许范围/], ['https://api.provider.com:22/x', /端口不在允许范围/], ['https://api.provider.com:7999/x', /端口不在允许范围/],
  ['https://api.provider.com:10000/x', /端口不在允许范围/], ['https://api.provider.com:65535/x', /端口不在允许范围/], ['https://api.provider.com:3000/x', /端口不在允许范围/],
  ['https://api.provider.com:6379/x', /端口不在允许范围/], ['https://api.provider.com:1/x', /端口不在允许范围/], ['https://api.provider.com:0/x', /端口不在允许范围|网址无效/],
  ['https://api.provider.com:99999/x', /网址无效/], ['https://api.provider.com:abc/x', /网址无效/], ['https://a.b.123/x', /网址无效|主机名不合规则/]
];
test('公网上游：恶意网址一律返回阴（不抛、不发请求）', async () => {
  const {全局, 调用们} = 造全局();
  const 宿主 = 造宿主({全局});
  for (const [网址, 期] of 恶意) {
    const 果 = await 跑(公(网址), {}, 宿主);
    assert.equal(果.状态, 200, 网址 + ' ' + 果.文);
    assert.match(果.文, /^请求失败:/, 网址);
    assert.match(果.文, 期, 网址 + ' → ' + 果.文);
  }
  assert.equal(调用们.length, 0, '恶意网址不得发出任何请求');
});

test('公网上游：无许可则抛事故；许可只列静态来源同样拒绝；不发请求', async () => {
  const {全局, 调用们} = 造全局();
  for (const 许可 of [{}, {OUTBOUND_ORIGINS: ['https://api.example.com']}, {OUTBOUND_ORIGINS: []}, {PUBLIC_HTTPS: ['*']}]) {
    const 果 = await 跑(公('https://api.provider.com/json'), {}, 造宿主({全局, 许可}));
    assert.equal(果.状态, 400, JSON.stringify(许可)); assert.match(果.文, /公网 HTTPS 上游未获授权/);
  }
  assert.equal(调用们.length, 0);
});

test('公网上游：参数违规抛事故（方法、标头、正文、超时）', async () => {
  const {全局, 调用们} = 造全局();
  const 宿主 = 造宿主({全局});
  const 试 = async (额外, 期) => { const 果 = await 跑(公('https://api.provider.com/json', 额外), {}, 宿主); assert.equal(果.状态, 400, JSON.stringify(额外)); assert.match(果.文, 期, JSON.stringify(额外)); };
  await 试({method: 'PUT'}, /方法只允许 GET 或 POST/);
  await 试({method: 'HEAD'}, /方法只允许 GET 或 POST/);
  await 试({timeout: 0}, /超时毫秒须在 1 至 900000/);
  await 试({timeout: 900001}, /超时毫秒须在 1 至 900000/);
  await 试({method: 'GET', body: 'x'}, /GET 不可带正文/);
  await 试({headers: [['Authorization', 'Bearer k']]}, /公网上游标头不允许/);
  await 试({headers: [['Cookie', 'a=b']]}, /公网上游标头不允许/);
  await 试({headers: [['X-Yuyan-Model', 'v']]}, /公网上游标头不允许/);
  await 试({headers: [['Content-Type', 'a'], ['content-type', 'b']]}, /公网上游标头重复/);
  await 试({headers: [['Bad Name', 'v']]}, /公网上游标头名无效/);
  await 试({headers: [['Content-Type', '含换行\nx']]}, /公网上游标头值无效/);
  await 试({headers: [['X-Yuyan-Request-ID', '0123ABCD-0123-4abc-8def-0123456789ab']]}, /公网上游标头值无效/);
  await 试({headers: [['X-Yuyan-Request-ID', '0123abcd-0123-4abc-8def-0123456789a']]}, /公网上游标头值无效/);
  await 试({headers: Array.from({length: 9}, (_, i) => ['Accept', 'a' + i])}, /公网上游标头过多/);
  await 试({method: 'POST', bodyPow: 21, bodyExtra: 'b'}, /公网上游请求正文超过 2 MiB/);
  const 好 = await 跑(公('https://api.provider.com/json', {method: 'POST', bodyPow: 21}), {}, 宿主);
  assert.equal(好.状态, 200); assert.equal(调用们.length, 1); assert.equal(调用们[0].init.body.length, 2097152);
});

test('公网上游：3xx 作为成功响应交给应用（状态码可读、Location 可读），不跟随', async () => {
  const {全局, 调用们} = 造全局();
  const 果 = await 跑(公('https://api.provider.com/redirect', {header: 'location'}), {}, 造宿主({全局}));
  assert.equal(果.文, '状态码=302‖头=https://elsewhere.example.com/');
  assert.equal(调用们[0].init.redirect, 'manual');
});

test('公网上游：超时与读取错误、取消、有限读取', async () => {
  const {全局, 调用们} = 造全局();
  const 宿主 = 造宿主({全局});
  const 甲 = await 跑(公('https://api.provider.com/slow', {read: 'chunks', timeout: 300}), {}, 宿主);
  assert.deepEqual(块们于(甲.文), ['甲']); assert.deepEqual(终于(甲.文), [3, '超时']);
  await 睡(30); assert.deepEqual(调用们.at(-1).观察.取消, ['TimeoutError']);
  const 乙 = await 跑(公('https://api.provider.com/hangfetch', {read: 'chunks', timeout: 200}), {}, 宿主);
  assert.match(乙.文, /^请求失败:公网上游请求失败或超时：.*TimeoutError/);
  const 丙 = await 跑(公('https://api.provider.com/late', {read: 'chunks', timeout: 150}), {}, 宿主);
  assert.equal(丙.文, '请求失败:公网上游请求超时');
  const 丁 = await 跑(公('https://api.provider.com/errmid', {read: 'chunks'}), {}, 宿主);
  assert.deepEqual(终于(丁.文), [2, 'Error: 上游炸了']);
  const 戊 = await 跑(公('https://api.provider.com/sse', {read: 'limited', limit: 30}), {}, 宿主);
  assert.match(戊.文, /‖有限状态1:字节0:$/);
  const 己 = await 跑(公('https://api.provider.com/json', {read: 'limited', limit: 1000}), {}, 宿主);
  assert.match(己.文, /‖有限状态0:字节\d+:\{"ok":true,"名":"豫言"\}$/);
});

test('旧的原流回应仍可用：请求授权网页上游后以上游流回应当前入站请求（只覆盖允许的头）', async () => {
  const 网络 = 造网络();
  const 回 = await 造宿主({网络}).fetch(new Request('https://x.test/uppass'), {});
  assert.equal(回.status, 200); assert.equal(回.headers.get('content-type'), 'text/event-stream; charset=utf-8'); assert.equal(回.headers.get('x-yuyan-model'), 'm9');
  assert.equal(await 回.text(), sse);
});
