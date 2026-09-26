// 网页答复 0.5.0 一致性测试：真实 Wasm + Node 宿主。
// 复跑：在私有暂存目录（含 dist/）里 `node --test <本文件>`；产物位置可用环境变量 产物根 指定（默认 <当前目录>/dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {pathToFileURL} from 'node:url';
import path from 'node:path';

const 产物 = path.resolve(process.env.产物根 ?? path.join(process.cwd(), 'dist'), '网页答复一致性');
const {创建云工宿主} = await import(pathToFileURL(path.join(产物, '宿主.mjs')).href);
const 程序模块 = await WebAssembly.compile(await readFile(path.join(产物, '程序.wasm')));
const 值桥模块 = await WebAssembly.compile(await readFile(path.join(产物, '值桥.wasm')));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {}});
const 上下文 = {waitUntil(承诺) { Promise.resolve(承诺).catch(() => {}); }};
const MiB = 1024 * 1024;
const 摘 = 字节 => createHash('sha256').update(字节).digest('hex');

// 发一条 JSON 指令；GET/HEAD 时经 ?cmd= 传递。返回状态、排序后的标头对、Set-Cookie 列表与正文字节。
async function 令(指令, 方法 = 'POST') {
  const 请 = 方法 === 'GET' || 方法 === 'HEAD'
    ? new Request('https://x.test/?cmd=' + encodeURIComponent(JSON.stringify(指令)), {method: 方法})
    : new Request('https://x.test/', {method: 方法, body: JSON.stringify(指令)});
  const 回 = await 宿主.fetch(请, {}, 上下文);
  const 体 = Buffer.from(await 回.arrayBuffer());
  return {状态: 回.status, 头: [...回.headers], 饼: 回.headers.getSetCookie(), 体, 文: 体.toString('utf8')};
}
// 应用把校验失败转成 500 与 err|<消息十六进制>；成功时应用不再自行答复。
function 错(果) {
  assert.equal(果.状态, 500, '应得到可捕获的失败，实得 ' + 果.状态 + ' ' + 果.文.slice(0, 200));
  assert.ok(果.文.startsWith('err|'));
  return Buffer.from(果.文.slice(4), 'hex').toString('utf8');
}
const 名列 = 果 => 果.头.map(项 => 项[0]);
const 文 = (头列, 额外 = {}) => ({fn: 'text', status: 200, type: 'text/plain; charset=utf-8', headers: 头列, body: 'x', ...额外});

test('带头文字：状态、内容类型、保序同名头、多个 Set-Cookie，正文原样', async () => {
  const 头列 = [['X-A', '1'], ['x-a', '2'], ['Set-Cookie', 'a=1; Path=/'], ['Set-Cookie', 'b=2; HttpOnly'], ['Cache-Control', 'private, max-age=5'], ['X-Empty', ''], ['X-Tab', 'a\tb']];
  const 果 = await 令(文(头列, {status: 201, body: '第一行\n第二行\t缩进 🙂 \u0000尾'}));
  assert.equal(果.状态, 201);
  assert.deepEqual(果.头, [['cache-control', 'private, max-age=5'], ['content-type', 'text/plain; charset=utf-8'], ['set-cookie', 'a=1; Path=/'], ['set-cookie', 'b=2; HttpOnly'], ['x-a', '1, 2'], ['x-empty', ''], ['x-tab', 'a\tb']]);
  assert.deepEqual(果.饼, ['a=1; Path=/', 'b=2; HttpOnly']);
  assert.equal(果.文, '第一行\n第二行\t缩进 🙂 \u0000尾');
});

test('不隐式添加任何标头（与旧癸象答复不同）', async () => {
  assert.deepEqual(名列(await 令(文([]))), ['content-type']);
  assert.deepEqual(名列(await 令({fn: 'json', status: 200, headers: [], body: '{"a":1}'})), ['content-type']);
  assert.deepEqual(名列(await 令({fn: 'empty', status: 200, headers: []})), []);
  assert.deepEqual(名列(await 令({fn: 'bytes', status: 200, type: 'application/octet-stream', headers: [], hex: ['00']})), ['content-type']);
  assert.deepEqual(名列(await 令({fn: 'redirect', status: 302, target: '/x', headers: []})), ['location']);
  const 旧 = await 令({fn: 'old-json', status: 200, body: '{"a":1}'});
  assert.deepEqual(名列(旧).sort(), ['cache-control', 'content-type', 'x-content-type-options'], '旧函数仍自动加 no-store 与 nosniff');
});

test('状态码校验：200 至 599；204、205、304 不得带非空正文', async () => {
  for (const 状态 of [200, 201, 299, 400, 404, 500, 599]) assert.equal((await 令(文([], {status: 状态}))).状态, 状态);
  for (const 状态 of [100, 101, 199, 600, 0, -1, 1000, 99999999999]) assert.match(错(await 令(文([], {status: 状态}))), /响应状态不在 200 至 599/, String(状态));
  for (const 状态 of [204, 205, 304]) {
    assert.match(错(await 令(文([], {status: 状态, body: 'x'}))), /不得带非空正文/, String(状态));
    const 空 = await 令(文([], {status: 状态, body: ''}));
    assert.equal(空.状态, 状态);
    assert.equal(空.体.length, 0);
    assert.match(错(await 令({fn: 'json', status: 状态, headers: [], body: '{}'})), /不得带非空正文/, 'JSON ' + 状态);
    assert.match(错(await 令({fn: 'bytes', status: 状态, type: 'a/b', headers: [], hex: ['00']})), /不得带非空正文/, '字节 ' + 状态);
    assert.equal((await 令({fn: 'bytes', status: 状态, type: 'a/b', headers: [], hex: []})).状态, 状态);
    assert.equal((await 令({fn: 'empty', status: 状态, headers: []})).状态, 状态);
  }
});

test('标头名校验：令牌字符、1 至 64 字节', async () => {
  const 合法 = ["!#$%&'*+-.^_`|~", 'aZ09', 'X-Custom-Name', 'a'.repeat(64)];
  for (const 名 of 合法) {
    const 果 = await 令(文([[名, 'v']]));
    assert.equal(果.状态, 200, JSON.stringify(名) + ' ' + 果.文.slice(0, 80));
    assert.ok(果.头.some(项 => 项[0] === 名.toLowerCase()), JSON.stringify(名));
  }
  for (const 名 of ['', ' ', 'a b', 'a:b', 'a\r\nb', 'a\nb', 'a\u0000b', '名', 'a(b)', 'a"b', 'a/b', 'a;b', 'a<b>', 'a@b', 'a[b]', 'a{b}', 'a\\b', 'a,b', 'a=b', 'a?b', ' a', 'a ', 'a'.repeat(65)]) {
    assert.match(错(await 令(文([[名, 'v']]))), /响应标头名称无效/, JSON.stringify(名));
  }
});

test('标头值校验：可见 ASCII、空格与水平制表，至多 8192 字节', async () => {
  const 可见 = Array.from({length: 95}, (_, i) => String.fromCharCode(32 + i)).join('');
  for (const 值 of ['', ' ', 'a\tb', 可见, 'a'.repeat(8192)]) {
    const 果 = await 令(文([['X-V', 值]]));
    assert.equal(果.状态, 200, JSON.stringify(值).slice(0, 50) + ' ' + 果.文.slice(0, 80));
    assert.equal(果.头.find(项 => 项[0] === 'x-v')[1], 值.trim().replace(/^[\t ]+|[\t ]+$/g, ''));
  }
  for (const 值 of ['a\rb', 'a\nb', 'a\r\nInjected: 1', 'a\u0000b', 'a\u0001b', 'a\u001fb', 'a\u007fb', 'é', '你好', 'a\u0080b', 'a'.repeat(8193), '\u000bx', '\u000cx']) {
    assert.match(错(await 令(文([['X-V', 值]]))), /响应标头值无效/, JSON.stringify(值).slice(0, 50));
  }
});

test('宿主管理的标头名不得由应用设置（不分大小写），内容类型只能经参数', async () => {
  const 禁 = ['Content-Length', 'Transfer-Encoding', 'Connection', 'Keep-Alive', 'Upgrade', 'TE', 'Trailer', 'Content-Type', 'Content-Encoding'];
  for (const 名 of 禁) for (const 形 of [名, 名.toLowerCase(), 名.toUpperCase()]) {
    for (const 指令 of [文([[形, 'x']]), {fn: 'json', status: 200, headers: [[形, 'x']], body: '{}'}, {fn: 'empty', status: 200, headers: [[形, 'x']]}, {fn: 'bytes', status: 200, type: 'a/b', headers: [[形, 'x']], hex: ['00']}, {fn: 'redirect', status: 302, target: '/x', headers: [[形, 'x']]}]) {
      assert.match(错(await 令(指令)), /由宿主或参数管理，不得设置/, 形 + ' ' + 指令.fn);
    }
  }
  for (const 名 of ['Location', 'Cache-Control', 'Content-Disposition', 'Content-Language', 'Content-Range', 'ETag', 'Vary', 'Link', 'Set-Cookie', 'Access-Control-Allow-Origin']) assert.equal((await 令(文([[名, 'x']]))).状态, 200, 名);
});

test('标头数量与 Set-Cookie 长度限额', async () => {
  const 六十四 = Array.from({length: 64}, (_, i) => ['X-H' + i, String(i)]);
  const 果 = await 令(文(六十四));
  assert.equal(果.状态, 200);
  assert.equal(果.头.length, 65);
  assert.match(错(await 令(文(六十四.concat([['X-Extra', '1']])))), /响应标头超过 64 项/);
  const 饼 = Array.from({length: 64}, (_, i) => ['Set-Cookie', 'c' + i + '=v']);
  assert.equal((await 令(文(饼))).饼.length, 64);
  assert.equal((await 令(文([['Set-Cookie', 'a=' + 'b'.repeat(4094)]]))).饼[0].length, 4096);
  assert.match(错(await 令(文([['Set-Cookie', 'a=' + 'b'.repeat(4095)]]))), /Set-Cookie 值超过 4096 字节/);
  assert.match(错(await 令(文([['set-cookie', 'a=' + 'b'.repeat(4095)]]))), /Set-Cookie 值超过 4096 字节/);
  assert.equal((await 令(文([['X-Long', 'a'.repeat(8192)]]))).状态, 200);
});

test('内容类型校验：type/subtype 后可缀分号参数，全文可见且至多 256 字节', async () => {
  for (const 类 of ['text/plain', 'text/plain; charset=utf-8', 'application/json;charset=utf-8', 'multipart/form-data; boundary=----x', 'application/vnd.api+json', 'text/plain ; a=b', 'text/plain;', "a-b.c/d_e!f#g$h%i&j'k*l+m^n`o|p~q", 'text/' + 'a'.repeat(240)]) {
    for (const 指令 of [文([], {type: 类}), {fn: 'bytes', status: 200, type: 类, headers: [], hex: ['00']}]) {
      const 果 = await 令(指令);
      assert.equal(果.状态, 200, JSON.stringify(类) + ' ' + 果.文.slice(0, 80));
      assert.equal(果.头.find(项 => 项[0] === 'content-type')[1], 类.trim());
    }
  }
  for (const 类 of ['', 'text', 'text/', '/plain', 'text/plain\r\nX: y', 'text/pl ain', '文本/文', 'text/pl\u0000ain', 'te xt/plain', 'text//plain', 'text/plain\u007f', 'a/' + 'b'.repeat(255), ' text/plain']) {
    for (const 指令 of [文([], {type: 类}), {fn: 'bytes', status: 200, type: 类, headers: [], hex: ['00']}]) {
      assert.match(错(await 令(指令)), /内容类型无效/, JSON.stringify(类).slice(0, 40) + ' ' + 指令.fn);
    }
  }
});

test('带头癸象文：规范化 JSON，内容类型固定，尺寸上限 2 MiB', async () => {
  const 果 = await 令({fn: 'json', status: 200, headers: [['Cache-Control', 'no-store'], ['Set-Cookie', 's=1']], body: ' { "b" : [1, 2 , 3], "a" : "x\\ny", "甲": {"c": null} } '});
  assert.equal(果.状态, 200);
  assert.equal(果.文, '{"b":[1,2,3],"a":"x\\ny","甲":{"c":null}}');
  assert.deepEqual(果.头, [['cache-control', 'no-store'], ['content-type', 'application/json; charset=utf-8'], ['set-cookie', 's=1']]);
  for (const 坏 of ['{bad', '', '[1,', '{"a":1}x']) assert.match(错(await 令({fn: 'json', status: 200, headers: [], body: 坏})), /JSON 响应正文无效/, JSON.stringify(坏));
  const 大 = await 令({fn: 'json', status: 200, headers: [], jsonWrap: true, bodyUnit: 'a', bodyCount: 2 * MiB - 2});
  assert.equal(大.体.length, 2 * MiB);
  assert.match(错(await 令({fn: 'json', status: 200, headers: [], jsonWrap: true, bodyUnit: 'a', bodyCount: 2 * MiB - 1})), /JSON 响应超过 2 MiB/);
});

test('带头空答：无正文、无内容类型，头保序', async () => {
  for (const 状态 of [200, 204, 205, 304, 404, 599]) {
    const 果 = await 令({fn: 'empty', status: 状态, headers: [['X-A', '1'], ['X-B', '2']]});
    assert.equal(果.状态, 状态);
    assert.equal(果.体.length, 0);
    assert.deepEqual(果.头, [['x-a', '1'], ['x-b', '2']]);
  }
});

test('带头字节：原字节，含全部字节值与零字节，至多 16 MiB', async () => {
  const 全 = Array.from({length: 256}, (_, i) => i.toString(16).padStart(2, '0'));
  const 果 = await 令({fn: 'bytes', status: 200, type: 'application/octet-stream', headers: [['X-N', '256']], hex: 全});
  assert.deepEqual([...果.体], Array.from({length: 256}, (_, i) => i));
  assert.equal((await 令({fn: 'bytes', status: 200, type: 'a/b', headers: [], hex: []})).体.length, 0);
  const 满 = await 令({fn: 'bytes', status: 200, type: 'a/b', headers: [], bytesCount: 16 * MiB, bytesFill: 200});
  assert.equal(满.体.length, 16 * MiB);
  assert.equal(摘(满.体), 摘(Buffer.alloc(16 * MiB, 200)));
  assert.match(错(await 令({fn: 'bytes', status: 200, type: 'a/b', headers: [], bytesCount: 16 * MiB + 1, bytesFill: 200})), /字节响应超过 16 MiB/);
});

test('带头文字：正文上限 8 MiB，按 UTF-8 字节计', async () => {
  const 满 = await 令(文([], {body: undefined, bodyUnit: 'a', bodyCount: 8 * MiB}));
  assert.equal(满.体.length, 8 * MiB);
  assert.match(错(await 令(文([], {body: undefined, bodyUnit: 'a', bodyCount: 8 * MiB + 1}))), /文字响应超过 8 MiB/);
  const 汉 = await 令(文([], {body: undefined, bodyUnit: '汉', bodyCount: 2796202}));
  assert.equal(汉.体.length, 8388606);
  assert.match(错(await 令(文([], {body: undefined, bodyUnit: '汉', bodyCount: 2796203}))), /文字响应超过 8 MiB/);
});

test('带头改址：五个状态，Location 为单斜路径或无用户信息的绝对 http(s) 网址', async () => {
  for (const 状态 of [301, 302, 303, 307, 308]) {
    const 果 = await 令({fn: 'redirect', status: 状态, target: '/a/b?x=1&y=%20#h', headers: [['Cache-Control', 'no-store'], ['Set-Cookie', 's=1']]});
    assert.equal(果.状态, 状态);
    assert.deepEqual(果.头, [['cache-control', 'no-store'], ['location', '/a/b?x=1&y=%20#h'], ['set-cookie', 's=1']]);
    assert.equal(果.体.length, 0);
  }
  for (const 状态 of [200, 201, 204, 300, 304, 305, 306, 309, 400, 0, 999]) assert.match(错(await 令({fn: 'redirect', status: 状态, target: '/x', headers: []})), /重定向状态不受支持/, String(状态));
  for (const 目标 of ['/', '/a', '/a/b?x=1#h', '/a//b', '/?q=//x', 'http://example.com', 'https://example.com:8443/p?q=1#f', 'http://[::1]/x', 'https://a.b.c/%E4%BD%A0', 'HTTP://EXAMPLE.COM', 'Https://Example.com/A']) {
    const 果 = await 令({fn: 'redirect', status: 302, target: 目标, headers: []});
    assert.equal(果.状态, 302, 目标 + ' ' + 果.文.slice(0, 60));
    assert.equal(果.头.find(项 => 项[0] === 'location')[1], 目标);
  }
  for (const 目标 of ['', ' /a', '/ a', '//evil.com', '//evil.com/x', '/\\evil.com', '/a\\b', 'a', './a', '../a', '?q=1', '#f', 'javascript:alert(1)', 'data:text/html,x', 'ftp://x/', 'mailto:a@b', 'http://', 'https:///x', 'http://u:p@h/', 'http://u@h/', 'http://h\\@evil/', '/a\r\nSet-Cookie: x=1', '/a\nb', '/a\tb', '/a\u0000', '/a\u007f', '/é', 'http://例子.com', '/' + 'a'.repeat(8192), 'http:/x', 'https://exa mple.com']) {
    assert.match(错(await 令({fn: 'redirect', status: 302, target: 目标, headers: []})), /重定向目标无效/, JSON.stringify(目标).slice(0, 50));
  }
  assert.equal((await 令({fn: 'redirect', status: 302, target: '/' + 'a'.repeat(8191), headers: []})).状态, 302);
  assert.match(错(await 令({fn: 'redirect', status: 302, target: '/x', headers: [['Location', '/y']]})), /不得再含 Location/);
  assert.match(错(await 令({fn: 'redirect', status: 302, target: '/x', headers: [['location', '/y']]})), /不得再含 Location/);
});

test('HEAD 与 GET：适配不特殊处理（宿主不发送正文）', async () => {
  const 指令 = 文([['X-A', '1']], {status: 200, body: 'body'});
  for (const 方法 of ['HEAD', 'GET', 'POST']) {
    const 果 = await 令(指令, 方法);
    assert.equal(果.状态, 200, 方法);
    assert.equal(果.文, 'body', 方法);
    assert.deepEqual(果.头, [['content-type', 'text/plain; charset=utf-8'], ['x-a', '1']], 方法);
  }
});

test('每次请求只有第一个最终响应生效', async () => {
  const 果 = await 令({fn: 'twice', status: 204, headers: [['X-First', 'yes']]});
  assert.equal(果.状态, 204);
  assert.deepEqual(果.头, [['x-first', 'yes']]);
});

test('旧函数回归：癸象文、空答、文字、同源改址、外址改址行为不变', async () => {
  const j = await 令({fn: 'old-json', status: 201, body: '{ "a" : [1,2] }'});
  assert.equal(j.状态, 201);
  assert.equal(j.文, '{"a":[1,2]}');
  assert.deepEqual(j.头, [['cache-control', 'no-store'], ['content-type', 'application/json; charset=utf-8'], ['x-content-type-options', 'nosniff']]);
  assert.match(错(await 令({fn: 'old-json', status: 204, body: '{}'})), /不得带 JSON 正文/);
  assert.match(错(await 令({fn: 'old-json', status: 600, body: '{}'})), /200 至 599/);
  const e = await 令({fn: 'old-empty', status: 204});
  assert.equal(e.状态, 204);
  assert.deepEqual(e.头, [['cache-control', 'no-store']]);
  const t = await 令({fn: 'old-text', status: 200, type: 'text/plain; charset=utf-8', body: '你好'});
  assert.equal(t.文, '你好');
  assert.deepEqual(t.头, [['cache-control', 'no-store'], ['content-type', 'text/plain; charset=utf-8'], ['x-content-type-options', 'nosniff']]);
  const s = await 令({fn: 'old-same', status: 302, target: 'https://x.test/next'});
  assert.equal(s.状态, 302);
  assert.equal(s.头.find(项 => 项[0] === 'location')[1], 'https://x.test/next');
  assert.match(错(await 令({fn: 'old-same', status: 302, target: 'https://evil.example/'})), /不是同源/);
  const x = await 令({fn: 'old-ext', status: 301, target: 'https://example.com/a'});
  assert.equal(x.状态, 301);
  assert.match(错(await 令({fn: 'old-ext', status: 301, target: 'http://example.com/a'})), /仅允许 HTTPS/);
});

// 确定性伪随机（mulberry32），便于复现。
function 随机器(种子) { let a = 种子 >>> 0; return () => { a = (a + 0x6D2B79F5) >>> 0; let t = a; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; }; }
const 字池 = Array.from("abcXYZ019-_.~!#$%&'*+^`|", c => c).concat([' ', '\t', '\r', '\n', '\u0000', '\u0001', '\u001f', '\u007f', '"', '\\', ':', ';', ',', '/', '(', ')', '<', '>', '@', '[', ']', '{', '}', '=', '?', 'é', '你', '🙂', '\u0080', 'ÿ']);
function 随机文(随, 最长) { const 长 = Math.floor(随() * 随() * 最长); let s = ''; for (let i = 0; i < 长; i++) s += 字池[Math.floor(随() * 字池.length)]; return s; }
const 令牌正则 = /^[!#$%&'*+.^_`|~0-9A-Za-z-]+$/;
const 禁名 = new Set(['content-length', 'transfer-encoding', 'connection', 'keep-alive', 'upgrade', 'te', 'trailer', 'content-type', 'content-encoding']);
function 期望头(名, 值) {
  if (!(Buffer.byteLength(名) >= 1 && Buffer.byteLength(名) <= 64 && 令牌正则.test(名))) return '响应标头名称无效';
  if (禁名.has(名.toLowerCase())) return '由宿主或参数管理';
  if (!/^[\t\x20-\x7e]*$/.test(值) || Buffer.byteLength(值) > 8192) return '响应标头值无效';
  if (名.toLowerCase() === 'set-cookie' && Buffer.byteLength(值) > 4096) return 'Set-Cookie 值超过 4096 字节';
  return null;
}
test('差分模糊：随机标头名与值的接受与拒绝与参考实现一致', async () => {
  const 随 = 随机器(20260925);
  let 接受 = 0, 拒绝 = 0;
  for (let i = 0; i < 3000; i++) {
    let 名 = 随机文(随, 12), 值 = 随机文(随, 30);
    if (i % 7 === 0) 名 = ['Set-Cookie', 'set-cookie', 'X-A', 'Content-Type', 'TE', 'x-empty'][Math.floor(随() * 6)];
    if (i % 11 === 0) 值 = 'v'.repeat([0, 4095, 4096, 4097, 8191, 8192, 8193][Math.floor(随() * 7)]);
    if (i % 13 === 0) 名 = 'n'.repeat([63, 64, 65][Math.floor(随() * 3)]);
    const 期 = 期望头(名, 值);
    const 果 = await 令(文([[名, 值]]));
    if (期 === null) { assert.equal(果.状态, 200, JSON.stringify([名, 值]).slice(0, 80) + ' ' + 果.文.slice(0, 80)); 接受++; }
    else { assert.match(错(果), new RegExp(期), JSON.stringify([名, 值]).slice(0, 80)); 拒绝++; }
  }
  assert.ok(接受 > 200 && 拒绝 > 200, `样本应两类都足够：接受 ${接受}，拒绝 ${拒绝}`);
});

const 目标可用 = 目标 => {
  if (目标.length === 0 || Buffer.byteLength(目标) > 8192) return false;
  if (!/^[\x21-\x7e]+$/.test(目标) || 目标.includes('\\')) return false;
  if (目标[0] === '/') return 目标[1] !== '/';
  const 小 = 目标.toLowerCase();
  const 前 = 小.startsWith('https://') ? 8 : 小.startsWith('http://') ? 7 : -1;
  if (前 < 0) return false;
  if (目标.length <= 前 || '/?#@'.includes(目标[前])) return false;
  const 址 = URL.parse(目标);
  return !!址 && (址.protocol === 'http:' || 址.protocol === 'https:') && !址.username && !址.password && 址.hostname !== '';
};
test('差分模糊：随机改址目标的接受与拒绝与参考实现一致', async () => {
  const 随 = 随机器(20260926);
  const 片 = ['/', 'a', 'b', '?', '#', '@', ':', '.', '%', '=', '&', '-', '_', 'http://', 'https://', 'HTTP://', 'x.example', 'h', '/x', '\\', ' ', '\u0001', 'é', '..', '[::1]', '0x7f', 'u:p@'];
  let 接受 = 0, 拒绝 = 0;
  for (let i = 0; i < 3000; i++) {
    let 目标 = '';
    const 段数 = Math.floor(随() * 6);
    for (let j = 0; j < 段数; j++) 目标 += 片[Math.floor(随() * 片.length)];
    if (i % 5 === 0) 目标 = ['http://', 'https://'][Math.floor(随() * 2)] + 目标;
    if (i % 17 === 0) 目标 = '/' + 'a'.repeat([8190, 8191, 8192][Math.floor(随() * 3)]);
    const 果 = await 令({fn: 'redirect', status: 302, target: 目标, headers: []});
    if (目标可用(目标)) { assert.equal(果.状态, 302, JSON.stringify(目标).slice(0, 80) + ' ' + 果.文.slice(0, 80)); 接受++; }
    else { assert.match(错(果), /重定向目标无效/, JSON.stringify(目标).slice(0, 80)); 拒绝++; }
  }
  assert.ok(接受 > 100 && 拒绝 > 100, `样本应两类都足够：接受 ${接受}，拒绝 ${拒绝}`);
});

const 类型可用 = 类 => {
  if (Buffer.byteLength(类) < 1 || Buffer.byteLength(类) > 256) return false;
  if (!/^[\t\x20-\x7e]*$/.test(类)) return false;
  return /^[!#$%&'*+.^_`|~0-9A-Za-z-]+\/[!#$%&'*+.^_`|~0-9A-Za-z-]+[ \t]*(;.*)?$/.test(类) || /^[!#$%&'*+.^_`|~0-9A-Za-z-]+\/[!#$%&'*+.^_`|~0-9A-Za-z-]+[ \t]*$/.test(类);
};
test('差分模糊：随机内容类型的接受与拒绝与参考实现一致', async () => {
  const 随 = 随机器(20260927);
  const 片 = ['text', 'plain', 'application', 'json', '/', ';', ' ', '\t', 'charset=utf-8', '=', '"', '-', '+', '.', 'x', '\u0001', 'é', '', '//'];
  let 接受 = 0, 拒绝 = 0;
  for (let i = 0; i < 2000; i++) {
    let 类 = '';
    const 段数 = 1 + Math.floor(随() * 7);
    for (let j = 0; j < 段数; j++) 类 += 片[Math.floor(随() * 片.length)];
    if (i % 9 === 0) 类 = 'a/' + 'b'.repeat([253, 254, 255, 256][Math.floor(随() * 4)]);
    const 果 = await 令(文([], {type: 类}));
    if (类型可用(类)) { assert.equal(果.状态, 200, JSON.stringify(类).slice(0, 80) + ' ' + 果.文.slice(0, 80)); 接受++; }
    else { assert.match(错(果), /内容类型无效/, JSON.stringify(类).slice(0, 80)); 拒绝++; }
  }
  assert.ok(接受 > 50 && 拒绝 > 50, `样本应两类都足够：接受 ${接受}，拒绝 ${拒绝}`);
});

// 文言：JSON 答复以宿主校验为准：坏文可捕、数值不失真、大数组不崩。汉语：无效 JSON（含未闭合字符串、坏转义）得到可捕获的失败；合法数值按宿主 JSON 规范化，不再被癸象往返回绕或截断。
test('JSON 答复经宿主校验：坏文可捕获，数值不失真，大数组不崩', async () => {
  for (const 坏 of ['"abc', '{"a":"x', '"\\q"', '-', '[1e', '{"a"', '\u0000']) {
    assert.match(错(await 令({fn: 'json', status: 200, headers: [], body: 坏})), /JSON 响应正文无效/, JSON.stringify(坏));
    assert.match(错(await 令({fn: 'old-json', status: 200, body: 坏})), /JSON 响应正文无效/, 'old ' + JSON.stringify(坏));
  }
  for (const [入, 出] of [['[12345678901234567890]', '[12345678901234567000]'], ['[0.1234567]', '[0.1234567]'], ['[1e21]', '[1e+21]'], [' { "a" : [ 1 , 2 ] } ', '{"a":[1,2]}'], ['"\\u00e9"', '"é"']]) {
    const 果 = await 令({fn: 'json', status: 200, headers: [], body: 入});
    assert.equal(果.状态, 200, 入);
    assert.equal(果.文, 出, 入);
  }
  const 大 = JSON.stringify(Array.from({length: 10000}, (_, i) => ({id: i, name: 'abc', tags: ['x', 'y'], ok: true})));
  const 果 = await 令({fn: 'json', status: 200, headers: [], body: 大});
  assert.equal(果.状态, 200);
  assert.equal(果.文, 大);
});

// 文言：旧改址术遇相对网址、旧文字术遇含换行之类，皆可捕之败，不越宿主之界。汉语：回应入站同源改址、回应入站外址改址 的目标不是可解析的绝对网址时，回应入站文字 的内容类型含 CR/LF 时，都抛可捕获的豫言异常。
test('旧改址与旧文字答复：相对网址与含换行的内容类型可捕获地拒绝', async () => {
  for (const fn of ['old-same', 'old-ext']) for (const target of ['/relative', 'not a url', '//x', '']) {
    assert.match(错(await 令({fn, status: 302, target})), /重定向目标不是有效的绝对网址/, fn + ' ' + JSON.stringify(target));
  }
  for (const type of ['text/plain\r\nX-A: 1', 'text/plain\nX']) assert.match(错(await 令({fn: 'old-text', status: 200, type, body: 'x'})), /内容类型含换行/, JSON.stringify(type));
});
