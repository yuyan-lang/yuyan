// 网址定位 0.2.0 一致性测试：真实 Wasm + Node 宿主；期望值取自同一 WHATWG URL 实现。
// 复跑：在私有暂存目录（含 dist/）里 `node --test <本文件>`；产物位置可用环境变量 产物根 指定（默认 <当前目录>/dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {pathToFileURL} from 'node:url';
import path from 'node:path';

const 产物 = path.resolve(process.env.产物根 ?? path.join(process.cwd(), 'dist'), '网址定位一致性');
const {创建云工宿主} = await import(pathToFileURL(path.join(产物, '宿主.mjs')).href);
const 程序模块 = await WebAssembly.compile(await readFile(path.join(产物, '程序.wasm')));
const 值桥模块 = await WebAssembly.compile(await readFile(path.join(产物, '值桥.wasm')));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {}});
const 上下文 = {waitUntil(承诺) { Promise.resolve(承诺).catch(() => {}); }};

async function 调(路径, 参数) {
  const 回 = await 宿主.fetch(new Request('https://x.test' + 路径 + '?' + Object.entries(参数).map(([k, v]) => k + '=' + encodeURIComponent(v)).join('&')), {}, 上下文);
  return {状态: 回.status, 文: await 回.text()};
}
const 字段 = ['href', 'origin', 'pathname', 'search', 'protocol', 'hostname', 'host', 'port', 'username', 'password', 'hash'];
const 期望 = 址 => Object.fromEntries(字段.map(名 => [名, 址[名]]));
function 成功(果) { assert.equal(果.状态, 200, 果.文); return JSON.parse(果.文); }
function 错(果) { assert.equal(果.状态, 400, '应得到可捕获的失败：' + 果.文); assert.ok(果.文.startsWith('err|')); return Buffer.from(果.文.slice(4), 'hex').toString('utf8'); }

test('解析绝对网址部件：十一个字段与 URL 标准一致，键序固定', async () => {
  for (const 文 of [
    'https://user:pw@Example.COM:8443/a/../b%20c?x=1&y=é#frag', 'http://a.example/', 'http://a.example', 'https://a.example:443/x', 'http://a.example:80/x', 'http://a.example:8080/x?', 'https://a.example/x#', 'https://a.example/?a=1&a=2#h1#h2',
    'https://例え.jp/パス?キー=値#見出し', 'http://[::1]:3000/p', 'http://0x7f.1/', 'http://192.168.0.1:81/', 'HTTPS://HOST.EXAMPLE/PATH', 'http://a.example/\u0001b', 'http://a.example/b\tc\nd', 'https://a.example/%E4%BD%A0?q=%E4%BD%A0#%E4%BD%A0', 'http://u@a.example/', 'http://:p@a.example/',
    'https://a.example/' + 'x'.repeat(60000)
  ]) {
    const 结果 = 成功(await 调('/abs', {u: 文}));
    assert.deepEqual(结果, 期望(new URL(文)), 文.slice(0, 60));
    assert.deepEqual(Object.keys(结果), 字段);
  }
});

test('解析绝对网址部件：无效网址与非 HTTP(S) 方案调用失败（可捕获）', async () => {
  for (const 文 of ['/rel', 'rel', '', '//a.example/x', 'http://', 'https://', 'http://a b/', 'http://a.example:99999/', 'not a url', '?q=1', '#h']) assert.match(错(await 调('/abs', {u: 文})), /网址不是有效的绝对网址/, JSON.stringify(文));
  for (const 文 of ['ftp://a.example/', 'file:///etc/passwd', 'mailto:a@b', 'javascript:alert(1)', 'data:text/html,x', 'ws://a.example/', 'wss://a.example/', 'blob:https://a.example/x']) assert.match(错(await 调('/abs', {u: 文})), /网址不是 HTTP\(S\)/, 文);
});

test('根据根址解析网址部件：相对、绝对、协议相对与各类引用', async () => {
  const 根 = 'http://h.example:8080/a/b/c?old=1#oldhash';
  for (const 文 of ['../x?y=1#z', '?q', '#h', '', '/abs', 'rel', './a/./b/../c', '//other.example/x', 'https://abs.example/p?q#f', '..', '.', '/', 'x y?z w#v w', '../../../..', '/中文?键=值#节', '\u0001', 'a\tb\nc']) {
    assert.deepEqual(成功(await 调('/rel', {u: 文, r: 根})), 期望(new URL(文, 根)), JSON.stringify(文));
  }
  assert.deepEqual(成功(await 调('/rel', {u: 'x', r: 'https://例え.jp'})), 期望(new URL('x', 'https://例え.jp')));
});

test('根据根址解析网址部件：根网址或结果不合规时失败', async () => {
  assert.match(错(await 调('/rel', {u: 'x', r: '/relative'})), /根网址不是有效的绝对网址/);
  assert.match(错(await 调('/rel', {u: 'x', r: ''})), /根网址不是有效的绝对网址/);
  assert.match(错(await 调('/rel', {u: 'x', r: 'ftp://h.example/'})), /根网址不是 HTTP\(S\)/);
  assert.match(错(await 调('/rel', {u: 'x', r: 'mailto:a@b'})), /根网址不是 HTTP\(S\)/);
  assert.match(错(await 调('/rel', {u: 'ftp://other.example/', r: 'http://h.example/'})), /网址不是 HTTP\(S\)/);
  assert.match(错(await 调('/rel', {u: 'javascript:alert(1)', r: 'http://h.example/'})), /网址不是 HTTP\(S\)/);
  assert.match(错(await 调('/rel', {u: 'http://', r: 'http://h.example/'})), /网址无法按根网址解析/);
  assert.match(错(await 调('/rel', {u: '//', r: 'http://h.example/'})), /网址无法按根网址解析/);
});

// 确定性伪随机（mulberry32），便于复现。
function 随机器(种子) { let a = 种子 >>> 0; return () => { a = (a + 0x6D2B79F5) >>> 0; let t = a; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; }; }

test('差分模糊：随机网址的部件与 URL 标准一致，失败情形也一致', async () => {
  const 随 = 随机器(20261005);
  const 片 = ['https://', 'http://', 'ftp://', 'HTTP://', 'user:pw@', 'h.example', 'Ex.COM', ':80', ':8080', ':443', '/', 'p', 'q', '?', 'a=1', '&', '#', 'f', '..', '.', '//', '\\', ' ', '%', '%41', '你', '例え.jp', '\u0001', '\t', '[::1]', '0x7f.1'];
  let 成 = 0, 败 = 0;
  for (let i = 0; i < 1200; i++) {
    let 文 = '';
    for (let j = 0, n = Math.floor(随() * 9); j < n; j++) 文 += 片[Math.floor(随() * 片.length)];
    if (i % 2 === 0) 文 = ['https://', 'http://', 'HTTP://'][Math.floor(随() * 3)] + ['h.example', 'Ex.COM', '例え.jp', '[::1]'][Math.floor(随() * 4)] + 文;
    const 址 = URL.parse(文);
    const 果 = await 调('/abs', {u: 文});
    if (址 && (址.protocol === 'http:' || 址.protocol === 'https:')) { assert.deepEqual(成功(果), 期望(址), JSON.stringify(文)); 成++; }
    else { assert.equal(果.状态, 400, JSON.stringify(文) + ' ' + 果.文.slice(0, 60)); 败++; }
    const 根 = 'https://base.example:8443/a/b?c=d#e';
    const 相 = URL.parse(文, 根);
    const 果2 = await 调('/rel', {u: 文, r: 根});
    if (相 && (相.protocol === 'http:' || 相.protocol === 'https:')) assert.deepEqual(成功(果2), 期望(相), '相对 ' + JSON.stringify(文));
    else assert.equal(果2.状态, 400, '相对 ' + JSON.stringify(文));
  }
  assert.ok(成 > 100 && 败 > 100, `样本应两类都足够：成功 ${成}，失败 ${败}`);
});
