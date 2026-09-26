// 网址查询 0.1.0 一致性测试：真实 Wasm + Node 宿主；期望值取自同一 WHATWG URL / URLSearchParams 实现。
// 复跑：在私有暂存目录（含 dist/）里 `node --test <本文件>`；产物位置可用环境变量 产物根 指定（默认 <当前目录>/dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {pathToFileURL} from 'node:url';
import path from 'node:path';

const 产物 = path.resolve(process.env.产物根 ?? path.join(process.cwd(), 'dist'), '网址查询一致性');
const {创建云工宿主} = await import(pathToFileURL(path.join(产物, '宿主.mjs')).href);
const 程序模块 = await WebAssembly.compile(await readFile(path.join(产物, '程序.wasm')));
const 值桥模块 = await WebAssembly.compile(await readFile(path.join(产物, '值桥.wasm')));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {}});
const 上下文 = {waitUntil(承诺) { Promise.resolve(承诺).catch(() => {}); }};
const MiB = 1024 * 1024;

async function 发(路径, {体, 参数 = {}} = {}) {
  const 查询 = Object.entries(参数).map(([k, v]) => k + '=' + encodeURIComponent(v)).join('&');
  const 回 = await 宿主.fetch(new Request('https://x.test' + 路径 + (查询 ? '?' + 查询 : ''), {method: 体 === undefined ? 'GET' : 'POST', body: 体}), {}, 上下文);
  return {状态: 回.status, 文: await 回.text()};
}
const 解十六 = 文 => Buffer.from(文, 'hex').toString('utf8');
// 结果不超过 2048 字节时应用给出十六进制；更大时给出 sha:<字节数>:<SHA-256>，此处折算为可比较的对象。
const 摘要文 = 文 => 'sha:' + Buffer.byteLength(文) + ':' + createHash('sha256').update(文).digest('hex');
function 成功十六(果, 期) {
  assert.equal(果.状态, 200, 果.文.slice(0, 200));
  if (果.文.startsWith('sha:')) return 期 !== undefined && Buffer.byteLength(期) > 2048 ? (果.文 === 摘要文(期) ? 期 : 果.文) : 果.文;
  return 解十六(果.文);
}
function 错(果) { assert.equal(果.状态, 400, '应得到可捕获的失败：' + 果.文.slice(0, 200)); assert.ok(果.文.startsWith('err|')); return 解十六(果.文.slice(4)); }
const 编码 = 项列 => 发('/enc', {体: JSON.stringify(项列)});
const 期编码 = 项列 => new URLSearchParams(项列).toString();

test('编码查询串：与 URLSearchParams 序列化一致，含空格、保留符号、UTF-8、控制字符与同名项', async () => {
  for (const 项列 of [
    [], [['a', 'b']], [['a b', 'c d']], [['k', '+']], [['k', '%']], [['k', '&=#?/']], [['k', "!'()~*-._"]], [['k', '~']],
    [['a', '1'], ['a', '2'], ['b', '']], [['', 'v']], [['', '']], [['名', '值🙂']], [['k', '\u0001\u001f\u007f']], [['k', 'a\nb\tc\r']], [['k', '\u0000']],
    [['k', ' ']], [['é', 'ü']], [['a', '1'], ['b', '2'], ['a', '3']], [['x'.repeat(1000), 'y'.repeat(5000)]], [['a b', 'c d']]
  ]) {
    assert.equal(成功十六(await 编码(项列), 期编码(项列)), 期编码(项列), JSON.stringify(项列).slice(0, 60));
  }
});

test('编码查询串：项数与总字节限额', async () => {
  const 千 = Array.from({length: 1024}, (_, i) => ['k' + i, 'v' + i]);
  assert.equal(成功十六(await 编码(千), 期编码(千)), 期编码(千));
  assert.match(错(await 编码(千.concat([['k', 'v']]))), /查询项超过 1024 项/);
  const 恰 = [['a'.repeat(MiB - 1), 'b']];
  assert.equal(成功十六(await 编码(恰), 期编码(恰)), 期编码(恰));
  assert.match(错(await 编码([['a'.repeat(MiB), 'b']])), /查询项总长度超过 1 MiB/);
  assert.match(错(await 编码([['a'.repeat(MiB - 1), 'bb']])), /查询项总长度超过 1 MiB/);
});

test('解析表单键值：保序 JSON 数组，加号为空格，宽松 UTF-8，首个问号被去掉', async () => {
  for (const 文 of [
    '', 'a=1', 'a=1&b=2', 'a=1&a=2&b=3&a=4', '+', 'a+b=c+d', '%20', 'a=%E4%BD%A0', 'a=%ff', 'a=%e4%bd', '%', '%zz', 'a=%zz&b=%2', '&&&', '=', '=v', 'k', 'k=', '?a=1', '??a=1', ' ', 'a=b=c', 'a=1;b=2', '%00', '\u0001=\u0002', 'é=ü&名=值🙂', 'a=%F0%9F%99%82', 'a=%25', 'x=' + 'y'.repeat(MiB - 4),
    'a=1\nb=2', 'k=\u007f'
  ]) {
    const 果 = await 发('/parse', {体: 文});
    assert.equal(果.状态, 200, JSON.stringify(文).slice(0, 40) + ' ' + 果.文.slice(0, 80));
    assert.deepEqual(JSON.parse(果.文), [...new URLSearchParams(文)], JSON.stringify(文).slice(0, 60));
  }
});

test('解析表单键值：结果是 JSON 数组文，输入上限 1 MiB', async () => {
  assert.equal((await 发('/parse', {体: 'a=1&b=%E4%BD%A0'})).文, '[["a","1"],["b","你"]]');
  assert.equal((await 发('/parse', {体: ''})).文, '[]');
  const 恰 = 'x='.padEnd(MiB, 'y');
  assert.equal(Buffer.byteLength(恰), MiB);
  assert.equal(JSON.parse((await 发('/parse', {体: 恰})).文)[0][1].length, MiB - 2);
  assert.match(错(await 发('/parse', {体: 恰 + 'y'})), /表单文字超过 1 MiB/);
  const 汉 = '名='.repeat(3);
  assert.deepEqual(JSON.parse((await 发('/parse', {体: 汉})).文), [...new URLSearchParams(汉)]);
});

test('设置网址查询参数：绝对与相对网址、替换同名项、保留其余参数与片段', async () => {
  const 基址 = 'https://yuyan-lang.org';
  const 期望 = (文, 键, 值) => {
    const 绝对 = URL.canParse(文);
    const 址 = 绝对 ? new URL(文) : new URL(文, 基址);
    址.searchParams.set(键, 值);
    return 绝对 ? 址.href : 址.pathname + 址.search + 址.hash;
  };
  for (const [文, 键, 值] of [
    ['/p?a=1&b=2&a=3#h', 'a', '新 值'], ['https://h.example/p?a=1', 'b', '2'], ['https://h.example/p', 'q', ''], ['https://h.example', 'q', 'v'], ['http://h.example:8080/a#f', 'k', '值🙂'],
    ['/p', '', 'v'], ['/p', 'k', 'a&b=c#d?e/f'], ['p', 'k', 'v'], ['?a=1', 'a', '2'], ['#h', 'k', 'v'], ['', 'k', 'v'], ['../x?y=1#z', 'k', 'v'], ['/a b?c d=e f', 'k', 'v'],
    ['/p?x=%7E&y=a+b', 'k', 'v'], ['/p', 'k', '\u0001\n\t'], ['/p', '\n', '\u0000'], ['https://例え.jp/パス?キー=値', '名', '值'], ['https://h.example/?a=1&b=2&c=3', 'b', 'x'], ['/p?a=1', 'a', ''],
    ['/' + 'x'.repeat(60000), 'k', 'v']
  ]) {
    const 果 = await 发('/set', {参数: {u: 文, k: 键, v: 值}});
    assert.equal(成功十六(果, 期望(文, 键, 值)), 期望(文, 键, 值), JSON.stringify([文, 键, 值]).slice(0, 60));
  }
});

test('设置网址查询参数：来源不得改变，非 HTTP(S) 绝对网址与无效网址失败，长度限额', async () => {
  for (const 文 of ['//evil.example/x', '\\\\evil.example\\x', '/\\evil.example', '//evil.example']) {
    const 消息 = 错(await 发('/set', {参数: {u: 文, k: 'k', v: 'v'}}));
    assert.ok(/相对网址不得改变来源|网址无效/.test(消息), 文 + ' → ' + 消息);
  }
  for (const 文 of ['javascript:alert(1)', 'data:text/html,x', 'ftp://h.example/x', 'mailto:a@b', 'file:///etc/passwd', 'ws://h.example/', 'blob:https://h.example/x']) assert.match(错(await 发('/set', {参数: {u: 文, k: 'k', v: 'v'}})), /绝对网址不是 HTTP\(S\)/, 文);
  assert.match(错(await 发('/set', {参数: {u: 'http://', k: 'k', v: 'v'}})), /网址无效/);
  assert.match(错(await 发('/set', {参数: {u: 'https://h.example:99999/', k: 'k', v: 'v'}})), /网址无效/);
  const 六四 = 64 * 1024;
  assert.equal((await 发('/set', {参数: {u: '/' + 'a'.repeat(六四 - 1), k: 'k', v: 'v'}})).状态, 200);
  assert.match(错(await 发('/set', {参数: {u: '/' + 'a'.repeat(六四), k: 'k', v: 'v'}})), /超过 64 KiB/);
  assert.equal((await 发('/set', {参数: {u: '/p', k: 'k'.repeat(六四), v: 'v'}})).状态, 200);
  assert.match(错(await 发('/set', {参数: {u: '/p', k: 'k'.repeat(六四 + 1), v: 'v'}})), /超过 64 KiB/);
  assert.equal((await 发('/set', {参数: {u: '/p', k: 'k', v: 'v'.repeat(六四)}})).状态, 200);
  assert.match(错(await 发('/set', {参数: {u: '/p', k: 'k', v: 'v'.repeat(六四 + 1)}})), /超过 64 KiB/);
});

// 确定性伪随机（mulberry32），便于复现。
function 随机器(种子) { let a = 种子 >>> 0; return () => { a = (a + 0x6D2B79F5) >>> 0; let t = a; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; }; }

const 字池 = Array.from("abcXYZ019-_.~!'()*+%&=#?/: ", c => c).concat(['\t', '\n', '\r', '\u0000', '\u0001', '\u001f', '\u007f', '"', '\\', 'é', '你', '🙂', ' ']);
function 随机文(随, 最长) { const 长 = Math.floor(随() * 随() * 最长); let s = ''; for (let i = 0; i < 长; i++) s += 字池[Math.floor(随() * 字池.length)]; return s; }

test('差分模糊：随机名值列的 编码查询串 与 URLSearchParams 一致', async () => {
  const 随 = 随机器(20261002);
  for (let i = 0; i < 400; i++) {
    const 项列 = Array.from({length: Math.floor(随() * 6)}, () => [随机文(随, 12), 随机文(随, 24)]);
    assert.equal(成功十六(await 编码(项列), 期编码(项列)), 期编码(项列), JSON.stringify(项列).slice(0, 80));
  }
});

test('差分模糊：随机字节的 解析表单键值 与 URLSearchParams（宽松 UTF-8）一致', async () => {
  const 随 = 随机器(20261003);
  const 词 = ['a', 'b=', '=', '&', '+', '%', '%41', '%zz', '%e4%bd%a0', '%ff', '?', ';', 'k', 'v', ' ', '\u0001', '你', '🙂'];
  for (let i = 0; i < 400; i++) {
    const 段 = [];
    for (let j = 0, n = Math.floor(随() * 10); j < n; j++) 段.push(Buffer.from(词[Math.floor(随() * 词.length)], 'utf8'));
    if (随() < 0.3) 段.push(Buffer.from([Math.floor(随() * 256), Math.floor(随() * 256)]));
    const 体 = Buffer.concat(段);
    const 期 = [...new URLSearchParams(new TextDecoder().decode(体))];
    const 果 = await 发('/parse', {体});
    assert.equal(果.状态, 200, JSON.stringify(体.toString('latin1')).slice(0, 60) + ' ' + 果.文.slice(0, 80));
    assert.deepEqual(JSON.parse(果.文), 期, JSON.stringify(体.toString('latin1')).slice(0, 60));
  }
});

test('差分模糊：随机网址与键值的 设置网址查询参数 与 URL 一致', async () => {
  const 随 = 随机器(20261004);
  const 基址 = 'https://yuyan-lang.org';
  const 片 = ['https://', 'http://', 'h.example', ':8080', '/', 'p', 'q', '?', 'a=1', '&', 'b=%20', '#', 'f', '..', '.', '//', '\\', ' ', '%', '你', '\u0001', '\t'];
  let 成 = 0, 败 = 0;
  for (let i = 0; i < 800; i++) {
    let 文 = '';
    for (let j = 0, n = Math.floor(随() * 8); j < n; j++) 文 += 片[Math.floor(随() * 片.length)];
    const 键 = 随机文(随, 8), 值 = 随机文(随, 16);
    const 绝对 = URL.parse(文);
    let 期;
    if (绝对) 期 = (绝对.protocol === 'http:' || 绝对.protocol === 'https:') ? (绝对.searchParams.set(键, 值), 绝对.href) : null;
    else { const 相对 = URL.parse(文, 基址); 期 = 相对 && 相对.origin === 基址 ? (相对.searchParams.set(键, 值), 相对.pathname + 相对.search + 相对.hash) : null; }
    const 果 = await 发('/set', {参数: {u: 文, k: 键, v: 值}});
    if (期 !== null) { assert.equal(成功十六(果, 期), 期, JSON.stringify([文, 键, 值]).slice(0, 80)); 成++; }
    else { assert.equal(果.状态, 400, JSON.stringify([文, 键, 值]).slice(0, 80) + ' ' + 果.文.slice(0, 60)); 败++; }
  }
  assert.ok(成 > 100 && 败 > 100, `样本应两类都足够：成功 ${成}，失败 ${败}`);
});

// 文言：表单项数至一千二十四为限，过之则败而可捕。汉语：解析表单键值 最多 1024 项，1025 项抛可捕获的豫言异常（免得百万项拖垮癸象解析）。
test('解析表单键值：项数上限 1024', async () => {
  const 项 = n => Array.from({length: n}, (_, i) => 'k' + i + '=v').join('&');
  assert.equal(JSON.parse((await 发('/parse', {体: 项(1024)})).文).length, 1024);
  assert.match(错(await 发('/parse', {体: 项(1025)})), /表单项数超过 1024/);
});
