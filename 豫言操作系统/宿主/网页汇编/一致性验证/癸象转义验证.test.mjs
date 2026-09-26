// 文言：以真豫言 Wasm 对拍 JSON 串之解（\uXXXX、代理对、孤代理项）与编（控制字符）。
// 汉语：真实 Wasm 对拍。构建“癸象转义验证应用”（见 说明.汉语.md），然后在构建根目录运行
//   node --test 豫言操作系统/宿主/网页汇编/一致性验证/癸象转义验证.test.mjs
// 产物目录默认为 <当前目录>/dist/癸象转义验证/，可用环境变量 YY_ESCAPE_DIST 指定；YY_ESCAPE_ROUNDS 调整随机轮数（默认 1200）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {pathToFileURL} from 'node:url';
import path from 'node:path';

const 根 = process.env.YY_ESCAPE_DIST
  ? pathToFileURL(process.env.YY_ESCAPE_DIST.replace(/\/?$/, '/'))
  : pathToFileURL(path.resolve(process.cwd(), 'dist/癸象转义验证') + '/');
const {创建云工宿主} = await import(new URL('宿主.mjs', 根));
const 程序模块 = await WebAssembly.compile(await readFile(new URL('程序.wasm', 根)));
const 值桥模块 = await WebAssembly.compile(await readFile(new URL('值桥.wasm', 根)));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {}});

const 调 = async (路径, {方法 = 'POST', 体, 头} = {}) => {
  const 回 = await 宿主.fetch(new Request('https://x.test' + 路径, {method: 方法, body: 体, headers: 头}), {});
  const 字节 = Buffer.from(await 回.arrayBuffer());
  return {状态: 回.status, 字节, 文: 字节.toString('utf8')};
};
const 圆 = 文 => 调('/round', {体: 文});
const 原 = 文 => 调('/raw', {体: 文});
const 编 = 文 => 调('/enc', {体: 文});
const 好 = 串 => 串.toWellFormed();
const 十六 = (值, 大写) => { const s = 值.toString(16).padStart(4, '0'); return 大写 ? s.toUpperCase() : s; };

let 种 = 20260925;
const 随 = n => { 种 = (Math.imul(种, 1664525) + 1013904223) >>> 0; return (种 >>> 8) % n; };
const 随机码 = () => {
  switch (随(10)) {
    case 0: return 随(0x20);                         // 控制字符（含 NUL、ESC）
    case 1: return 随(0x80);
    case 2: return 0x22;
    case 3: return 0x5C;
    case 4: return 0xD800 + 随(0x800);               // 孤立代理项
    case 5: return 随(0x10000);
    case 6: return 0x10000 + 随(0x100000);           // 非 BMP
    case 7: return [0x7F, 0x2028, 0x2029, 0xFEFF, 0xFFFD, 0xFFFE, 0xFFFF, 0, 0x1B, 0x2F][随(10)];
    case 8: return 0x4E00 + 随(0x5000);              // 中文
    default: return 0x20 + 随(0x5F);                 // 可打印 ASCII
  }
};
const 随机串 = 最长 => {
  let 串 = ''; const 长 = 随(最长 + 1);
  for (let k = 0; k < 长; k++) { const c = 随机码(); 串 += c >= 0x10000 ? String.fromCodePoint(c) : String.fromCharCode(c); }
  return 串;
};
const 次数 = Number(process.env.YY_ESCAPE_ROUNDS ?? 1200);

test('中文、emoji 代理对、大小写十六进制、混合转义：解码字节与 JSON.parse 一致', async () => {
  const 例 = [
    '"中文"', '"\\u4e2d\\u6587"', '"\\u4E2D\\u6587"', '"\\u4e2D\\u6E87"',
    '"\\ud83d\\ude00"', '"\\uD83D\\uDE00"', '"\\ud83D\\uDe00"', '"😀"', '"a\\ud83d\\ude00b\\ud83d\\ude01c"',
    '"\\u0041\\u00e9\\u20ac"', '"\\uffff\\uFFFF\\ue000\\uf8ff"',
    '"\\/"', '"\\u002f"', '"\\u0022\\u005c"', '"\\n\\t\\r\\b\\f\\\\\\/\\""',
    '"\\u001b[31m红\\u001b[0m"', '"\\u007f"', '"\\u2028\\u2029"', '"\\u0000"', '"a\\u0000b"', '""',
    '"\\u0001\\u0002\\u001f"', '"\\u0008\\u0009\\u000a\\u000c\\u000d"', '"\\u000b\\u000e\\u000f"',
    '"甲\\u4e59\\ud83d\\ude00\\n丁"',
  ];
  for (const 文 of 例) {
    const 果 = await 原(文);
    assert.equal(果.状态, 200, 文);
    assert.deepEqual(果.字节, Buffer.from(好(JSON.parse(文)), 'utf8'), 文);
  }
  // 明确的字节：\u0000 → 字节 0；emoji → F0 9F 98 80
  assert.deepEqual((await 原('"\\u0000"')).字节, Buffer.from([0]));
  assert.deepEqual((await 原('"\\ud83d\\ude00"')).字节, Buffer.from([0xF0, 0x9F, 0x98, 0x80]));
});

test('孤立高/低代理项替换为 U+FFFD，成对者合并', async () => {
  const FFFD = Buffer.from([0xEF, 0xBF, 0xBD]);
  const 例 = [
    ['"\\ud800"', [FFFD]], ['"\\udc00"', [FFFD]], ['"\\udbff"', [FFFD]], ['"\\udfff"', [FFFD]],
    ['"\\ud83d\\u0041"', [FFFD, Buffer.from('A')]], ['"\\ud83dA"', [FFFD, Buffer.from('A')]], ['"\\ud83d\\n"', [FFFD, Buffer.from('\n')]],
    ['"\\ud83d\\ud83d"', [FFFD, FFFD]], ['"\\ude00\\ud83d"', [FFFD, FFFD]], ['"\\ude00\\ude00"', [FFFD, FFFD]],
    ['"\\ud83d\\ud83d\\ude00"', [FFFD, Buffer.from('😀')]], ['"x\\ud83d"', [Buffer.from('x'), FFFD]],
  ];
  for (const [文, 段] of 例) {
    const 果 = await 原(文);
    assert.deepEqual(果.字节, Buffer.concat(段), 文);
    assert.deepEqual(果.字节, Buffer.from(好(JSON.parse(文)), 'utf8'), 文);
  }
});

test('解析再编码：字符串输出与 JSON.stringify 逐字节一致', async () => {
  const 例 = [
    '"\\u4e2d\\u6587"', '"\\ud83d\\ude00"', '"\\u001b"', '"\\u001B"', '"\\u0000"', '"\\u007f"', '"\\u2028"', '"\\/"',
    '"\\u0008\\u0009\\u000a\\u000c\\u000d"', '"\\u0001\\u001f"', '"\\ud83d"', '"\\n\\t\\r\\b\\f\\\\\\/\\""',
  ];
  for (const 文 of 例) {
    const 果 = await 圆(文);
    assert.equal(果.状态, 200, 文);
    assert.equal(果.文, JSON.stringify(好(JSON.parse(文))), 文);
  }
  // 编码规则逐项：短转义、\u00xx 小写、斜杠/DEL/U+2028/U+2029 不转义
  assert.equal((await 圆('"\\u0001\\u001f\\u001b\\u001B"')).文, '"\\u0001\\u001f\\u001b\\u001b"');
  assert.equal((await 圆('"\\u0008\\u0009\\u000a\\u000c\\u000d"')).文, '"\\b\\t\\n\\f\\r"');
  assert.equal((await 圆('"\\/\\u007f\\u2028\\u2029"')).文, '"/\u007f\u2028\u2029"');
  assert.equal((await 圆('"\\u0000"')).文, '"\\u0000"');
});

test('嵌套对象与数组：键名、值都含转义', async () => {
  const 文 = '{"\\u0061":"\\u00e9","b":["\\ud83d\\ude00",null,true,false,1,-2,"\\u001b"],"\\u4e2d\\u6587":{"\\u0022":[{"\\n":"\\t"}]},"c\\/d":""}';
  const 果 = await 圆(文);
  assert.equal(果.状态, 200);
  assert.deepEqual(JSON.parse(果.文), JSON.parse(文));
  assert.equal(果.文, '{"a":"é","b":["😀",null,true,false,1,-2,"\\u001b"],"中文":{"\\"":[{"\\n":"\\t"}]},"c/d":""}');
  // 正文整体是数组/数字/字面量
  for (const 简 of ['null', 'true', 'false', '-12', '[]', '{}', '[[],{}]', '["\\u001b",1]']) {
    const 简果 = await 圆(简);
    assert.equal(简果.状态, 200, 简);
    assert.equal(简果.文, JSON.stringify(JSON.parse(简)), 简);
  }
});

test('编码器：任意字符串（含全部控制字符）编码结果等于 JSON.stringify', async () => {
  const 全部控制 = Array.from({length: 32}, (_, k) => String.fromCharCode(k)).join('');
  const 例 = [
    '', 'abc', 'a"b', 'a\\b', 'a/b', '中文😀', 全部控制, '甲' + 全部控制 + '乙', 'x\u007fy', '\u2028\u2029', '"\\"',
    '\u001b[31m', 'NUL\u0000中间', '\n\t\r\b\f', '\u0000', '\u0001', '\u001f', 'a\u001bb"c\\d\u0001中', '😀\u0000😀',
  ];
  for (const 串 of 例) {
    const 果 = await 编(串);
    assert.equal(果.状态, 200, JSON.stringify(串));
    assert.equal(果.文, JSON.stringify(串), JSON.stringify(串));
  }
  // 每个 UTF-16 码元（除代理项）逐个：字符夹在字母之间
  for (let 码 = 0; 码 < 0x3000; 码 += 1) {
    if (码 >= 0xD800 && 码 <= 0xDFFF) continue;
    const 串 = 'a' + String.fromCharCode(码) + 'b';
    const 果 = await 编(串);
    assert.equal(果.文, JSON.stringify(串), '码 ' + 码.toString(16));
  }
});

test('随机语料：JSON.stringify → 豫言解析 → 豫言编码 → JSON.parse 等于原串', async () => {
  for (let 轮 = 0; 轮 < 次数; 轮++) {
    const 串 = 随机串(40);
    const 文 = JSON.stringify(串);
    const 果 = await 圆(文);
    assert.equal(果.状态, 200, 文);
    assert.equal(JSON.parse(果.文), 好(串), '轮 ' + 轮 + ' ' + 文);
    assert.equal(果.文, JSON.stringify(好(串)), '轮 ' + 轮);
  }
});

test('随机语料：解码字节、编码字符串、宿主往返（/raw /enc /host）', async () => {
  for (let 轮 = 0; 轮 < 次数; 轮++) {
    const 串 = 随机串(30);
    const 期 = 好(串);
    // 解码：JSON.stringify 对控制字符与孤立代理项输出 \u 转义
    const 解 = await 原(JSON.stringify(串));
    assert.deepEqual(解.字节, Buffer.from(期, 'utf8'), '解码 轮 ' + 轮);
    // 编码：请求正文（UTF-8）原样进入豫言后再编码
    // 请求正文经 text() 读取时，标准会剥去开头的 U+FEFF（BOM），故编码用例统一加一个前缀字符
    const 编果 = await 编('x' + 期);
    assert.equal(编果.文, JSON.stringify('x' + 期), '编码 轮 ' + 轮);
    // 宿主 request.json() → JSON.stringify → 豫言解析 → 豫言编码：生产路径
    const 宿 = await 调('/host', {体: JSON.stringify({键: 串, 列: [串, {[串]: 1}]}), 头: {'content-type': 'application/json'}});
    assert.equal(宿.状态, 200, '宿主 轮 ' + 轮);
    assert.deepEqual(JSON.parse(宿.文), JSON.parse(JSON.stringify({键: 期, 列: [期, {[期]: 1}]})), '宿主 轮 ' + 轮);
  }
});

test('随机语料：随机转义写法（大小写十六进制、\\/、拼合的代理对）的解码', async () => {
  const 短 = [['\\n', '\n'], ['\\t', '\t'], ['\\r', '\r'], ['\\b', '\b'], ['\\f', '\f'], ['\\\\', '\\'], ['\\/', '/'], ['\\"', '"']];
  const 原文 = ['a', 'Z', '0', ' ', '中', '文', '😀', 'é', '\u007f', '\u2028', '{', '}', ',', ':'];
  for (let 轮 = 0; 轮 < 次数; 轮++) {
    let 文 = '"', 期 = ''; const 段 = 随(24);
    for (let k = 0; k < 段; k++) {
      const 类 = 随(6);
      if (类 === 0) { const [源, 义] = 短[随(短.length)]; 文 += 源; 期 += 义; }
      else if (类 <= 2) { const 片 = 原文[随(原文.length)]; 文 += 片; 期 += 片; }
      else if (类 === 3) { const u = 随(0x10000); 文 += '\\u' + 十六(u, 随(2)); 期 += String.fromCharCode(u); }
      else if (类 === 4) { const h = 0xD800 + 随(0x400), l = 0xDC00 + 随(0x400); 文 += '\\u' + 十六(h, 随(2)) + '\\u' + 十六(l, 随(2)); 期 += String.fromCharCode(h, l); }
      else { const u = 0xD800 + 随(0x800); 文 += '\\u' + 十六(u, 随(2)); 期 += String.fromCharCode(u); }
    }
    文 += '"';
    const 果 = await 原(文);
    assert.deepEqual(果.字节, Buffer.from(好(期), 'utf8'), '轮 ' + 轮 + ' ' + 文);
    const 圆果 = await 圆(文);
    assert.equal(圆果.文, JSON.stringify(好(期)), '往返 轮 ' + 轮 + ' ' + 文);
  }
});

test('超长字符串（数十万字符，含大量转义）', async () => {
  const 块 = ['a', '中', '😀', '\n', '\u0000', '\u001b', '"', '\\', '\ud800', '\u2028', 'z'];
  let 串 = ''; for (let k = 0; k < 120000; k++) 串 += 块[k % 块.length];
  const 文 = JSON.stringify(串);
  assert.ok(文.length > 150000);
  const t0 = Date.now();
  const 解 = await 原(文);
  assert.deepEqual(解.字节, Buffer.from(好(串), 'utf8'));
  const 圆果 = await 圆(文);
  assert.equal(圆果.文, JSON.stringify(好(串)));
  const 编果 = await 编(好(串));
  assert.equal(编果.文, JSON.stringify(好(串)));
  // 一百万个 \u4e2d 的六字节转义
  const 大 = '"' + '\\u4e2d'.repeat(200000) + '"';
  assert.equal((await 原(大)).字节.length, 600000);
  console.log('  超长字符串三路对拍耗时 ' + (Date.now() - t0) + ' ms');
});

test('宿主路径：查询参数与标头带控制字符（宿主 JSON.stringify 输出 \\u00xx）', async () => {
  const 串们 = ['\u001b[31m红\u001b[0m', 'a\u0001b', '\u007f', '😀\u001f', '"\\', '\t制表', '\u2028'];
  for (const 串 of 串们) {
    const 果 = await 调('/q?p=' + encodeURIComponent(串), {方法: 'GET'});
    assert.equal(果.状态, 200, JSON.stringify(串));
    assert.equal(果.文, 串, JSON.stringify(串));
  }
  // NUL 经 %00 进入查询参数
  const 零 = await 调('/q?p=a%00b', {方法: 'GET'});
  assert.equal(零.状态, 200);
  assert.deepEqual(零.字节, Buffer.from('a\u0000b'));
  // 标头值可含 ESC（HTTP 头不允许 NUL/CR/LF）
  const 头 = await 调('/h', {方法: 'GET', 头: {'x-t': 'v\u001bw'}});
  assert.equal(头.状态, 200);
  assert.equal(头.文, 'v\u001bw');
  // 宿主 request.json() 生产路径：带 ESC 的用户消息不再触发 500
  const 消息 = {role: 'user', content: '你好\u001b[1m粗体\u001b[0m\n第二行\u0000\u0001'};
  const 宿 = await 调('/host', {体: JSON.stringify(消息), 头: {'content-type': 'application/json'}});
  assert.equal(宿.状态, 200);
  assert.deepEqual(JSON.parse(宿.文), 消息);
});

test('畸形转义一律使请求失败（Wasm 中止），且不影响后续请求', async () => {
  const 坏 = [
    '"\\u"', '"\\u1"', '"\\u12"', '"\\u123"', '"\\uZZZZ"', '"\\u12G4"', '"\\u00e"', '"\\u 123"', '"\\u-123"', '"\\u+123"', '"\\u0x12"',
    '"\\x41"', '"\\0"', '"\\a"', '"\\ "', '"\\v"', '"\\U0041"', '"\\N"', '"abc', '"abc\\"',
    '"\\ud83d\\u12"', '"\\ud83d\\uZZZZ"', '"\\u12\\"34"',
  ];
  for (const 文 of 坏) {
    await assert.rejects(原(文), 错 => 错 instanceof WebAssembly.RuntimeError || /unreachable|RuntimeError|out of bounds/i.test(String(错)), 文);
    await assert.rejects(圆(文), undefined, 文);
  }
  // 数组、对象中的畸形转义同样中止
  await assert.rejects(圆('["ok","\\uZZZZ"]'));
  await assert.rejects(圆('{"k\\u12":1}'));
  // 一次中止不污染下一次请求（每个事件新建 Wasm 实例）
  assert.equal((await 圆('"\\u4e2d"')).文, '"中"');
  assert.equal((await 编('a\u001bb')).文, '"a\\u001bb"');
});
