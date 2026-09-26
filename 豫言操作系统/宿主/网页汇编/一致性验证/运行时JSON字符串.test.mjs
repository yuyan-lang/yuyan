// 文言：直验网页汇编运行时之 $json_string，穷举 \uXXXX 与代理对，兼验畸形必止。
// 汉语：把 豫言操作系统/宿主/网页汇编/运行时.wat 连同 运行时JSON字符串试验壳.wat 组装成一个模块，
// 直接调用 $json_string：穷举全部 65536 个 \uXXXX（大小写两式）与全部 1048576 个高低代理对，
// 再验孤立代理项、短转义、非零起点、畸形转义（必须触发 unreachable 陷阱）、随机 JSON.stringify 往返与超长串。
// 复跑（在语言仓根目录或含 豫言操作系统 符号链接与 yy网页汇编宿主 的私有暂存目录）：
//   node --test 豫言操作系统/宿主/网页汇编/一致性验证/运行时JSON字符串.test.mjs
// 环境变量：YY_ROOT（仓根，默认当前目录）、YY_WASM_HOST（yy网页汇编宿主 路径，默认 <仓根>/yy网页汇编宿主）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdtempSync, readFileSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';

const 仓根 = path.resolve(process.env.YY_ROOT ?? process.cwd());
const 宿主程序 = process.env.YY_WASM_HOST ?? path.join(仓根, 'yy网页汇编宿主');
const 运行时 = readFileSync(path.join(仓根, '豫言操作系统/宿主/网页汇编/运行时.wat'), 'utf8');
const 试验壳 = readFileSync(new URL('./运行时JSON字符串试验壳.wat', import.meta.url), 'utf8');
const 临时 = mkdtempSync(path.join(tmpdir(), 'yy-json-'));
let 二进制;
try {
  writeFileSync(path.join(临时, '模块.wat'), `(module\n${运行时}\n${试验壳}\n)\n`);
  execFileSync(宿主程序, ['--组装', path.join(临时, '模块.wat'), path.join(临时, '模块.wasm')], {cwd: 仓根, stdio: 'inherit'});
  二进制 = readFileSync(path.join(临时, '模块.wasm'));
} finally { rmSync(临时, {recursive: true, force: true}); }
const {instance} = await WebAssembly.instantiate(二进制, {'yuyan:gc-host/v1': {call: () => null}});
const {mem, decode, consumed} = instance.exports;
const 字节 = new Uint8Array(mem.buffer);
const 出偏 = 16 * 1024 * 1024;

// 解码 输入（字符串或字节）自 起 处的 JSON 字符串；返回 {字节, 耗}；畸形时抛 WebAssembly.RuntimeError
const 解 = (输入, 起 = 0) => {
  const b = typeof 输入 === 'string' ? Buffer.from(输入, 'utf8') : 输入;
  字节.set(b, 0);
  const n = decode(b.length, 起, 出偏);
  return {字节: Buffer.from(字节.subarray(出偏, 出偏 + n)), 耗: consumed.value};
};
const 陷 = (输入, 起 = 0) => { try { 解(输入, 起); return false; } catch (错) { assert.ok(错 instanceof WebAssembly.RuntimeError, String(错)); return true; } };
const 十六 = (值, 大写) => { const s = 值.toString(16).padStart(4, '0'); return 大写 ? s.toUpperCase() : s; };
const FFFD = Buffer.from([0xEF, 0xBF, 0xBD]);

test('穷举全部 65536 个 \\uXXXX（大小写两式）：代理项 → U+FFFD，其余 → UTF-8', () => {
  for (let u = 0; u < 65536; u++) {
    for (const 大 of [false, true]) {
      const 果 = 解('"\\u' + 十六(u, 大) + '"');
      const 期 = (u >= 0xD800 && u <= 0xDFFF) ? FFFD : Buffer.from(String.fromCharCode(u), 'utf8');
      assert.deepEqual(果.字节, 期, 'u=' + u.toString(16));
      assert.equal(果.耗, 8);
    }
  }
});

test('穷举全部 1048576 个高低代理对：合并为四字节 UTF-8', () => {
  const 头 = Buffer.from('"\\u0000\\u0000"');
  字节.set(头, 0);
  const 写 = (偏, 值) => { const s = 值.toString(16).padStart(4, '0'); for (let k = 0; k < 4; k++) 字节[偏 + k] = s.charCodeAt(k); };
  const 期 = Buffer.alloc(4);
  let 计 = 0;
  for (let h = 0xD800; h < 0xDC00; h++) {
    for (let l = 0xDC00; l < 0xE000; l++) {
      计++; 写(3, h); 写(9, l);
      const n = decode(头.length, 0, 出偏);
      const cp = 0x10000 + ((h - 0xD800) << 10) + (l - 0xDC00);
      期[0] = 0xF0 | (cp >> 18); 期[1] = 0x80 | ((cp >> 12) & 63); 期[2] = 0x80 | ((cp >> 6) & 63); 期[3] = 0x80 | (cp & 63);
      if (n !== 4 || consumed.value !== 14 || 字节[出偏] !== 期[0] || 字节[出偏 + 1] !== 期[1] || 字节[出偏 + 2] !== 期[2] || 字节[出偏 + 3] !== 期[3]) assert.fail(`h=${h.toString(16)} l=${l.toString(16)} n=${n} 耗=${consumed.value}`);
    }
  }
  assert.equal(计, 1048576);
  for (const [h, l] of [[0xD83D, 0xDE00], [0xD800, 0xDC00], [0xDBFF, 0xDFFF], [0xD801, 0xDC37]]) {
    const 文 = `"\\u${十六(h)}\\u${十六(l)}"`;
    assert.deepEqual(解(文).字节, Buffer.from(JSON.parse(文), 'utf8'));
  }
});

test('孤立高/低代理项与不成对组合', () => {
  const 例 = [
    ['"\\ud800"', [FFFD], 8], ['"\\udc00"', [FFFD], 8], ['"\\udbff"', [FFFD], 8], ['"\\udfff"', [FFFD], 8],
    ['"\\ud800\\u0041"', [FFFD, Buffer.from('A')], 14], ['"\\ud800\\ud800"', [FFFD, FFFD], 14],
    ['"\\ud800\\ud800\\udc00"', [FFFD, Buffer.from('\u{10000}')], 20],
    ['"\\ud800\\n"', [FFFD, Buffer.from('\n')], 10], ['"\\ud800abc"', [FFFD, Buffer.from('abc')], 11],
    ['"\\ud800\\\\"', [FFFD, Buffer.from('\\')], 10], ['"\\udc00\\ud800"', [FFFD, FFFD], 14], ['"\\udc00\\udc00"', [FFFD, FFFD], 14],
    ['"a\\ud83d"', [Buffer.from('a'), FFFD], 9], ['"\\ud83d\\/"', [FFFD, Buffer.from('/')], 10],
    ['"\\ud83d\\u00e9"', [FFFD, Buffer.from('é')], 14], ['"\\ud83d\\uD83D\\uDE00"', [FFFD, Buffer.from('😀')], 20],
  ];
  for (const [文, 段, 耗] of 例) {
    const 果 = 解(文);
    assert.deepEqual(果.字节, Buffer.concat(段), 文);
    assert.equal(果.耗, 耗, 文);
    assert.deepEqual(果.字节, Buffer.from(JSON.parse(文).toWellFormed(), 'utf8'), 文);
  }
});

test('短转义、\\/、混合、\\u0000、DEL、U+2028、非 BMP 原文与原始控制字符', () => {
  const 例 = [
    ['"\\n\\t\\r\\b\\f\\\\\\/\\""', '\n\t\r\b\f\\/"'],
    ['"\\u001b[31m\\u007f\\u2028\\u2029"', '\u001b[31m\u007f  '],
    ['"中文😀\\u4e2d\\u6587\\ud83d\\ude00"', '中文😀中文😀'],
    ['"\\u0000"', '\u0000'], ['"a\\u0000b"', 'a\u0000b'], ['""', ''],
    ['"\\u0041\\u00e9\\u20ac"', 'Aé€'], ['"\\uFFFF\\uffff\\uE000\\uf8ff"', '￿￿'],
    // 原始（未转义）控制字符与多字节字符保持宽容，原样保留
    ['"\u001b\u0000\u007f 😀\n\t"', '\u001b\u0000\u007f 😀\n\t'],
  ];
  for (const [文, 期] of 例) {
    const 果 = 解(文);
    assert.deepEqual(果.字节, Buffer.from(期, 'utf8'), 文);
    assert.equal(果.耗, Buffer.byteLength(文), 文);
  }
  assert.deepEqual([...解('"\\u0000"').字节], [0]);
});

test('非零起点与多个字符串', () => {
  const 文 = '["a\\u0041","\\ud83d\\ude00x","\\n"]';
  const b = Buffer.from(文);
  const 起们 = [];
  for (let i = 0, 内 = false; i < b.length; i++) { if (b[i] === 0x5C && 内) { i++; continue; } if (b[i] === 0x22) { if (!内) 起们.push(i); 内 = !内; } }
  assert.equal(起们.length, 3);
  const 期 = JSON.parse(文);
  起们.forEach((起, k) => { const 果 = 解(b, 起); assert.equal(果.字节.toString('utf8'), 期[k]); assert.equal(b[起 + 果.耗 - 1], 0x22); });
});

test('畸形输入一律触发陷阱（unreachable 或数组越界）', () => {
  const 坏 = [
    '"\\u"', '"\\u1"', '"\\u12"', '"\\u123"', '"\\uZZZZ"', '"\\u12G4"', '"\\u00e"', '"\\u 123"', '"\\u-123"', '"\\u+123"', '"\\u0x12"',
    '"\\x41"', '"\\0"', '"\\a"', '"\\ "', '"\\v"', '"\\U0041"', '"\\N"', '"abc', '"abc\\"', '"\\', '"', 'abc', '',
    '"\\ud83d\\u12"', '"\\ud83d\\uZZZZ"', '"\\ud83d\\u12G4"', '"\\u12\\"34"', '"\\u\\"abc"',
    '"\\ud83d\\ude0', '"\\ud83d\\ude00', '"\\ud83d\\u', '"\\ud83d\\',
  ];
  for (const 文 of 坏) assert.ok(陷(文), '应陷阱：' + JSON.stringify(文));
  assert.ok(陷('"a" "b"', 1));   // 起点不是引号
  assert.ok(陷('"a"', 3));       // 起点越界
});

test('随机 JSON.stringify 往返（含控制字符、孤立代理项、非 BMP、引号、反斜杠）', () => {
  let 种 = 987654321; const 随 = n => { 种 = (Math.imul(种, 1664525) + 1013904223) >>> 0; return (种 >>> 8) % n; };
  const 码 = () => {
    switch (随(9)) {
      case 0: return 随(0x20);
      case 1: return 随(0x80);
      case 2: return 0x22;
      case 3: return 0x5C;
      case 4: return 0xD800 + 随(0x800);
      case 5: return 随(0x10000);
      case 6: return 0x10000 + 随(0x100000);
      case 7: return [0x7F, 0x2028, 0x2029, 0xFEFF, 0xFFFD, 0xFFFE, 0xFFFF, 0][随(8)];
      default: return 0x4E00 + 随(0x5000);
    }
  };
  for (let 轮 = 0; 轮 < 30000; 轮++) {
    let 串 = ''; const 长 = 随(30);
    for (let k = 0; k < 长; k++) { const c = 码(); 串 += c >= 0x10000 ? String.fromCodePoint(c) : String.fromCharCode(c); }
    const 文 = JSON.stringify(串);
    const 果 = 解(文);
    assert.deepEqual(果.字节, Buffer.from(串.toWellFormed(), 'utf8'), JSON.stringify(文));
    assert.equal(果.耗, Buffer.byteLength(文));
  }
});

test('超长字符串：大量转义与百万字节', () => {
  const 块 = ['a', '中', '😀', '\n', '\u0000', '\u001b', '"', '\\', '\ud800', ' '];
  let 串 = ''; for (let k = 0; k < 200000; k++) 串 += 块[k % 块.length];
  const 文 = JSON.stringify(串);
  const 果 = 解(文);
  assert.deepEqual(果.字节, Buffer.from(串.toWellFormed(), 'utf8'));
  assert.equal(果.耗, Buffer.byteLength(文));
  const 大 = 解('"' + '\\u4e2d'.repeat(500000) + '"');
  assert.equal(大.字节.length, 1500000);
});
