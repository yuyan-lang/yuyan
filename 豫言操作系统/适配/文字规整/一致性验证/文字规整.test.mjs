// 文言：以真 Wasm 与 Node 自身之 String 方法对拍，验文字规整适配之精确。汉语：加载已构建的“文字规整一致性”产物，请求正文为输入的原始 UTF-8 字节，响应为结果的原始字节，全程不经 JSON。
// 用法见同目录说明：在私有暂存根目录执行 `node --test <本文件>`，产物根目录由环境变量 YY_DIST_ROOT 指定（默认 ./dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const 产物 = pathToFileURL(path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '文字规整一致性') + '/');
const {创建云工宿主} = await import(new URL('宿主.mjs', 产物));
const 程序模块 = await WebAssembly.compile(await readFile(new URL('程序.wasm', 产物)));
const 值桥模块 = await WebAssembly.compile(await readFile(new URL('值桥.wasm', 产物)));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {}});

// 文言：平台外壳于启动之前核对应用之要求与宿主之所供；此测同其所核。汉语：与生成的 入口.mjs 启动时相同，用 接口核对.mjs 核对应用要求与宿主支持清单。
const {核对接口装载} = await import(new URL('接口核对.mjs', 产物));
const 应用要求 = JSON.parse(await readFile(new URL('接口要求组.json', 产物), 'utf8'));
const 宿主提供 = JSON.parse(await readFile(new URL('宿主提供组.json', 产物), 'utf8'));
test('装载前的接口核对通过，且应用要求含本接口', () => {
  核对接口装载({程序模块, 应用要求, 宿主提供, 宿主: '云工'});
  assert.ok(应用要求.some(项 => 项.接口名称 === '豫言操作系统文字规整' && 项.接口版本 === '0.2.0'), '应用要求里应有本接口 0.2.0');
  assert.ok(宿主提供.some(项 => 项.接口名称 === '豫言操作系统文字规整' && 项.接口版本 === '0.2.0'), '宿主支持清单里应有本接口 0.2.0');
});
const 解码 = new TextDecoder('utf-8', {ignoreBOM: true});

const 调 = async (操作, 文, 次数 = 1, 附加 = '') => {
  const 回 = await 宿主.fetch(new Request(`https://x.test/?op=${操作}&n=${次数}${附加}`, {method: 'POST', body: Buffer.from(文, 'utf8')}), {});
  return {状态: 回.status, 文: 解码.decode(await 回.arrayBuffer())};
};
const 去 = async 文 => { const r = await 调('trim', 文); assert.equal(r.状态, 200, r.文); return r.文; };
const 小写 = async 文 => { const r = await 调('lower', 文); assert.equal(r.状态, 200, r.文); return r.文; };
const 长 = async 文 => { const r = await 调('len', 文); assert.equal(r.状态, 200, r.文); return Number(r.文); };
const 字母数字 = async 文 => { const r = await 调('alnum', 文); assert.equal(r.状态, 200, r.文); return r.文 === 'true'; };
// 文言：三术并验，任何差异皆显其码点。汉语：同一输入同时对拍三个函数，失败时用码点列表显示输入，避免不可见字符。
const 显 = 文 => [...文].map(c => 'U+' + c.codePointAt(0).toString(16).toUpperCase().padStart(4, '0')).join(' ').slice(0, 300);
const 对拍 = async 文 => {
  assert.equal(await 去(文), 文.trim(), 'trim：' + 显(文));
  assert.equal(await 小写(文), 文.toLowerCase(), 'lower：' + 显(文));
  assert.equal(await 长(文), 文.length, 'len：' + 显(文));
  assert.equal(await 字母数字(文), /^[\p{L}\p{N}]+$/u.test(文), 'alnum：' + 显(文));
};
const 码点串 = (起, 止) => { let 文 = ''; for (let c = 起; c < 止; c++) if (c < 0xD800 || c > 0xDFFF) 文 += String.fromCodePoint(c); return 文; };

test('规范中的一致性样例', async () => {
  assert.equal(await 去('\u3000 a b\u3000'), 'a b');
  assert.equal(await 去('\uFEFFx\uFEFF'), 'x');
  assert.equal(await 去('\u200Bx'), '\u200Bx');
  assert.equal(await 去(' \t\r\n\u3000\u00A0\uFEFF\u2028\u2029'), '');
  assert.equal(await 小写('ABC ÜÏ'), 'abc üï');
  assert.equal(await 小写('İ'), 'i̇');
  assert.deepEqual([await 小写('ΑΣ'), await 小写('ΑΣΑ'), await 小写('Σ')], ['ας', 'ασα', 'σ']);
  assert.deepEqual([await 长('abc'), await 长('豫言'), await 长('😀'), await 长('')], [3, 2, 2, 0]);
});

test('JS 引擎认定的空白集合与规范所列一致', () => {
  const 空白 = [];
  for (let c = 0; c <= 0x10FFFF; c++) if ((c < 0xD800 || c > 0xDFFF) && String.fromCodePoint(c).trim() === '') 空白.push(c);
  const 规范 = [9, 10, 11, 12, 13, 32, 0xA0, 0x1680, ...Array.from({length: 11}, (_, i) => 0x2000 + i), 0x2028, 0x2029, 0x202F, 0x205F, 0x3000, 0xFEFF];
  assert.deepEqual(空白, 规范);
  for (const 非 of [0x85, 0x180E, 0x200B, 0x200C, 0x200D, 0x2060, 0x1C, 0x1F, 0x00, 0x7F]) assert.notEqual(String.fromCodePoint(非).trim(), '', 'U+' + 非.toString(16));
});

test('去首尾空白：每个空白码点及其相邻码点，在首、尾、两端与中间', async () => {
  const 空白 = [];
  for (let c = 0; c <= 0x10FFFF; c++) if ((c < 0xD800 || c > 0xDFFF) && String.fromCodePoint(c).trim() === '') 空白.push(c);
  const 候选 = new Set();
  for (const c of 空白) for (const d of [c - 1, c, c + 1]) if (d >= 0 && (d < 0xD800 || d > 0xDFFF)) 候选.add(d);
  for (const c of 候选) {
    const 字 = String.fromCodePoint(c);
    for (const 文 of [字, 字 + 'a', 'a' + 字, 字 + 'a' + 字, 字 + 字 + 'ab' + 字 + 字, 'a' + 字 + 'b', 字 + ' ' + 字 + 'x' + 字 + ' ' + 字]) {
      assert.equal(await 去(文), 文.trim(), 'trim：' + 显(文));
    }
  }
});

test('全部 Unicode 码点分块对拍：去首尾空白（夹在空白中）、转小写、UTF-16 长度', async () => {
  for (let 起 = 0; 起 < 0x110000; 起 += 2048) {
    const 块 = 码点串(起, Math.min(起 + 2048, 0x110000));
    if (!块) continue;
    assert.equal(await 小写(块), 块.toLowerCase(), `lower 块 U+${起.toString(16)}`);
    assert.equal(await 长(块), 块.length, `len 块 U+${起.toString(16)}`);
    const 夹 = ' \u3000\t' + 块 + '\u00A0\n\uFEFF';
    assert.equal(await 去(夹), 夹.trim(), `trim 块 U+${起.toString(16)}`);
  }
});

test('全为字母或数字吗：规范样例与类别边界', async () => {
  for (const 文 of ['abc123', '豫言', 'é', 'Ⅷ', '１２', 'ǅ', 'ª', '²', '\u{20000}']) assert.equal(await 字母数字(文), true, 显(文));
  for (const 文 of ['', 'a_b', 'a-b', 'a b', '😀', '\u0301', 'a\n', '\u3000', '\u2028', '\uFEFF', '_', '-', '.', '\u0000']) assert.equal(await 字母数字(文), false, 显(文));
  // 每个字母或数字码点的类别边界：类别翻转处的前后码点单独对拍
  const 是 = 码 => /^[\p{L}\p{N}]$/u.test(String.fromCodePoint(码));
  let 前 = false;
  for (let 码 = 0; 码 < 0x110000; 码++) {
    if (码 >= 0xD800 && 码 <= 0xDFFF) continue;
    const 现 = 是(码);
    if (现 !== 前) for (const 边 of [码 - 1, 码]) if (边 >= 0 && (边 < 0xD800 || 边 > 0xDFFF)) {
      const 字 = String.fromCodePoint(边);
      assert.equal(await 字母数字(字), 是(边), 'alnum 边界：' + 显(字));
      assert.equal(await 字母数字('a' + 字 + '1'), 是(边), 'alnum 夹字：' + 显(字));
    }
    前 = 现;
  }
});

test('全为字母或数字吗：全部码点分块——全为字母数字的块为阳，混入任一异类为阴', async () => {
  const 全 = [], 非 = [];
  for (let 码 = 0; 码 < 0x110000; 码++) {
    if (码 >= 0xD800 && 码 <= 0xDFFF) continue;
    (/^[\p{L}\p{N}]$/u.test(String.fromCodePoint(码)) ? 全 : 非).push(码);
  }
  for (let 起 = 0; 起 < 全.length; 起 += 4096) {
    const 块 = String.fromCodePoint(...全.slice(起, 起 + 4096));
    assert.equal(await 字母数字(块), true, `alnum 块 ${起}`);
    assert.equal(await 字母数字(块 + '_'), false, `alnum 块加下划线 ${起}`);
  }
  for (let 起 = 0; 起 < 非.length; 起 += 4096) {
    const 块 = String.fromCodePoint(...非.slice(起, 起 + 4096));
    assert.equal(await 字母数字(块), false, `alnum 非块 ${起}`);
  }
});

test('土耳其 İ、词尾 Σ、ẞ、组合字符与特殊映射', async () => {
  const 样本 = ['İ', 'İstanbul İSTANBUL', 'ı I i', 'ΑΣ', 'ΑΣ.', 'ΑΣΑ', 'Σ', 'ΑΣΣ', 'ΑΣ́', 'ΟΔΥΣΣΕΥΣ', 'ΣΑΣ', 'Σ.Σ', 'ΑΣ Β', 'ΑΣ’', 'α.Σ', 'ẞ', 'Straße STRASSE', 'ǅ ǈ ǋ ǲ', 'Ⅷ Ⓐ Ａ', 'ᲐᲑᲒ', 'Ꭰ', '\u{10400}\u{10412}', '\u{1E900}', 'ǰ ŉ ΐ', 'ABCİİ', 'İ'.repeat(10)];
  for (const 文 of 样本) await 对拍(文);
  assert.equal(await 长('İ'), 1);
  assert.equal(await 长(await 小写('İ')), 2);
  assert.equal(await 长(await 小写('İ'.repeat(1000))), 2000);
});

test('JSON 与桥的敌意字符：引号、反斜线、控制字符、NUL、U+2028/9、BOM、增补平面', async () => {
  const 样本 = ['"', '\\', '\\\\', '\\n', '\\u0041', '\\ud83d\\ude00', '{"a":1}', '\u0000', 'A\u0000B', '\u0001\u0002\u001f', '\u007f', '\r\n', '\n\n\n', '\t\t', '\b\f', '\u2028\u2029', '\uFEFF', '\uFEFF\uFEFF x \uFEFF', '\uFFFE\uFFFF', '\u{10FFFF}', '\u{1F600}\u{1F468}\u200D\u{1F469}\u200D\u{1F467}', '`${x}`', '</script>', '%41%zz', '\u0085A\u0085', '\u180EA\u180E', '\u200BA\u200B', ' A\u0000 ', '\u0000 A \u0000'];
  for (const 文 of 样本) await 对拍(文);
});

// 文言：种子固定，故失败可复现。汉语：确定性的线性同余随机数，失败时可用同一种子复现。
const 随机 = 种子 => () => { 种子 = (Math.imul(种子, 1664525) + 1013904223) >>> 0; return 种子 / 4294967296; };
test('随机字符串对拍（固定种子）', async () => {
  const 字母 = [' ', '\t', '\n', '\r', '\u3000', '\u00A0', '\uFEFF', '\u2028', '\u200B', '\u0085', 'a', 'B', 'z', 'Z', '0', '9', '-', '.', 'İ', 'ı', 'Σ', 'σ', 'ς', 'Α', 'α', 'Ω', 'ω', 'É', 'é', 'Ǆ', 'ǆ', 'ẞ', 'ß', '豫', '言', '🌟', '\u{10400}', '\u{10428}', '́', '\u200D', '"', '\\', '\u0000', '\u001f', 'I', 'i', 'Ⅷ', 'ⅷ', 'Ａ', 'ａ'];
  const 下一 = 随机(20260925);
  for (let 次 = 0; 次 < 1500; 次++) {
    const 长度 = Math.floor(下一() * 40);
    let 文 = '';
    for (let 位 = 0; 位 < 长度; 位++) 文 += 字母[Math.floor(下一() * 字母.length)];
    await 对拍(文);
  }
});

test('大小限额：恰 1 MiB 可用，多一字节抛出豫言异常', async () => {
  const 满 = 'A'.repeat(1048576);
  assert.equal(await 长(满), 1048576);
  assert.equal(await 小写(满), 'a'.repeat(1048576));
  assert.equal(await 去(' ' + 'x'.repeat(1048575)), 'x'.repeat(1048575));
  const 汉满 = '豫'.repeat(349525) + 'a';
  assert.equal(Buffer.byteLength(汉满, 'utf8'), 1048576);
  assert.equal(await 长(汉满), 349526);
  for (const 操作 of ['trim', 'lower', 'len', 'alnum']) {
    const r = await 调(操作, 'A'.repeat(1048577));
    assert.equal(r.状态, 400, 操作);
    assert.equal(r.文, '错误：文字超过 1 MiB', 操作);
  }
  const 汉多 = '豫'.repeat(349525) + 'ab';
  assert.equal((await 调('lower', 汉多)).状态, 400);
});

test('超大输入的语义：一百万个增补字符', async () => {
  const 文 = '\u{1F600}'.repeat(200000);
  assert.equal(await 长(文), 400000);
  assert.equal(await 小写(文), 文);
  const 大写希腊 = 'ΑΣ '.repeat(100000);
  assert.equal(await 小写(大写希腊), 大写希腊.toLowerCase());
});

test('同一事件内重复调用六千次不耗尽宿主句柄', async () => {
  for (const 操作 of ['trim', 'lower', 'len', 'alnum']) {
    const r = await 调(操作, '  ÀB  ', 6000);
    assert.equal(r.状态, 200, r.文);
    assert.equal(r.文, 操作 === 'trim' ? 'ÀB' : 操作 === 'lower' ? '  àb  ' : 操作 === 'alnum' ? 'false' : '6');
  }
});

test('并发事件互不干扰', async () => {
  const 文们 = Array.from({length: 64}, (_, 序) => `  İ${序}Σ${'x'.repeat(序)}\u3000`);
  const 结果 = await Promise.all(文们.map(async 文 => [await 去(文), await 小写(文), await 长(文)]));
  结果.forEach((r, 序) => assert.deepEqual(r, [文们[序].trim(), 文们[序].toLowerCase(), 文们[序].length]));
});

test('非法 UTF-8 的字符串：先按 WHATWG 规则换成 U+FFFD 再运算', async () => {
  // 应用里用 字节转字符串(200) 造出含孤立字节 0xC8 的豫言字符串：a、0xC8、B
  const 期 = new TextDecoder('utf-8', {ignoreBOM: true}).decode(Uint8Array.of(0x61, 0xC8, 0x42));
  assert.equal(期, 'a\uFFFDB');
  assert.deepEqual(await 调('trim', '', 1, '&bad=1'), {状态: 200, 文: 期});
  assert.deepEqual(await 调('lower', '', 1, '&bad=1'), {状态: 200, 文: 期.toLowerCase()});
  assert.deepEqual(await 调('len', '', 1, '&bad=1'), {状态: 200, 文: '3'});
});

test('未知操作由应用报错（非本包路径）', async () => {
  const r = await 调('upper', 'x');
  assert.equal(r.状态, 400);
  assert.match(r.文, /未知操作/);
});
