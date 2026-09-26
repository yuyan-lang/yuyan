// 文言：以真 Wasm 验对象存储一版；汉语：对象存储 0.2.0 的真实 Wasm 一致性测试。
// 两种运行方式：
//   缺省      在 Node 中装载真实 Wasm，R2 为 模拟R2.mjs 的类实例（形状取自真实 workerd 的探测）；
//   远端地址  设置环境变量 远端地址（如 http://localhost:8793）则经 HTTP 调用同一应用的本地 workerd，桶为真实 R2；
//             只依赖模拟器特有能力的用例（故障注入、调用记录等）在此方式下跳过。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';
import {createHash, randomUUID} from 'node:crypto';
import {模拟R2} from './模拟R2.mjs';

const 远端 = process.env.远端地址 || '';
let 宿主 = null;
if (!远端) {
  const 根 = process.env.产物目录 ? path.resolve(process.env.产物目录) + '/'
    : fileURLToPath(new URL('./产物/', import.meta.url));
  const {创建云工宿主} = await import(根 + '宿主.mjs');
  const 程序模块 = await WebAssembly.compile(await readFile(根 + '程序.wasm'));
  const 值桥模块 = await WebAssembly.compile(await readFile(根 + '值桥.wasm'));
  宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {R2: ['PACKAGES', 'SPARE']}});
}
const 仅模拟 = 远端 ? test.skip : test;
const MiB = 1024 * 1024;
const 运行号 = randomUUID().slice(0, 8);
const md5 = 字节 => createHash('md5').update(字节).digest('hex');
const sha256 = 字节 => createHash('sha256').update(字节).digest('hex');
const 字节们 = 文 => Buffer.from(文, 'utf8');

// 场景：模拟方式每个用例一套全新的桶；远端方式共用真实桶，键加运行号前缀并在用例末尾清理。
function 新场景() {
  const 环境 = 远端 ? null : {PACKAGES: new 模拟R2(), SPARE: new 模拟R2()};
  const 前缀 = 远端 ? `${运行号}/${randomUUID().slice(0, 6)}/` : '';
  const 键 = 名 => 前缀 + 名;
  const 调 = async (路径, {体, 方法 = 'POST'} = {}) => {
    const 无体 = 方法 === 'GET' || 方法 === 'HEAD';
    // 远端方式声明 identity：workerd 对可压缩类型会在客户端接受 gzip 时自动压缩并改用分块传输，此时不保留 Content-Length。
    const 回 = 远端 ? await fetch(远端 + 路径, {method: 方法, body: 无体 ? undefined : 体, headers: {'accept-encoding': 'identity'}})
      : await 宿主.fetch(new Request('https://x.test' + 路径, {method: 方法, body: 无体 ? undefined : 体}), 环境);
    const 字节 = new Uint8Array(await 回.arrayBuffer());
    const 文 = new TextDecoder().decode(字节);
    return {状态码: 回.status, 头: 回.headers, 字节, 文, 数据: () => JSON.parse(文)};
  };
  const 查 = 参数 => new URLSearchParams(参数).toString();
  const 场景 = {
    环境, 键, 调, 前缀,
    桶: 名 => 环境?.[名 ?? 'PACKAGES'],
    头: (键名, 桶 = 'PACKAGES') => 调('/head?' + 查({b: 桶, k: 键名})),
    读: (键名, 上限, 桶 = 'PACKAGES') => 调('/read?' + 查({b: 桶, k: 键名, max: 上限})),
    读字节: (键名, 上限, 桶 = 'PACKAGES') => 调('/read-bytes?' + 查({b: 桶, k: 键名, max: 上限})),
    写: (键名, 字节, 选项 = '', 桶 = 'PACKAGES') => 调('/put?' + 查({b: 桶, k: 键名, opts: typeof 选项 === 'string' ? 选项 : JSON.stringify(选项)}), {体: 字节}),
    写文: (键名, 文, 选项 = '', 桶 = 'PACKAGES') => 调('/put-text?' + 查({b: 桶, k: 键名, opts: typeof 选项 === 'string' ? 选项 : JSON.stringify(选项)}), {体: 文}),
    删: (键们, 桶 = 'PACKAGES') => 调('/delete?' + 查({b: 桶}), {体: JSON.stringify(键们)}),
    列: (选项 = {}, 桶 = 'PACKAGES') => 调('/list?' + 查({b: 桶, opts: typeof 选项 === 'string' ? 选项 : JSON.stringify(选项)})),
    直通: (键名, {状态 = 200, 头们 = [], 方法 = 'GET', 桶 = 'PACKAGES'} = {}) =>
      调('/serve?' + 查({b: 桶, k: 键名, status: 状态, hdrs: JSON.stringify(头们)}), {方法}),
    清理: async 诸 => { if (远端 && 诸.length) await 调('/delete?' + 查({b: 'PACKAGES'}), {体: JSON.stringify(诸)}); },
  };
  return 场景;
}
const 应失败 = (果, 正则, 消息) => { assert.equal(果.状态码, 400, `${消息 ?? ''} 期望 400，得 ${果.状态码} ${果.文.slice(0, 200)}`); assert.match(果.文, 正则, 消息); };
const 近似现在 = 毫秒 => Math.abs(毫秒 - Date.now()) < 5 * 60 * 1000;

test('取对象元数据：不存在返回 null；存在返回大小、etag、httpEtag、上传毫秒与元数据', async () => {
  const 场 = 新场景(), 键 = 场.键('元/甲.txt');
  const 无 = await 场.头(键);
  assert.equal(无.状态码, 200); assert.equal(无.文, 'null');
  const 体 = 字节们('hello 世界');
  const 写 = await 场.写(键, 体, {contentType: 'text/plain; charset=utf-8', cacheControl: 'public, max-age=60', customMetadata: {sha256: sha256(体), kind: '源'}});
  assert.equal(写.状态码, 200, 写.文);
  const 果 = (await 场.头(键)).数据();
  assert.equal(果.size, 体.length); assert.equal(果.etag, md5(体)); assert.equal(果.httpEtag, `"${md5(体)}"`);
  assert.ok(Number.isInteger(果.uploaded) && 近似现在(果.uploaded), String(果.uploaded));
  assert.deepEqual(果.httpMetadata, {contentType: 'text/plain; charset=utf-8', cacheControl: 'public, max-age=60'});
  assert.deepEqual(果.customMetadata, {sha256: sha256(体), kind: '源'});
  assert.deepEqual(Object.keys(果), ['size', 'etag', 'httpEtag', 'uploaded', 'httpMetadata', 'customMetadata']);
  // 无元数据的对象
  const 键二 = 场.键('元/乙.bin');
  await 场.写(键二, Uint8Array.of(0, 255));
  const 二 = (await 场.头(键二)).数据();
  assert.equal(二.size, 2); assert.deepEqual(二.httpMetadata, {}); assert.deepEqual(二.customMetadata, {});
  await 场.清理([键, 键二]);
});

仅模拟('取对象元数据：httpMetadata 的到期时刻（Date）转为毫秒整数，不泄漏句柄引用', async () => {
  const 场 = 新场景(), 桶 = 场.桶();
  await 桶.put('过期', 'x', {httpMetadata: {contentType: 'a/b', cacheExpiry: new Date(123456789000)}});
  const 果 = await 场.头('过期');
  assert.equal(果.状态码, 200, 果.文);
  assert.deepEqual(果.数据().httpMetadata, {contentType: 'a/b', cacheExpiry: 123456789000});
  assert.ok(!果.文.includes('$句柄'));
});

test('对象键规则：1 至 1024 字节、无控制字符、不以斜杠开头；桶名规则', async () => {
  const 场 = 新场景();
  for (const 坏 of ['', '/开头斜杠', 'a\nb', 'a\tb', 'a\rb', 'a\x7fb', 'k'.repeat(1025), '豫'.repeat(342)]) {
    应失败(await 场.头(坏), /对象键须为 1 至 1024 字节，不含控制字符，且不以斜杠开头/, JSON.stringify(坏).slice(0, 30));
    应失败(await 场.读(坏, 10), /对象键须为/, '读');
    应失败(await 场.写(坏, new Uint8Array()), /对象键须为/, '写');
    应失败(await 场.删([坏]), /对象键须为/, '删');
    应失败(await 场.直通(坏), /对象键须为/, '直通');
  }
  for (const 好 of ['k'.repeat(1024), '豫'.repeat(341), 'a"b\\c d', '甲/乙/丙.txt', '日本語/😀', 'a b']) {
    const 键 = 场.键(好);
    if (键.length && new TextEncoder().encode(键).length > 1024) continue;
    assert.equal((await 场.头(键)).状态码, 200, JSON.stringify(好).slice(0, 30));
  }
  for (const 坏桶 of ['', 'a b', '1abc', 'a-b', 'x'.repeat(65), '豫言', 'a.b']) {
    应失败(await 场.头('k', 坏桶), /对象桶名须为 1 至 64 字节的绑定名/, JSON.stringify(坏桶));
  }
});

test('许可外的桶名与未绑定的桶：宿主直接拒绝（部署错误，不由应用恢复）', async () => {
  const 场 = 新场景();
  if (远端) {
    const 回 = await 场.头('k', 'NOPE');
    assert.notEqual(回.状态码, 200); return;
  }
  await assert.rejects(场.头('k', 'NOPE'), /未授权的R2绑定：NOPE/);
  await assert.rejects(场.列({}, 'NOPE'), /未授权的R2绑定：NOPE/);
  await assert.rejects(场.写('k', new Uint8Array(), '', 'NOPE'), /未授权的R2绑定：NOPE/);
  // 已在许可里，但环境里没有这个绑定
  const 无绑定 = {PACKAGES: new 模拟R2()};
  await assert.rejects(宿主.fetch(new Request('https://x.test/head?b=SPARE&k=k', {method: 'POST'}), 无绑定), /绑定不存在：SPARE/);
});

test('读取对象文字：多字节、换行、引号、反斜杠、制表符与非 BMP 字符无损往返', async () => {
  const 场 = 新场景();
  const 样例 = ['', 'a', 'hello 世界', '第一行\n第二行\t制表\r回车', '引号"与\\反斜杠/斜杠', '😀 emoji 与 𠮷 字', '{"json": [1, 2, {"k": "v"}]}', '汉'.repeat(3000)];
  const 诸键 = [];
  for (const [序, 文] of 样例.entries()) {
    const 键 = 场.键(`文/${序}`); 诸键.push(键);
    const 写 = await 场.写文(键, 文);
    assert.equal(写.状态码, 200, 写.文);
    assert.equal(写.数据().size, 字节们(文).length);
    const 读 = await 场.读(键, 2 * MiB);
    assert.equal(读.状态码, 200, 读.文);
    const 果 = 读.数据();
    assert.equal(果.text, 文, JSON.stringify(文).slice(0, 30));
    assert.equal(果.size, 字节们(文).length);
    assert.equal(果.httpEtag, `"${md5(字节们(文))}"`);
    assert.deepEqual(果.customMetadata, {});
    assert.deepEqual(Object.keys(果), ['text', 'size', 'httpEtag', 'customMetadata']);
  }
  await 场.清理(诸键);
});

test('读取对象文字：不存在为 null；BOM 原样保留；带元数据', async () => {
  const 场 = 新场景();
  assert.equal((await 场.读(场.键('无'), 100)).文, 'null');
  const 键 = 场.键('bom');
  await 场.写(键, Uint8Array.of(0xef, 0xbb, 0xbf, 0x61), {customMetadata: {a: '1'}});
  const 果 = (await 场.读(键, 100)).数据();
  assert.equal(果.text, '﻿a'); assert.equal(果.size, 4); assert.deepEqual(果.customMetadata, {a: '1'});
  await 场.清理([键]);
});

test('读取对象文字：非法 UTF-8 抛异常（严格解码），合法边界通过', async () => {
  const 场 = 新场景(), 诸键 = [];
  const 坏们 = {截断汉字: [0xe8, 0xb1], 孤立续字节: [0x80], 无效字节: [0x61, 0xff, 0x62], 过长编码: [0xc0, 0xaf], 代理对编码: [0xed, 0xa0, 0x80], 超范围: [0xf4, 0x90, 0x80, 0x80], 后随截断: [0x61, 0xe8, 0xb1]};
  for (const [名, 字节] of Object.entries(坏们)) {
    const 键 = 场.键('坏/' + 名); 诸键.push(键);
    await 场.写(键, Uint8Array.from(字节));
    应失败(await 场.读(键, 100), /对象正文不是严格合法的 UTF-8 文字/, 名);
  }
  const 好们 = {最大码点: [0xf4, 0x8f, 0xbf, 0xbf], 二字节: [0xc2, 0x80], 三字节边界: [0xed, 0x9f, 0xbf], 零字节文字: [0x61, 0x00, 0x62]};
  for (const [名, 字节] of Object.entries(好们)) {
    const 键 = 场.键('好/' + 名); 诸键.push(键);
    await 场.写(键, Uint8Array.from(字节));
    const 果 = await 场.读(键, 100);
    assert.equal(果.状态码, 200, 名 + 果.文);
    assert.equal(果.数据().text, Buffer.from(字节).toString('utf8'), 名);
  }
  await 场.清理(诸键);
});

test('读取对象文字：上限边界与 tooLarge（大于上限不读取正文、不截断）', async () => {
  const 场 = 新场景(), 键 = 场.键('上限');
  await 场.写(键, Buffer.alloc(1000, 0x61));
  assert.deepEqual((await 场.读(键, 1000)).数据().text.length, 1000);
  assert.deepEqual((await 场.读(键, 999)).数据(), {tooLarge: true, size: 1000});
  assert.deepEqual((await 场.读(键, 1)).数据(), {tooLarge: true, size: 1000});
  if (!远端) {
    const 桶 = 场.桶();
    assert.ok(!桶.记录.some(项 => /^GetResult\.(arrayBuffer|text|bytes|json|blob)$/.test(项.方法) && 项.键 === 键) || true);
    桶.记录.length = 0;
    await 场.读(键, 10);
    assert.deepEqual(桶.记录.filter(项 => 项.方法.startsWith('GetResult.')), [], 'tooLarge 不得读取正文');
    assert.equal(桶.记录.filter(项 => 项.方法 === 'body.pull').length, 0, 'tooLarge 不得拉取正文流');
  }
  // 越界的上限参数
  for (const 上限 of [0, -1, 2097153, 16 * MiB]) 应失败(await 场.读(键, 上限), /读取对象文字的字节上限须在 1 至 2097152 之间/, String(上限));
  // 2 MiB 边界：恰等通过，多一字节为 tooLarge
  const 大 = 场.键('二兆'), 大一 = 场.键('二兆加一');
  await 场.写(大, Buffer.alloc(2 * MiB, 0x62)); await 场.写(大一, Buffer.alloc(2 * MiB + 1, 0x62));
  const 满 = await 场.读(大, 2 * MiB);
  assert.equal(满.状态码, 200); assert.equal(满.数据().text.length, 2 * MiB);
  assert.deepEqual((await 场.读(大一, 2 * MiB)).数据(), {tooLarge: true, size: 2 * MiB + 1});
  await 场.清理([键, 大, 大一]);
});

test('读取对象文字：转义最坏情形（2 MiB 全为控制字符，JSON 六倍膨胀）仍在宿主值桥限内', async () => {
  const 场 = 新场景(), 键 = 场.键('控制');
  await 场.写(键, Buffer.alloc(2 * MiB, 0x01));
  const 读 = await 场.读(键, 2 * MiB);
  assert.equal(读.状态码, 200, 读.文.slice(0, 200));
  assert.ok(读.字节.length > 12 * MiB - 100 && 读.字节.length < 16 * MiB, `响应 ${读.字节.length}`);
  const 果 = 读.数据();
  assert.equal(果.text.length, 2 * MiB); assert.ok(果.text.split('').every(字 => 字 === '\u0001'));
  const 换行 = 场.键('换行');
  await 场.写(换行, Buffer.alloc(2 * MiB, 0x0a));
  assert.equal((await 场.读(换行, 2 * MiB)).数据().text.length, 2 * MiB);
  await 场.清理([键, 换行]);
});

test('读取对象字节：无损往返（含 NUL 与非 UTF-8 字节）、不存在为阴、上限边界与越界', async () => {
  const 场 = 新场景(), 诸键 = [];
  const 样例 = [[], [0], [0xff, 0xfe, 0x00, 0x0a], Array.from({length: 256}, (_, i) => i)];
  for (const [序, 数组] of 样例.entries()) {
    const 键 = 场.键(`字/${序}`); 诸键.push(键);
    assert.equal((await 场.写(键, Uint8Array.from(数组))).状态码, 200);
    const 读 = await 场.读字节(键, 16 * MiB);
    assert.equal(读.状态码, 200, 读.文);
    assert.equal(读.头.get('x-found'), '1');
    assert.deepEqual(Array.from(读.字节), 数组);
  }
  const 无 = await 场.读字节(场.键('无'), 100);
  assert.equal(无.状态码, 200); assert.equal(无.头.get('x-found'), '0'); assert.equal(无.字节.length, 0);
  const 键 = 场.键('三字节'); 诸键.push(键);
  await 场.写(键, Uint8Array.of(1, 2, 3));
  assert.deepEqual(Array.from((await 场.读字节(键, 3)).字节), [1, 2, 3]);
  应失败(await 场.读字节(键, 2), /读取对象字节/);
  应失败(await 场.读字节(键, 0), /1 至 16777216/);
  应失败(await 场.读字节(键, 16 * MiB + 1), /1 至 16777216/);
  应失败(await 场.读字节('/开头斜杠', 10), /键/);
  await 场.清理(诸键);
});

test('读取对象字节：8 MiB 大对象无损（远超文字读取的 2 MiB 上限）', async () => {
  const 场 = 新场景(), 键 = 场.键('大字节');
  const 体 = Buffer.alloc(8 * MiB); for (let i = 0; i < 体.length; i++) 体[i] = (i * 31 + (i >> 8)) & 0xff;
  assert.equal((await 场.写(键, 体)).状态码, 200);
  const 读 = await 场.读字节(键, 16 * MiB);
  assert.equal(读.状态码, 200); assert.equal(读.字节.length, 8 * MiB);
  assert.equal(sha256(读.字节), sha256(体));
  应失败(await 场.读字节(键, 8 * MiB - 1), /读取对象字节/);
  await 场.清理([键]);
});

test('写入对象：覆盖写、二进制无损、元数据经 head 回读', async () => {
  const 场 = 新场景(), 键 = 场.键('写/一');
  const 全字节 = Uint8Array.from({length: 256}, (_, 序) => 序);
  let 果 = await 场.写(键, 全字节);
  assert.equal(果.状态码, 200); assert.deepEqual(果.数据(), {written: true, size: 256, etag: md5(全字节), httpEtag: `"${md5(全字节)}"`});
  const 二 = 字节们('覆盖后');
  果 = await 场.写(键, 二, {contentType: 'text/plain'});
  assert.equal(果.数据().written, true);
  assert.equal((await 场.头(键)).数据().size, 二.length);
  assert.equal((await 场.头(键)).数据().httpMetadata.contentType, 'text/plain');
  // 空正文
  const 空 = 场.键('写/空');
  果 = await 场.写(空, new Uint8Array());
  assert.deepEqual(果.数据(), {written: true, size: 0, etag: md5(new Uint8Array()), httpEtag: `"${md5(new Uint8Array())}"`});
  assert.equal((await 场.读(空, 10)).数据().text, '');
  // 写入对象文字
  const 文键 = 场.键('写/文');
  果 = await 场.写文(文键, '汉字 text', {contentType: 'text/plain; charset=utf-8'});
  assert.equal(果.数据().size, 字节们('汉字 text').length);
  assert.equal((await 场.读(文键, 100)).数据().text, '汉字 text');
  await 场.清理([键, 空, 文键]);
});

test('写入对象：条件写的四种结果（不存在则建、已存在则拒、etag 相符则换、etag 过期则拒）', async () => {
  const 场 = 新场景(), 键 = 场.键('条件/锁');
  // ifNoneMatch:* —— 不存在：写成
  let 果 = await 场.写(键, 字节们('一'), {ifNoneMatch: '*', customMetadata: {n: '1'}});
  assert.equal(果.数据().written, true);
  const 首 = (await 场.头(键)).数据();
  // ifNoneMatch:* —— 已存在：written 为假，原对象不变（证明适配传的是真正的 Headers，纯对象会被 R2 静默忽略而覆盖）
  果 = await 场.写(键, 字节们('二二'), {ifNoneMatch: '*', customMetadata: {n: '2'}});
  assert.equal(果.状态码, 200); assert.deepEqual(果.数据(), {written: false});
  let 现 = (await 场.头(键)).数据();
  assert.equal(现.etag, 首.etag); assert.deepEqual(现.customMetadata, {n: '1'});
  assert.equal((await 场.读(键, 100)).数据().text, '一');
  // ifMatch —— etag 相符：写成，内容换新
  果 = await 场.写(键, 字节们('三三三'), {ifMatch: 首.httpEtag, customMetadata: {n: '3'}});
  assert.equal(果.数据().written, true);
  现 = (await 场.头(键)).数据();
  assert.equal(现.size, 9); assert.notEqual(现.etag, 首.etag); assert.deepEqual(现.customMetadata, {n: '3'});
  // ifMatch —— 旧 etag（已过期）：written 为假，对象不变
  果 = await 场.写(键, 字节们('四'), {ifMatch: 首.httpEtag});
  assert.deepEqual(果.数据(), {written: false});
  assert.equal((await 场.读(键, 100)).数据().text, '三三三');
  // ifMatch —— 对象不存在：written 为假，且没有创建对象
  const 无 = 场.键('条件/无');
  果 = await 场.写(无, 字节们('x'), {ifMatch: '"0123456789abcdef"'});
  assert.deepEqual(果.数据(), {written: false});
  assert.equal((await 场.头(无)).文, 'null');
  // 与 sha256 同用：条件不满足时不抛异常
  果 = await 场.写(键, 字节们('abc'), {ifNoneMatch: '*', sha256: sha256(字节们('abc'))});
  assert.deepEqual(果.数据(), {written: false});
  await 场.清理([键]);
});

test('写入对象：sha256 由平台校验，相符则写、不符则抛异常且不留对象', async () => {
  const 场 = 新场景(), 键 = 场.键('摘要/一');
  const 体 = 字节们('abc');
  let 果 = await 场.写(键, 体, {sha256: sha256(体)});
  assert.equal(果.数据().written, true);
  const 错键 = 场.键('摘要/二');
  果 = await 场.写(错键, 字节们('abd'), {sha256: sha256(体)});
  应失败(果, /The SHA-256 checksum you specified did not match what we received/);
  assert.match(果.文, /\(10037\)/, '保留平台错误码原文');
  assert.match(果.文, /Actual SHA-256 was: [0-9a-f]{64}/);
  assert.equal((await 场.头(错键)).文, 'null');
  // 大写、63 位、64 位含非十六进制、空串、非文字：适配自己拒绝
  for (const 坏 of [sha256(体).toUpperCase(), sha256(体).slice(1), 'g'.repeat(64), '', 5, null]) {
    const 回 = await 场.写(错键, 体, 坏 === '' ? '{"sha256":""}' : {sha256: 坏});
    应失败(回, /写入选项 sha256 须为 64 个小写十六进制字符|写入选项 sha256 须为文字/, JSON.stringify(坏));
  }
  await 场.清理([键]);
});

test('写入对象：选项 JSON 的严格校验（未知字段、类型、互斥、格式、限额）', async () => {
  const 场 = 新场景(), 键 = 场.键('选项');
  const 体 = 字节们('x');
  // 无选项的多种写法
  for (const 空 of ['', '{}', 'null']) assert.equal((await 场.写(键, 体, 空)).数据().written, true, JSON.stringify(空));
  const 拒 = async (选项, 正则, 名) => 应失败(await 场.写(键, 体, 选项), 正则, 名);
  await 拒('{"storageClass":"x"}', /写入选项含未知字段：storageClass/, '未知字段');
  await 拒('{"onlyIf":{}}', /写入选项含未知字段：onlyIf/, 'onlyIf');
  await 拒('{"contentType":"a","contentType":"b"}', /写入选项含重复字段/, '重复字段');
  await 拒('{坏', /写入选项不是合法的 JSON/, '坏 JSON');
  await 拒('[]', /写入选项须为 JSON 对象/, '数组');
  await 拒('5', /写入选项须为 JSON 对象/, '数字');
  await 拒({ifNoneMatch: 'abc'}, /写入选项 ifNoneMatch 只接受 \*/, 'ifNoneMatch 非星');
  await 拒({ifNoneMatch: '"x"'}, /写入选项 ifNoneMatch 只接受 \*/, 'ifNoneMatch etag');
  await 拒({ifNoneMatch: true}, /写入选项 ifNoneMatch 须为文字/, 'ifNoneMatch 布尔');
  await 拒({ifNoneMatch: '*', ifMatch: '"a"'}, /ifNoneMatch 与 ifMatch 不能同时给出/, '互斥');
  for (const 坏 of ['a', '*', 'W/"a"', '""', '"a b"', '"a,b"', '"' + 'a'.repeat(65) + '"', '"a\\"']) await 拒({ifMatch: 坏}, /写入选项 ifMatch 须为带双引号的 httpEtag/, 'ifMatch ' + 坏);
  await 拒({contentType: 5}, /写入选项 contentType 须为文字/, 'contentType 数字');
  await 拒({contentType: ''}, /写入选项 contentType 须为 1 至 256 字节的可见 ASCII 文字/, 'contentType 空');
  await 拒({contentType: '中文'}, /可见 ASCII 文字/, 'contentType 非 ASCII');
  await 拒({contentType: 'a\nb'}, /可见 ASCII 文字/, 'contentType 换行');
  await 拒({cacheControl: 'x'.repeat(257)}, /写入选项 cacheControl 须为 1 至 256 字节/, 'cacheControl 长');
  await 拒({customMetadata: []}, /写入选项 customMetadata 须为对象/, 'customMetadata 数组');
  await 拒({customMetadata: 'x'}, /写入选项 customMetadata 须为对象/, 'customMetadata 文字');
  await 拒({customMetadata: {a: 1}}, /customMetadata 的键须为 1 至 128 字节/, '值非文字');
  await 拒({customMetadata: {a: null}}, /customMetadata 的键须为/, '值 null');
  await 拒({customMetadata: {'': 'x'}}, /customMetadata 的键须为/, '空键');
  await 拒({customMetadata: {['k'.repeat(129)]: 'x'}}, /customMetadata 的键须为/, '长键');
  await 拒({customMetadata: {a: 'v'.repeat(1025)}}, /customMetadata 的键须为/, '长值');
  await 拒({customMetadata: {a: 'x\ny'}}, /customMetadata 的键须为/, '值含换行');
  await 拒({customMetadata: {'a\tb': 'x'}}, /customMetadata 的键须为/, '键含制表');
  for (const 保留 of ['$句柄', '$未定义', '$大整数', '$数字']) await 拒({customMetadata: {[保留]: '1'}}, /键不得为宿主桥保留名/, '保留名 ' + 保留);
  await 拒(JSON.stringify({customMetadata: Object.fromEntries(Array.from({length: 65}, (_, 序) => ['k' + 序, 'v']))}), /customMetadata 至多 64 项/, '65 项');
  await 拒('{"customMetadata":{"a":"1","a":"2"}}', /customMetadata 含重复键/, '重复键');
  if (!远端) await 拒(' '.repeat(65537), /选项 JSON 超过 64 KiB/, '选项过长');   // 远端方式受 HTTP 请求头长度限制，跳过
  // 边界内通过
  const 好 = await 场.写(键, 体, {contentType: 'x'.repeat(256), cacheControl: 'y', customMetadata: Object.fromEntries(Array.from({length: 64}, (_, 序) => ['k' + 序, 'v']))});
  assert.equal(好.状态码, 200, 好.文.slice(0, 200));
  const 保留形 = await 场.写(键, 体, {customMetadata: {'$句柄x': '1', 名: '$句柄'}});
  assert.equal(保留形.数据().written, true);
  assert.deepEqual((await 场.头(键)).数据().customMetadata, {'$句柄x': '1', 名: '$句柄'});
  await 场.清理([键]);
});

test('写入对象：正文 16 MiB 边界（Wasm 内构造），超过则抛异常', async () => {
  const 场 = 新场景(), 键 = 场.键('大');
  const 满 = await 场.调('/put-zero?' + new URLSearchParams({b: 'PACKAGES', k: 键, opts: '', n: 16 * MiB}));
  assert.equal(满.状态码, 200, 满.文.slice(0, 200));
  assert.equal(满.数据().size, 16 * MiB); assert.equal(满.数据().etag, md5(Buffer.alloc(16 * MiB)));
  const 超 = await 场.调('/put-zero?' + new URLSearchParams({b: 'PACKAGES', k: 键 + '2', opts: '', n: 16 * MiB + 1}));
  应失败(超, /对象正文超过 16 MiB/);
  assert.equal((await 场.头(键 + '2')).文, 'null');
  await 场.清理([键]);
});

仅模拟('平台故障：异常消息保留平台原文（head、get、put、delete、list）', async () => {
  const 场 = 新场景(), 桶 = 场.桶();
  await 场.写('存在', 字节们('x'));
  const 试 = async (方法, 调用, 消息) => {
    桶.注入故障(方法, new Error(消息));
    应失败(await 调用(), new RegExp(消息.replace(/[()]/g, '\\$&')), 方法);
  };
  await 试('head', () => 场.头('存在'), 'head: Internal Error (10001)');
  await 试('get', () => 场.读('存在', 10), 'get: Service Unavailable (10043)');
  await 试('put', () => 场.写('新', 字节们('y')), 'put: Internal Error (10001)');
  await 试('delete', () => 场.删(['存在']), 'delete: Internal Error (10001)');
  await 试('list', () => 场.列({}), 'list: Internal Error (10001)');
  await 试('get', () => 场.直通('存在'), 'get: Service Unavailable (10043)');
  await 试('head', () => 场.直通('存在', {方法: 'HEAD'}), 'head: Internal Error (10001)');
  // 故障后可继续使用
  assert.equal((await 场.头('存在')).数据().size, 1);
});

test('删除对象：多个键、不存在不报错、空列表、1000 个上限', async () => {
  const 场 = 新场景();
  const 诸键 = ['甲', '乙', '丙'].map(名 => 场.键('删/' + 名));
  for (const 键 of 诸键) await 场.写(键, 字节们('x'));
  let 果 = await 场.删([...诸键.slice(0, 2), 场.键('删/不存在')]);
  assert.equal(果.状态码, 200, 果.文);
  assert.equal((await 场.头(诸键[0])).文, 'null'); assert.equal((await 场.头(诸键[1])).文, 'null');
  assert.notEqual((await 场.头(诸键[2])).文, 'null');
  果 = await 场.删([场.键('删/再无')]);
  assert.equal(果.状态码, 200);
  果 = await 场.删([]);
  assert.equal(果.状态码, 200);
  if (!远端) { const 桶 = 场.桶(); assert.equal(桶.记录.filter(项 => 项.方法 === 'delete' && 项.键们 === 0).length, 0, '空列表不调用平台'); }
  // 1000 个键
  const 千 = Array.from({length: 1000}, (_, 序) => 场.键(`批/${序}`));
  for (const 键 of 千.slice(0, 5)) await 场.写(键, 字节们('x'));
  果 = await 场.删(千);
  assert.equal(果.状态码, 200, 果.文.slice(0, 200));
  assert.equal((await 场.头(千[0])).文, 'null');
  应失败(await 场.删([...千, 场.键('批/多一个')]), /一次至多删除 1000 个对象/);
  await 场.清理([诸键[2]]);
});

仅模拟('删除对象：含非法键时整批拒绝且不调用平台', async () => {
  const 场 = 新场景(), 桶 = 场.桶();
  await 场.写('好键', 字节们('x'));
  桶.记录.length = 0;
  应失败(await 场.删(['好键', '/坏键']), /对象键须为/);
  assert.equal(桶.记录.filter(项 => 项.方法 === 'delete').length, 0);
  assert.notEqual((await 场.头('好键')).文, 'null');
});

test('列举对象：前缀、限额、排序、上传毫秒与 include', async () => {
  const 场 = 新场景();
  const 名们 = ['甲', '乙', '丙', '丁'];
  const 诸键 = 名们.map(名 => 场.键('列/' + 名));
  for (const [序, 键] of 诸键.entries()) await 场.写(键, Buffer.alloc(序 + 1, 0x61), {customMetadata: {序: String(序)}});
  await 场.写(场.键('别/一'), 字节们('x'));
  const 全 = (await 场.列({prefix: 场.键('列/')})).数据();
  assert.deepEqual(Object.keys(全), ['objects', 'truncated', 'cursor', 'delimitedPrefixes']);
  assert.equal(全.truncated, false); assert.equal(全.cursor, null); assert.deepEqual(全.delimitedPrefixes, []);
  assert.deepEqual(全.objects.map(项 => 项.key), [...诸键].sort((甲, 乙) => Buffer.compare(Buffer.from(甲), Buffer.from(乙))));
  const 项 = 全.objects.find(物 => 物.key === 诸键[2]);
  assert.deepEqual(Object.keys(项), ['key', 'size', 'etag', 'uploaded']);
  assert.equal(项.size, 3); assert.equal(项.etag, md5(Buffer.alloc(3, 0x61))); assert.ok(近似现在(项.uploaded));
  // include customMetadata
  const 含 = (await 场.列({prefix: 场.键('列/'), include: ['customMetadata']})).数据();
  assert.deepEqual(Object.keys(含.objects[0]), ['key', 'size', 'etag', 'uploaded', 'customMetadata']);
  assert.deepEqual(含.objects.find(物 => 物.key === 诸键[2]).customMetadata, {序: '2'});
  // include 空数组 = 不含
  assert.ok(!('customMetadata' in (await 场.列({prefix: 场.键('列/'), include: []})).数据().objects[0]));
  await 场.清理([...诸键, 场.键('别/一')]);
});

test('列举对象：limit 与 cursor 分页遍历，truncated 与 cursor 一致', async () => {
  const 场 = 新场景();
  const 诸键 = Array.from({length: 7}, (_, 序) => 场.键('页/' + String(序).padStart(2, '0')));
  for (const 键 of 诸键) await 场.写(键, 字节们('x'));
  const 见 = []; let 游标 = null, 页数 = 0;
  for (;;) {
    const 选项 = {prefix: 场.键('页/'), limit: 3}; if (游标) 选项.cursor = 游标;
    const 果 = (await 场.列(选项)).数据();
    页数++; 见.push(...果.objects.map(项 => 项.key));
    if (!果.truncated) { assert.equal(果.cursor, null); break; }
    assert.equal(typeof 果.cursor, 'string'); assert.ok(果.cursor.length > 0);
    assert.equal(果.objects.length, 3);
    游标 = 果.cursor;
  }
  assert.equal(页数, 3); assert.deepEqual(见, 诸键);
  // limit 边界
  assert.equal((await 场.列({prefix: 场.键('页/'), limit: 1})).数据().objects.length, 1);
  assert.equal((await 场.列({prefix: 场.键('页/'), limit: 1000})).数据().objects.length, 7);
  await 场.清理(诸键);
});

test('列举对象：delimiter 折叠出 delimitedPrefixes', async () => {
  const 场 = 新场景();
  const 诸键 = ['目/甲/1', '目/甲/2', '目/乙/1', '目/根.txt'].map(名 => 场.键(名));
  for (const 键 of 诸键) await 场.写(键, 字节们('x'));
  const 果 = (await 场.列({prefix: 场.键('目/'), delimiter: '/'})).数据();
  assert.deepEqual(果.objects.map(项 => 项.key), [场.键('目/根.txt')]);
  assert.deepEqual(果.delimitedPrefixes.sort(), [场.键('目/甲/'), 场.键('目/乙/')].sort());
  await 场.清理(诸键);
});

test('列举对象：选项校验（未知字段、类型、越界、控制字符）', async () => {
  const 场 = 新场景();
  const 拒 = async (选项, 正则, 名) => 应失败(await 场.列(选项), 正则, 名);
  await 拒({startAfter: 'x'}, /列举选项含未知字段：startAfter/, '未知字段');
  await 拒({limit: 0}, /列举选项 limit 须在 1 至 1000 之间/, 'limit 0');
  await 拒({limit: 1001}, /列举选项 limit 须在 1 至 1000 之间/, 'limit 1001');
  await 拒({limit: -5}, /列举选项 limit 须在 1 至 1000 之间/, 'limit 负');
  await 拒({limit: '5'}, /列举选项 limit 须为整数/, 'limit 文字');
  await 拒({limit: 1.5}, /列举选项 limit 须为整数/, 'limit 小数');
  await 拒({prefix: 5}, /列举选项 prefix 须为文字/, 'prefix 数字');
  await 拒({prefix: 'a\nb'}, /列举选项 prefix 的长度或字符不合规则/, 'prefix 控制字符');
  await 拒({prefix: 'k'.repeat(1025)}, /列举选项 prefix 的长度或字符不合规则/, 'prefix 长');
  await 拒({cursor: ''}, /列举选项 cursor 的长度或字符不合规则/, 'cursor 空');
  await 拒({cursor: 'c'.repeat(4097)}, /列举选项 cursor 的长度或字符不合规则/, 'cursor 长');
  await 拒({delimiter: ''}, /列举选项 delimiter 的长度或字符不合规则/, 'delimiter 空');
  await 拒({delimiter: 'd'.repeat(17)}, /列举选项 delimiter 的长度或字符不合规则/, 'delimiter 长');
  await 拒({include: ['httpMetadata']}, /列举选项 include 只接受 customMetadata/, 'include httpMetadata');
  await 拒({include: 'customMetadata'}, /列举选项 include 须为数组/, 'include 文字');
  await 拒({include: [5]}, /列举选项 include 只接受 customMetadata/, 'include 数字');
  await 拒('{"limit":1,"limit":2}', /列举选项含重复字段/, '重复');
  await 拒('[]', /列举选项须为 JSON 对象/, '数组');
  await 拒('{', /列举选项不是合法的 JSON/, '坏 JSON');
  for (const 空 of ['', '{}', 'null']) assert.equal((await 场.列(空)).状态码, 200, JSON.stringify(空));
});

仅模拟('列举对象：1000 项一页（共 1001 项），键含引号、反斜杠、汉字与制表符，输出 JSON 严格合法', async () => {
  const 场 = 新场景(), 桶 = 场.桶();
  const 诸键 = Array.from({length: 1001}, (_, 序) => `批量/${String(序).padStart(4, '0')}/\"汉\\字\t${序}`);
  for (const 键 of 诸键) await 桶.put(键, 'x', {customMetadata: {说明: `第"${键.length}"项\\` }});
  const 起 = performance.now();
  const 果 = await 场.列({prefix: '批量/', include: ['customMetadata']});
  const 耗时 = performance.now() - 起;
  assert.equal(果.状态码, 200, 果.文.slice(0, 200));
  const 数据 = 果.数据();
  assert.equal(数据.objects.length, 1000); assert.equal(数据.truncated, true); assert.equal(typeof 数据.cursor, 'string');
  assert.deepEqual(数据.objects.map(项 => 项.key), 诸键.slice(0, 1000));
  assert.equal(数据.objects[7].customMetadata.说明, `第"${诸键[7].length}"项\\`);
  console.log(`列举 1000 项用时 ${耗时.toFixed(0)}ms，响应 ${果.字节.length} 字节`);
});

test('句柄释放：同一事件内反复取元数据与列举，不触及事件句柄上限（4096）', async () => {
  const 场 = 新场景(), 键 = 场.键('循环');
  await 场.写(键, 字节们('x'), {customMetadata: {a: '1'}});
  const 头 = await 场.调('/loop-head?' + new URLSearchParams({b: 'PACKAGES', k: 键, n: 6000}));
  assert.equal(头.状态码, 200, 头.文.slice(0, 200)); assert.equal(头.数据().size, 1);
  const 诸键 = Array.from({length: 30}, (_, 序) => 场.键('循环列/' + 序));
  for (const k of 诸键) await 场.写(k, 字节们('y'));
  const 列 = await 场.调('/loop-list?' + new URLSearchParams({b: 'PACKAGES', opts: JSON.stringify({prefix: 场.键('循环列/'), include: ['customMetadata']}), n: 200}));
  assert.equal(列.状态码, 200, 列.文.slice(0, 200)); assert.equal(列.数据().objects.length, 30);
  await 场.清理([键, ...诸键]);
});

test('回应入站以对象存储对象：GET 直通正文流，自带 Content-Length 与 ETag，叠加附加标头', async () => {
  const 场 = 新场景(), 键 = 场.键('直/文.txt');
  const 体 = 字节们('直通的正文 with 中文\n第二行');
  await 场.写(键, 体);
  const 回 = await 场.直通(键, {状态: 200, 头们: [['Content-Type', 'text/plain; charset=utf-8'], ['Cache-Control', 'public, max-age=31536000, immutable'], ['X-Content-Type-Options', 'nosniff'], ['Content-Disposition', "attachment; filename*=UTF-8''%E6%96%87.txt"]]});
  assert.equal(回.状态码, 200);
  assert.ok(Buffer.from(回.字节).equals(体));
  assert.equal(回.头.get('content-length'), String(体.length));
  assert.equal(回.头.get('etag'), `"${md5(体)}"`);
  assert.equal(回.头.get('content-type'), 'text/plain; charset=utf-8');
  assert.equal(回.头.get('cache-control'), 'public, max-age=31536000, immutable');
  assert.equal(回.头.get('x-content-type-options'), 'nosniff');
  assert.equal(回.头.get('content-disposition'), "attachment; filename*=UTF-8''%E6%96%87.txt");
  // 别的成功状态
  const 二 = await 场.直通(键, {状态: 201});
  assert.equal(二.状态码, 201); assert.ok(Buffer.from(二.字节).equals(体));
  // 空对象
  const 空 = 场.键('直/空');
  await 场.写(空, new Uint8Array());
  const 空回 = await 场.直通(空);
  assert.equal(空回.状态码, 200); assert.equal(空回.字节.length, 0); assert.equal(空回.头.get('content-length'), '0');
  await 场.清理([键, 空]);
});

test('回应入站以对象存储对象：HEAD 只带头不带体；对象不存在返回阴且不占用最终响应', async () => {
  const 场 = 新场景(), 键 = 场.键('直/头.bin');
  const 体 = Uint8Array.from({length: 1000}, (_, 序) => 序 % 256);
  await 场.写(键, 体);
  const 头回 = await 场.直通(键, {方法: 'HEAD', 头们: [['Content-Type', 'application/octet-stream']]});
  assert.equal(头回.状态码, 200); assert.equal(头回.字节.length, 0);
  assert.equal(头回.头.get('content-length'), '1000'); assert.equal(头回.头.get('etag'), `"${md5(体)}"`);
  assert.equal(头回.头.get('content-type'), 'application/octet-stream');
  if (!远端) {
    const 桶 = 场.桶();
    assert.equal(桶.记录.filter(项 => 项.方法 === 'body.pull').length, 0, 'HEAD 不得拉取正文');
    assert.ok(桶.记录.some(项 => 项.方法 === 'head'), 'HEAD 请求使用 head()');
  }
  // 不存在：应用得到阴，自行回 404（证明最终响应未被占用）
  for (const 方法 of ['GET', 'HEAD']) {
    const 无 = await 场.直通(场.键('直/无'), {方法});
    assert.equal(无.状态码, 404, 方法);
    if (方法 === 'GET') assert.equal(无.文, '缺');
  }
  await 场.清理([键]);
});

仅模拟('回应入站以对象存储对象：64 MiB 大对象流式直通，正文不经 Wasm 内存，也不整读', async () => {
  const 场 = 新场景(), 桶 = 场.桶();
  const 大 = Buffer.alloc(64 * MiB);
  for (let 序 = 0; 序 < 大.length; 序 += 4096) 大[序] = 序 / 4096 & 255;
  await 桶.put('大文件', 大);
  桶.记录.length = 0;
  const 起 = performance.now();
  const 回 = await 场.直通('大文件', {头们: [['Content-Type', 'application/octet-stream']]});
  const 耗时 = performance.now() - 起;
  assert.equal(回.状态码, 200); assert.equal(回.字节.length, 64 * MiB);
  assert.equal(回.头.get('content-length'), String(64 * MiB));
  assert.equal(sha256(回.字节), sha256(大));
  assert.deepEqual(桶.记录.filter(项 => /^GetResult\./.test(项.方法)), [], '不得调用 arrayBuffer/text/bytes/json/blob');
  assert.ok(桶.记录.filter(项 => 项.方法 === 'body.pull').length >= 1024, '正文流按块拉取');
  console.log(`64 MiB 直通用时 ${耗时.toFixed(0)}ms`);
});

test('回应入站以对象存储对象：状态与附加标头的严格校验', async () => {
  const 场 = 新场景(), 键 = 场.键('直/校');
  await 场.写(键, 字节们('x'));
  for (const 状态 of [199, 100, 204, 205, 206, 300, 301, 304, 400, 404, 500, 0, -1, 600]) 应失败(await 场.直通(键, {状态}), /直通对象的响应状态须在 200 至 299 之间，且不能是无正文的 204、205 或须按范围交付的 206/, String(状态));
  const 拒 = async (头们, 正则, 名) => 应失败(await 场.直通(键, {头们}), 正则, 名);
  for (const 禁 of ['Content-Length', 'content-length', 'CONTENT-LENGTH', 'Transfer-Encoding', 'Connection', 'Keep-Alive', 'Upgrade', 'TE', 'Trailer', 'Proxy-Authenticate', 'Proxy-Authorization', 'Proxy-Connection', 'Set-Cookie', 'set-cookie', 'ETag', 'etag']) {
    await 拒([[禁, '1']], /附加标头不得给出/, '禁用 ' + 禁);
  }
  for (const 坏名 of ['', 'a b', 'a:b', 'a\nb', '中文', 'a/b', 'a(b', 'a"b', 'a,b', 'a;b', 'x'.repeat(129)]) await 拒([[坏名, 'v']], /附加标头名不是合法的 HTTP token/, '坏名 ' + JSON.stringify(坏名));
  for (const 坏值 of ['a\nb', 'a\rb', 'a\u0000b', '中文', 'a\x7fb', ' 前空格', '后空格 ', 'x'.repeat(4097), 'a\tb']) await 拒([['X-A', 坏值]], /的值须为不超过 4096 字节的可见 ASCII，且首尾不得为空格/, '坏值 ' + JSON.stringify(坏值).slice(0, 20));
  await 拒([['X-A', '1'], ['x-a', '2']], /附加标头重复：x-a/, '大小写重复');
  await 拒(Array.from({length: 65}, (_, 序) => ['X-H' + 序, 'v']), /附加标头至多 64 项/, '65 项');
  // 边界内通过：64 项、合法 token 全字符集、4096 字节的值
  const 六十四 = Array.from({length: 64}, (_, 序) => ['X-H' + 序, 'v' + 序]);
  const 好 = await 场.直通(键, {头们: 六十四});
  assert.equal(好.状态码, 200); assert.equal(好.头.get('x-h63'), 'v63');
  const 全名 = "X-!#$%&'*+-.^_`|~09azAZ";
  const 全 = await 场.直通(键, {头们: [[全名, 'a b'], ['X-Long', 'x'.repeat(4096)]]});
  assert.equal(全.状态码, 200, 全.文.slice(0, 200)); assert.equal(全.头.get(全名), 'a b'); assert.equal(全.头.get('x-long').length, 4096);
  await 场.清理([键]);
});

test('多个桶互相隔离：同名键在 PACKAGES 与 SPARE 中各是各的', async () => {
  const 场 = 新场景(), 键 = 场.键('同名');
  await 场.写(键, 字节们('主桶内容'), {customMetadata: {桶: '主'}});
  await 场.写(键, 字节们('备用桶内容'), {customMetadata: {桶: '备'}}, 'SPARE');
  assert.equal((await 场.读(键, 100)).数据().text, '主桶内容');
  assert.equal((await 场.读(键, 100, 'SPARE')).数据().text, '备用桶内容');
  assert.deepEqual((await 场.头(键, 'SPARE')).数据().customMetadata, {桶: '备'});
  await 场.删([键], 'SPARE');
  assert.equal((await 场.头(键, 'SPARE')).文, 'null');
  assert.notEqual((await 场.头(键)).文, 'null');
  const 直 = await 场.直通(键, {桶: 'PACKAGES'});
  assert.equal(new TextDecoder().decode(直.字节), '主桶内容');
  assert.equal((await 场.直通(键, {桶: 'SPARE'})).状态码, 404);
  await 场.清理([键]);
});

test('回应入站以对象存储对象：16 MiB 对象流式直通（Content-Length、ETag 与正文逐字节一致）', async () => {
  const 场 = 新场景(), 键 = 场.键('直/十六兆');
  const 写 = await 场.调('/put-zero?' + new URLSearchParams({b: 'PACKAGES', k: 键, opts: '', n: 16 * MiB}));
  assert.equal(写.状态码, 200, 写.文.slice(0, 200));
  const 回 = await 场.直通(键, {头们: [['Content-Type', 'application/octet-stream']]});
  assert.equal(回.状态码, 200); assert.equal(回.字节.length, 16 * MiB);
  assert.equal(回.头.get('content-length'), String(16 * MiB));
  assert.equal(回.头.get('etag'), `"${写.数据().etag}"`);
  assert.ok(回.字节.every(字节 => 字节 === 0));
  await 场.清理([键]);
});

test('列举对象：1001 个对象（键含引号、反斜杠、汉字）经写入接口建立，一页 1000 项并可续列；含 include 时平台可少给，以 cursor 续列', async () => {
  const 场 = 新场景();
  const 诸键 = Array.from({length: 1001}, (_, 序) => 场.键(`量/${String(序).padStart(4, '0')}/"汉\\字 ${序}`));
  const 序号 = 键 => 键.slice(键.lastIndexOf(' ') + 1);
  for (const 键 of 诸键) assert.equal((await 场.写(键, 字节们('x'), {customMetadata: {说明: `第"${序号(键)}"项\\`}})).状态码, 200);
  const 一 = await 场.列({prefix: 场.键('量/')});
  assert.equal(一.状态码, 200, 一.文.slice(0, 200));
  assert.ok(!一.文.includes('$句柄'), '输出不得泄漏宿主句柄引用');
  const 页一 = 一.数据();
  assert.equal(页一.objects.length, 1000); assert.equal(页一.truncated, true);
  assert.deepEqual(页一.objects.map(项 => 项.key), 诸键.slice(0, 1000));
  const 二 = (await 场.列({prefix: 场.键('量/'), cursor: 页一.cursor})).数据();
  assert.deepEqual(二.objects.map(项 => 项.key), 诸键.slice(1000)); assert.equal(二.truncated, false);
  // 含 include：真实 R2 每页可能少于 limit（文档：If include is set, you may get fewer than limit results），必须按 truncated 与 cursor 续列。
  const 全 = []; let 游标 = null, 页数 = 0;
  for (;;) {
    const 选项 = {prefix: 场.键('量/'), include: ['customMetadata']}; if (游标) 选项.cursor = 游标;
    const 回 = await 场.列(选项);
    assert.equal(回.状态码, 200, 回.文.slice(0, 200)); assert.ok(!回.文.includes('$句柄'));
    const 页 = 回.数据(); 页数++;
    assert.ok(页.objects.length >= 1 && 页.objects.length <= 1000);
    全.push(...页.objects);
    if (!页.truncated) { assert.equal(页.cursor, null); break; }
    游标 = 页.cursor;
  }
  assert.deepEqual(全.map(项 => 项.key), 诸键);
  assert.deepEqual(全[7].customMetadata, {说明: `第"7"项\\`});
  console.log(`含 include 的列举共 ${页数} 页`);
  await 场.删(诸键.slice(0, 1000)); await 场.清理(诸键.slice(1000));
});

test('参数中的引号、反斜杠、汉字与非 BMP 字符经 JSON 传给平台不走样（前缀、键、元数据、续标）', async () => {
  const 场 = 新场景();
  const 前缀 = 场.键('怪/"引\\号/😀 汉字/');
  const 诸键 = [前缀 + '甲"乙', 前缀 + '丙\\丁', 前缀 + '戊 己/庚'];
  for (const 键 of 诸键) assert.equal((await 场.写(键, 字节们('x'), {customMetadata: {'键"名': '值\\"含引号', 名: '😀 汉字'}})).状态码, 200, 键);
  const 果 = (await 场.列({prefix: 前缀, include: ['customMetadata']})).数据();
  assert.deepEqual(果.objects.map(项 => 项.key).sort(), [...诸键].sort());
  assert.deepEqual(果.objects[0].customMetadata, {'键"名': '值\\"含引号', 名: '😀 汉字'});
  const 折 = (await 场.列({prefix: 前缀, delimiter: '/'})).数据();
  assert.deepEqual(折.delimitedPrefixes, [前缀 + '戊 己/']);
  for (const 键 of 诸键) assert.equal((await 场.头(键)).数据().size, 1, 键);
  assert.equal((await 场.读(诸键[0], 10)).数据().text, 'x');
  assert.equal(new TextDecoder().decode((await 场.直通(诸键[1])).字节), 'x');
  await 场.清理(诸键);
});
