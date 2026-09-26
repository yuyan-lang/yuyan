// 文言：以真 Wasm 验压缩一版二术，并与节点 zlib 对拍；汉语：压缩 0.1.0 的真实 Wasm 一致性测试（Node）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';
import zlib from 'node:zlib';

// 两种运行方式：缺省在 Node 中直接装载真实 Wasm；设置环境变量 远端地址（如 http://localhost:8791）则经 HTTP
// 调用同一应用的真实 workerd（wrangler dev）。产物目录环境变量指向 dist/<输出名>，缺省为同级 产物/。
const 远端 = process.env.远端地址 || '';
let 宿主 = null;
if (!远端) {
  const 根 = process.env.产物目录 ? path.resolve(process.env.产物目录) + '/'
    : fileURLToPath(new URL('./产物/', import.meta.url));
  const {创建云工宿主} = await import(根 + '宿主.mjs');
  const 程序模块 = await WebAssembly.compile(await readFile(根 + '程序.wasm'));
  const 值桥模块 = await WebAssembly.compile(await readFile(根 + '值桥.wasm'));
  宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {}});
}
const MiB = 1024 * 1024;
const 上限 = 134217728;
const 请 = async (径, 体) => {
  const 回 = 远端 ? await fetch(远端 + 径, {method: 'POST', body: 体})
    : await 宿主.fetch(new Request('https://x.test' + 径, {method: 'POST', body: 体}), {});
  const 字节 = new Uint8Array(await 回.arrayBuffer());
  return {状态码: 回.status, 状态: 回.headers.has('x-status') ? Number(回.headers.get('x-status')) : null, 字节, 文: 回.status === 200 ? '' : new TextDecoder().decode(字节)};
};
const 原始 = (数据, 最大) => 请('/raw?max=' + 最大, 数据);
const 条目 = (数据, 方法, crc, 大小, 最大) => 请(`/entry?method=${方法}&crc=${crc}&size=${大小}&max=${最大}`, 数据);
const 相同 = (甲, 乙) => Buffer.compare(Buffer.from(甲), Buffer.from(乙)) === 0;
const 压 = (数据, 选项) => zlib.deflateRawSync(数据, 选项);
// 确定性伪随机，便于复现。
const 造随机 = (种子 => () => (种子 = (Math.imul(种子, 1664525) + 1013904223) >>> 0) / 4294967296);
const 随机 = 造随机(20260925);
const 随机字节 = n => Uint8Array.from({length: n}, () => Math.floor(随机() * 256));
const 类文本 = n => Buffer.from('豫言操作系统 压缩测试 abc 0123456789\n'.repeat(Math.ceil(n / 30))).subarray(0, n);

test('解压原始压缩：正常路径与 zlib.inflateRawSync 逐字节一致', async () => {
  const 样例 = [类文本(10), 类文本(1000), 类文本(100000), Buffer.from('a'), Buffer.alloc(70000, 0x61), random字节样例()];
  function random字节样例() { return Buffer.from(随机字节(30000)); }
  for (const 原 of 样例) {
    for (const 级别 of [0, 1, 6, 9]) {
      const 数据 = 压(原, {level: 级别});
      const 果 = await 原始(数据, 上限);
      assert.equal(果.状态码, 200); assert.equal(果.状态, 0, `len=${原.length} level=${级别}`);
      assert.ok(相同(果.字节, zlib.inflateRawSync(数据)) && 相同(果.字节, 原), `len=${原.length} level=${级别}`);
    }
  }
});

test('解压原始压缩：空输出的合法流、空输入、损坏、截断、尾随字节', async () => {
  let 果 = await 原始(Uint8Array.of(3, 0), 10);
  assert.equal(果.状态, 0); assert.equal(果.字节.length, 0);
  果 = await 原始(压(Buffer.alloc(0)), 10);
  assert.equal(果.状态, 0); assert.equal(果.字节.length, 0);
  果 = await 原始(new Uint8Array(), 10);
  assert.equal(果.状态, 2); assert.equal(果.字节.length, 0);
  果 = await 原始(Uint8Array.of(0xff, 0xff, 0xff, 0xff), 10);
  assert.equal(果.状态, 2);
  const 好 = 压(类文本(5000));
  for (const 截 of [1, 2, 5, 好.length - 1, 好.length - 3]) {
    果 = await 原始(好.subarray(0, 截), 上限);
    assert.equal(果.状态, 2, `截断到 ${截}`); assert.equal(果.字节.length, 0);
  }
  果 = await 原始(Buffer.concat([好, Buffer.from([1, 2, 3])]), 上限);
  assert.equal(果.状态, 2, '尾随多余字节按损坏处理（与 DecompressionStream 一致，不同于 inflateRawSync）');
  果 = await 原始(Buffer.concat([好, 好]), 上限);
  assert.equal(果.状态, 2, '两个流首尾相接也视为尾随字节');
});

test('解压原始压缩：输出上限的边界（恰等、多一字节、最小上限）', async () => {
  const 原 = 类文本(50000), 数据 = 压(原);
  let 果 = await 原始(数据, 50000);
  assert.equal(果.状态, 0); assert.ok(相同(果.字节, 原));
  果 = await 原始(数据, 49999);
  assert.equal(果.状态, 1); assert.equal(果.字节.length, 0);
  果 = await 原始(数据, 1);
  assert.equal(果.状态, 1);
  果 = await 原始(压(Buffer.from('x')), 1);
  assert.equal(果.状态, 0); assert.equal(Buffer.from(果.字节).toString(), 'x');
  果 = await 原始(压(Buffer.from('xy')), 1);
  assert.equal(果.状态, 1);
  果 = await 原始(压(Buffer.alloc(0)), 1);
  assert.equal(果.状态, 0);
  // 恰好跨过分块（16384 字节一块）的输出长度
  for (const n of [16383, 16384, 16385, 32768, 65536, 65537]) {
    const 数 = 类文本(n), 果二 = await 原始(压(数), n);
    assert.equal(果二.状态, 0, String(n)); assert.ok(相同(果二.字节, 数), String(n));
    assert.equal((await 原始(压(数), n - 1)).状态, 1, String(n));
  }
});

test('解压原始压缩：多块输出（数 MiB）与随机数据', async () => {
  const 大 = Buffer.concat([类文本(3 * MiB), Buffer.from(随机字节(MiB)), Buffer.alloc(2 * MiB, 7)]);
  const 数据 = 压(大, {level: 6});
  const 果 = await 原始(数据, 上限);
  assert.equal(果.状态, 0); assert.equal(果.字节.length, 大.length); assert.ok(相同(果.字节, 大));
  // 同一数据在不同上限下：足够则成，差一字节则 1
  assert.equal((await 原始(数据, 大.length)).状态, 0);
  assert.equal((await 原始(数据, 大.length - 1)).状态, 1);
});

test('解压原始压缩：压缩炸弹在累计超限时立即取消，不继续解压', async () => {
  const 炸 = 压(Buffer.alloc(200 * MiB));
  assert.ok(炸.length < 300 * 1024, `压缩后 ${炸.length}`);
  const 内存前 = process.memoryUsage().rss;
  let 起 = performance.now();
  let 果 = await 原始(炸, MiB);
  const 耗时小 = performance.now() - 起;
  assert.equal(果.状态, 1); assert.equal(果.字节.length, 0);
  起 = performance.now();
  果 = await 原始(炸, 32 * MiB);
  const 耗时大 = performance.now() - 起;
  assert.equal(果.状态, 1);
  const 内存增 = (process.memoryUsage().rss - 内存前) / MiB;
  console.log(`炸弹：上限 1 MiB 用时 ${耗时小.toFixed(0)}ms，上限 32 MiB 用时 ${耗时大.toFixed(0)}ms，RSS 增 ${内存增.toFixed(0)} MiB`);
  // 完整解出 200 MiB 需数百毫秒以上且内存翻倍；取消后的用时应与上限成正比而远小于全解。
  assert.ok(耗时小 < 耗时大 + 50, '小上限不应比大上限更慢');
  assert.ok(内存增 < 400, `内存增长过大：${内存增}`);
  // 输出恰好满足上限的大流
  const 千 = 压(Buffer.alloc(8 * MiB, 3));
  果 = await 原始(千, 8 * MiB);
  assert.equal(果.状态, 0); assert.equal(果.字节.length, 8 * MiB);
  果 = await 原始(千, 8 * MiB - 1);
  assert.equal(果.状态, 1);
});

test('解压原始压缩：参数越界与超大输入抛豫言异常', async () => {
  const 数据 = 压(Buffer.from('abc'));
  for (const 最大 of [0, -1, 上限 + 1]) {
    const 果 = await 原始(数据, 最大);
    assert.equal(果.状态码, 400, String(最大)); assert.match(果.文, /解压最大输出须在 1 至 134217728 字节之间/);
  }
  assert.equal((await 原始(数据, 上限)).状态, 0);
  assert.equal((await 原始(数据, 1)).状态码, 200);
});

// ----- 解压归档条目 -----
const 存储条目 = 原 => ({数据: Buffer.from(原), 方法: 0, crc: zlib.crc32(原), 大小: 原.length, 原: Buffer.from(原)});
const 压缩条目 = (原, 选项) => ({数据: 压(原, 选项), 方法: 8, crc: zlib.crc32(原), 大小: 原.length, 原: Buffer.from(原)});
const 验 = async (项, 期状态, 最大 = 上限, 消息) => {
  const 果 = await 条目(项.数据, 项.方法, 项.crc, 项.大小, 最大);
  assert.equal(果.状态码, 200, 消息 + ' ' + 果.文); assert.equal(果.状态, 期状态, 消息);
  if (期状态 === 0) assert.ok(相同(果.字节, 项.原), 消息); else assert.equal(果.字节.length, 0, 消息);
  return 果;
};

test('解压归档条目：存储与 deflate 正常条目，CRC-32 与大小均已核对', async () => {
  for (const 原 of [类文本(1), 类文本(777), 类文本(100000), Buffer.from(随机字节(4096)), Buffer.alloc(300000, 9)]) {
    await 验(存储条目(原), 0, 上限, `存储 ${原.length}`);
    for (const 级别 of [0, 1, 6, 9]) await 验(压缩条目(原, {level: 级别}), 0, 上限, `deflate ${原.length} L${级别}`);
  }
  // CRC-32 标准向量
  const 串 = Buffer.from('123456789');
  assert.equal(zlib.crc32(串), 0xcbf43926);
  await 验(存储条目(串), 0, 上限, '123456789');
});

test('解压归档条目：空条目（存储、deflate 空流 03 00、deflate 存储块）', async () => {
  await 验(存储条目(Buffer.alloc(0)), 0, 1, '存储空');
  await 验({数据: Uint8Array.of(3, 0), 方法: 8, crc: 0, 大小: 0, 原: Buffer.alloc(0)}, 0, 1, 'deflate 空');
  await 验(压缩条目(Buffer.alloc(0), {level: 0}), 0, 1, 'deflate level0 空');
  // 空条目的 CRC 不为零则不符
  const 空 = 存储条目(Buffer.alloc(0)); 空.crc = 1;
  await 验(空, 3, 1, '空条目 CRC 错');
  const 空二 = 存储条目(Buffer.alloc(0)); 空二.大小 = 1;
  await 验(空二, 3, 1, '空条目大小错');
});

test('解压归档条目：存储块边界 0/1/65534/65535/65536/65537/131070/131071/16 MiB', async () => {
  for (const n of [0, 1, 65534, 65535, 65536, 65537, 131069, 131070, 131071, 196605, 196606]) {
    const 原 = Buffer.from(随机字节(n));
    await 验(存储条目(原), 0, Math.max(n, 1) + 5, `存储 ${n}`);
    await 验(压缩条目(原, {level: 0}), 0, Math.max(n, 1) + 5, `deflate 存储块 ${n}`);
  }
  const 大 = Buffer.from(随机字节(16 * MiB));
  const 起 = performance.now();
  await 验(存储条目(大), 0, 16 * MiB, '存储 16 MiB');
  console.log(`存储 16 MiB 含 CRC-32 校验用时 ${(performance.now() - 起).toFixed(0)}ms`);
});

test('解压归档条目：数据被改一字节 → 存储与 deflate 存储块给 3，哈夫曼流给 2 或 3', async () => {
  const 原 = 类文本(200000);
  const 存 = 存储条目(原); 存.数据 = Buffer.from(存.数据); 存.数据[12345] ^= 0x55;
  await 验(存, 3, 上限, '存储改一字节');
  const 存块 = 压缩条目(原, {level: 0}); 存块.数据 = Buffer.from(存块.数据); 存块.数据[70000] ^= 0x01;
  await 验(存块, 3, 上限, 'deflate 存储块改一字节');
  // 哈夫曼流：以 zlib 为参照，能解出而 CRC 不符为 3，不能解为 2
  for (const 位 of [3, 10, 100, 1000]) {
    const 项 = 压缩条目(原, {level: 6}); 项.数据 = Buffer.from(项.数据); 项.数据[位] ^= 0x10;
    let 期;
    try { const {buffer, engine} = zlib.inflateRawSync(项.数据, {info: true}); 期 = engine.bytesWritten < 项.数据.length ? 2 : (zlib.crc32(buffer) === 项.crc && buffer.length === 项.大小 ? 0 : 3); }
    catch { 期 = 2; }
    const 果 = await 条目(项.数据, 8, 项.crc, 项.大小, 上限);
    assert.equal(果.状态, 期, `位 ${位}`);
  }
});

test('解压归档条目：改预期 CRC、预期大小偏大偏小，均为 3', async () => {
  for (const 项 of [存储条目(类文本(5000)), 压缩条目(类文本(5000), {level: 6}), 压缩条目(类文本(5000), {level: 0})]) {
    await 验({...项, crc: (项.crc ^ 1) >>> 0}, 3, 上限, `crc 改 ${项.方法}`);
    await 验({...项, crc: 0}, 3, 上限, `crc 零 ${项.方法}`);
    await 验({...项, crc: 4294967295}, 3, 上限, `crc 全一 ${项.方法}`);
    await 验({...项, 大小: 项.大小 + 1}, 3, 上限, `大小偏大 ${项.方法}`);
    await 验({...项, 大小: 项.大小 - 1}, 3, 上限, `大小偏小 ${项.方法}`);
    await 验({...项, 大小: 0}, 3, 上限, `大小零 ${项.方法}`);
    await 验({...项, 大小: 65536 + 项.大小}, 3, 上限, `大小差 2^16 ${项.方法}`);
  }
});

test('解压归档条目：损坏、截断、尾随字节 → 状态 2；存储永不为 2', async () => {
  const 项 = 压缩条目(类文本(50000), {level: 6});
  await 验({...项, 数据: 项.数据.subarray(0, 项.数据.length - 2)}, 2, 上限, '截断');
  await 验({...项, 数据: 项.数据.subarray(0, 1)}, 2, 上限, '截到一字节');
  await 验({...项, 数据: new Uint8Array()}, 2, 上限, '空数据按 deflate');
  await 验({...项, 数据: Buffer.concat([项.数据, Buffer.from([9, 9, 9])])}, 2, 上限, '尾随字节');
  await 验({...项, 数据: Uint8Array.of(0xff, 0xff, 0xff, 0xff, 0xff)}, 2, 上限, '乱字节');
  // 存储方法的数据是任意字节，只可能 3
  const 存 = 存储条目(Buffer.from(随机字节(1000))); 存.crc = (存.crc + 1) >>> 0;
  await 验(存, 3, 上限, '存储 CRC 错');
});

test('解压归档条目：输出上限（恰等为 0，多一字节为 1）与压缩炸弹', async () => {
  const 原 = 类文本(300000);
  for (const 项 of [存储条目(原), 压缩条目(原, {level: 6})]) {
    await 验(项, 0, 原.length, `恰等 ${项.方法}`);
    // 诚实条目的预期大小就是实际大小，故实际输出超限时预期大小也超限，是参数错误；
    // 状态 1 只出现在预期大小（来自不可信的归档目录）小于实际输出的说谎条目上。
    await 验({...项, 大小: 1}, 1, 原.length - 1, `说谎条目少一 ${项.方法}`);
    await 验({...项, 大小: 1}, 1, 1, `说谎条目最小上限 ${项.方法}`);
    await 验({...项, 大小: 原.length - 1}, 3, 原.length, `预期偏小一 ${项.方法}`);
    // 预期大小超过最大输出是参数错误
    const 果 = await 条目(项.数据, 项.方法, 项.crc, 项.大小, 原.length - 1);
    assert.equal(果.状态码, 400); assert.match(果.文, /预期解压大小须在 0 至最大输出之间/);
  }
  const 炸原 = Buffer.alloc(200 * MiB);
  const 炸 = 压缩条目(炸原, {level: 9}).数据;
  const 起 = performance.now();
  const 果 = await 条目(炸, 8, 0, 1000, 8 * MiB);
  console.log(`归档炸弹（上限 8 MiB）用时 ${(performance.now() - 起).toFixed(0)}ms`);
  assert.equal(果.状态, 1); assert.equal(果.字节.length, 0);
  // 最大上限（128 MiB）下的炸弹：累计满 128 MiB 后才取消
  const 起二 = performance.now();
  const 果二 = await 条目(炸, 8, 0, 1000, 上限);
  console.log(`归档炸弹（上限 128 MiB）用时 ${(performance.now() - 起二).toFixed(0)}ms`);
  assert.equal(果二.状态, 1);
  // 真实大小 = 上限的大条目通过校验
  const 满 = 压缩条目(Buffer.alloc(8 * MiB, 5), {level: 6});
  await 验(满, 0, 8 * MiB, '8 MiB 全 5');
});

test('解压归档条目：参数错误抛豫言异常', async () => {
  const 项 = 存储条目(Buffer.from('abc'));
  for (const 方法 of [-1, 1, 2, 7, 9, 12, 93]) {
    const 果 = await 条目(项.数据, 方法, 项.crc, 项.大小, 100);
    assert.equal(果.状态码, 400, `方法 ${方法}`); assert.match(果.文, /归档条目方法须为 0（存储）或 8（deflate）/);
  }
  for (const crc of [-1, 4294967296, 99999999999]) {
    const 果 = await 条目(项.数据, 0, crc, 项.大小, 100);
    assert.equal(果.状态码, 400, `crc ${crc}`); assert.match(果.文, /CRC-32 须在 0 至 4294967295 之间/);
  }
  for (const 大小 of [-1, 101]) {
    const 果 = await 条目(项.数据, 0, 项.crc, 大小, 100);
    assert.equal(果.状态码, 400, `大小 ${大小}`); assert.match(果.文, /预期解压大小须在 0 至最大输出之间/);
  }
  for (const 最大 of [0, -5, 上限 + 1]) {
    const 果 = await 条目(项.数据, 0, 项.crc, 项.大小, 最大);
    assert.equal(果.状态码, 400, `最大 ${最大}`); assert.match(果.文, /解压最大输出须在 1 至 134217728 字节之间/);
  }
  assert.equal((await 条目(项.数据, 0, 4294967295 & 项.crc, 项.大小, 上限)).状态码, 200);
  const 边 = await 条目(项.数据, 0, 项.crc, 3, 3);
  assert.equal(边.状态, 0);
});

test('解压归档条目：与 zlib 参照模型的随机对拍（不含超限情形）', async () => {
  let 组数 = 0, 状态计数 = [0, 0, 0, 0];
  for (let 轮 = 0; 轮 < 150; 轮++) {
    const 长 = Math.floor(随机() * 3000);
    const 原 = 随机() < 0.5 ? Buffer.from(随机字节(长)) : 类文本(长);
    const 方法 = 随机() < 0.4 ? 0 : 8;
    let 数据 = 方法 === 0 ? Buffer.from(原) : Buffer.from(压(原, {level: [0, 1, 6, 9][Math.floor(随机() * 4)]}));
    let crc = zlib.crc32(原), 大小 = 原.length;
    const 扰 = 随机();
    if (扰 < 0.2 && 数据.length) 数据[Math.floor(随机() * 数据.length)] ^= 1 << Math.floor(随机() * 8);
    else if (扰 < 0.3) crc = (crc + 1) >>> 0;
    else if (扰 < 0.4) 大小 += 1;
    else if (扰 < 0.5 && 数据.length > 1) 数据 = 数据.subarray(0, 数据.length - 1);
    else if (扰 < 0.55) 数据 = Buffer.concat([数据, Buffer.from([0])]);
    // 参照：先取输出与合法性
    let 出, 合法 = true;
    if (方法 === 0) 出 = 数据;
    else { try { const {buffer, engine} = zlib.inflateRawSync(数据, {info: true}); if (engine.bytesWritten < 数据.length) 合法 = false; 出 = buffer; } catch { 合法 = false; } }
    const 最大 = Math.max(大小, 1) + 100000, 期 = !合法 ? 2 : (出.length > 最大 ? 1 : (zlib.crc32(出) === crc && 出.length === 大小 ? 0 : 3));
    if (期 !== 2 && 大小 > 最大) continue;
    const 果 = await 条目(数据, 方法, crc, 大小, 最大);
    assert.equal(果.状态码, 200, `轮 ${轮}`); assert.equal(果.状态, 期, `轮 ${轮} 方法 ${方法} 长 ${长} 扰 ${扰.toFixed(2)}`);
    if (期 === 0) assert.ok(相同(果.字节, 出), `轮 ${轮}`);
    状态计数[期]++; 组数++;
  }
  console.log('随机对拍组数', 组数, '状态分布(0/1/2/3)', 状态计数.join('/'));
  assert.ok(状态计数[0] > 20 && 状态计数[2] > 5 && 状态计数[3] > 5);
});

test('解压原始压缩：随机乱字节与随机扰动的合法流，与 zlib 参照模型对拍（含尾随字节判为损坏）', async () => {
  const 参照 = (数据, 最大) => {
    try {
      const {buffer, engine} = zlib.inflateRawSync(数据, {info: true});
      if (engine.bytesWritten < 数据.length) return [2, null];
      return buffer.length > 最大 ? [1, null] : [0, buffer];
    } catch { return [2, null]; }
  };
  const 计 = [0, 0, 0]; let 组 = 0;
  for (let 轮 = 0; 轮 < 400; 轮++) {
    let 数据;
    const 种 = 随机();
    if (种 < 0.35) 数据 = Buffer.from(随机字节(Math.floor(随机() * 60)));
    else {
      数据 = Buffer.from(压(随机() < 0.5 ? 类文本(Math.floor(随机() * 3000)) : Buffer.from(随机字节(Math.floor(随机() * 2000))), {level: [0, 1, 6, 9][Math.floor(随机() * 4)]}));
      const 扰 = 随机();
      if (扰 < 0.4 && 数据.length) 数据[Math.floor(随机() * 数据.length)] ^= 1 << Math.floor(随机() * 8);
      else if (扰 < 0.55 && 数据.length > 1) 数据 = 数据.subarray(0, 1 + Math.floor(随机() * (数据.length - 1)));
      else if (扰 < 0.65) 数据 = Buffer.concat([数据, Buffer.from(随机字节(1 + Math.floor(随机() * 4)))]);
    }
    const 最大 = 1 + Math.floor(随机() * 5000);
    const [期状态, 期字节] = 参照(数据, 最大);
    const 果 = await 原始(数据, 最大);
    assert.equal(果.状态码, 200, `轮 ${轮}`);
    // 既损坏又超限者，宿主流的缓冲深浅不同可能给 1 或 2：只要求非 0，不判具体值。
    if (期状态 === 2 && 果.状态 === 1) { 计[1]++; continue; }
    if (期状态 === 1 && 果.状态 === 2) { 计[2]++; continue; }
    assert.equal(果.状态, 期状态, `轮 ${轮} 长 ${数据.length} 最大 ${最大}`);
    if (期状态 === 0) assert.ok(相同(果.字节, 期字节), `轮 ${轮}`);
    组++;
  }
  console.log('原始压缩随机对拍：严格一致', 组, '组；模糊组（1/2 互换）', 计[1] + 计[2]);
  assert.ok(组 > 300);
});

test('句柄释放：同一事件内反复解压（成功、超限、损坏、校验不符路径），不触及事件句柄上限（4096）', async () => {
  const 原 = 类文本(3000), 好 = 压(原), 存 = 存储条目(原), 压条 = 压缩条目(原, {level: 6});
  const 轮 = async (径, 体, 期状态, 消息) => {
    const 果 = await 请(径, 体);
    assert.equal(果.状态码, 200, `${消息} ${果.文.slice(0, 200)}`); assert.equal(果.状态, 期状态, 消息);
  };
  await 轮('/loop-raw?max=100000&n=3000', 好, 0, '原始成功');
  await 轮('/loop-raw?max=100&n=3000', 好, 1, '原始超限（取消路径）');
  await 轮('/loop-raw?max=100000&n=3000', 好.subarray(0, 好.length - 3), 2, '原始截断');
  await 轮(`/loop-entry?method=0&crc=${存.crc}&size=${存.大小}&max=100000&n=1500`, 存.数据, 0, '存储成功');
  await 轮(`/loop-entry?method=0&crc=${(存.crc + 1) >>> 0}&size=${存.大小}&max=100000&n=1500`, 存.数据, 3, '存储 CRC 错');
  await 轮(`/loop-entry?method=8&crc=${压条.crc}&size=${压条.大小}&max=100000&n=1500`, 压条.数据, 0, 'deflate 成功');
  await 轮(`/loop-entry?method=8&crc=${(压条.crc + 1) >>> 0}&size=${压条.大小}&max=100000&n=1500`, 压条.数据, 3, 'deflate CRC 错（复解路径）');
  await 轮(`/loop-entry?method=8&crc=${压条.crc}&size=${压条.大小}&max=100000&n=1500`, 压条.数据.subarray(0, 10), 2, 'deflate 损坏（复解路径）');
});
