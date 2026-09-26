// 文言：以真 Wasm 验密码摘要二版；汉语：密码摘要 0.2.0 的真实 Wasm 一致性测试（Node）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {fileURLToPath} from 'node:url';
import path from 'node:path';

// 产物目录：环境变量 产物目录 指向 dist/<输出名>；缺省为同级 产物/。
const 根 = process.env.产物目录 ? path.resolve(process.env.产物目录) + '/'
  : fileURLToPath(new URL('./产物/', import.meta.url));
const {创建云工宿主} = await import(根 + '宿主.mjs');
const 程序模块 = await WebAssembly.compile(await readFile(根 + '程序.wasm'));
const 值桥模块 = await WebAssembly.compile(await readFile(根 + '值桥.wasm'));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {}});
const 调 = async (径, 体) => {
  const 回 = await 宿主.fetch(new Request('https://x.test' + 径, 体 === undefined ? {} : {method: 'POST', body: 体}), {});
  return {状态: 回.status, 文: await 回.text()};
};
const 十六 = 内容 => createHash('sha256').update(内容).digest('hex');
const 文摘要 = async 文 => (await 调('/sha-text?t=' + encodeURIComponent(文))).文;
const 字节摘要 = async 字节 => (await 调('/sha-bytes', 字节)).文;
const 恒等 = async (甲, 乙) => (await 调('/eq?a=' + encodeURIComponent(甲) + '&b=' + encodeURIComponent(乙))).文;

test('文字摘要：空文、短文、汉字与标准向量', async () => {
  assert.equal(await 文摘要(''), 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
  assert.equal(await 文摘要('abc'), 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
  for (const 文 of ['豫言', '甲乙丙丁戊己庚辛壬癸', 'a'.repeat(1000), '带 空格 与 标点，！？"\\']) {
    assert.equal(await 文摘要(文), 十六(Buffer.from(文, 'utf8')), 文.slice(0, 8));
  }
});

test('字节摘要：与文字摘要同义，并保留零字节与 0xFF', async () => {
  assert.equal(await 字节摘要(new Uint8Array()), 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
  assert.equal(await 字节摘要(Buffer.from('abc')), 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
  const 杂 = Uint8Array.of(0, 255, 1, 254, 0, 0, 128, 127, 0);
  assert.equal(await 字节摘要(杂), 十六(杂));
  const 全 = Uint8Array.from({length: 256}, (_, 序) => 序);
  assert.equal(await 字节摘要(全), 十六(全));
  assert.equal(await 字节摘要(Buffer.from('豫言')), await 文摘要('豫言'));
});

test('字节摘要：1 MiB 与 16 MiB 边界，超过 16 MiB 失败', async () => {
  const 一兆 = new Uint8Array(1024 * 1024).map((_, 序) => (序 * 31 + 7) & 255);
  assert.equal(await 字节摘要(一兆), 十六(一兆));
  const 上限 = new Uint8Array(16 * 1024 * 1024).map((_, 序) => (序 * 17 + 3) & 255);
  assert.equal(await 字节摘要(上限), 十六(上限));
  const 超 = new Uint8Array(16 * 1024 * 1024 + 1);
  const 果 = await 调('/sha-bytes', 超).catch(错 => ({错}));
  // 请求体 > 16 MiB 时，读取请求体字节的宿主桥先失败；无论哪层拒绝，都不得返回摘要。
  assert.ok(果.错 || 果.状态 !== 200, '超过 16 MiB 必须失败');
});

test('字节摘要：适配自身的 16 MiB 上限（Wasm 内构造零字节串）', async () => {
  for (const n of [0, 1, 3, 65536, 16 * 1024 * 1024]) {
    const 果 = await 调('/sha-zero?n=' + n);
    assert.equal(果.状态, 200, String(n)); assert.equal(果.文, 十六(new Uint8Array(n)), String(n));
  }
  const 超 = await 调('/sha-zero?n=' + (16 * 1024 * 1024 + 1));
  assert.equal(超.状态, 400); assert.match(超.文, /SHA-256 输入超过 16 MiB/);
});

test('恒时相等文字：相等、不等、长度不同、空文与汉字', async () => {
  assert.equal(await 恒等('', ''), '真');
  assert.equal(await 恒等('abc', 'abc'), '真');
  assert.equal(await 恒等('abc', 'abd'), '假');
  assert.equal(await 恒等('abc', 'Abc'), '假');
  assert.equal(await 恒等('abc', 'abcd'), '假');
  assert.equal(await 恒等('abcd', 'abc'), '假');
  assert.equal(await 恒等('', 'a'), '假');
  assert.equal(await 恒等('豫言操作系统', '豫言操作系统'), '真');
  assert.equal(await 恒等('豫言操作系统', '豫言操作系纺'), '假');
  // 字节数相同而内容不同：三字节汉字与三个 ASCII。
  assert.equal(await 恒等('甲', 'abc'), '假');
  const 长 = 'x'.repeat(4096);
  assert.equal(await 恒等(长, 长), '真');
  assert.equal(await 恒等(长, 长.slice(0, -1) + 'y'), '假');
  assert.equal(await 恒等('y' + 长.slice(1), 长), '假');
  assert.equal(await 恒等(`${'a'.repeat(63)}b`, `${'a'.repeat(63)}c`), '假');
});

test('恒时相等文字：首差异与末差异耗时同量级，证明不提前退出', async () => {
  const n = 64 * 1024, 基 = 'a'.repeat(n);
  const 首异 = 'b' + 基.slice(1), 末异 = 基.slice(0, -1) + 'b';
  const 测 = async 乙 => {
    let 最小 = Infinity;
    for (let 轮 = 0; 轮 < 5; 轮++) {
      const 起 = performance.now();
      assert.equal(await 恒等(基, 乙), '假');
      最小 = Math.min(最小, performance.now() - 起);
    }
    return 最小;
  };
  await 测(首异);   // 预热
  const 首 = await 测(首异), 末 = await 测(末异);
  // 提前退出的实现会使首异比末异快数百倍；这里只取宽松比值以避免抖动。
  assert.ok(首 > 末 * 0.25 && 首 < 末 * 4, `首异 ${首.toFixed(2)}ms 末异 ${末.toFixed(2)}ms`);
});
