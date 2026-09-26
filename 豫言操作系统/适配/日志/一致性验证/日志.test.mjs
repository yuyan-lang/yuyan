// 文言：以真 Wasm 验日志适配之分级与截断。汉语：用 Node 加载已构建的“日志一致性”产物，替换全局 console 的 error/warn/info 捕获调用。
// 用法见同目录说明：在私有暂存根目录执行 `node --test <本文件>`，产物根目录由环境变量 YY_DIST_ROOT 指定（默认 ./dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const 产物 = pathToFileURL(path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '日志一致性') + '/');
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
  assert.ok(应用要求.some(项 => 项.接口名称 === '豫言操作系统日志' && 项.接口版本 === '0.1.0'), '应用要求里应有本接口 0.1.0');
  assert.ok(宿主提供.some(项 => 项.接口名称 === '豫言操作系统日志' && 项.接口版本 === '0.1.0'), '宿主支持清单里应有本接口 0.1.0');
});

const 上限 = 8192;
const 标记 = '…（已截断）';
const 字节数 = 文 => Buffer.byteLength(文, 'utf8');
const 级别们 = ['error', 'warn', 'info', 'log', 'debug'];

// 文言：暂换 console 之五级，记其每呼；毕则复之。汉语：捕获全局 console 的调用，返回按级别分类的参数列表。
const 捕获 = async (径, 文) => {
  const 原 = Object.fromEntries(级别们.map(级 => [级, console[级]]));
  const 记 = Object.fromEntries(级别们.map(级 => [级, []]));
  for (const 级 of 级别们) console[级] = (...参数) => { 记[级].push(参数); };
  try {
    const 回 = await 宿主.fetch(new Request('https://x.test' + 径, {method: 'POST', body: 文}), {});
    return {状态: 回.status, 体: await 回.text(), 记};
  } finally {
    for (const 级 of 级别们) console[级] = 原[级];
  }
};
const 只有一条 = (记, 级, 期) => {
  for (const 他 of 级别们) if (他 !== 级) assert.equal(记[他].length, 0, `不应写入 ${他} 级`);
  assert.equal(记[级].length, 1, `${级} 级应恰有一条`);
  assert.equal(记[级][0].length, 1, '每条日志只有一个参数');
  assert.equal(typeof 记[级][0][0], 'string');
  if (期 !== undefined) assert.equal(记[级][0][0], 期);
  return 记[级][0][0];
};

test('三个函数各对应一个 console 级别', async () => {
  for (const [径, 级] of [['/error', 'error'], ['/warn', 'warn'], ['/info', 'info']]) {
    const {状态, 记} = await 捕获(径, '你好');
    assert.equal(状态, 200);
    只有一条(记, 级, '你好');
  }
});

test('不超过 8192 字节者原样写出，含空串、换行、引号、反斜线与控制字符', async () => {
  const 样本 = ['', 'a', '第一行\n第二行\r\n第三行\t制表', '"双引号" \\反斜线\\ \'单引号\'', '\u0001\u0002控制\u007f字符', '  前后空白  ', '中间的\uFEFF字节序标记保留', '😀🎉 emoji', 'a'.repeat(上限)];
  for (const 文 of 样本) {
    const {状态, 记} = await 捕获('/info', 文);
    assert.equal(状态, 200);
    只有一条(记, 'info', 文);
  }
});

// 文言：值桥之「文字」今以 ignoreBOM 之解码器读字节，文首 BOM 得存，此测遂为常测。汉语：值桥.mjs 的 文字() 已改用 ignoreBOM=true 的 TextDecoder，字符串开头的 U+FEFF 不再被吞掉，本用例转为常规用例。
test('文首的 U+FEFF 原样写出', async () => {
  const {记} = await 捕获('/info', '\uFEFF开头');
  只有一条(记, 'info', '\uFEFF开头');
});

test('恰 8192 字节不截断，8193 字节截断并附标记且总长不超过上限', async () => {
  {
    const {记} = await 捕获('/error', 'x'.repeat(上限));
    assert.equal(只有一条(记, 'error').length, 上限);
  }
  {
    const 原 = 'x'.repeat(上限 + 1);
    const {记} = await 捕获('/error', 原);
    const 出 = 只有一条(记, 'error');
    assert.ok(出.endsWith(标记));
    assert.equal(字节数(出), 上限, 'ASCII 时应恰好填满上限');
    assert.equal(出, 'x'.repeat(上限 - 字节数(标记)) + 标记);
  }
});

test('多字节字符只在字符之间截断，不裂字，且尽量填满', async () => {
  const 组 = [['豫', 3], ['😀', 4], ['é', 2], ['a', 1]];
  for (const [字, 宽] of 组) {
    for (const 多 of [1, 2, 3, 5]) {
      const 原 = 'x'.repeat(多) + 字.repeat(Math.ceil(10000 / 宽));
      const {记} = await 捕获('/warn', 原);
      const 出 = 只有一条(记, 'warn');
      assert.ok(出.endsWith(标记), `${字}×${多}`);
      assert.ok(!出.includes('�'), `${字}×${多}：不得出现替换字符`);
      assert.ok(字节数(出) <= 上限, `${字}×${多}：总长 ${字节数(出)}`);
      assert.ok(上限 - 字节数(出) < 宽, `${字}×${多}：应尽量填满，缺 ${上限 - 字节数(出)} 字节`);
      const 前 = 出.slice(0, 出.length - 标记.length);
      assert.ok(原.startsWith(前), `${字}×${多}：保留部分须是原文前缀`);
      assert.equal([...前].every(c => c === 'x' || c === 字), true);
    }
  }
});

test('超长文字（1 MiB）也截断到上限之内', async () => {
  const 原 = '豫言日志'.repeat(90000);
  assert.ok(字节数(原) > 1024 * 1024);
  const {记} = await 捕获('/info', 原);
  const 出 = 只有一条(记, 'info');
  assert.ok(出.endsWith(标记));
  assert.ok(字节数(出) <= 上限);
  assert.ok(原.startsWith(出.slice(0, -标记.length)));
});

test('大输入：1、8、15 MiB 走宿主截取快路径，超过 16 MiB 的巨串在豫言内生成并走逐字节路径，结果一致', async () => {
  for (const [兆, 字] of [[1, 'x'], [8, '豫'], [15, '😀']]) {
    const 原 = 字.repeat(Math.ceil(兆 * 1048576 / Buffer.byteLength(字)));
    const 始 = performance.now();
    const {状态, 记} = await 捕获('/info', 原);
    const 出 = 只有一条(记, 'info');
    console.log(`    ${兆} MiB（${字}）耗时 ${(performance.now() - 始).toFixed(0)} ms`);
    assert.equal(状态, 200);
    assert.ok(出.endsWith(标记));
    assert.ok(字节数(出) <= 上限);
    assert.ok(上限 - 字节数(出) < Buffer.byteLength(字));
    assert.ok(!出.includes('\uFFFD'));
    assert.ok(原.startsWith(出.slice(0, -标记.length)));
  }
  // 巨串由应用内部生成（查询参数 kib 指定 KiB 数，字符为 a），不经宿主桥的 16 MiB 交换限制
  for (const 千字节 of [17 * 1024, 40 * 1024]) {
    const 始 = performance.now();
    const {状态, 体, 记} = await 捕获(`/info?kib=${千字节}`, '');
    console.log(`    ${千字节 / 1024} MiB 巨串耗时 ${(performance.now() - 始).toFixed(0)} ms，状态 ${状态} ${状态 === 200 ? '' : 体.slice(0, 80)}`);
    assert.equal(状态, 200, 体);
    const 出 = 只有一条(记, 'info');
    assert.equal(出, 'a'.repeat(上限 - 字节数(标记)) + 标记);
  }
});

test('组合字符序列也不在字符内部被切开成乱码', async () => {
  const 原 = 'é'.repeat(6000);
  const {记} = await 捕获('/info', 原);
  const 出 = 只有一条(记, 'info');
  assert.ok(出.endsWith(标记));
  assert.ok(字节数(出) <= 上限);
  assert.ok(!出.includes('�'));
  assert.ok(原.startsWith(出.slice(0, -标记.length)));
});

test('每次调用恰写一条，连续调用互不影响', async () => {
  const {记} = await 捕获('/error', '甲');
  只有一条(记, 'error', '甲');
  const 乙 = await 捕获('/warn', '乙');
  只有一条(乙.记, 'warn', '乙');
});

test('未知路径由应用报错，且不写日志', async () => {
  const {状态, 体, 记} = await 捕获('/debug', 'x');
  assert.equal(状态, 400);
  assert.match(体, /未知路径/);
  for (const 级 of 级别们) assert.equal(记[级].length, 0);
});
