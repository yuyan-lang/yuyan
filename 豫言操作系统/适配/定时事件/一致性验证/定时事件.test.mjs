// 文言：以真 Wasm 与模拟控制器验定时事件适配。汉语：用 Node 加载已构建的“定时事件一致性”产物，覆盖 cron/计划时刻读取、事件寿命、非定时事件与异常路径。
// 用法见同目录说明：在私有暂存根目录执行 `node --test <本文件>`，产物根目录由环境变量 YY_DIST_ROOT 指定（默认 ./dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const 产物 = pathToFileURL(path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '定时事件一致性') + '/');
const {创建云工宿主} = await import(new URL('宿主.mjs', 产物));
const 程序模块 = await WebAssembly.compile(await readFile(new URL('程序.wasm', 产物)));
const 值桥模块 = await WebAssembly.compile(await readFile(new URL('值桥.wasm', 产物)));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {KV: ['KV']}});

// 文言：平台外壳于启动之前核对应用之要求与宿主之所供；此测同其所核。汉语：与生成的 入口.mjs 启动时相同，用 接口核对.mjs 核对应用要求与宿主支持清单。
const {核对接口装载} = await import(new URL('接口核对.mjs', 产物));
const 应用要求 = JSON.parse(await readFile(new URL('接口要求组.json', 产物), 'utf8'));
const 宿主提供 = JSON.parse(await readFile(new URL('宿主提供组.json', 产物), 'utf8'));
test('装载前的接口核对通过，且应用要求含本接口', () => {
  核对接口装载({程序模块, 应用要求, 宿主提供, 宿主: '云工'});
  assert.ok(应用要求.some(项 => 项.接口名称 === '豫言操作系统定时事件' && 项.接口版本 === '0.1.0'), '应用要求里应有本接口 0.1.0');
  assert.ok(宿主提供.some(项 => 项.接口名称 === '豫言操作系统定时事件' && 项.接口版本 === '0.1.0'), '宿主支持清单里应有本接口 0.1.0');
});

// 文言：写入须待时而后成，用以显事之寿。汉语：put 故意延迟，若宿主不等程序结束，测试读到的键值会是空的。
class 模拟KV {
  constructor(延迟毫秒 = 40) { this.数据 = new Map(); this.延迟毫秒 = 延迟毫秒; this.完成时刻 = 0; }
  async put(键, 值) { await new Promise(完成 => setTimeout(完成, this.延迟毫秒)); this.数据.set(键, 值); this.完成时刻 = performance.now(); }
  async get(键) { return this.数据.get(键) ?? null; }
}
class 模拟控制器 {
  constructor(cron, scheduledTime) { this.cron = cron; this.scheduledTime = scheduledTime; this.重试禁用 = false; }
  noRetry() { this.重试禁用 = true; }
}
const 模拟上下文 = () => ({承诺: [], waitUntil(承诺) { this.承诺.push(承诺); }, passThroughOnException() {}});
const 定时 = async (cron, 时刻, KV = new 模拟KV()) => {
  const 上下文 = 模拟上下文();
  const 控制器 = new 模拟控制器(cron, 时刻);
  const 始 = performance.now();
  await 宿主.scheduled(控制器, {KV}, 上下文);
  return {KV, 上下文, 控制器, 结束: performance.now(), 始, 文: KV.数据.get('定时')};
};

test('读取 cron 与计划时刻，整数原样返回', async () => {
  const {文, 控制器} = await 定时('*/10 * * * *', 1758801600000);
  assert.equal(文, '{"cron":"*/10 * * * *","scheduledTime":1758801600000}');
  assert.deepEqual(JSON.parse(文), {cron: '*/10 * * * *', scheduledTime: 1758801600000});
  assert.equal(控制器.重试禁用, false, '适配不得调用 noRetry');
});

test('计划时刻边界与位数：0、Date 上限、安全整数上限', async () => {
  for (const 时刻 of [0, 1, 1758801600000, 8640000000000000, Number.MAX_SAFE_INTEGER]) {
    const {文} = await 定时('* * * * *', 时刻);
    assert.equal(文, `{"cron":"* * * * *","scheduledTime":${时刻}}`, String(时刻));
    assert.equal(JSON.parse(文).scheduledTime, 时刻);
  }
});

test('异常的计划时刻使读取失败，且是可捕获的豫言异常', async () => {
  for (const 坏 of [-1, 1.5, NaN, Infinity, 1e21, undefined, '007', 12345678901234567890]) {
    const {文} = await 定时('* * * * *', 坏);
    assert.match(文, /^错误：定时事件的计划时刻无效$/, String(坏));
  }
});

test('cron 内的引号、反斜线与中文经 JSON 无损往返', async () => {
  for (const cron of ['a"b\\c', '0 9 * * MON-FRI', '豫言 * * * *', '\\\\ "" \\"', '']) {
    const {文} = await 定时(cron, 1700000000000);
    assert.equal(JSON.parse(文).cron, cron, JSON.stringify(cron));
  }
});

test('事件寿命：调用待程序内的异步写入完成后才结束，且交给 waitUntil', async () => {
  const KV = new 模拟KV(80);
  const {上下文, 结束} = await 定时('*/10 * * * *', 1758801600000, KV);
  assert.ok(KV.数据.has('定时'), '定时调用返回时写入必须已完成');
  assert.ok(KV.完成时刻 <= 结束, '写入完成不得晚于调用结束');
  assert.equal(上下文.承诺.length, 1, '宿主应把程序运行交给 waitUntil');
  await 上下文.承诺[0];
});

test('程序在定时事件里抛出未捕获异常时调用失败', async () => {
  const 上下文 = 模拟上下文();
  const KV = new 模拟KV(5);
  await assert.rejects(宿主.scheduled(new 模拟控制器('boom', 1758801600000), {KV}, 上下文));
  assert.equal(KV.数据.get('定时'), '{"cron":"boom","scheduledTime":1758801600000}', '失败前的写入已完成');
  await 上下文.承诺[0];
});

test('普通请求里读取定时事件得到可捕获异常', async () => {
  const 回 = await 宿主.fetch(new Request('https://x.test/'), {KV: new 模拟KV()});
  assert.equal(回.status, 400);
  assert.equal(await 回.text(), '当前事件不是定时事件');
});

test('同一宿主连续处理多次定时事件互不干扰', async () => {
  const 结果 = await Promise.all([1, 2, 3, 4].map(序 => 定时(`${序} * * * *`, 1700000000000 + 序)));
  结果.forEach((项, 序) => assert.equal(项.文, `{"cron":"${序 + 1} * * * *","scheduledTime":${1700000000000 + 序 + 1}}`));
});
