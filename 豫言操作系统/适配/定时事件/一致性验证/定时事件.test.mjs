// 文言：以真 Wasm 与模拟控制器验定时事件适配。汉语：用 Node 加载已构建的“定时事件一致性”产物，经类型化入口 处理定时事件 覆盖 cron/计划时刻读取、事件寿命、异常路径与薄宿主的单实例。
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
// 文言：计程序模之实例数，以验一宿主一实例。汉语：数程序模块被实例化的次数，用来验证薄宿主每个宿主只建一个实例（宿主每次调用时才查 WebAssembly.Instance，故可在此包一层）。
const 原实例 = WebAssembly.Instance;
let 程序实例数 = 0;
WebAssembly.Instance = new Proxy(原实例, {construct(目标, 参数, 新目标) { if (参数[0] === 程序模块) 程序实例数++; return Reflect.construct(目标, 参数, 新目标); }});
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {KV: ['KV']}});

// 文言：平台外壳于启动之前核对应用之要求与宿主之所供；此测同其所核。汉语：与生成的 入口.mjs 启动时相同，用 接口核对.mjs 核对应用要求与宿主支持清单。
const {核对接口装载} = await import(new URL('接口核对.mjs', 产物));
const 应用要求 = JSON.parse(await readFile(new URL('接口要求组.json', 产物), 'utf8'));
const 宿主提供 = JSON.parse(await readFile(new URL('宿主提供组.json', 产物), 'utf8'));
test('装载前的接口核对通过，且应用要求含本接口', () => {
  核对接口装载({程序模块, 应用要求, 宿主提供, 宿主: '云工'});
  assert.ok(应用要求.some(项 => 项.接口名称 === '豫言操作系统定时事件' && 项.接口版本 === '0.2.0'), '应用要求里应有本接口 0.2.0');
  assert.ok(宿主提供.some(项 => 项.接口名称 === '豫言操作系统定时事件' && 项.接口版本 === '0.2.0'), '宿主支持清单里应有本接口 0.2.0');
  assert.ok(WebAssembly.Module.exports(程序模块).some(项 => 项.name === '豫言操作系统定时事件/处理定时事件'), '程序应导出入口 处理定时事件');
});

// 文言：写入须待时而后成，用以显事之寿。汉语：put 故意延迟，若宿主不等程序结束，测试读到的键值会是空的。
class 模拟KV {
  constructor(延迟毫秒 = 40) { this.数据 = new Map(); this.写入 = []; this.延迟毫秒 = 延迟毫秒; this.完成时刻 = 0; }
  async put(键, 值) { await new Promise(完成 => setTimeout(完成, this.延迟毫秒)); this.数据.set(键, 值); this.写入.push(值); this.完成时刻 = performance.now(); }
  async get(键) { return this.数据.get(键) ?? null; }
}
class 模拟控制器 {
  constructor(cron, scheduledTime) { this.cron = cron; this.scheduledTime = scheduledTime; this.重试禁用 = false; }
  noRetry() { this.重试禁用 = true; }
}
const 模拟上下文 = () => ({承诺: [], waitUntil(承诺) { this.承诺.push(承诺); }, passThroughOnException() {}});
const 定时 = async (cron, 时刻, KV = new 模拟KV(), 用宿主 = 宿主) => {
  const 上下文 = 模拟上下文();
  const 控制器 = new 模拟控制器(cron, 时刻);
  const 始 = performance.now();
  await 用宿主.scheduled(控制器, {KV}, 上下文);
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

test('cron 不是字符串时读取失败，且是可捕获的豫言异常', async () => {
  for (const 坏 of [undefined, 5, null]) {
    const {文} = await 定时(坏, 1700000000000);
    assert.equal(文, '错误：定时事件的 cron 不是字符串', String(坏));
  }
});
test('事件寿命：调用待程序内的异步写入完成后才结束', async () => {
  const KV = new 模拟KV(80);
  const {结束} = await 定时('*/10 * * * *', 1758801600000, KV);
  assert.ok(KV.数据.has('定时'), '定时调用返回时写入必须已完成');
  assert.ok(KV.完成时刻 <= 结束, '写入完成不得晚于调用结束');
});

test('程序在定时事件里抛出未捕获异常时调用失败', async () => {
  const 上下文 = 模拟上下文();
  const KV = new 模拟KV(5);
  await assert.rejects(宿主.scheduled(new 模拟控制器('boom', 1758801600000), {KV}, 上下文));
  assert.equal(KV.数据.get('定时'), '{"cron":"boom","scheduledTime":1758801600000}', '失败前的写入已完成');
});

test('程序没有对应入口的事件按失败处理（提案 C0001），不静默当作成功', async () => {
  await assert.rejects(宿主.fetch(new Request('https://x.test/'), {KV: new 模拟KV()}), /程序没有导出入口 豫言操作系统网页服务\/处理入站网页请求/);
  const 批次 = {queue: 'q', messages: [], ackAll() {}, retryAll() {}};
  await assert.rejects(宿主.queue(批次, {}, 模拟上下文()), /程序没有导出入口 豫言操作系统消息队列\/处理队列批次/);
});

// 文言：平台之 env 一隔离体恒为一物，故重叠之事共一 KV，各记其所写。
// 汉语：平台的 env 在同一隔离体里是同一个对象（薄宿主只记最近一次事件的 env），所以重叠的事件共用一个 KV，逐次记下各自写入的值。
test('同一宿主重叠处理多次定时事件互不干扰', async () => {
  const KV = new 模拟KV();
  await Promise.all([1, 2, 3, 4].map(序 => 定时(`${序} * * * *`, 1700000000000 + 序, KV)));
  assert.deepEqual([...KV.写入].sort(), [1, 2, 3, 4].map(序 => `{"cron":"${序} * * * *","scheduledTime":${1700000000000 + 序}}`).sort());
});

// 文言：薄宿主一宿主一实例，重叠之事交错于其中；入口抛出则弃之，后事另建。
// 汉语：薄宿主每个宿主只有一个实例，重叠的事件在同一实例里交错执行；入口把异常抛出 Wasm 时作废这个实例，后来的事件另建。
test('薄宿主：顺序与重叠事件共用一个实例，入口失败后另建实例', async () => {
  const 新宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {KV: ['KV']}});
  const 起 = 程序实例数;
  for (let 序 = 0; 序 < 5; 序++) assert.equal((await 定时('* * * * *', 序, new 模拟KV(1), 新宿主)).文, `{"cron":"* * * * *","scheduledTime":${序}}`);
  assert.equal(程序实例数 - 起, 1, '五次顺序事件只建一个实例');
  const 共KV = new 模拟KV(30);
  await Promise.all([1, 2, 3].map(序 => 定时('* * * * *', 序, 共KV, 新宿主)));
  assert.deepEqual([...共KV.写入].sort(), [1, 2, 3].map(序 => `{"cron":"* * * * *","scheduledTime":${序}}`));
  assert.equal(程序实例数 - 起, 1, '三个重叠事件在同一实例里交错执行');
  await assert.rejects(定时('boom', 1, new 模拟KV(1), 新宿主));
  const 败后 = 程序实例数;
  const 后KV = new 模拟KV(30);
  await Promise.all([1, 2, 3].map(序 => 定时('* * * * *', 序, 后KV, 新宿主)));
  assert.equal(后KV.写入.length, 3);
  assert.equal(程序实例数, 败后 + 1, '失败的实例被作废，后来的事件另建一个实例');
});
