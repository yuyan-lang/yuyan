// 组合范例：用网页事件流与持久频道实现“AI 服务商连接”持久对象的缩影，真实 Wasm + Node。
// /connect 是出站长连接（事件流，带心跳，新连接替换旧连接）；/dispatch 与 /chunk 是另外的事件，
// 经持久频道把任务推给长连接、把服务商回传的分片转给正在读取的调用方。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {载入应用, 创建模拟D1, 睡, 等到, 流读取器, 请求} from '../工具.mjs';

const 新宿主 = await 载入应用();
const 活动表 = 'CREATE TABLE 活动请求(槽 INTEGER PRIMARY KEY CHECK(槽 = 1), id TEXT NOT NULL, 序号 INTEGER NOT NULL, 总量 INTEGER NOT NULL);';
const 编号 = 序 => `00000000-0000-4000-8000-${String(序).padStart(12, '0')}`;
async function 起() {
  const {sql, DB} = 创建模拟D1(活动表);
  const 宿主 = 新宿主({D1: ['DB']});
  const 状态 = {waitUntil() {}};
  const 调 = (路径, 选项) => 宿主.durableFetch(请求(路径, 选项), {DB}, 状态);
  const 发 = (路径, 体) => 调(路径, {method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify(体)});
  const 状态查询 = async () => (await 调('/status')).json();
  const 活动行数 = () => sql.prepare('SELECT COUNT(*) AS n FROM 活动请求').get().n;
  return {调, 发, 状态查询, 活动行数, sql, 关: () => sql.close()};
}
const 任务体 = (序, 附加 = {}) => ({id: 编号(序), protocol: 'openai', body: {model: 'demo', stream: true, messages: [{role: 'user', content: '你好，模型'}]}, ...附加});
const 连接 = async (环, 心跳 = 5000) => {
  const 回 = await 环.调(`/connect?心跳=${心跳}`);
  assert.equal(回.status, 200);
  return {回, 读: new 流读取器(回)};
};
// 读到下一条“data:”帧（跳过心跳注释），返回其 JSON
const 下一任务 = async (读, 超时 = 3000) => {
  for (;;) {
    const 帧 = await 读.下一帧(超时);
    if (帧 === null) throw new Error('流已结束');
    if (帧.startsWith('data: ')) return JSON.parse(帧.slice(6));
  }
};

test('缩影：离线状态与未连接时的派发', async () => {
  const 环 = await 起();
  try {
    assert.deepEqual(await 环.状态查询(), {online: false, active: 0});
    const 回 = await 环.发('/dispatch', 任务体(1));
    assert.equal(回.status, 503);
    assert.deepEqual(await 回.json(), {error: '服务商软件未连接'});
    assert.equal((await 环.调('/connect', {method: 'POST'})).status, 405);
    assert.equal((await 环.调('/dispatch')).status, 405);
    assert.equal((await 环.调('/nope')).status, 404);
  } finally { 环.关(); }
});

test('缩影：/connect 响应头、首帧 : connected 与 50 毫秒心跳 : ping', async () => {
  const 环 = await 起();
  try {
    const {回, 读} = await 连接(环, 50);
    assert.equal(回.headers.get('content-type'), 'text/event-stream; charset=utf-8');
    assert.equal(回.headers.get('cache-control'), 'no-store');
    assert.equal(回.headers.get('x-accel-buffering'), 'no');
    assert.equal(await 读.下一帧(), ': connected');
    const 起点 = Date.now();
    for (let i = 0; i < 4; i++) assert.equal(await 读.下一帧(1000), ': ping');
    const 用时 = Date.now() - 起点;
    assert.ok(用时 >= 120 && 用时 < 1000, `4 次心跳用时应约 200 毫秒，实得 ${用时}`);
    assert.deepEqual(await 环.状态查询(), {online: true, active: 0});
    await 读.取消();
    await 等到(async () => (await 环.状态查询()).online === false, {说明: '服务商断开后被察觉并清理'});
  } finally { 环.关(); }
});

test('缩影：端到端——任务经频道推给长连接，分片经 /chunk 转给调用方，完成后收尾', async () => {
  const 环 = await 起();
  try {
    const {读: 服务商} = await 连接(环);
    assert.equal(await 服务商.下一帧(), ': connected');
    const 派发 = await 环.发('/dispatch', 任务体(1));
    assert.equal(派发.status, 200);
    assert.equal(派发.headers.get('content-type'), 'text/event-stream; charset=utf-8');
    const 调用方 = new 流读取器(派发);
    assert.deepEqual(await 下一任务(服务商), {type: 'request', id: 编号(1), protocol: 'openai', body: 任务体(1).body});
    assert.deepEqual(await 环.状态查询(), {online: true, active: 1});
    const 分片 = (序, 数据, 附加 = {}) => 环.发('/chunk', {id: 编号(1), sequence: 序, data: 数据, ...附加});
    assert.deepEqual(await (await 分片(1, 'data: {"delta":"你"}\n\n')).json(), {ok: true});
    assert.deepEqual(await (await 分片(1, '重复')).json(), {ok: true, duplicate: true}, '序号不大于已收序号：重复分片被忽略');
    assert.deepEqual(await (await 分片(2, 'data: {"delta":"好"}\n\n')).json(), {ok: true});
    const 缺口 = await 分片(4, 'x');
    assert.equal(缺口.status, 409);
    assert.deepEqual(await 缺口.json(), {error: '分片顺序不符'});
    assert.equal((await 分片(0, 'x')).status, 400);
    const 过长 = await 分片(3, 'x'.repeat(32769));
    assert.equal(过长.status, 400);
    assert.deepEqual(await 过长.json(), {error: '分片内容无效'});
    assert.deepEqual(await (await 分片(3, 'data: [DONE]\n\n', {done: true})).json(), {ok: true});
    assert.equal(await 调用方.全文(), 'data: {"delta":"你"}\n\ndata: {"delta":"好"}\n\ndata: [DONE]\n\n');
    await 等到(async () => (await 环.状态查询()).active === 0, {说明: '请求收尾'});
    assert.equal(环.活动行数(), 0);
    const 晚 = await 分片(4, 'x');
    assert.equal(晚.status, 404);
    assert.deepEqual(await 晚.json(), {error: '请求已结束'});
  } finally { 环.关(); }
});

test('缩影：一个请求同一时刻只处理一个，另一个得 503', async () => {
  const 环 = await 起();
  try {
    const {读: 服务商} = await 连接(环);
    await 服务商.下一帧();
    const 甲 = new 流读取器(await 环.发('/dispatch', 任务体(1)));
    await 下一任务(服务商);
    const 乙 = await 环.发('/dispatch', 任务体(2));
    assert.equal(乙.status, 503);
    assert.deepEqual(await 乙.json(), {error: '服务商正处理另一请求'});
    await 环.发('/chunk', {id: 编号(1), sequence: 1, data: 'ok', done: true});
    assert.equal(await 甲.全文(), 'ok');
    await 等到(() => 环.活动行数() === 0, {说明: '收尾'});
    const 丙 = await 环.发('/dispatch', 任务体(3));
    assert.equal(丙.status, 200);
    await 下一任务(服务商);
    await 环.发('/chunk', {id: 编号(3), sequence: 1, data: '', done: true});
    assert.equal(await new 流读取器(丙).全文(), '');
  } finally { 环.关(); }
});

test('缩影：大请求被切成多条频道消息，长连接侧按序还原成完整的一帧', async () => {
  const 环 = await 起();
  try {
    const {读: 服务商} = await 连接(环);
    await 服务商.下一帧();
    const 大体 = 任务体(1, {body: {model: 'demo', 消息: '豫言abc'.repeat(12000)}});
    const 甲 = new 流读取器(await 环.发('/dispatch', 大体));
    const 任务 = await 下一任务(服务商);
    assert.deepEqual(任务, {type: 'request', id: 编号(1), protocol: 'openai', body: 大体.body});
    assert.ok(Buffer.byteLength(JSON.stringify(大体)) > 100000, '请求帧远超单条频道消息的 64 KiB');
    await 环.发('/chunk', {id: 编号(1), sequence: 1, data: '', done: true});
    assert.equal(await 甲.全文(), '');
    const 超大 = await 环.发('/dispatch', 任务体(2, {body: {model: 'demo', 消息: 'x'.repeat(262200)}}));
    assert.equal(超大.status, 413);
    assert.deepEqual(await 超大.json(), {error: '模型请求过长'});
  } finally { 环.关(); }
});

test('缩影：服务商报错——调用方的流以网络错误终止，请求收尾', async () => {
  const 环 = await 起();
  try {
    const {读: 服务商} = await 连接(环);
    await 服务商.下一帧();
    const 调用方 = new 流读取器(await 环.发('/dispatch', 任务体(1)));
    await 下一任务(服务商);
    await 环.发('/chunk', {id: 编号(1), sequence: 1, data: 'data: 部分\n\n'});
    assert.equal(await 调用方.下一帧(), 'data: 部分');
    await 环.发('/chunk', {id: 编号(1), sequence: 2, data: '', error: true});
    const 块 = await 调用方.下一块();
    assert.equal(块.类型, '错');
    assert.equal(块.错 instanceof Error ? 块.错.message : 块.错, '服务商执行失败');
    await 等到(() => 环.活动行数() === 0, {说明: '收尾'});
  } finally { 环.关(); }
});

test('缩影：调用方取消——服务商收到 cancel 帧，请求收尾，可再派发', async () => {
  const 环 = await 起();
  try {
    const {读: 服务商} = await 连接(环);
    await 服务商.下一帧();
    const 调用方 = new 流读取器(await 环.发('/dispatch', 任务体(1)));
    await 下一任务(服务商);
    await 环.发('/chunk', {id: 编号(1), sequence: 1, data: 'data: 甲\n\n'});
    assert.equal(await 调用方.下一帧(), 'data: 甲');
    await 调用方.取消('调用方走了');
    await 环.发('/chunk', {id: 编号(1), sequence: 2, data: 'data: 乙\n\n'});
    assert.deepEqual(await 下一任务(服务商), {type: 'cancel', id: 编号(1)});
    await 等到(() => 环.活动行数() === 0, {说明: '收尾'});
    const 再 = await 环.发('/dispatch', 任务体(2));
    assert.equal(再.status, 200);
    await 再.body.cancel();
  } finally { 环.关(); }
});

test('缩影：服务商长时间不回——调用方的流以超时错误终止，服务商不收 cancel', async () => {
  const 环 = await 起();
  try {
    const {读: 服务商} = await 连接(环, 60000);
    await 服务商.下一帧();
    const 调用方 = new 流读取器(await 环.发('/dispatch?超时=200', 任务体(1)));
    await 下一任务(服务商);
    const 起点 = Date.now();
    const 块 = await 调用方.下一块();
    assert.equal(块.类型, '错');
    assert.equal(块.错 instanceof Error ? 块.错.message : 块.错, '服务商响应超时');
    const 用时 = Date.now() - 起点;
    assert.ok(用时 >= 150 && 用时 < 1000, `用时 ${用时}`);
    await 等到(() => 环.活动行数() === 0, {说明: '收尾'});
    const 再 = await 服务商.下一块(300);
    assert.equal(再.类型, '超时', '服务商侧没有多余帧');
  } finally { 环.关(); }
});

test('缩影：新连接替换旧连接——旧连接正常结束，任务只发给新连接', async () => {
  const 环 = await 起();
  try {
    const {读: 甲} = await 连接(环, 5000);
    assert.equal(await 甲.下一帧(), ': connected');
    const {读: 乙} = await 连接(环, 5000);
    assert.equal(await 乙.下一帧(), ': connected');
    assert.equal(await 甲.下一帧(), null, '旧连接的流以正常结束收场');
    assert.deepEqual(await 环.状态查询(), {online: true, active: 0});
    const 调用方 = new 流读取器(await 环.发('/dispatch', 任务体(1)));
    assert.deepEqual(await 下一任务(乙), {type: 'request', id: 编号(1), protocol: 'openai', body: 任务体(1).body});
    await 环.发('/chunk', {id: 编号(1), sequence: 1, data: '完', done: true});
    assert.equal(await 调用方.全文(), '完');
    const {读: 丙} = await 连接(环, 5000);
    assert.equal(await 丙.下一帧(), ': connected');
    assert.equal(await 乙.下一帧(), null);
    assert.deepEqual(await 环.状态查询(), {online: true, active: 0});
  } finally { 环.关(); }
});

test('缩影：服务商断开后应用察觉并清理，随后派发得 503', async () => {
  const 环 = await 起();
  try {
    const {读} = await 连接(环, 40);
    await 读.下一帧();
    await 读.取消();
    await 等到(async () => (await 环.状态查询()).online === false, {说明: '断开被察觉'});
    const 回 = await 环.发('/dispatch', 任务体(1));
    assert.equal(回.status, 503);
    assert.deepEqual(await 回.json(), {error: '服务商软件未连接'});
  } finally { 环.关(); }
});

test('缩影：心跳与任务帧交错，长连接侧仍按序收到完整任务', async () => {
  const 环 = await 起();
  try {
    const {读: 服务商} = await 连接(环, 30);
    await 服务商.下一帧();
    const 甲 = new 流读取器(await 环.发('/dispatch', 任务体(1)));
    await 睡(100);
    const 任务 = await 下一任务(服务商);
    assert.equal(任务.id, 编号(1));
    for (let 序 = 1; 序 <= 20; 序++) await 环.发('/chunk', {id: 编号(1), sequence: 序, data: `分片${序};`, done: 序 === 20});
    assert.equal(await 甲.全文(), Array.from({length: 20}, (_, i) => `分片${i + 1};`).join(''));
  } finally { 环.关(); }
});
