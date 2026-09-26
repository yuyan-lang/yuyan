// 持久频道在真实 workerd（wrangler dev --local，同一个 Durable Object 实例上的多个并发 durable-fetch）中的复验，走真实 HTTP。
// 前提：已按 说明 的“在本地 workerd 里复跑”起好 持久频道一致性 应用，环境变量 远端地址 指向它。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {睡, 等到, 流读取器} from './云端复验工具.mjs';

import {基址} from './云端复验工具.mjs';
const 地址 = (径, 参 = {}) => 基址 + '/' + encodeURIComponent(径) + '?' + new URLSearchParams(参);
const 文 = async (径, 参) => (await fetch(地址(径, 参))).text();
const 流 = async (径, 参, 选项) => new 流读取器(await fetch(地址(径, 参), 选项));
let 序 = 0;
const 新名 = 前缀 => `${前缀}${Date.now()}x${序++}`;   // 每个测试用自己的频道名，互不干扰（同一持久对象实例）

test('云端：跨请求订阅与发布，多订阅者按序各得一份', async () => {
  const 名 = 新名('c');
  const 读们 = await Promise.all([1, 2, 3].map(() => 流('订阅转发', {名, 超时: 10000, 最多: 100})));
  assert.equal(await 文('订阅数', {名}), '3');
  assert.equal(await 文('发布多', {名, 前缀: 'm', 数: 100}), '300');
  for (const 读 of 读们) {
    const 帧们 = await 读.读完();
    assert.equal(帧们.length, 101);
    for (let i = 0; i < 100; i++) assert.equal(帧们[i], 'data: 消息:m' + i);
    assert.equal(帧们[100], 'data: 已满');
  }
  await 等到(async () => await 文('订阅数', {名}) === '0', {说明: '订阅取消'});
});

test('云端：超时精度与订阅数探测不影响等待', async () => {
  const 名 = 新名('t');
  const 结果 = (await 文('等待', {名, 超时: 300})).split('|');
  assert.equal(结果[0], '1');
  assert.ok(Number(结果[2]) >= 290 && Number(结果[2]) < 700, '用时 ' + 结果[2]);
  const 等 = 文('等待', {名, 超时: 600});
  await 睡(80);
  for (let i = 0; i < 5; i++) { await 文('订阅数', {名}); await 睡(30); }
  const 结果2 = (await 等).split('|');
  assert.equal(结果2[0], '1');
  assert.ok(Number(结果2[2]) >= 570 && Number(结果2[2]) < 1000, '探测不应改变超时长短，实得 ' + 结果2[2]);
});

test('云端：关闭——已排队的消息先交付，再得状态 2 与原因', async () => {
  const 名 = 新名('k');
  const 读 = await 流('订阅转发', {名, 超时: 10000, 最多: 0});
  assert.equal(await 文('发布多', {名, 前缀: 'm', 数: 3}), '3');
  assert.equal(await 文('关闭', {名, 因: '收摊'}), '1');
  assert.deepEqual(await 读.读完(), ['data: 消息:m0', 'data: 消息:m1', 'data: 消息:m2', 'data: 终:收摊']);
});

test('云端：文字规则——64 KiB 与空消息（零长块）可发，越界被拒', async () => {
  const 名 = 新名('z');
  const 读 = await 流('订阅转发', {名, 超时: 10000, 最多: 3});
  assert.equal(await 文('发布大', {名, 字: 'a', 长: 65536}), '1');
  assert.equal(await 文('发布大', {名, 字: '空', 长: 0}), '1');
  assert.equal(await 文('发布', {名, 文: '尾'}), '1');
  assert.equal(await 文('发布大', {名, 字: 'a', 长: 65537}), '错误:持久频道文字超过 65536 字节');
  assert.equal(await 文('发布大', {名, 字: '高', 长: 1}), '错误:持久频道文字不得以字节 0xFF 起首');
  const 帧们 = await 读.读完();
  assert.equal(帧们[0], 'data: 消息:' + 'a'.repeat(65536));
  assert.equal(帧们[1], 'data: 消息:');
  assert.equal(帧们[2], 'data: 消息:尾');
});

test('云端：订阅上限 256 与积压断开', async () => {
  assert.equal(await 文('订阅上限', {名: 新名('u'), 次数: 300}), '256|256|持久频道订阅数已达上限 256|0');
  const 积 = (await 文('积压', {名: 新名('b'), 数: 20, 长: 65536})).split('|');
  const 序列 = 积[0].split(',').map(Number);
  assert.ok(序列.indexOf(0) > 10 && 序列.indexOf(0) < 20, '前若干条送达后积压超限：' + 积[0]);
  assert.equal(积[1], '2');
  assert.equal(积[2], '订阅积压超过 1 MiB，已被断开');
});

test('云端：一个订阅内收发数千条消息不耗尽句柄', async () => {
  assert.equal(await 文('大量', {名: 新名('v'), 数: 3000}), '3000|3000');
});

test('云端：频道上限 64——第 65 个频道在真实持久对象里由宿主拒绝（请求失败）；订阅不随事件结束回收，会一直占用名额', async () => {
  // 用专属的持久对象实例（x-do 头），免得占满主实例的 64 个频道名额。
  const 专属 = {headers: {'x-do': 'limit' + Date.now()}};
  const 前缀 = 新名('q');
  assert.equal(await (await fetch(地址('频道上限', {前缀, 数: 64}), 专属)).text(), '64');
  const 回 = await fetch(地址('频道上限', {前缀: 前缀 + 'z', 数: 1}), 专属);
  console.log('  [观察] 已有 64 个（前一事件结束后仍未回收的）频道时，再订阅新频道：HTTP', 回.status);
  assert.notEqual(回.status, 200, '事件结束后订阅仍占着频道名额（宿主未自动回收）；订阅新频道由宿主拒绝');
});

test('云端：先订阅再回放——窗口内的事件恰好出现一次（真实 D1）', async () => {
  const 追加 = 内容 => 文('追加', {文: 内容});
  for (let i = 1; i <= 3; i++) await 追加('事件' + i);
  const 起 = Date.now();
  const 等流 = fetch(地址('事件流', {after: 0, 时限: 1200, 窗口: 400}));
  await 等到(async () => await 文('订阅数', {名: 'events'}) === '1', {说明: '订阅建立'});
  for (let i = 4; i <= 6; i++) await 追加('事件' + i);
  const 读 = new 流读取器(await 等流);
  await 睡(100);
  for (let i = 7; i <= 8; i++) await 追加('事件' + i);
  const 帧们 = await 读.读完();
  const 号们 = 帧们.filter(帧 => 帧.startsWith('id: ')).map(帧 => Number(帧.split('\n')[0].slice(4)));
  const 基 = 号们[0];
  assert.deepEqual(号们, Array.from({length: 号们.length}, (_, i) => 基 + i), '连续无重复：' + 号们);
  assert.ok(号们.length >= 8);
});

test('云端：客户端断开后应用仍被频道消息唤醒并收尾（断开在写入之后才被察觉）', async () => {
  const 名 = 新名('d');
  const 日志 = async 键 => JSON.parse(await 文('日志', {键})).map(行 => 行.值);
  const 基线 = (await 日志('订阅已取消')).length;
  const 控制 = new AbortController();
  const 读 = new 流读取器(await fetch(地址('订阅记', {名, 超时: 60000}), {signal: 控制.signal}));
  await 睡(200);
  控制.abort();
  let 次数 = 0;
  await 等到(async () => { 次数++; await 文('发布', {名, 文: 'm' + 次数}); await 睡(150); return (await 日志('订阅已取消')).length > 基线; }, {超时: 20000, 说明: '应用发现断开并取消订阅'});
  console.log(`  [观察] 客户端断开后又发布了 ${次数} 条消息，应用才发现断开并取消订阅（本地 workerd，HTTP/1.1）`);
  assert.equal(await 文('订阅数', {名}), '0');
});
