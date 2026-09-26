// 组合范例“服务商连接缩影”在真实 workerd（Durable Object）中的端到端复验，走真实 HTTP。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {睡, 等到, 流读取器} from '../云端复验工具.mjs';

import {基址} from '../云端复验工具.mjs';
const 头 = {'x-do': 'provider' + Date.now()};
const 取 = (径, 选项 = {}) => fetch(基址 + 径, {...选项, headers: {...头, ...(选项.headers ?? {})}});
const 发 = (径, 体) => 取(径, {method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify(体)});
const 状态 = async () => (await 取('/status')).json();
const 编号 = 序 => `00000000-0000-4000-8000-${String(序).padStart(12, '0')}`;
const 任务体 = 序 => ({id: 编号(序), protocol: 'openai', body: {model: 'demo', stream: true, messages: [{role: 'user', content: '你好，模型'}]}});
const 下一任务 = async (读, 总时限 = 8000) => {
  const 止 = Date.now() + 总时限;
  while (Date.now() < 止) { const 帧 = await 读.下一帧(2000); if (帧 === null) throw new Error('流已结束'); if (帧.startsWith('data: ')) return JSON.parse(帧.slice(6)); }
  throw new Error('等待任务帧超时');
};

test('云端缩影：连接首帧、心跳、端到端、替换、断开', async () => {
  assert.deepEqual(await 状态(), {online: false, active: 0});
  assert.equal((await 发('/dispatch', 任务体(1))).status, 503);

  // 长连接甲：首帧与心跳
  const 甲回 = await 取('/connect?心跳=100');
  assert.equal(甲回.headers.get('content-type'), 'text/event-stream; charset=utf-8');
  assert.equal(甲回.headers.get('x-accel-buffering'), 'no');
  const 甲 = new 流读取器(甲回);
  assert.equal(await 甲.下一帧(), ': connected');
  assert.equal(await 甲.下一帧(2000), ': ping');
  assert.equal(await 甲.下一帧(2000), ': ping');
  assert.deepEqual(await 状态(), {online: true, active: 0});

  // 端到端：派发→长连接收到任务→分片→完成
  const 调用方 = new 流读取器(await 发('/dispatch', 任务体(1)));
  assert.deepEqual(await 下一任务(甲), {type: 'request', id: 编号(1), protocol: 'openai', body: 任务体(1).body});
  for (let 序 = 1; 序 <= 5; 序++) assert.deepEqual(await (await 发('/chunk', {id: 编号(1), sequence: 序, data: `data: ${序}\n\n`, done: 序 === 5})).json(), {ok: true});
  assert.equal(await 调用方.全文(), 'data: 1\n\ndata: 2\n\ndata: 3\n\ndata: 4\n\ndata: 5\n\n');
  await 等到(async () => (await 状态()).active === 0, {说明: '请求收尾'});

  // 调用方取消 → 服务商收到 cancel
  const 调用方2 = new 流读取器(await 发('/dispatch', 任务体(2)));
  await 下一任务(甲);
  await 发('/chunk', {id: 编号(2), sequence: 1, data: 'data: x\n\n'});
  assert.equal(await 调用方2.下一帧(), 'data: x');
  await 调用方2.取消('走了');
  // 本地 workerd（HTTP/1.1）在连接断开之后，要再向调用方写入几次才会察觉断开；服务商实际会持续回传分片
  let 序 = 2;
  let 已收到取消 = false;
  const 发分片直到取消 = (async () => { while (序 < 30 && !已收到取消) { await 发('/chunk', {id: 编号(2), sequence: 序, data: 'data: y\n\n'}); 序++; await 睡(50); } })();
  const 取消帧 = await 下一任务(甲).finally(() => { 已收到取消 = true; });
  await 发分片直到取消;
  console.log(`  [观察] 调用方断开后，服务商又回传了 ${序 - 2} 个分片，应用才察觉并发出 cancel`);
  assert.deepEqual(取消帧, {type: 'cancel', id: 编号(2)});
  await 等到(async () => (await 状态()).active === 0, {说明: '取消后收尾'});

  // 新连接乙替换旧连接甲
  const 乙 = new 流读取器(await 取('/connect?心跳=100'));
  assert.equal(await 乙.下一帧(), ': connected');
  assert.equal(await 甲.下一帧(3000), null, '旧连接正常结束');
  assert.deepEqual(await 状态(), {online: true, active: 0});
  const 调用方3 = new 流读取器(await 发('/dispatch', 任务体(3)));
  assert.equal((await 下一任务(乙)).id, 编号(3));
  await 发('/chunk', {id: 编号(3), sequence: 1, data: '完', done: true});
  assert.equal(await 调用方3.全文(), '完');

  // 服务商断开：心跳写入终将发现断开，应用清理
  await 乙.取消();
  const 起 = Date.now();
  await 等到(async () => (await 状态()).online === false, {超时: 10000, 说明: '断开被察觉'});
  console.log(`  [观察] 服务商断开后约 ${Date.now() - 起} 毫秒应用察觉并清理（心跳 100 毫秒）`);
  assert.equal((await 发('/dispatch', 任务体(4))).status, 503);
});
