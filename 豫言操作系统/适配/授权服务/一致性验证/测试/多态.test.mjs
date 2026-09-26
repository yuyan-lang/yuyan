import {test, after} from 'node:test';
import assert from 'node:assert/strict';
import {编, 解, 睡, 造流, 服务桩, 跑, 造宿主} from './桩.mjs';

// 保活：让期限计时器（AbortSignal.timeout 的定时器不占事件循环）在等待期间不致令进程提前退出。
const 保活 = setInterval(() => {}, 1000);
after(() => clearInterval(保活));
const 行为 = (路径, 请求, 桩, 记录) => {
  const 观察 = (桩.观察[路径] = {});
  switch (路径) {
    case '/small': return new Response('ok', {status: 200});
    case '/seqA': return new Response(造流(['甲1', '甲2', '甲3'], {间隔: 3, 观察}), {status: 200});
    case '/seqB': return new Response(造流(['乙1', '乙2'], {间隔: 5, 观察}), {status: 200});
    case '/storm': return new Response(造流(Array.from({length: 6000}, (_, i) => String(i % 10).repeat(4)), {间隔: 0, 观察}), {status: 200});
    default: return new Response('未知', {status: 404});
  }
};
const 网络 = async (url) => new Response(造流(['data: x\n\n'], {观察: {}}), {status: 200});

test('句柄不竭：同一事件内连续 1200 次发请求并读完，临时宿主句柄被及时释放', async () => {
  const s = new 服务桩(行为);
  const 果 = await 跑({op: 'multi', mode: 'many', n: 1200}, {SVC: s}, 造宿主({时限毫秒: 120000}));
  assert.equal(果.状态, 200, 果.文);
  assert.equal(果.文, '成功=1200');
  assert.equal(s.请求们.length, 1200);
});

test('大量小块：6000 个小块经有限读取读完（每块的字节句柄都被释放）', async () => {
  const s = new 服务桩(行为);
  const 果 = await 跑({op: 'svc', url: 'https://svc/storm', read: 'limited', limit: 100000, timeout: 60000}, {SVC: s}, 造宿主({时限毫秒: 120000}));
  assert.equal(果.状态, 200, 果.文.slice(0, 100));
  assert.match(果.文, /‖有限状态0:字节24000:/);
});

test('多个响应柄的读取状态互不干扰（交错读取）', async () => {
  const s = new 服务桩(行为);
  const 果 = await 跑({op: 'multi', mode: 'interleave'}, {SVC: s});
  assert.equal(果.状态, 200, 果.文);
  assert.match(果.文, /^〔甲1\/乙1〕〔甲2\/乙2〕.*甲终1乙终1$/, 果.文);
  const 全 = [...果.文.matchAll(/〔([^〕]*)〕/g)].map(m => m[1]);
  assert.equal(全.map(x => x.split('/')[0]).join(''), '甲1甲2甲3');
});

test('已开始增量读取的柄：旧的整体读取与原流转发抛可捕获的事故，不崩溃', async () => {
  const s = new 服务桩(行为);
  const 果 = await 跑({op: 'multi', mode: 'guard'}, {SVC: s}, 造宿主({网络}));
  assert.equal(果.状态, 200, 果.文);
  const 段们 = 果.文.split('‖');
  assert.match(段们[0], /授权服务响应正文已被增量读取，不可再整体读取/);
  assert.match(段们[1], /授权服务响应正文已被增量读取，不可再作原答/);
  assert.match(段们[2], /网页上游响应正文已被增量读取，不可再整体读取/);
  assert.match(段们[3], /网页上游响应正文已被增量读取，不可再作流答/);
});

test('读取函数的参数校验：块上限 1..65536、字节上限 1..8388608、标头名须为令牌', async () => {
  const s = new 服务桩(行为);
  const 果 = await 跑({op: 'multi', mode: 'argcheck'}, {SVC: s});
  assert.equal(果.状态, 200, 果.文);
  const 段们 = 果.文.split('‖');
  assert.match(段们[0], /授权服务响应块上限须在 1 至 65536/);
  assert.match(段们[1], /授权服务响应块上限须在 1 至 65536/);
  assert.match(段们[2], /授权服务响应字节上限须在 1 至 8388608/);
  assert.match(段们[3], /授权服务响应字节上限须在 1 至 8388608/);
  assert.match(段们[4], /授权服务响应标头名无效/);
  assert.match(段们[5], /授权服务响应标头名无效/);
});

test('读取授权服务癸象正文文：2xx 而正文不是 JSON 时抛可捕获的事故，事件不失败（路由代理修订）', async () => {
  const s = new 服务桩(() => new Response('<html>不是 JSON</html>', {status: 200, headers: {'content-type': 'text/html'}}));
  const 果 = await 跑({op: 'multi', mode: 'badjson'}, {SVC: s}, 造宿主({网络}));
  assert.equal(果.状态, 200, 果.文);
  assert.match(果.文, /授权服务 JSON 正文无效：SyntaxError: /);
});
