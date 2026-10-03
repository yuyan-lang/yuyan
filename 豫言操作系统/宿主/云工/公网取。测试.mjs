// 文言：验公网请求专术（云工公网请求发起）所用之 fetch 窄口：许可含 https://* 乃许，验 https、无凭据，转址恒 manual；空许可、http、带凭据皆拒。
// 汉语：公网请求专用原语（云工公网请求发起）所用 fetch 窄口（宿主.mjs 的 造公网取）的单元测试：许可 OUTBOUND_ORIGINS 含 https://* 才放行，
//   网址须为 https、不带用户名或密码，redirect 一律改为 manual（网页上游规范：不跟随重定向，3xx 作为成功响应返回）；空许可、http、带凭据都拒绝。
// 运行：node --test 豫言操作系统/宿主/云工/公网取。测试.mjs（全树测试自动发现）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {造公网取} from './宿主.mjs';

// 文言：伪全局，其 fetch 录其所受。汉语：假的 全局：fetch 记下收到的网址与选项，返回固定响应；其余取真实全局。
const 造伪全局 = () => {
  const 记录 = [];
  const 全局 = Object.create(globalThis, {fetch: {value: async (网址, 选项) => { 记录.push({网址, 选项}); return new Response('好', {status: 200}); }}});
  return {记录, 全局};
};
const 公网许可 = {OUTBOUND_ORIGINS: ['https://*']};

test('许可含 https://* 时放行，redirect 改为 manual，其余选项照传', async () => {
  const {记录, 全局} = 造伪全局();
  const 取 = 造公网取({许可: 公网许可, 全局});
  const 回 = await 取('https://上游.例子/接口?甲=1', {method: 'POST', headers: {'Content-Type': 'application/json'}, body: '{}'});
  assert.equal(await 回.text(), '好');
  assert.equal(记录.length, 1);
  assert.equal(new URL(记录[0].网址).hostname, new URL('https://上游.例子/').hostname);
  assert.equal(记录[0].选项.redirect, 'manual');
  assert.equal(记录[0].选项.method, 'POST');
  assert.equal(记录[0].选项.body, '{}');
});

test('redirect 不是 manual 时一律改为 manual', async () => {
  const {记录, 全局} = 造伪全局();
  const 取 = 造公网取({许可: 公网许可, 全局});
  await 取('https://上游.例子/', {redirect: 'follow'});
  await 取('https://上游.例子/', {redirect: 'error'});
  await 取('https://上游.例子/');
  assert.deepEqual(记录.map(项 => 项.选项.redirect), ['manual', 'manual', 'manual']);
});

test('空许可与只列真实来源的许可都拒绝 fetch，不发请求', () => {
  const {记录, 全局} = 造伪全局();
  for (const 许可 of [{}, {OUTBOUND_ORIGINS: []}, {OUTBOUND_ORIGINS: ['https://上游.例子']}, {OUTBOUND_ORIGINS: 'https://*'}]) {
    assert.throws(() => 造公网取({许可, 全局})('https://上游.例子/'), /云工宿主不开放此全局：fetch/);
  }
  assert.equal(记录.length, 0);
});

test('http 与其他协议拒绝', () => {
  const {记录, 全局} = 造伪全局();
  const 取 = 造公网取({许可: 公网许可, 全局});
  assert.throws(() => 取('http://上游.例子/'), /只许 https/);
  assert.throws(() => 取('ftp://上游.例子/'), /只许 https/);
  assert.throws(() => 取('不是网址'), /网址无效/);
  assert.equal(记录.length, 0);
});

test('网址带用户名或密码拒绝', () => {
  const {记录, 全局} = 造伪全局();
  const 取 = 造公网取({许可: 公网许可, 全局});
  assert.throws(() => 取('https://甲:乙@上游.例子/'), /不得带用户名或密码/);
  assert.throws(() => 取('https://甲@上游.例子/'), /不得带用户名或密码/);
  assert.equal(记录.length, 0);
});

test('Request 对象按其网址核对，选项不是对象时拒绝', async () => {
  const {记录, 全局} = 造伪全局();
  const 取 = 造公网取({许可: 公网许可, 全局});
  await 取(new Request('https://上游.例子/甲'), {});
  assert.equal(记录[0].选项.redirect, 'manual');
  assert.throws(() => 取(new Request('http://上游.例子/甲')), /只许 https/);
  assert.throws(() => 取('https://上游.例子/', '不是对象'), /选项须为对象/);
});
