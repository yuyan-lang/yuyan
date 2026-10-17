// 文言：验宿主新工厂：SSE 解析器逐字节、网页能力之限额与背压管线；不涉 Wasm，只以 JSDOM 为页。
// 汉语：宿主单元测试：直接调用 创建SSE解析器 与 创建网页能力（真实的事件队列 + JSDOM 页面），覆盖不能经探针传递的大载荷限额、
//       字节级 SSE 解析、背压管线的放行顺序与清理。
import test from 'node:test';
import assert from 'node:assert/strict';
import {JSDOM, 宿主模块} from './夹具.mjs';

const {创建SSE解析器, 创建网页能力, 创建事件队列} = 宿主模块;
const 编码 = new TextEncoder();

// ---------------------------------------------------------------------------
// 一、SSE 解析器
// ---------------------------------------------------------------------------
const 喂全 = (解析器, 文, 分块 = 文.length) => {
  const 字节 = 编码.encode(文);
  const 事们 = [];
  let 重试 = null;
  let 致命 = null;
  for (let i = 0; i < 字节.length; i += 分块) {
    const 出 = 解析器.喂(字节.slice(i, i + 分块));
    事们.push(...出.事件们);
    if (出.重试 !== null) 重试 = 出.重试;
    致命 ??= 出.致命;
  }
  return {事们, 重试, 致命};
};

test('SSE 解析：id、event、多行 data、注释、无冒号字段、行首空格去一个', () => {
  const 解析器 = 创建SSE解析器();
  const {事们} = 喂全(解析器, ': 注释\nid: 1\ndata: 甲\n\nevent: x\ndata: 第一\ndata:第二\ndata\n\nid:2\ndata:  两空格\n\n');
  assert.deepEqual(事们, [
    {事件号: '1', 事件名: 'message', 数据: '甲'},
    {事件号: '1', 事件名: 'x', 数据: '第一\n第二\n'},
    {事件号: '2', 事件名: 'message', 数据: ' 两空格'}
  ]);
});

test('SSE 解析：LF、CR、CRLF 行结束符混用；CR 与 LF 被分块切开也只算一个行结束', () => {
  for (const 分块 of [1, 2, 3, 7, 1000]) {
    const 解析器 = 创建SSE解析器();
    const {事们} = 喂全(解析器, 'data: 甲\r\n\r\ndata: 乙\r\rdata: 丙\n\n', 分块);
    assert.deepEqual(事们.map(事 => 事.数据), ['甲', '乙', '丙'], `分块 ${分块}`);
  }
});

test('SSE 解析：多字节字符被切在块中间也不乱；流首 BOM 被忽略', () => {
  const 文 = '﻿data: 😀汉字\n\n';
  for (const 分块 of [1, 2, 3, 5]) {
    const 解析器 = 创建SSE解析器();
    const {事们} = 喂全(解析器, 文, 分块);
    assert.deepEqual(事们.map(事 => 事.数据), ['😀汉字'], `分块 ${分块}`);
  }
});

test('SSE 解析：空 data 行也派发（数据为空文字），没有 data 的事件不派发；id 含 NUL 被忽略', () => {
  const 解析器 = 创建SSE解析器();
  const {事们} = 喂全(解析器, 'data:\n\nevent: 空\n\nid: a\u0000b\ndata: 有\n\n');
  assert.deepEqual(事们, [
    {事件号: '', 事件名: 'message', 数据: ''},
    {事件号: '', 事件名: 'message', 数据: '有'}
  ]);
  assert.equal(解析器.最后号(), '');
});

test('SSE 解析：retry 只认十进制数字；末尾未以空行结束的事件被丢弃；重置连接保留末事件号', () => {
  const 解析器 = 创建SSE解析器({起始事件号: '9'});
  const 甲 = 喂全(解析器, 'retry: 250\ndata: 完整\n\nretry: abc\nid: 5\ndata: 未完');
  assert.equal(甲.重试, 250);
  assert.deepEqual(甲.事们.map(事 => 事.数据), ['完整']);
  assert.equal(甲.事们[0].事件号, '9', '起始事件号是首个事件的事件号');
  assert.equal(解析器.最后号(), '5', 'id 字段即使事件未完成也更新末事件号');
  解析器.重置连接();
  const 乙 = 喂全(解析器, 'data: 新\n\n');
  assert.deepEqual(乙.事们, [{事件号: '5', 事件名: 'message', 数据: '新'}]);
});

test('SSE 解析：单个事件的数据超过上限是致命错误', () => {
  const 解析器 = 创建SSE解析器({单事上限: 100});
  const {事们, 致命} = 喂全(解析器, 'data: ' + 'x'.repeat(200) + '\n\n');
  assert.equal(致命, '事件过大');
  assert.deepEqual(事们, []);
  const 二 = 创建SSE解析器({单事上限: 100});
  assert.equal(喂全(二, ('data: ' + 'y'.repeat(60) + '\n').repeat(3) + '\n').致命, '事件过大', '多行累计超限');
});

// ---------------------------------------------------------------------------
// 二、网页能力：大载荷限额与背压管线
// ---------------------------------------------------------------------------
const 造能力 = (选项 = {}) => {
  const 窗 = new JSDOM('<!doctype html><body></body>', {url: 'https://yuyan-lang.org/cloud/', pretendToBeVisual: true}).window;
  let 能力;
  const 队列 = 创建事件队列({离队钩子: 项 => 能力?.离队(项)});
  能力 = 创建网页能力({根: 窗.document, 全局: 窗, 网络: 选项.网络 ?? (async () => new Response('')), 路径: 窗.location.href, 储存: 窗.localStorage, 队列, 已关闭: () => 队列.已关闭(),
    定时: {造: () => '1', 取消: () => false}, ...选项.其余});
  return {窗, 能力, 队列};
};
const 取全部 = 队列 => {
  const 果 = [];
  for (;;) {
    const 项 = 队列.等待(() => true, 项 => 项, () => null, false);
    if (!项 || typeof 项.then === 'function') break;
    果.push(项);
  }
  return 果;
};

test('大载荷限额：储存值、请求 JSON、请求正文、编译内容都在宿主校验', () => {
  const {能力} = 造能力();
  assert.throws(() => 能力.运行('储存.写入', ['本地', 'k', 'v'.repeat(1048577)]), /1048576/);
  assert.equal(能力.运行('储存.写入', ['本地', 'k', 'v'.repeat(1048576)]), 'true');
  assert.throws(() => 能力.运行('请求.发起', ['x'.repeat(10 * 1024 * 1024 + 1)]), /10 MiB/);
  assert.throws(() => 能力.运行('请求.发起', [JSON.stringify({网址: '/x', 方法: 'POST', 正文: 'a'.repeat(8 * 1024 * 1024 + 1)})]), /8 MiB/);
  assert.throws(() => 能力.运行('编译.启动', [JSON.stringify({'a。豫': 'a'.repeat(20 * 1024 * 1024), 'b。豫': 'b'.repeat(13 * 1024 * 1024)})]), /32 MiB/);
  assert.throws(() => 能力.运行('不存在.操作', []), /不受支持/);
  assert.throws(() => 能力.运行('定时', []), /不受支持/);
  assert.throws(() => 能力.运行('请求.constructor', []), /不受支持/);
});

test('网页能力：关闭后不能再发请求、开事件源、启动编译；清理中止在途请求', async () => {
  let 已中止 = false;
  const {能力, 队列} = 造能力({网络: (址, 初) => new Promise((_, 拒) => { 初.signal.addEventListener('abort', () => { 已中止 = true; 拒(Error('中止')); }); })});
  能力.运行('请求.发起', [JSON.stringify({网址: '/挂', 超时毫秒: 600000})]);
  assert.equal(能力.状态().请求数, 1);
  队列.关闭();
  能力.清理();
  await new Promise(完成 => setTimeout(完成, 10));
  assert.equal(已中止, true);
  assert.equal(能力.状态().请求数, 0);
  assert.throws(() => 能力.运行('请求.发起', [JSON.stringify({网址: '/x'})]), /已关闭/);
  assert.throws(() => 能力.运行('事件源.打开', ['/e', '']), /已关闭/);
  assert.throws(() => 能力.运行('编译.启动', ['{"a。豫":"x"}']), /已关闭/);
});

test('背压管线：事件源在队列里的未取批达 8 个后暂缓，取走一个补一个，顺序不变', async () => {
  // 服务端一次性给出 100 个事件（每个事件独占一批需要超过 256 项或 1 MiB，故用 40 个 ~30KB 的事件凑出多批）
  const 大 = 'x'.repeat(30000);
  const 体 = Array.from({length: 100}, (_, i) => `id: ${i + 1}\ndata: ${大}\n\n`).join('');
  const 分块们 = [];
  for (let i = 0; i < 100; i += 10) 分块们.push(编码.encode(Array.from({length: 10}, (_, j) => `id: ${i + j + 1}\ndata: ${大}\n\n`).join('')));
  let 已读块 = 0;
  const 网络 = async () => new Response(new ReadableStream({
    pull(控制) { if (已读块 < 分块们.length) 控制.enqueue(分块们[已读块++]); else 控制.close(); }
  }, {highWaterMark: 0}), {status: 200, headers: {'Content-Type': 'text/event-stream'}});
  const {能力, 队列} = 造能力({网络});
  const 号 = Number(能力.运行('事件源.打开', ['/e', '']));
  await new Promise(完成 => setTimeout(完成, 100));
  const 积压 = 队列.状态().各类积压.事件流;
  assert.ok(积压 >= 8 && 积压 <= 12, `未取批应约 8（含打开事件），实际 ${积压}`);
  assert.ok(已读块 < 分块们.length, '暂缓读流：没有把全部十块读完');
  assert.equal(队列.状态().各类丢弃.事件流 ?? 0, 0);
  // 逐个取走，直到收齐 100 个事件
  const 见 = [];
  while (见.length < 100) {
    const 项 = await 队列.等待(类型 => 类型 === '事件流', 项 => 项, () => null, false);
    assert.equal(项.事件.订阅号, 号);
    if (项.事件.名称 !== '消息') continue;
    for (const 事 of 项.事件.详情.批) 见.push(Number(事.事件号));
  }
  assert.deepEqual(见, Array.from({length: 100}, (_, i) => i + 1));
  能力.清理();
});
