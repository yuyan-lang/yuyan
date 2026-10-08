// 汉语：只对预编的豫言原生服务和测试产物验收消息转发。文言：惟验预编豫言原生服务与测试产物之消息转递。
import assert from 'node:assert/strict';
import {创建原生子程序桥} from './子程序.mjs';

const [服务路径, 子路径, 忙路径] = process.argv.slice(2);
assert.ok(服务路径 && 子路径, '须传原生服务与测试产物路径');
const 桥 = 创建原生子程序桥({服务路径, 程序: new Map([['验收', {入口: 子路径}], ...(忙路径 ? [['忙', {入口: 忙路径}]] : [])])});
try {
  assert.deepEqual(await 桥.启动('未授', [], new Uint8Array(), []), [1, -1]);
  const 输入 = new TextEncoder().encode('中文输入\n');
  const [码, 柄] = await 桥.启动('验收', ['子'], 输入, []);
  assert.equal(码, 0);
  let 果;
  do { 果 = await 桥.收取(柄); } while (果[0] === 0);
  assert.equal(果[0], 1);
  assert.equal(果[1], 7);
  assert.deepEqual(果[2], 输入);
  assert.equal(new TextDecoder().decode(果[3]), '错误流验收\n');
  assert.equal((await 桥.收取(柄))[0], -8);
  assert.equal(await 桥.终止(柄), 8);
  if (忙路径) {
    const [启码, 忙柄] = await 桥.启动('忙', ['子'], new Uint8Array(), []);
    assert.equal(启码, 0);
    assert.equal((await 桥.收取(忙柄))[0], 0);
    assert.equal(await 桥.终止(忙柄), 0);
    let 终果;
    do { 终果 = await 桥.收取(忙柄); } while (终果[0] === 0);
    assert.equal(终果[0], 1);
    assert.notEqual(终果[1], 0);
    assert.equal((await 桥.收取(忙柄))[0], -8);
    console.log('节点转发运行中状态、终止及终止后收取验收通过');
  }
  console.log('节点转发复用原生授权、输入输出与句柄释放验收通过');
} finally { 桥.关闭(); }

const {创建能力, 带型实现, 能力清理} = await import('./应用宿主.mjs');
const 能力 = 创建能力({
  程序路径: 子路径, 应用参数: [],
  授权: {目录: new Map(), 子程序: new Map([['验收', {入口: 子路径}]]), 环境: new Set(), 源: new Set()}
});
try {
  const 实 = 能力[带型实现].诺节宿主;
  assert.equal(实.诺节启动子程序.异步, true);
  const 编码 = 文 => new TextEncoder().encode(文);
  const [码, 柄] = await 实.诺节启动子程序(编码('验收'), [编码('子')], 编码('中文输入\n'), []);
  assert.equal(码, 0);
  let 果;
  do { 果 = await 实.诺节收取子程序(BigInt(柄)); } while (果[0] === 0);
  assert.equal(果[0], 1);
  assert.equal(果[1], 7);
  assert.deepEqual(果[2], 编码('中文输入\n'));
  assert.equal((await 实.诺节收取子程序(BigInt(柄)))[0], -8);
  console.log('正式节点宿主带型能力转发原生服务验收通过');
} finally { await 能力[能力清理](); }
