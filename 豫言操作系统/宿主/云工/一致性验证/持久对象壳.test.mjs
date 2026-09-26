// 文言：以真实豫言 Wasm 与持久对象模拟，验持久事务二版、持久告警、持久独占、事时之限与响应先返之义。
// 汉语：运行前先用私有暂存构建“持久对象壳一致性应用”（见同目录 说明.汉语.md），并设置环境变量
// 持久对象壳产物=<dist/持久对象壳一致性应用 的绝对路径>，再执行 node --test 持久对象壳.test.mjs。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {join} from 'node:path';
import {pathToFileURL} from 'node:url';
import {创建模拟持久对象} from './模拟持久对象.mjs';
import {创建模拟慢网络} from './模拟慢网络.mjs';

const 产物 = process.env.持久对象壳产物;
if (!产物) throw new Error('请设置环境变量 持久对象壳产物 为构建产物目录（dist/持久对象壳一致性应用）');
const {创建云工宿主} = await import(pathToFileURL(join(产物, '宿主.mjs')));
const 程序模块 = await WebAssembly.compile(await readFile(join(产物, '程序.wasm')));
const 值桥模块 = await WebAssembly.compile(await readFile(join(产物, '值桥.wasm')));
const 许可 = JSON.parse(await readFile(join(产物, '许可.json'), 'utf8'));

// 文言：每案自造宿主、对象与慢网，互不相染。汉语：每个用例新建宿主、模拟持久对象和模拟网络；配置直接传给宿主。
const 新对象 = ({配置, 初始} = {}) => {
  const 网络 = 创建模拟慢网络();
  const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可, 网络: 网络.fetch, 执行配置: 配置});
  const 对象 = 创建模拟持久对象({宿主, 初始});
  return {对象, 网络, 仓: 对象.storage, 状态: 对象.状态};
};
const 请求体 = 体 => new Request('https://do.test/', {method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify(体)});
const 发 = async (对象, 体) => { const 回 = await 对象.fetch(请求体(体)); assert.equal(回.status, 200); return 回.text(); };
const 睡 = 毫秒 => new Promise(完成 => setTimeout(完成, 毫秒));
const 预置 = async 对象 => { for (const [键, 值] of [['p:a', '1'], ['p:b', '2'], ['p:c', '3']]) assert.equal(await 发(对象, {op: 'put', k: 键, text: 值}), 'OK'); };
const 仓值 = 仓 => Object.fromEntries(仓.数据);

test('直接仓：读写删、null 与缺键、结构化存取', async () => {
  const {对象, 仓} = 新对象();
  assert.equal(await 发(对象, {op: 'kind'}), 'K|durable-fetch');
  assert.equal(await 发(对象, {op: 'get', k: '甲'}), 'M|');
  assert.equal(await 发(对象, {op: 'put', k: '甲', text: '{"n":1,"名":"乙","列":[true,null]}'}), 'OK');
  assert.deepEqual(仓.数据.get('甲'), {n: 1, 名: '乙', 列: [true, null]}, '以结构化值而非字符串存储');
  assert.equal(await 发(对象, {op: 'get', k: '甲'}), 'E|{"n":1,"名":"乙","列":[true,null]}');
  assert.equal(await 发(对象, {op: 'put', k: 'nul', text: 'null'}), 'OK');
  assert.equal(await 发(对象, {op: 'get', k: 'nul'}), 'E|null', '已存 JSON null 与缺键不混');
  assert.equal(await 发(对象, {op: 'del', k: '甲'}), 'D|1');
  assert.equal(await 发(对象, {op: 'del', k: '甲'}), 'D|0');
  assert.equal(仓.数据.has('甲'), false);
  // 与额度应用同式：旧 JS 对象直接可读
  await 仓.put('旧籍', {day: '2026-09-25', visitors: {a: {minute: 1}}});
  assert.equal(await 发(对象, {op: 'get', k: '旧籍'}), 'E|{"day":"2026-09-25","visitors":{"a":{"minute":1}}}');
});

test('直接仓：非法写入抛可捕获异常且不写仓', async () => {
  const {对象, 仓} = 新对象();
  assert.equal(await 发(对象, {op: 'put', k: 'k', text: '{'}), 'X|持久仓写入失败：持久值不是有效 JSON');
  // 请求体与响应体各限 2 MiB（网页入站/网页答复适配），故大值由应用内部自倍而成：2^21 字节加引号超过 2 MiB
  assert.equal(await 发(对象, {op: 'put-big', k: 'k', exp: 21}), 'X|持久仓写入失败：持久值超过 2 MiB');
  assert.equal(await 发(对象, {op: 'put', k: '', text: '1'}), 'X|持久键须为 1 至 2048 字节');
  assert.equal(await 发(对象, {op: 'put', k: 'k'.repeat(2049), text: '1'}), 'X|持久键须为 1 至 2048 字节');
  assert.equal(await 发(对象, {op: 'put', k: '汉'.repeat(683), text: '1'}), 'X|持久键须为 1 至 2048 字节', '键按 UTF-8 字节计');
  assert.equal(await 发(对象, {op: 'put', k: '汉'.repeat(682), text: '1'}), 'OK', '2046 字节可用');
  for (const 保留 of ['$句柄', '$未定义', '$大整数', '$数字']) {
    assert.equal(await 发(对象, {op: 'put', k: 'k', text: JSON.stringify({外: [{[保留]: '1'}]})}), 'X|持久仓写入失败：持久值含宿主保留键 ' + 保留);
  }
  assert.equal(await 发(对象, {op: 'put', k: 'k', text: JSON.stringify({$句柄: 1, 另: 2})}), 'OK', '含多键的对象不是保留形');
  assert.equal(仓.数据.size, 2, '仅 2046 字节键与含多键对象两次写入成功');
  assert.equal(await 发(对象, {op: 'get', k: '汉'.repeat(683)}), 'X|持久键须为 1 至 2048 字节');
  assert.equal(await 发(对象, {op: 'del', k: ''}), 'X|持久键须为 1 至 2048 字节');
  // 1 MiB 的值可往返（桥上限 16 MiB，值上限 2 MiB）；请求体接近 2 MiB 的写入也可通过
  assert.equal(await 发(对象, {op: 'put-big', k: '大', exp: 20}), 'OK');
  assert.equal(await 发(对象, {op: 'getlen', k: '大'}), 'N|' + (2 ** 20 + 2));
  assert.equal(await 发(对象, {op: 'get', k: '大'}), 'E|"' + 'y'.repeat(2 ** 20) + '"');
  const 近二兆 = 'q'.repeat(2 * 1024 * 1024 - 200);
  assert.equal(await 发(对象, {op: 'put', k: '近', text: JSON.stringify(近二兆)}), 'OK');
});

test('直接仓：嵌套 32 层以内可写可读，33 层被拒绝', async () => {
  const {对象, 仓} = 新对象();
  const 嵌 = 层 => '['.repeat(层) + '1' + ']'.repeat(层);
  assert.equal(await 发(对象, {op: 'put', k: '三十二', text: 嵌(32)}), 'OK');
  assert.equal(await 发(对象, {op: 'get', k: '三十二'}), 'E|' + 嵌(32), '写入成功的值必可读回');
  assert.equal(await 发(对象, {op: 'put', k: '三十三', text: 嵌(33)}), 'X|持久仓写入失败：持久值嵌套超过 32 层');
  const 对象嵌 = 层 => '{"a":'.repeat(层) + '1' + '}'.repeat(层);
  assert.equal(await 发(对象, {op: 'put', k: '对象三十二', text: 对象嵌(32)}), 'OK');
  assert.equal(await 发(对象, {op: 'put', k: '对象三十三', text: 对象嵌(33)}), 'X|持久仓写入失败：持久值嵌套超过 32 层');
  assert.equal(仓.数据.has('三十三'), false);
});

test('直接仓：列举选项、空串视同未指定、非法选项失败', async () => {
  const {对象} = 新对象();
  await 预置(对象);
  await 发(对象, {op: 'put', k: 'q:z', text: '"外"'});
  assert.equal(await 发(对象, {op: 'list', opts: '{"prefix":"p:"}'}), 'L|[["p:a",1],["p:b",2],["p:c",3]]');
  assert.equal(await 发(对象, {op: 'list', opts: '{"prefix":"p:","limit":2,"reverse":true}'}), 'L|[["p:c",3],["p:b",2]]');
  assert.equal(await 发(对象, {op: 'list', opts: '{"prefix":"","start":"","startAfter":"p:a","end":"","limit":9,"reverse":false}'}), 'L|[["p:b",2],["p:c",3],["q:z","外"]]');
  assert.equal(await 发(对象, {op: 'list', opts: '{"start":"p:b","end":"p:c"}'}), 'L|[["p:b",2]]');
  assert.equal(await 发(对象, {op: 'list', opts: '{"startAfter":"p:a","prefix":"p:"}'}), 'L|[["p:b",2],["p:c",3]]');
  assert.equal(await 发(对象, {op: 'list', opts: '{}'}), 'L|[["p:a",1],["p:b",2],["p:c",3],["q:z","外"]]');
  for (const [选项, 因] of [
    ['{"foo":1}', '列举选项含未知字段：foo'],
    ['{"prefix":1}', '列举选项 prefix 须为字符串'],
    ['{"limit":0}', '列举选项 limit 须为不小于 1 的整数'],
    ['{"limit":1.5}', '列举选项 limit 须为不小于 1 的整数'],
    ['{"limit":"3"}', '列举选项 limit 须为不小于 1 的整数'],
    ['{"reverse":1}', '列举选项 reverse 须为爻'],
    ['{"start":"a","startAfter":"b"}', '列举选项不能同时指定 start 与 startAfter'],
    ['[]', '列举选项须为 JSON 对象'],
    ['不是JSON', '列举选项不是有效 JSON']
  ]) assert.equal(await 发(对象, {op: 'list', opts: 选项}), 'X|持久列举失败：' + 因, 选项);
});

test('直接仓：列举结果超过 8 MiB 时失败并提示分页', async () => {
  const {对象, 仓} = 新对象();
  for (let 序 = 0; 序 < 6; 序++) await 仓.put('大:' + 序, 'z'.repeat(1500 * 1024));
  assert.equal((await 发(对象, {op: 'list', opts: '{"prefix":"大:","limit":1}'})).length, 2 + 2 + 5 + 1 + (1500 * 1024 + 2) + 2, '一条 1.5 MiB 可列');
  assert.equal(await 发(对象, {op: 'list', opts: '{"prefix":"大:"}'}), 'X|持久列举失败：列举结果超过 8 MiB，请用 limit 分页');
});

test('直接仓：清空持久仓清 KV 与告警', async () => {
  const {对象, 仓} = 新对象();
  await 预置(对象);
  assert.equal(await 发(对象, {op: 'alarm-set', t: 1900000000000}), 'OK');
  assert.equal(await 发(对象, {op: 'clear'}), 'OK');
  assert.equal(仓.数据.size, 0);
  assert.equal(await 发(对象, {op: 'list', opts: '{}'}), 'L|[]');
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|0|0', 'SQLite 后端 deleteAll 连告警一并清除（云端待验：依兼容日期）');
});

test('持久事务：事务内读写删列、提交后仓中可见', async () => {
  const {对象, 仓} = 新对象();
  await 预置(对象);
  assert.equal(await 发(对象, {op: 'tx', args: '{"name":"basic"}'}), 'T|[["p:b",2],["p:c",3],["p:d",4]]', '事务内见本事务未提交之写');
  assert.deepEqual(仓值(仓), {'p:b': 2, 'p:c': 3, 'p:d': 4});
});

test('持久事务：回滚持久事务弃本事务诸写', async () => {
  const {对象, 仓} = 新对象();
  await 预置(对象);
  assert.equal(await 发(对象, {op: 'tx', args: '{"name":"rollback"}'}), 'T|[["p:a",1],["p:c",3],["p:e",5]]');
  assert.deepEqual(仓值(仓), {'p:a': 1, 'p:b': 2, 'p:c': 3});
});

test('持久事务：处理入口异常、缺结果都使执行持久事务抛可捕获异常且不提交', async () => {
  const {对象, 仓} = 新对象();
  await 预置(对象);
  assert.match(await 发(对象, {op: 'tx', args: '{"name":"throw"}'}), /^X\|持久事务失败：/);
  assert.equal(仓.数据.has('x'), false);
  assert.equal(await 发(对象, {op: 'tx', args: '{"name":"no-result"}'}), 'X|持久事务失败：豫言事务回调未供结果');
  assert.deepEqual(仓值(仓), {'p:a': 1, 'p:b': 2, 'p:c': 3});
});

test('持久事务：事务内列举选项与错误', async () => {
  const {对象} = 新对象();
  await 预置(对象);
  assert.equal(await 发(对象, {op: 'tx', args: JSON.stringify({name: 'list', opts: '{"prefix":"p:","limit":1}'})}), 'T|"[[\\"p:a\\",1]]"');
  assert.equal(await 发(对象, {op: 'tx', args: JSON.stringify({name: 'list', opts: '{"prefix":1}'})}), 'T|"X|持久列举失败：列举选项 prefix 须为字符串"');
});

test('持久事务：事务函数只在事务入口内可用，直接仓函数在事务入口内失败', async () => {
  const {对象} = 新对象();
  for (const 名 of ['put', 'get', 'delete', 'list', 'rollback', 'params', 'finish']) {
    assert.equal(await 发(对象, {op: 'tx-outside', which: 名}), 'X|当前事件没有持久事务', 名);
  }
  assert.equal(await 发(对象, {op: 'tx', args: '{"name":"misuse"}'}), 'T|"X|持久事务处理入口内须用持久事务函数，不可直接访问持久仓"');
});

test('持久告警：读、设、替换、删除与非法时刻', async () => {
  const {对象} = 新对象();
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|0|0');
  assert.equal(await 发(对象, {op: 'alarm-set', t: 1900000000000}), 'OK');
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|1|1900000000000');
  assert.equal(await 发(对象, {op: 'alarm-set', t: 1900000005000}), 'OK');
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|1|1900000005000', '单个对象仅一个告警，设置即替换');
  assert.equal(await 发(对象, {op: 'alarm-set', t: 0}), 'OK');
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|1|0');
  assert.equal(await 发(对象, {op: 'alarm-set', t: 8640000000000000}), 'OK');
  assert.equal(await 发(对象, {op: 'alarm-del'}), 'OK');
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|0|0');
  assert.equal(await 发(对象, {op: 'alarm-del'}), 'OK', '无告警时删除无事');
  for (const 时 of [-1, 8640000000000001]) assert.equal(await 发(对象, {op: 'alarm-set', t: 时}), 'X|持久告警时刻须在 0 至 8640000000000000 之间');
  assert.equal(await 发(对象, {op: 'retry'}), 'R|0', '非告警事件重试数恒零');
});

test('持久告警：事务内的告警变更与本事务同命运（本地 workerd 4.129.0 实测；云端待验）', async () => {
  const {对象, 仓} = 新对象();
  // 回滚：事务内设置的告警一并撤销
  assert.equal(await 发(对象, {op: 'tx', args: JSON.stringify({name: 'alarm', t: 1900000000123})}), 'T|"rolled"');
  assert.equal(仓.数据.has('z'), false, '事务写入已回滚');
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|0|0', '回滚使事务内设置的告警一并撤销');
  // 提交：事务内设置的告警生效
  assert.equal(await 发(对象, {op: 'tx', args: JSON.stringify({name: 'alarm-commit', t: 1900000000456})}), 'T|"committed"');
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|1|1900000000456');
  // 事务内删除告警后回滚：告警恢复
  assert.equal(await 发(对象, {op: 'tx', args: '{"name":"alarm-del-rollback"}'}), 'T|"rolled"');
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|1|1900000000456');
  // 事务处理入口抛错：同样撤销
  assert.match(await 发(对象, {op: 'tx', args: '{"name":"throw"}'}), /^X\|持久事务失败：/);
  assert.equal(await 发(对象, {op: 'alarm-get'}), 'A|1|1900000000456');
});

test('持久事务：事务回调运行期间，其他事件排队等候（同独占区）', async () => {
  const {对象, 网络} = 新对象();
  const 事务 = 发(对象, {op: 'tx', args: JSON.stringify({name: 'wait', base: 'https://slow.test', id: '务', ms: 150})});
  await 睡(40);
  const 普通 = 发(对象, {op: 'kind'});
  assert.equal(await 事务, 'T|"waited"');
  assert.equal(await 普通, 'K|durable-fetch');
  const 终 = 网络.区间('务')[0][1];
  const 日志 = 对象.事件日志.filter(项 => 项.种类 === 'fetch');
  assert.equal(日志[1].到达 < 终, true);
  assert.equal(日志[1].投递 >= 终 - 1, true, '事务回调结束后才投递到达于事务期间的事件');
});

test('持久事务：回滚之后再用本事务，平台报错使执行持久事务失败', async () => {
  const {对象, 仓} = 新对象();
  assert.equal(await 发(对象, {op: 'tx', args: '{"name":"after-rollback"}'}), 'T|"X|持久事务写入失败：Cannot put() on rolled back transaction"');
  assert.equal(仓.数据.size, 0);
});

test('持久告警：告警事件的重试数、处理中告警视为未设、失败后重试', async () => {
  const {对象, 仓} = 新对象();
  await 仓.put('alarm-mode', 'fail');
  await 发(对象, {op: 'alarm-set', t: Date.now() + 1000});
  await assert.rejects(对象.触发告警({重试数: 0}), /告警故意失败|RuntimeError/);
  assert.notEqual(await 仓.getAlarm(), null, '处理失败后平台保留告警以待重试');
  await assert.rejects(对象.触发告警({重试数: 1}), /告警故意失败|RuntimeError/);
  await 仓.put('alarm-mode', 'record');
  await 对象.触发告警({重试数: 2});
  assert.equal(await 仓.getAlarm(), null, '成功且未重设则不再有告警');
  assert.equal(await 仓.get('alarm-log'), '0:0;1:0;2:0;', '重试数 0、1、2；处理开始时告警视为已清除');
});

test('持久告警：告警内重设与删除', async () => {
  const {对象, 仓} = 新对象();
  await 仓.put('alarm-mode', 'rearm');
  await 仓.put('alarm-next', 1900000009999);
  await 仓.setAlarm(Date.now() + 5);
  await 对象.触发告警();
  assert.equal(await 仓.getAlarm(), 1900000009999);
  await 仓.put('alarm-mode', 'delete');
  await 仓.setAlarm(Date.now() + 5);
  await 对象.触发告警();
  assert.equal(await 仓.getAlarm(), null);
});

test('持久告警：告警处理入口内可开独占区与事务', async () => {
  const {对象, 仓} = 新对象();
  await 预置(对象);
  await 仓.put('alarm-mode', 'block');
  await 仓.setAlarm(Date.now() + 5);
  await 对象.触发告警();
  assert.equal(await 仓.get('alarm-block'), '{"read":"in-block"}|[["p:b",2],["p:c",3],["p:d",4]]');
  assert.equal(对象.状态.重置次数, 0);
});

test('非持久对象事件：告警、仓、独占函数抛可捕获异常，重试数为零', async () => {
  const 网络 = 创建模拟慢网络();
  const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可, 网络: 网络.fetch});
  const 发W = async 体 => (await 宿主.fetch(请求体(体), {})).text();
  assert.equal(await 发W({op: 'kind'}), 'K|fetch');
  assert.equal(await 发W({op: 'alarm-get'}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'alarm-set', t: 1}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'alarm-del'}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'retry'}), 'R|0');
  assert.equal(await 发W({op: 'get', k: 'a'}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'put', k: 'a', text: '1'}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'del', k: 'a'}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'list', opts: '{}'}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'clear'}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'block', name: 'echo', args: '{}'}), 'X|当前事件不属于持久对象');
  assert.equal(await 发W({op: 'tx', args: '{"name":"basic"}'}), 'X|持久事务失败：仅持久对象事件可启动事务');
});

test('持久独占：名称与 JSON 参数往返、仓与事务可在区内使用', async () => {
  const {对象, 仓} = 新对象();
  await 预置(对象);
  assert.equal(await 发(对象, {op: 'block', name: 'echo', args: '{"a": [1, 2],"中":"文"}'}), 'B|{"name":"echo","args":{"a": [1, 2],"中":"文"}}', '参数文字原样传入');
  assert.equal(await 发(对象, {op: 'block', name: 'kind', args: '{}'}), 'B|"durable-block"');
  assert.equal(await 发(对象, {op: 'block', name: 'store', args: '{}'}), 'B|{"read":"in-block"}');
  assert.equal(仓.数据.get('blk'), 'in-block');
  assert.equal(仓.数据.has('blk-n'), false);
  assert.equal(await 发(对象, {op: 'block', name: 'tx-inside', args: '{}'}), 'B|[["p:b",2],["p:c",3],["p:d",4]]', '独占区内可启动事务');
  assert.equal(对象.状态.重置次数, 0);
});

test('持久独占：区内失败、缺结果、重复结果、坏结果都不重置对象', async () => {
  const {对象, 仓, 状态} = 新对象();
  assert.match(await 发(对象, {op: 'block', name: 'fail', args: '{}'}), /^X\|持久独占失败：RuntimeError: illegal cast（多为豫言处理入口抛出了未捕获的异常/);
  assert.equal(await 发(对象, {op: 'block', name: 'no-result', args: '{}'}), 'X|持久独占失败：豫言独占区未供结果');
  assert.equal(await 发(对象, {op: 'block', name: 'bad-result', args: '{}'}), 'B|"X|持久独占结果无效：独占区结果不是有效 JSON"');
  assert.equal(await 发(对象, {op: 'block', name: 'double-result', args: '{}'}), 'B|1');
  assert.equal(仓.数据.get('double-msg'), 'X|持久独占结果无效：独占区结果只能设置一次');
  assert.equal(await 发(对象, {op: 'block', name: 'nested', args: '{}'}), 'B|"X|独占区内不可再启动独占区"');
  assert.equal(状态.重置次数, 0, '区内失败不抛给平台回调，故对象不被重置');
  assert.equal(await 发(对象, {op: 'kind'}), 'K|durable-fetch', '失败后对象仍正常服务');
});

test('持久独占：入参校验与限额', async () => {
  const {对象} = 新对象();
  assert.equal(await 发(对象, {op: 'block', name: 'echo', args: '不是JSON'}), 'X|持久独占失败：独占区参数不是有效 JSON');
  assert.equal(await 发(对象, {op: 'block', name: '', args: '{}'}), 'X|持久独占失败：独占区操作名须为 1 至 128 字节');
  assert.equal(await 发(对象, {op: 'block', name: '名'.repeat(43), args: '{}'}), 'X|持久独占失败：独占区操作名须为 1 至 128 字节', '129 字节');
  assert.doesNotMatch(await 发(对象, {op: 'block', name: '名'.repeat(42), args: '{}'}), /操作名须为/, '126 字节的名字通过宿主校验（处理入口不认识它，另行失败）');
  assert.match(await 发(对象, {op: 'block', name: '名'.repeat(42), args: '{}'}), /^X\|持久独占失败：RuntimeError: illegal cast（多为豫言处理入口抛出了未捕获的异常/);
});

test('持久独占：大参数与大结果（请求体 2 MiB 上限之外，由应用内部自造）', async () => {
  const {对象} = 新对象();
  // 4 MiB 参数：宿主校验 8 MiB、桥交换 16 MiB，均可通过；echo 结果比参数多 23 字节
  const 参长 = 6 + 2 ** 22 + 2;
  assert.equal(await 发(对象, {op: 'block-big', name: 'echo', exp: 22}), 'B|' + (22 + 参长 + 1));
  assert.equal(await 发(对象, {op: 'block-big', name: 'echo', exp: 21}), 'B|' + (22 + 6 + 2 ** 21 + 2 + 1));
  // 8 MiB 加封装超过参数上限
  assert.equal(await 发(对象, {op: 'block-big', name: 'echo', exp: 23}), 'X|持久独占失败：独占区参数超过 8 MiB');
  // 结果：4 MiB 可通；2^23 字节加引号超过 8 MiB，完成持久独占失败但可捕获
  assert.equal(await 发(对象, {op: 'block-len', name: 'big-result', args: '{"exp":22}'}), 'B|' + (2 ** 22 + 2));
  assert.equal(await 发(对象, {op: 'block', name: 'big-result', args: '{"exp":23}'}), 'B|"X|持久独占结果无效：独占区结果超过 8 MiB"');
});

test('持久事务：4 MiB 的事务参数可往返（豫言 JSON 规范化对大参数不构成瓶颈）', async () => {
  const {对象} = 新对象();
  const 始 = performance.now();
  assert.equal(await 发(对象, {op: 'tx-big', exp: 22}), 'T|11');
  assert.equal(performance.now() - 始 < 5000, true, '4 MiB 事务参数应在数秒内完成');
});

test('值桥：宿主到豫言的字符串可达 16 MiB，超过则中止本次事件', async () => {
  const {对象, 仓} = 新对象();
  await 仓.put('k', 'z'.repeat(4 * 1024 * 1024));
  assert.equal(await 发(对象, {op: 'getlen', k: 'k'}), 'N|' + (4 * 1024 * 1024 + 2), '4 MiB 值可读入豫言（超过 2 MiB 响应体上限的只是网页答复适配）');
  await 仓.put('k', 'z'.repeat(15 * 1024 * 1024));
  assert.equal(await 发(对象, {op: 'getlen', k: 'k'}), 'N|' + (15 * 1024 * 1024 + 2));
  await 仓.put('k', 'z'.repeat(17 * 1024 * 1024));
  await assert.rejects(对象.fetch(请求体({op: 'getlen', k: 'k'})), /宿主交换数据超过上限/);
});

test('值桥与宿主：请求体经 Request.text() 可读至 15 MiB（网页入站的 2 MiB 是适配自设之限，不是宿主或桥之限）', async () => {
  const {对象} = 新对象();
  const 读体 = (字节数, 头 = {}) => 对象.fetch(new Request('https://do.test/', {method: 'POST', headers: {'x-op': 'body-len', ...头}, body: 'b'.repeat(字节数)}));
  assert.equal(await (await 读体(5 * 1024 * 1024)).text(), 'N|' + 5 * 1024 * 1024, '站点发布请求（含 4 MiB base64）可读');
  assert.equal(await (await 读体(15 * 1024 * 1024)).text(), 'N|' + 15 * 1024 * 1024);
  await assert.rejects(读体(17 * 1024 * 1024), /宿主交换数据超过上限/);
  // 对照：走网页入站的 JSON 正文读取，超过 2 MiB 则由适配报错（豫言异常未捕获，出壳为非法转型）
  const 回 = 对象.fetch(请求体({op: 'put', k: 'k', text: JSON.stringify('y'.repeat(2 * 1024 * 1024 + 100))}));
  await assert.rejects(回, /illegal cast/);
});

test('持久独占：区内等待外部输入输出时，其他事件排队而不交错', async () => {
  const {对象, 网络, 仓} = 新对象();
  // 对照：不在独占区内的读—等—写，并发者交错而丢更新
  const 直行 = await Promise.all([1, 2, 3, 4, 5].map(序 => 发(对象, {op: 'counter-plain', ms: 30, id: '直' + 序})));
  assert.equal((await 仓.get('n')) < 5, true, '非独占的并发计数丢失更新：' + 直行.join(','));
  assert.equal(网络.相交('直1', '直2'), true, '非独占请求的外部等待区间交错');
  await 仓.delete('n');
  网络.清空();
  // 独占：五个并发请求，各进独占区做读—等—写
  const 独占 = await Promise.all([1, 2, 3, 4, 5].map(序 => 发(对象, {op: 'block', name: 'counter', args: JSON.stringify({ms: 30, id: '占' + 序})})));
  assert.equal(await 仓.get('n'), 5, '独占区内不丢更新');
  assert.deepEqual(独占.map(文 => Number(JSON.parse(文.slice(2)).n)).sort(), [1, 2, 3, 4, 5]);
  for (let 甲 = 1; 甲 <= 5; 甲++) for (let 乙 = 甲 + 1; 乙 <= 5; 乙++) assert.equal(网络.相交('占' + 甲, '占' + 乙), false, `独占区 ${甲} 与 ${乙} 不得交错`);
  assert.equal(对象.状态.重置次数, 0);
});

test('持久独占：独占期间到达的普通事件被闸挡住，区毕才投递', async () => {
  const {对象, 网络} = 新对象();
  const 独占 = 发(对象, {op: 'block', name: 'wait', args: JSON.stringify({ms: 120, id: '占'})});
  await 睡(30);
  const 普通 = 发(对象, {op: 'kind'});
  assert.equal(await 普通, 'K|durable-fetch');
  await 独占;
  const 占终 = 网络.区间('占')[0][1];
  const 日志 = 对象.事件日志.filter(项 => 项.种类 === 'fetch');
  assert.equal(日志.length, 2);
  assert.equal(日志[1].投递 >= 占终 - 1, true, '普通事件在独占区结束之后才被投递');
  assert.equal(日志[1].到达 < 占终, true, '它在独占区进行中到达');
});

test('事时之限：配置解析与各事件种类的时限选择', async () => {
  const {解析执行配置, 选事件时限, 默认事件时限毫秒, 事件时限上限毫秒, 独占类事件默认时限毫秒, 独占类事件时限上限毫秒} = await import(pathToFileURL(join(产物, '宿主.mjs')));
  assert.equal(默认事件时限毫秒, 30000);
  assert.equal(事件时限上限毫秒, 900000);
  assert.equal(独占类事件默认时限毫秒, 25000);
  assert.equal(独占类事件时限上限毫秒, 30000);
  assert.equal(解析执行配置(null).size, 0);
  assert.equal(解析执行配置({}).size, 0);
  assert.deepEqual([...解析执行配置({事件时限毫秒: {默认: 1000, 'durable-alarm': 900000}})], [['默认', 1000], ['durable-alarm', 900000]]);
  const 选 = (配置, 种类) => 选事件时限(解析执行配置(配置), 种类);
  assert.equal(选(null, 'durable-fetch'), 30000);
  assert.equal(选(null, 'durable-alarm'), 30000);
  assert.equal(选(null, 'durable-block'), 25000, '独占区默认 25 秒，低于平台 30 秒重置线');
  assert.equal(选(null, 'durable-transaction'), 25000);
  assert.equal(选({事件时限毫秒: {默认: 200}}, 'durable-alarm'), 200, '默认项适用于未列种类');
  assert.equal(选({事件时限毫秒: {默认: 200}}, 'durable-block'), 25000, '默认项不适用于独占类事件');
  assert.equal(选({事件时限毫秒: {默认: 200, 'durable-alarm': 900000}}, 'durable-alarm'), 900000);
  assert.equal(选({事件时限毫秒: {'durable-block': 30000}}, 'durable-block'), 30000);
  for (const [配置, 因] of [
    ['字符串', /须为对象/], [[], /须为对象/], [{别的: 1}, /未知字段：别的/], [{事件时限毫秒: []}, /事件时限毫秒须为对象/],
    [{事件时限毫秒: {'durable-alrm': 1000}}, /未知事件种类：durable-alrm/],
    [{事件时限毫秒: {默认: 0}}, /1 至 900000/], [{事件时限毫秒: {默认: 900001}}, /1 至 900000/],
    [{事件时限毫秒: {默认: 1.5}}, /1 至 900000/], [{事件时限毫秒: {默认: '1000'}}, /1 至 900000/],
    [{事件时限毫秒: {'durable-block': 30001}}, /durable-block 时限不得超过 30000/],
    [{事件时限毫秒: {'durable-transaction': 60000}}, /durable-transaction 时限不得超过 30000/]
  ]) assert.throws(() => 解析执行配置(配置), 因);
  assert.throws(() => 创建云工宿主({程序模块, 值桥模块, 许可, 执行配置: {事件时限毫秒: {'durable-alrm': 1}}}), /未知事件种类/, '宿主创建即验配置');
});

test('事时之限：构建器把应用目录的执行配置书入产物 动态资源.mjs 的导出，且许可不受污染', async () => {
  const 资源 = await import(pathToFileURL(join(产物, '动态资源.mjs')));
  const 配置 = 资源.执行配置;
  assert.deepEqual(配置, {事件时限毫秒: {默认: 30000, 'durable-alarm': 900000, 'durable-block': 30000}});
  assert.doesNotThrow(() => 创建云工宿主({程序模块, 值桥模块, 许可, 执行配置: 配置}));
  assert.equal(JSON.stringify(Object.keys(许可)), '["OUTBOUND_ORIGINS"]', '许可.json 结构未被污染');
  const 入口 = await readFile(join(产物, '入口.mjs'), 'utf8');
  assert.match(入口, /构建资源\.执行配置 \?\? null/, '入口壳以命名空间导入读取配置，旧器产物无此导出时守默认');
});

test('事时之限：告警时限可远大于默认，超限则明确失败', async () => {
  // 时限 250 毫秒：三次 100 毫秒之等待，第三次前必超限
  {
    const {对象, 仓} = 新对象({配置: {事件时限毫秒: {'durable-alarm': 250}}});
    await 仓.put('alarm-mode', 'long'); await 仓.put('alarm-n', 5); await 仓.put('alarm-ms', 100);
    await 仓.setAlarm(Date.now() + 5);
    await assert.rejects(对象.触发告警(), /豫言执行超过时限/);
    assert.equal(仓.数据.has('alarm-done'), false);
    assert.notEqual(await 仓.getAlarm(), null, '超限失败令平台重试');
    assert.equal(await 仓.get('alarm-log'), '0:0;', '告警入口已运行至超限');
  }
  // 时限 5000 毫秒：同一任务成功
  {
    const {对象, 仓} = 新对象({配置: {事件时限毫秒: {'durable-alarm': 5000}}});
    await 仓.put('alarm-mode', 'long'); await 仓.put('alarm-n', 3); await 仓.put('alarm-ms', 100);
    await 仓.setAlarm(Date.now() + 5);
    await 对象.触发告警();
    assert.equal(await 仓.get('alarm-done'), 1);
  }
});

test('事时之限：各事件种类分别配置，未列者用默认', async () => {
  // durable-fetch 200 毫秒而告警未列（走默认 30 秒）
  {
    const {对象, 仓} = 新对象({配置: {事件时限毫秒: {'durable-fetch': 200}}});
    await assert.rejects(对象.fetch(请求体({op: 'sleep', n: 4, ms: 100})), /豫言执行超过时限/);
    await 仓.put('alarm-mode', 'long'); await 仓.put('alarm-n', 4); await 仓.put('alarm-ms', 100); await 仓.setAlarm(Date.now() + 5);
    await 对象.触发告警();
    assert.equal(await 仓.get('alarm-done'), 1, '告警不受 durable-fetch 配置影响');
  }
  // 默认 200 毫秒，仅告警放宽
  {
    const {对象, 仓} = 新对象({配置: {事件时限毫秒: {默认: 200, 'durable-alarm': 5000}}});
    await assert.rejects(对象.fetch(请求体({op: 'sleep', n: 4, ms: 100})), /豫言执行超过时限/);
    await 仓.put('alarm-mode', 'long'); await 仓.put('alarm-n', 4); await 仓.put('alarm-ms', 100); await 仓.setAlarm(Date.now() + 5);
    await 对象.触发告警();
    assert.equal(await 仓.get('alarm-done'), 1);
  }
  // 无配置：durable-fetch 默认 30 秒，长等待成功
  {
    const {对象} = 新对象();
    assert.equal(await 发(对象, {op: 'sleep', n: 4, ms: 100}), 'S|done');
  }
});

test('事时之限：独占区有自己的时限，区内超限只失败该区', async () => {
  const {对象, 状态} = 新对象({配置: {事件时限毫秒: {'durable-block': 250}}});
  assert.equal(await 发(对象, {op: 'block', name: 'long', args: '{"n":6,"ms":100}'}), 'X|持久独占失败：豫言执行超过时限');
  assert.equal(await 发(对象, {op: 'block', name: 'long', args: '{"n":1,"ms":10}'}), 'B|{"done":true}');
  assert.equal(状态.重置次数, 0);
  // 外层长事件（durable-fetch 默认 30 秒）里再开区，各计各的时限
  assert.equal(await 发(对象, {op: 'sleep', n: 3, ms: 100}), 'S|done');
});

test('durableFetch：响应先返，Wasm 继续运行，ctx.waitUntil 记录其存续', async () => {
  const {对象, 仓, 状态} = 新对象();
  const 始 = performance.now();
  const 回 = await 对象.fetch(请求体({op: 'early', ms: 150}));
  const 耗 = performance.now() - 始;
  assert.equal(await 回.text(), 'early');
  assert.equal(耗 < 120, true, `响应应在等待外部之前返回，实耗 ${耗} 毫秒`);
  assert.equal(仓.数据.has('after'), false, '响应返回时后续写入尚未发生');
  assert.equal(状态.保活.length >= 1, true, '宿主把运行承诺交给 ctx.waitUntil');
  await 状态.等待保活();
  assert.equal(仓.数据.get('after'), 'done', '保活承诺完成后后续写入已落库');
});

test('durableFetch：响应之后 Wasm 失败只写日志，不影响已交付的响应', async () => {
  const {对象, 状态} = 新对象();
  const 日志 = [];
  const 原 = console.error;
  console.error = (...参) => 日志.push(参.join(' '));
  try {
    const 回 = await 对象.fetch(请求体({op: 'early-fail', ms: 30}));
    assert.equal(回.status, 200);
    assert.equal(await 回.text(), 'early');
    await 状态.等待保活();
  } finally { console.error = 原; }
  assert.equal(日志.some(文 => 文.includes('[豫言] 响应已交付后运行失败')), true, 日志.join('|'));
});

test('durableFetch：响应之前失败则 fetch 拒绝', async () => {
  const {对象} = 新对象();
  await assert.rejects(对象.fetch(请求体({op: 'boom'})), /illegal cast|RuntimeError|Error/);
});
