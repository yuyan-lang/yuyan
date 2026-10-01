// 文言：边界胶水之单测：签名之解、边界段之读、标量之转、无实之桩、异步之断、导出之包。
// 汉语：宿主边界胶水 边界.mjs 的单元测试：签名解析与报错、「豫言边界」段的读取、标量参数与结果的转换、
//   缺实现时的桩、没标异步却返回 Promise 的报错、应用接口导出的读取与包装。嵌在元组里的值要经值桥，由黄金样例 边界回环 在 Node 工具宿主上端到端覆盖。
// 运行：node --test 豫言操作系统/宿主/网页汇编/边界。测试.mjs
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {解析签名, 读边界段, 是豫言模块, 造边界导入, 读边界导出, 造边界导出, 启动导出名} from './边界.mjs';

const 编码器 = new TextEncoder();
const 无号 = 数 => { const 字节 = []; do { let 字 = 数 & 127; 数 >>>= 7; if (数) 字 |= 128; 字节.push(字); } while (数); return 字节; };
const 名 = 文 => { const 字节 = [...编码器.encode(文)]; return [...无号(字节.length), ...字节]; };
const 段 = (号, 体) => [号, ...无号(体.length), ...体];

// 文言：手造一模：导入若干函数，附边界段。汉语：手工拼一个模块：类型段、导入段（各导入一个函数），再附「豫言边界」段。
function 造模块(导入们, 段文) {
  const 型们 = 导入们.map(({参, 果}) => [0x60, ...无号(参.length), ...参, ...无号(果.length), ...果]);
  const 型段 = 段(1, [...无号(型们.length), ...型们.flat()]);
  const 导段 = 段(2, [...无号(导入们.length), ...导入们.flatMap(({模, 字}, 序) => [...名(模), ...名(字), 0, ...无号(序)])]);
  const 自段 = 段(0, [...名('豫言边界'), ...编码器.encode(段文)]);
  return new WebAssembly.Module(new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0, ...型段, ...导段, ...自段]));
}

const i64 = 0x7e, f64 = 0x7c, i32 = 0x7f;

test('解析签名：各种形与嵌套', () => {
  const 签 = 解析签名('串，列〔组〔整，变〔串｜〕〕〕→组〔爻，列〔整〕〕');
  assert.deepEqual(签.参, [{种: '串'}, {种: '列', 元素: {种: '组', 项: [{种: '整'}, {种: '变', 支: [[{种: '串'}], []]}]}}]);
  assert.deepEqual(签.果, {种: '组', 项: [{种: '爻'}, {种: '列', 元素: {种: '整'}}]});
  assert.deepEqual(解析签名('→元').参, []);
  assert.deepEqual(解析签名('组〔〕→变〔｜整，小｜〕').果, {种: '变', 支: [[], [{种: '整'}, {种: '小'}], []]});
  assert.deepEqual(解析签名('资→资').参, [{种: '资'}]);
});

test('解析签名：不合文法时报错', () => {
  assert.throws(() => 解析签名('整'), /期待“→”/);
  assert.throws(() => 解析签名('甲→元'), /未知的形/);
  assert.throws(() => 解析签名('→元元'), /多余文字/);
  assert.throws(() => 解析签名('列〔整→元'), /期待“〕”/);
});

test('读边界段：按“模块⇥字段”取签名；没有这一段时为空', () => {
  const 模块 = 造模块([{模: '测试', 字: '加', 参: [i64, i64], 果: [i64]}], '导入\t测试\t加\t整，整→整\n未知\t行\n');
  const 表 = 读边界段(模块);
  assert.equal(表.size, 1);
  assert.equal(表.get('测试\t加').文, '整，整→整');
  assert.equal(是豫言模块(模块), true);
  const 空 = new WebAssembly.Module(new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0]));
  assert.equal(读边界段(空), null);
  assert.equal(是豫言模块(空), false);
});

test('造边界导入：标量直传，爻转布尔，结果按形转换', () => {
  const 模块 = 造模块([
    {模: '测试', 字: '加', 参: [i64, i64], 果: [i64]},
    {模: '测试', 字: '半', 参: [f64], 果: [f64]},
    {模: '测试', 字: '非', 参: [i32], 果: [i32]},
    {模: '测试', 字: '空', 参: [], 果: []},
  ], '导入\t测试\t加\t整，整→整\n导入\t测试\t半\t小→小\n导入\t测试\t非\t爻→爻\n导入\t测试\t空\t→元\n');
  let 见 = null;
  const 导入 = 造边界导入(模块, {}, {测试: {
    加: (甲, 乙) => Number(甲 + 乙),
    半: 数 => 数 / 2,
    非: 值 => { 见 = 值; return !值; },
    空: () => 42,
  }});
  assert.equal(导入.测试.加(2n, 3n), 5n);
  assert.equal(导入.测试.半(3), 1.5);
  assert.equal(导入.测试.非(1), 0);
  assert.equal(见, true);
  assert.equal(导入.测试.空(), undefined);
  assert.deepEqual(导入.未绑定, []);
});

test('造边界导入：没有实现的给桩，调用时报接口函数未绑定', () => {
  const 模块 = 造模块([{模: '标准库', 字: '打印行', 参: [], 果: []}], '导入\t标准库\t打印行\t→元\n');
  const 导入 = 造边界导入(模块, {}, {});
  assert.deepEqual(导入.未绑定, ['标准库.打印行']);
  assert.throws(() => 导入.标准库.打印行(), /接口函数未绑定：标准库\.打印行/);
});

test('造边界导入：缺签名的导入报错，旧 call 导入不归胶水', () => {
  const 模块 = 造模块([{模: '测试', 字: '甲', 参: [], 果: []}, {模: 'yuyan:gc-host/v1', 字: 'call', 参: [], 果: []}], '');
  assert.throws(() => 造边界导入(模块, {}, {}), /缺少边界签名/);
  const 模块乙 = 造模块([{模: 'yuyan:gc-host/v1', 字: 'call', 参: [], 果: []}], '');
  assert.deepEqual(Object.keys(造边界导入(模块乙, {}, {})), []);
});

test('造边界导入：没标异步却返回 Promise 时报错', () => {
  const 模块 = 造模块([{模: '测试', 字: '等', 参: [], 果: [i64]}], '导入\t测试\t等\t→整\n');
  const 导入 = 造边界导入(模块, {}, {测试: {等: () => Promise.resolve(1n)}});
  assert.throws(() => 导入.测试.等(), /返回了 Promise/);
});

test('造边界导入：标了异步的实现套 WebAssembly.Suspending', {skip: typeof WebAssembly.Suspending !== 'function'}, () => {
  const 模块 = 造模块([{模: '测试', 字: '等', 参: [], 果: [i64]}], '导入\t测试\t等\t→整\n');
  const 实 = async () => 1n;
  实.异步 = true;
  const 导入 = 造边界导入(模块, {}, {测试: {等: 实}});
  assert.ok(导入.测试.等 instanceof WebAssembly.Suspending);
});

// 文言：手造一模：导出若干函数，附边界段。汉语：手工拼一个模块：类型段、函数段、导出段、代码段（各函数一个体），再附「豫言边界」段。
function 造导出模块(函们, 段文) {
  const 型们 = 函们.map(({参, 果}) => [0x60, ...无号(参.length), ...参, ...无号(果.length), ...果]);
  const 型段 = 段(1, [...无号(型们.length), ...型们.flat()]);
  const 函段 = 段(3, [...无号(函们.length), ...函们.flatMap((_, 序) => 无号(序))]);
  const 出段 = 段(7, [...无号(函们.length), ...函们.flatMap(({名: 出名}, 序) => [...名(出名), 0, ...无号(序)])]);
  const 体们 = 函们.map(({体}) => { const 全 = [0, ...体, 0x0b]; return [...无号(全.length), ...全]; });
  const 码段 = 段(10, [...无号(体们.length), ...体们.flat()]);
  const 自段 = 段(0, [...名('豫言边界'), ...编码器.encode(段文)]);
  return new WebAssembly.Module(new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0, ...型段, ...函段, ...出段, ...码段, ...自段]));
}

// 文言：取第几参之令。汉语：local.get 序。
const 取参 = 序 => [0x20, 序];

test('读边界导出：按导出名取签名；只认导出行', () => {
  const 模块 = 造导出模块([{名: '测试/空', 参: [], 果: [], 体: []}], '导入\t标准库\t打印行\t串→元\n导出\t测试/空\t→元\n导出\t缺列\n');
  const 表 = 读边界导出(模块);
  assert.deepEqual([...表.keys()], ['测试/空']);
  assert.equal(表.get('测试/空').文, '→元');
  assert.equal(启动导出名, '豫言操作系统启动/启动程序');
});

test('造边界导出：参数按形转成 Wasm 值，结果按形转回', () => {
  const 模块 = 造导出模块([
    {名: '测试/比', 参: [i64, i32], 果: [i32], 体: 取参(1)},
    {名: '测试/同', 参: [f64], 果: [f64], 体: 取参(0)},
    {名: '测试/整', 参: [i64], 果: [i64], 体: 取参(0)},
    {名: '测试/空', 参: [], 果: [], 体: []},
  ], '导出\t测试/比\t整，爻→爻\n导出\t测试/同\t小→小\n导出\t测试/整\t整→整\n导出\t测试/空\t→元\n');
  const 实例 = new WebAssembly.Instance(模块, {});
  const 导出 = 造边界导出(实例, 模块, {});
  assert.equal(导出['测试/比'](5n, true), true);
  assert.equal(导出['测试/比'](5, false), false);
  assert.equal(导出['测试/同'](1.5), 1.5);
  assert.equal(导出['测试/整'](7), 7n);
  assert.equal(导出['测试/空'](), undefined);
});

test('造边界导出：段里登记了而实例没有的导出报错；没有导出行时为空', () => {
  const 模块 = 造导出模块([{名: '测试/空', 参: [], 果: [], 体: []}], '导出\t测试/缺\t→元\n');
  assert.throws(() => 造边界导出(new WebAssembly.Instance(模块, {}), 模块, {}), /模块却没有这个函数：测试\/缺/);
  const 模块乙 = 造导出模块([{名: '测试/空', 参: [], 果: [], 体: []}], '');
  assert.deepEqual(Object.keys(造边界导出(new WebAssembly.Instance(模块乙, {}), 模块乙, {})), []);
});

test('造边界导出：异步时经 WebAssembly.promising，返回承诺', {skip: typeof WebAssembly.promising !== 'function'}, async () => {
  const 模块 = 造导出模块([{名: '测试/整', 参: [i64], 果: [i64], 体: 取参(0)}], '导出\t测试/整\t整→整\n');
  const 导出 = 造边界导出(new WebAssembly.Instance(模块, {}), 模块, {}, {异步: true});
  const 果 = 导出['测试/整'](9n);
  assert.ok(果 instanceof Promise);
  assert.equal(await 果, 9n);
});
