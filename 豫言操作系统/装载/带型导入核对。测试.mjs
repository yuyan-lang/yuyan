// 文言：手造之模验装载之核：带型导入可受，旧通调与余者一并列报。
// 汉语：用手工拼出的小模块验证装载核对对带类型导入的规则：标准库、构建基础、平台接口包与应用要求的接口包的带类型导入放行，
//   导入模块不对、缺签名、不是函数导入（含旧的 yuyan:gc-host/v1.call）的一次列全报“Wasm 宿主导入形状不符”；没有「豫言边界」段的不是豫言程序；模块表不可读时按字节核对同样成立。
// 运行：node --test 豫言操作系统/装载/带型导入核对。测试.mjs
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {核对接口装载} from './接口核对.mjs';

const 编码器 = new TextEncoder();
const 无号 = 数 => { const 字节 = []; do { let 字 = 数 & 127; 数 >>>= 7; if (数) 字 |= 128; 字节.push(字); } while (数); return 字节; };
const 名 = 文 => { const 字节 = [...编码器.encode(文)]; return [...无号(字节.length), ...字节]; };
const 段 = (号, 体) => [号, ...无号(体.length), ...体];

// 文言：造一模：诸导入（函或全局）、一 _start、可选之边界段与他名之导出。汉语：拼一个模块：导入若干函数（或一个全局），定义并导出 _start，可选附「豫言边界」段；额外导出们 是同一函数的其他导出名。
function 造字节(导入们, 段文 = null, 额外导出们 = []) {
  const 型们 = [[0x60, 0, 0], ...导入们.filter(项 => !项.全局).map(({参 = [], 果 = []}) => [0x60, ...无号(参.length), ...参, ...无号(果.length), ...果])];
  let 型号 = 1;
  const 导入体 = 导入们.flatMap(项 => 项.全局 ? [...名(项.模), ...名(项.字), 3, 0x7f, 0] : [...名(项.模), ...名(项.字), 0, ...无号(型号++)]);
  const 函数数 = 导入们.filter(项 => !项.全局).length;
  return new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0,
    ...段(1, [...无号(型们.length), ...型们.flat()]),
    ...段(2, [...无号(导入们.length), ...导入体]),
    ...段(3, [1, 0]),
    ...段(7, [1 + 额外导出们.length, ...名('_start'), 0, ...无号(函数数), ...额外导出们.flatMap(出名 => [...名(出名), 0, ...无号(函数数)])]),
    ...段(10, [1, 2, 0, 0x0b]),
    ...(段文 === null ? [] : 段(0, [...名('豫言边界'), ...编码器.encode(段文)]))]);
}
const i64 = 0x7e;
const 打印行 = {模: '标准库', 字: '打印行'}, 存放 = {模: '构建基础', 字: '存放包上下文'};
const 通调 = {模: 'yuyan:gc-host/v1', 字: 'call'};
const 签名文 = '导入\t标准库\t打印行\t串→元\n导入\t构建基础\t存放包上下文\t串→串\n导入\t豫言操作系统时间\t读取当前Unix毫秒\t→整\n导入\t测试\t甲\t→元\n导入\t诺节宿主\t读文件\t串→串\n';
const 时间清单 = {
  格式版本: 4, 接口所有者: '豫言', 接口名称: '豫言操作系统时间', 接口版本: '0.2.0',
  解析闭包: [{所有者: '豫言', 名称: '豫言操作系统时间', 版本: '0.2.0'}], 直接依赖: [],
  函数: [{模块: '时点', 函数: '读取当前Unix毫秒', 方向: '宿主', 签名: '→[「 有 」；「 整数 」]'}]
};
const 核对 = (字节, 应用要求 = [], 宿主 = '节点') =>
  核对接口装载({程序模块: new WebAssembly.Module(字节), 程序字节: 字节, 应用要求, 宿主提供: 应用要求, 宿主});

test('只有带类型导入（标准库、构建基础）的程序放行', () => {
  assert.equal(核对(造字节([打印行, 存放], 签名文)), true);
});

test('旧的 call 导入拒绝；只有它而没有「豫言边界」段的不是豫言程序', () => {
  assert.throws(() => 核对(造字节([通调, 打印行], 签名文)), /yuyan:gc-host\/v1\.call（「豫言边界」段里没有它的签名）/u);
  assert.throws(() => 核对(造字节([通调])), /不是豫言程序/u);
});

test('应用要求的接口包的带类型导入放行，未要求的拒绝', () => {
  const 时间 = {模: '豫言操作系统时间', 字: '读取当前Unix毫秒', 果: [i64]};
  assert.equal(核对(造字节([打印行, 时间], 签名文), [时间清单]), true);
  assert.throws(() => 核对(造字节([打印行, 时间], 签名文)), /Wasm 宿主导入形状不符：豫言操作系统时间\.读取当前Unix毫秒（导入模块不是/u);
});

test('不合的导入一次列全：模块不对、缺签名、不是函数导入', () => {
  assert.throws(() => 核对(造字节([{模: '测试', 字: '甲'}, {模: '标准库', 字: '无签名'}, 打印行], 签名文)),
    错 => /测试\.甲（导入模块不是/u.test(错.message) && /标准库\.无签名（「豫言边界」段里没有它的签名）/u.test(错.message));
  assert.throws(() => 核对(造字节([{模: '标准库', 字: '全局', 全局: true}], 签名文)), /标准库\.全局（不是函数导入）/u);
});

test('没有「豫言边界」段的模块不是豫言程序', () => {
  assert.throws(() => 核对(造字节([])), /Wasm 宿主导入形状不符：不是豫言程序/u);
  assert.throws(() => 核对(造字节([{模: '别处', 字: 'f'}])), /不是豫言程序/u);
});

test('浏览器模块表不可读时按字节核对，规则相同', () => {
  const 原导入 = WebAssembly.Module.imports, 原导出 = WebAssembly.Module.exports, 原段 = WebAssembly.Module.customSections;
  try {
    WebAssembly.Module.imports = () => { throw Error('模块表不可读'); };
    WebAssembly.Module.exports = () => { throw Error('模块表不可读'); };
    WebAssembly.Module.customSections = () => { throw Error('模块表不可读'); };
    assert.equal(核对(造字节([打印行, 存放], 签名文), [], '浏览器'), true);
    assert.throws(() => 核对(造字节([打印行, {模: '测试', 字: '甲'}], 签名文), [], '浏览器'), /测试\.甲（导入模块不是/u);
    assert.throws(() => 核对(造字节([{模: '标准库', 字: '全局', 全局: true}], 签名文), [], '浏览器'), /不是函数导入/u);
    assert.throws(() => 核对(造字节([]), [], '浏览器'), /不是豫言程序/u);
  } finally {
    WebAssembly.Module.imports = 原导入;
    WebAssembly.Module.exports = 原导出;
    WebAssembly.Module.customSections = 原段;
  }
});

test('平台接口包的带类型导入放行（宿主可给桩）', () => {
  assert.equal(核对(造字节([打印行, {模: '诺节宿主', 字: '读文件'}], 签名文)), true);
});

test('给了带型实现时，可移植接口的导入须全部由宿主实现，缺的一次列全', () => {
  const 时间 = {模: '豫言操作系统时间', 字: '读取当前Unix毫秒', 果: [i64]};
  const 字节 = 造字节([打印行, 时间, {模: '诺节宿主', 字: '读文件'}], 签名文);
  const 带实现核对 = 带型实现 => 核对接口装载({程序模块: new WebAssembly.Module(字节), 程序字节: 字节,
    应用要求: [时间清单], 宿主提供: [时间清单], 宿主: '节点', 带型实现});
  assert.throws(() => 带实现核对({}), /宿主没有实现可移植接口的这些函数：豫言操作系统时间\.读取当前Unix毫秒$/u);
  assert.equal(带实现核对({豫言操作系统时间: {读取当前Unix毫秒: () => 0n}}), true);
  assert.equal(带实现核对(undefined), true);
});

test('给了应用提供时，应用提供的接口函数须有导出 接口名称/函数名；模块表不可读时按字节核对同样成立', () => {
  const 启动清单 = {接口名称: '豫言操作系统启动', 函数: [{模块: '程序入口', 函数: '启动程序', 方向: '应用', 签名: '→[「 有 」；「 有 」]'}]};
  const 核 = (字节, 宿主 = '节点') => 核对接口装载({程序模块: new WebAssembly.Module(字节), 程序字节: 字节, 应用要求: [], 宿主提供: [], 宿主, 应用提供: [启动清单]});
  assert.throws(() => 核(造字节([打印行], 签名文)), /Wasm 缺少应用提供的接口导出：豫言操作系统启动\/启动程序$/u);
  assert.equal(核(造字节([打印行], 签名文, ['豫言操作系统启动/启动程序'])), true);
  const 原导入 = WebAssembly.Module.imports, 原导出 = WebAssembly.Module.exports, 原段 = WebAssembly.Module.customSections;
  try {
    WebAssembly.Module.imports = () => { throw Error('模块表不可读'); };
    WebAssembly.Module.exports = () => { throw Error('模块表不可读'); };
    WebAssembly.Module.customSections = () => { throw Error('模块表不可读'); };
    assert.throws(() => 核(造字节([打印行], 签名文), '浏览器'), /Wasm 缺少应用提供的接口导出/u);
    assert.equal(核(造字节([打印行], 签名文, ['豫言操作系统启动/启动程序']), '浏览器'), true);
  } finally {
    WebAssembly.Module.imports = 原导入;
    WebAssembly.Module.exports = 原导出;
    WebAssembly.Module.customSections = 原段;
  }
});
