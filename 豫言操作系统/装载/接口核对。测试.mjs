// 文言：取真客器与二独立簿，改签、改摘、阙供，皆当拒于启前。
// 汉语：使用官网真实 Wasm 和独立宿主支持清单检查正常及篡改场景。
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {dirname, resolve} from 'node:path';
import test from 'node:test';
import {核对接口装载} from './接口核对.mjs';

const 根 = resolve(dirname(fileURLToPath(import.meta.url)), '../../');
const 云 = resolve(根, '../yuyan-cloud');
const 读取清单 = 路径 => JSON.parse(readFileSync(路径, 'utf8'));
const 读取 = (名, 宿主) => ({
  要求: 读取清单(resolve(云, `dist/构建源码/dist/操作系统绑定/${名}/规范清单.json`)),
  提供: 读取清单(resolve(根, `豫言操作系统/适配/${名}/宿主提供.json`)),
  宿主
});
const 可运行 = existsSync(resolve(云, 'dist/网站/资源/首页应用/程序.wasm')) &&
  existsSync(resolve(云, 'dist/官网路由/程序.wasm')) &&
  ['网页界面', '剪贴板', '网址', '网页转发'].every(名 =>
    existsSync(resolve(云, `dist/构建源码/dist/操作系统绑定/${名}/规范清单.json`)));
const 网页 = 可运行 ? ['网页界面', '剪贴板', '网址'].map(名 => 读取(名, '浏览器')) : [];
const 云工 = 可运行 ? [读取('网页转发', '云工')] : [];
const 程序 = 可运行 ? {
  浏览器: new WebAssembly.Module(readFileSync(resolve(云, 'dist/网站/资源/首页应用/程序.wasm'))),
  云工: new WebAssembly.Module(readFileSync(resolve(云, 'dist/官网路由/程序.wasm')))
} : {};
const 核对 = (组, 宿主) => 核对接口装载({
  程序模块: 程序[宿主], 应用要求: 组.map(项 => 项.要求),
  宿主提供: 组.map(项 => 项.提供), 宿主
});

test('官网首页与路由的真实 Wasm 在装载前通过独立接口清单', {skip: !可运行}, () => {
  assert.equal(核对(网页, '浏览器'), true);
  assert.equal(核对(云工, '云工'), true);
});

test('浏览器模块表不可读时以原始 Wasm 字节核对导入导出', {skip: !可运行}, () => {
  const 原导入 = WebAssembly.Module.imports, 原导出 = WebAssembly.Module.exports;
  const 字节 = readFileSync(resolve(云, 'dist/网站/资源/首页应用/程序.wasm'));
  try {
    WebAssembly.Module.imports = () => {throw Error('模块表不可读');};
    WebAssembly.Module.exports = () => {throw Error('模块表不可读');};
    assert.equal(核对接口装载({程序模块: 程序.浏览器, 程序字节: 字节,
      应用要求: 网页.map(项 => 项.要求), 宿主提供: 网页.map(项 => 项.提供), 宿主: '浏览器'}), true);
    assert.throws(() => 核对接口装载({程序模块: 程序.浏览器,
      程序字节: new Uint8Array([0,97,115,109,1,0,0,0]),
      应用要求: 网页.map(项 => 项.要求), 宿主提供: 网页.map(项 => 项.提供), 宿主: '浏览器'}),
    /Wasm 宿主导入形状不符/u);
  } finally {
    WebAssembly.Module.imports = 原导入;
    WebAssembly.Module.exports = 原导出;
  }
});

test('缺少宿主接口时拒绝装载', {skip: !可运行}, () => {
  assert.throws(() => 核对接口装载({程序模块: 程序.浏览器,
    应用要求: 网页.map(项 => 项.要求), 宿主提供: [网页[0].提供], 宿主: '浏览器'}),
  /宿主不支持接口/u);
});

test('源级签名或规范摘要改变时拒绝装载', {skip: !可运行}, () => {
  const 签名 = structuredClone(网页[0].要求);
  签名.函数[0].签名 = '→[「 有 」；「 整数 」]';
  assert.throws(() => 核对([{...网页[0], 要求: 签名}], '浏览器'), /接口规范或签名不一致/u);
  const 摘要 = structuredClone(云工[0].要求);
  摘要.规范摘要[0].摘要 = '0'.repeat(64);
  assert.throws(() => 核对([{...云工[0], 要求: 摘要}], '云工'), /接口规范或签名不一致/u);
});

test('错误的 Wasm 模块在应用启动前拒绝', {skip: !可运行}, () => {
  const 空模块 = new WebAssembly.Module(new Uint8Array([0,97,115,109,1,0,0,0]));
  assert.throws(() => 核对接口装载({程序模块: 空模块,
    应用要求: [云工[0].要求], 宿主提供: [云工[0].提供], 宿主: '云工'}),
  /Wasm 宿主导入形状不符/u);
});
