// 汉语：核验正式应用同时具备标准TCP绑定与公共文件能力，未授监听以协议结果拒绝。文言：核正式应用兼有标准TCP之绑及公共文件之能，未授监听以协议之果拒之。
import assert from 'node:assert/strict';
import {创建能力, 带型实现, 能力清理} from './应用宿主.mjs';
const 表 = 创建能力({授权: {目录: new Map(), 子程序: new Map(), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: '/应用.wasm', 输出() {}, 张量线程数: 1});
try {
  const 标准 = 表[带型实现].豫章;
  for (const 名 of ['开门', '叩门', '入门', '迎', '听', '听帛', '续告', '续告帛', '守候', '门号', '勿滞', '缄口', '别', '误由']) assert.equal(typeof 标准[名], 'function', 名);
  assert.equal(typeof 标准.投书, 'function');
  assert.deepEqual(标准.开门(new TextEncoder().encode('127.0.0.1'), 8123n, 128n), [-1, -1]);
  assert.equal(typeof 表[带型实现].诺驿东道.诺驿卷取箧, 'function');
  assert.equal(typeof 表[带型实现].豫寓时辰.读今纪元毫秒, 'function');
  console.log('正式应用TCP、数据报与公共文件共用绑定通过');
} finally { 表[能力清理](); }
