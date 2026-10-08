// 汉语：核验正式应用同时具备标准TCP绑定与公共文件能力，未授监听以协议结果拒绝。文言：核正式应用兼有标准TCP之绑及公共文件之能，未授监听以协议之果拒之。
import assert from 'node:assert/strict';
import {创建能力, 带型实现, 能力清理} from './应用宿主.mjs';
const 表 = 创建能力({授权: {目录: new Map(), 子程序: new Map(), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: '/应用.wasm', 输出() {}, 张量线程数: 1});
try {
  const 标准 = 表[带型实现].标准库;
  for (const 名 of ['监听', '开始连接', '完成连接', '接受', '读取', '读取字节串', '从字节序数写入', '从字节序数写入字节串', '等待', '获取本地端口', '设置无延迟', '关闭写入', '关闭', '错误消息']) assert.equal(typeof 标准['传输控制协议_' + 名], 'function');
  assert.equal(typeof 标准.数据报_交换, 'function');
  assert.deepEqual(标准.传输控制协议_监听(new TextEncoder().encode('127.0.0.1'), 8123n, 128n), [-1, -1]);
  assert.equal(typeof 表[带型实现].诺节宿主.诺节文件取得目录, 'function');
  assert.equal(typeof 表[带型实现].豫言操作系统时间.读取当前Unix毫秒, 'function');
  console.log('正式应用TCP、数据报与公共文件共用绑定通过');
} finally { 表[能力清理](); }
