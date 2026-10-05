// 文言：验节点应用宿主于平台接口之包之带型导入：诺节宿主、系统库调用尽实之，云工通用四十六，安全外壳密码惟求SHA256；异步者标之；小数之果去壳，外部值之列展之。
// 汉语：Node 应用宿主的平台接口包带类型导入：诺节宿主、系统库调用全部实现，云工宿主实现四十六个通用原语，安全外壳密码只实现求SHA256；
//   网络发送、显示、图形与 async 的云工原语标“异步”，其余同步；小数结果去掉 {小数} 外壳，系统库调用的外部值列展成数值数组。
import test from 'node:test';
import assert from 'node:assert/strict';
import {创建能力, 带型实现, 能力清理, 派生平台实现} from './应用宿主.mjs';
import {平台导入旧名} from '../云工/值桥.mjs';

const 造能力 = () => 创建能力({授权: {目录: new Map(), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: '/程序.wasm', 输出: () => {}, 张量线程数: 1});

test('诺节宿主与系统库调用全部实现，云工宿主实现四十六个，安全外壳密码只有求SHA256', async () => {
  const 能力 = 造能力();
  try {
    const 实 = 能力[带型实现];
    for (const 包 of ['诺节宿主', '系统库调用']) {
      const 缺 = Object.keys(平台导入旧名[包]).filter(字段 => typeof 实[包]?.[字段] !== 'function');
      assert.deepEqual(缺, [], 包 + ' 缺实现');
    }
    assert.equal(Object.keys(平台导入旧名.诺节宿主).length, 75);
    assert.equal(Object.keys(平台导入旧名.系统库调用).length, 18);
    assert.equal(Object.keys(实.云工宿主).length, 46);
    assert.deepEqual(Object.keys(实.安全外壳密码), ['求SHA256']);
    assert.equal(实.中央张量宿主, undefined);
    assert.equal(实.浏览器宿主, undefined);
  } finally {
    await 能力[能力清理]?.();
  }
});

test('只有异步实现标“异步”', async () => {
  const 能力 = 造能力();
  try {
    const 实 = 能力[带型实现];
    const 异步们 = Object.entries(实).flatMap(([包, 表]) => Object.entries(表).filter(([, 函]) => 函.异步).map(([字段]) => 包 + '.' + 字段)).sort();
    assert.ok(异步们.includes('诺节宿主.诺节网络发送'));
    assert.ok(异步们.includes('诺节宿主.诺节显示操作'));
    assert.ok(异步们.includes('诺节宿主.诺节图形操作'));
    assert.ok(异步们.includes('云工宿主.云工调用方法'));
    assert.ok(!异步们.includes('诺节宿主.诺节张量矩阵乘'));
    assert.ok(!异步们.includes('诺节宿主.诺节字体操作'));
    assert.equal(异步们.filter(名 => 名.startsWith('云工宿主.')).length, 13);
  } finally {
    await 能力[能力清理]?.();
  }
});

test('小数结果去壳，外部值的列展成数值数组', () => {
  const 收到 = [];
  const 旧表 = {
    豫言_节点_张量取小数: () => ({小数: 1.5}),
    豫言_节点_外部库调用: (址, 签名, 参们) => { 收到.push(参们); return {小数: 2.5}; },
    豫言_节点_外部库读小数: () => ({小数: 0.25})
  };
  const 表 = 派生平台实现(旧表);
  assert.equal(表.诺节宿主.诺节张量取小数('1', 0n), 1.5);
  assert.equal(表.系统库调用.外部库读小数原语(0n, '单'), 0.25);
  assert.equal(表.系统库调用.外部调用得小数原语(1n, '双：长双', [[0, 3n], [1, 0.5]]), 2.5);
  assert.deepEqual(收到[0], [3n, 0.5]);
  assert.equal(表.系统库调用.外部调用无返回原语(1n, '无：', []), undefined);
  assert.throws(() => 表.系统库调用.外部调用得整数原语(1n, '长：长', [[2]]), /无值/u);
});
