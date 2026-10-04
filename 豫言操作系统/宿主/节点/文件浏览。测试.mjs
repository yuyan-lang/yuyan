// 文言：验目录之列与信息之查，兼验根、子目录、越界、链与伪权。汉语：验证目录列举、文件信息及授权边界，覆盖根、子目录、符号链接和无效目录权。
import test from 'node:test';
import assert from 'node:assert/strict';
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {创建能力, 带型实现, 能力清理} from './应用宿主.mjs';

test('目录查询沿用授权根与相对路径边界', async () => {
  const 临时 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy文件浏览-'));
  const 根 = 路径.join(临时, '授权');
  文件系统.mkdirSync(根);
  文件系统.mkdirSync(路径.join(根, '文档'));
  文件系统.mkdirSync(路径.join(根, '空目录'));
  文件系统.writeFileSync(路径.join(根, '文档', '说明.txt'), '你好');
  文件系统.writeFileSync(路径.join(临时, '外部.txt'), '外部');
  文件系统.symlinkSync(路径.join(临时, '外部.txt'), 路径.join(根, '外链'));
  const 能力 = 创建能力({授权: {目录: new Map([['桌面', {路径: 根, 可写: false}]]), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: '/程序.wasm', 输出: () => {}, 张量线程数: 1});
  const 文 = 值 => new TextEncoder().encode(值);
  const 实 = 能力[带型实现].诺节宿主;
  try {
    const [码, 目录号] = 实.诺节文件取得目录(文('桌面'));
    assert.equal(码, 0);
    const 号 = 文(目录号);
    const 根果 = 实.诺节文件列目录(号, 文(''));
    assert.equal(根果[0], 0);
    assert.deepEqual(根果[1].sort((甲, 乙) => 甲[0].localeCompare(乙[0])), [['外链', 2], ['文档', 1], ['空目录', 1]].sort((甲, 乙) => 甲[0].localeCompare(乙[0])));
    assert.deepEqual(实.诺节文件列目录(号, 文('空目录')), [0, [], '']);
    assert.deepEqual(实.诺节文件列目录(号, 文('文档')), [0, [['说明.txt', 0]], '']);
    assert.deepEqual(实.诺节文件查询信息(号, 文('文档/说明.txt')), [0, 0, 6, '']);
    assert.deepEqual(实.诺节文件查询信息(号, 文('')), [0, 1, 0, '']);
    for (const 径 of ['..', '../外部.txt', '/文档', '文档//说明.txt', '文档/./说明.txt', '文档\0说明.txt', '外链']) {
      assert.equal(实.诺节文件列目录(号, 文(径))[0], 7, 径);
      assert.equal(实.诺节文件查询信息(号, 文(径))[0], 7, 径);
    }
    assert.equal(实.诺节文件列目录(号, 文('文档/说明.txt'))[0], 7);
    assert.equal(实.诺节文件查询信息(号, 文('不存在'))[0], 4);
    assert.equal(实.诺节文件列目录(文('伪造'), 文(''))[0], 3);
    assert.equal(实.诺节文件取得目录(文('未授权'))[0], 1);
  } finally {
    await 能力[能力清理]?.();
    文件系统.rmSync(临时, {recursive: true, force: true});
  }
});
