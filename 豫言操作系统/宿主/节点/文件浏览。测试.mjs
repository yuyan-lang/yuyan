// 文言：验目录之列与信息之查，兼验根、子目录、越界、链与伪权。汉语：验证目录列举、文件信息及授权边界，覆盖根、子目录、符号链接和无效目录权。
import test from 'node:test';
import assert from 'node:assert/strict';
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {创建能力, 带型实现, 能力清理} from './应用宿主.mjs';

// 汉语：真实文件覆盖、追加与创建，并核对失败时不截断外部文件。文言：验实文之覆盖、追加与创建，败不得截外文。
test('可写目录创建覆盖和追加，拒绝只读及越界', async () => {
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy文件写入-'));
  const 外 = 根 + '-外部.txt';
  文件系统.writeFileSync(外, '外部原文');
  文件系统.symlinkSync(外, 路径.join(根, '外链'));
  文件系统.mkdirSync(路径.join(根, '目录'));
  const 能力 = 创建能力({授权: {目录: new Map([['写', {路径: 根, 可写: true}], ['读', {路径: 根, 可写: false}]]), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: '/程序.wasm', 输出: () => {}, 张量线程数: 1});
  const 文 = 值 => new TextEncoder().encode(值);
  const 实 = 能力[带型实现].诺节宿主;
  try {
    const 写 = 文(实.诺节文件取得目录(文('写'))[1]), 读 = 文(实.诺节文件取得目录(文('读'))[1]);
    const 写文 = (路, 追加, 值) => {
      const [码, 号] = 实.诺节文件开启写入(写, 文(路), 追加);
      assert.equal(码, 0);
      assert.equal(实.诺节文件写入(文(号), 文(值))[0], 0);
      assert.equal(实.诺节文件关闭(文(号))[0], 0);
    };
    写文('新文件.txt', false, '甲乙');
    写文('新文件.txt', true, '丙');
    assert.equal(文件系统.readFileSync(路径.join(根, '新文件.txt'), 'utf8'), '甲乙丙');
    写文('新文件.txt', false, '丁');
    assert.equal(文件系统.readFileSync(路径.join(根, '新文件.txt'), 'utf8'), '丁');
    写文('追加创建.txt', true, '新');
    assert.equal(实.诺节文件开启写入(读, 文('新文件.txt'), false)[0], 1);
    for (const 路 of ['', '../越界', '/绝对', '外链', '目录']) assert.equal(实.诺节文件开启写入(写, 文(路), false)[0], 7, 路);
    assert.equal(实.诺节文件开启写入(写, 文('缺父/文件'), false)[0], 4);
    assert.equal(实.诺节文件开启写入(文('伪造'), 文('文件'), false)[0], 3);
    assert.equal(文件系统.readFileSync(外, 'utf8'), '外部原文');
    assert.equal(文件系统.readFileSync(路径.join(根, '新文件.txt'), 'utf8'), '丁');
  } finally { await 能力[能力清理]?.(); 文件系统.rmSync(根, {recursive: true, force: true}); 文件系统.rmSync(外); }
});

// 汉语：创建目录须可写授权，不能借父目录符号链接逃出根。文言：造目录须授写权，不得借父链出根。
test('创建目录核对写权、父目录及边界', async () => {
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy造目录-'));
  文件系统.symlinkSync(系统.tmpdir(), 路径.join(根, '外链'));
  const 能力 = 创建能力({授权: {目录: new Map([['写', {路径: 根, 可写: true}], ['读', {路径: 根, 可写: false}]]), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: '/程序.wasm', 输出: () => {}, 张量线程数: 1});
  const 文 = 值 => new TextEncoder().encode(值);
  const 实 = 能力[带型实现].诺节宿主;
  try {
    const 写 = 文(实.诺节文件取得目录(文('写'))[1]);
    const 读 = 文(实.诺节文件取得目录(文('读'))[1]);
    assert.deepEqual(实.诺节文件创建目录(写, 文('新目录')), [0, '']);
    assert.ok(文件系统.statSync(路径.join(根, '新目录')).isDirectory());
    assert.equal(实.诺节文件创建目录(写, 文('新目录'))[0], 5);
    assert.equal(实.诺节文件创建目录(读, 文('拒绝'))[0], 1);
    assert.equal(实.诺节文件创建目录(写, 文('缺父/子'))[0], 4);
    for (const 径 of ['', '../越界', '/绝对', '外链/越界']) assert.equal(实.诺节文件创建目录(写, 文(径))[0], 7, 径);
    assert.equal(实.诺节文件创建目录(文('伪造'), 文('目录'))[0], 3);
  } finally {
    await 能力[能力清理]?.();
    文件系统.rmSync(根, {recursive: true, force: true});
  }
});

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
