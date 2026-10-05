// 汉语：使用真实节点子进程核对输入、环境隔离、参数及失败结果。文言：以实节点子进程验输入、环境之隔、参数与败果。
import test from 'node:test';
import assert from 'node:assert/strict';
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {创建子程序能力} from './子程序.mjs';

test('具名子程序传输入、参数与环境，调用间不留环境', () => {
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy子程序-'));
  const 入口 = 路径.join(根, '入口.mjs');
  文件系统.writeFileSync(入口, "import * as 文件系统 from 'node:fs'; process.stdout.write(JSON.stringify([process.argv.slice(3), process.env.豫言验收 ?? '', 文件系统.readFileSync(0, 'utf8')])); process.stderr.write('中文错误流');");
  const 能力 = 创建子程序能力({程序: new Map([['验收', {入口, 目录: 根}]]), 环境: new Set(['豫言验收'])});
  const 字 = new TextEncoder().encode('中文输入');
  try {
    const 果 = 能力.运行('验收', ['甲 乙', '--参数'], 字, [['豫言验收', '环境甲']]);
    assert.deepEqual(果.slice(0, 2), [0, 0]);
    assert.deepEqual(JSON.parse(能力.解码输出(果[2])), [['甲 乙', '--参数'], '环境甲', '中文输入']);
    assert.equal(能力.解码输出(果[3]), '中文错误流');
    const 再果 = 能力.运行('验收', [], new Uint8Array(), []);
    assert.equal(JSON.parse(能力.解码输出(再果[2]))[1], '');
    assert.equal(能力.运行('未授', [], 字, [])[0], 1);
    assert.equal(能力.运行('验收', [], 字, [['未授', '值']])[0], 1);
    assert.equal(能力.运行('验收', [], 字, [['豫言验收', '甲'], ['豫言验收', '乙']])[0], 7);
  } finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
});

test('非零退出保留输出，启动失败有独立错误', () => {
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy子程序退出-'));
  const 入口 = 路径.join(根, '入口.mjs');
  文件系统.writeFileSync(入口, "process.stdout.write('失败输出'); process.exit(17);");
  const 能力 = 创建子程序能力({程序: new Map([['验收', {入口, 目录: 根}], ['坏路径', {入口, 目录: 路径.join(根, '不存在')}]])});
  try {
    const 果 = 能力.运行('验收', [], new Uint8Array(), []);
    assert.deepEqual(果.slice(0, 2), [0, 17]);
    assert.equal(能力.解码输出(果[2]), '失败输出');
    assert.equal(能力.运行('坏路径', [], new Uint8Array(), [])[0], 8);
  } finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
});
