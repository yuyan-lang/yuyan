// 汉语：通过正式原壳验证当前路径删除、空目录及失败保留。文言：循正式原壳验当径之删、空目录及败而存文。
import 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {spawnSync as 启动} from 'node:child_process';
import assert from 'node:assert/strict';
const 入口 = 路径.join(路径.resolve(process.argv[2]), '启动.mjs');
const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy原壳删除-'));
try {
  const 子 = 路径.join(根, '子');
  文件系统.mkdirSync(子);
  文件系统.writeFileSync(路径.join(子, '文'), '待删');
  文件系统.mkdirSync(路径.join(子, '空'));
  文件系统.mkdirSync(路径.join(子, '非空'));
  文件系统.writeFileSync(路径.join(子, '非空', '文'), '保留');
  const 行们 = ['「文件」之「切换」于「子」', '「文件」之「删除」于「文」', '「文件」之「删除」于「空」', '「文件」之「删除」于「非空」', '「文件」之「删除」于「/」', '「退出」'];
  const 果 = 启动(process.execPath, [入口, '--授权控制台', '主控制台', '--授权目录', '桌面=' + 根], {input: 行们.join('\n') + '\n', encoding: 'utf8', timeout: 120000});
  assert.equal(果.status, 0, 果.stderr);
  assert.equal(文件系统.existsSync(路径.join(子, '文')), false, 果.stdout);
  assert.equal(文件系统.existsSync(路径.join(子, '空')), false, 果.stdout);
  assert.equal(文件系统.readFileSync(路径.join(子, '非空', '文'), 'utf8'), '保留');
  assert.ok(果.stdout.includes('已删除') && 果.stdout.includes('错误：'), 果.stdout);
  const 拒 = 启动(process.execPath, [入口, '--授权控制台', '主控制台', '--授权只读目录', '桌面=' + 根], {input: '「文件」之「删除」于「子/非空/文」\n「退出」\n', encoding: 'utf8', timeout: 120000});
  assert.equal(拒.status, 0, 拒.stderr);
  assert.ok(拒.stdout.includes('目录未获授权'), 拒.stdout);
  assert.equal(文件系统.readFileSync(路径.join(子, '非空', '文'), 'utf8'), '保留');
  console.log('正式原壳相对路径删除、空目录删除及非空、根、只读保留通过');
} finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
