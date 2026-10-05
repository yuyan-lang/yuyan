// 汉语：正式原壳调用公共字节复制，核对跨段二进制、覆盖截断、空文件及拒写保留。文言：正式原壳调公字节之复制，核跨段二进制、覆盖断尾、空文及拒写存原。
import 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {spawnSync as 启动} from 'node:child_process';
import assert from 'node:assert/strict';
const 入口 = 路径.join(路径.resolve(process.argv[2]), '启动.mjs');
const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy原壳复制-'));
try {
  const 子 = 路径.join(根, '子目录'); 文件系统.mkdirSync(子);
  const 字节 = Buffer.alloc(1048576 + 31);
  for (let 序 = 0; 序 < 字节.length; 序++) 字节[序] = (序 * 37) & 255;
  文件系统.writeFileSync(路径.join(子, '源.bin'), 字节);
  文件系统.writeFileSync(路径.join(子, '目标.bin'), Buffer.alloc(字节.length + 1000, 255));
  文件系统.writeFileSync(路径.join(子, '空.bin'), Buffer.alloc(0));
  文件系统.writeFileSync(路径.join(子, '空目标.bin'), '原有内容须截断');
  const 行们 = ['「文件」之「切换」于「子目录」', '「文件」之「复制」于「源.bin」于「目标.bin」', '「文件」之「复制」于「空.bin」于「空目标.bin」', '「文件」之「复制」于「不存在.bin」于「目标.bin」', '「退出」'];
  const 果 = 启动(process.execPath, [入口, '--授权控制台', '主控制台', '--授权目录', '桌面=' + 根], {input: 行们.join('\n') + '\n', encoding: 'utf8', timeout: 120000});
  assert.equal(果.status, 0, 果.stderr);
  assert.ok(果.stdout.includes('已复制 1048607 字节') && 果.stdout.includes('已复制 0 字节') && 果.stdout.includes('错误：'), 果.stdout);
  assert.deepEqual(文件系统.readFileSync(路径.join(子, '目标.bin')), 字节);
  assert.deepEqual(文件系统.readFileSync(路径.join(子, '源.bin')), 字节);
  assert.equal(文件系统.statSync(路径.join(子, '空目标.bin')).size, 0);
  const 拒 = 启动(process.execPath, [入口, '--授权控制台', '主控制台', '--授权只读目录', '桌面=' + 根], {input: '「文件」之「复制」于「子目录/空.bin」于「子目录/目标.bin」\n「退出」\n', encoding: 'utf8', timeout: 120000});
  assert.equal(拒.status, 0, 拒.stderr);
  assert.ok(拒.stdout.includes('目录未获授权'), 拒.stdout);
  assert.deepEqual(文件系统.readFileSync(路径.join(子, '目标.bin')), 字节);
  console.log('正式原壳跨段二进制复制、覆盖截断、空文件与失败保目标通过');
} finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
