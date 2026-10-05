// 汉语：正式原壳移动跨段字节与空文件，失败须保源。文言：正式原壳移跨段字与空文，败须存源。
import 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {spawnSync as 启动} from 'node:child_process';
import assert from 'node:assert/strict';
const 入口 = 路径.join(路径.resolve(process.argv[2]), '启动.mjs');
const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy原壳移动-'));
try {
  const 子 = 路径.join(根, '子'); 文件系统.mkdirSync(子);
  const 字节 = Buffer.alloc(1048576 + 31);
  for (let 序 = 0; 序 < 字节.length; 序++) 字节[序] = (序 * 37) & 255;
  文件系统.writeFileSync(路径.join(子, '源'), 字节);
  文件系统.writeFileSync(路径.join(子, '目标'), Buffer.alloc(字节.length + 1000));
  文件系统.writeFileSync(路径.join(子, '空'), Buffer.alloc(0));
  文件系统.writeFileSync(路径.join(子, '同'), '保留');
  const 行们 = ['「文件」之「切换」于「子」', '「文件」之「移动」于「源」于「目标」', '「文件」之「移动」于「空」于「空目标」', '「文件」之「移动」于「同」于「同」', '「文件」之「移动」于「同」于「缺父/目标」', '「退出」'];
  const 果 = 启动(process.execPath, [入口, '--授权控制台', '主控制台', '--授权目录', '桌面=' + 根], {input: 行们.join('\n') + '\n', encoding: 'utf8', timeout: 120000});
  assert.equal(果.status, 0, 果.stderr);
  assert.ok(果.stdout.includes('已移动') && 果.stdout.includes('源与目标相同') && 果.stdout.includes('错误：'), 果.stdout);
  assert.equal(文件系统.existsSync(路径.join(子, '源')), false);
  assert.deepEqual(文件系统.readFileSync(路径.join(子, '目标')), 字节);
  assert.equal(文件系统.existsSync(路径.join(子, '空')), false);
  assert.equal(文件系统.statSync(路径.join(子, '空目标')).size, 0);
  assert.equal(文件系统.readFileSync(路径.join(子, '同'), 'utf8'), '保留');
  const 拒 = 启动(process.execPath, [入口, '--授权控制台', '主控制台', '--授权只读目录', '桌面=' + 根], {input: '「文件」之「移动」于「子/同」于「只读目标」\n「退出」\n', encoding: 'utf8', timeout: 120000});
  assert.equal(拒.status, 0, 拒.stderr);
  assert.ok(拒.stdout.includes('目录未获授权'), 拒.stdout);
  assert.equal(文件系统.existsSync(路径.join(根, '只读目标')), false);
  assert.equal(文件系统.readFileSync(路径.join(子, '同'), 'utf8'), '保留');
  console.log('正式原壳跨段二进制及空文件移动、覆盖截断、同路径与失败保源通过');
} finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
