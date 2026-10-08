// 汉语：真实稀疏文件验完整偏移与顺读位置，共核由本轮源码构建。文言：实疏文验全偏移与顺读之位，共核取本轮之源而构。
import assert from 'node:assert/strict';
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {spawnSync} from 'node:child_process';
import {创建能力, 带型实现, 能力清理} from './应用宿主.mjs';
const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy定位-'));
const 共核模块 = new WebAssembly.Module(文件系统.readFileSync(process.env.YY文件定位共享 ?? 'yy文件定位共享.wasm'));
assert.ok(WebAssembly.Module.exports(共核模块).some(项 => 项.name === '内存' && 项.kind === 'memory'), '共核须导出中文内存');
const 远偏 = 4294967301;
const 描述符 = 文件系统.openSync(路径.join(根, '疏文'), 'w');
文件系统.writeSync(描述符, Buffer.from('甲乙'), 0, 6, 0);
文件系统.writeSync(描述符, Buffer.from('远'), 0, 3, 远偏);
文件系统.closeSync(描述符);
if (process.argv[2]) {
  const 原生柄 = 文件系统.openSync(路径.join(根, '疏文'), 'r');
  try {
    const 果 = spawnSync(路径.resolve(process.argv[2]), [], {stdio: ['ignore', 'pipe', 'pipe', 原生柄]});
    assert.equal(果.status, 0, 果.stderr?.toString() ?? String(果.error));
  } finally { 文件系统.closeSync(原生柄); }
}
const 能力 = 创建能力({授权: {目录: new Map([['读', {路径: 根, 可写: false}]]), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: '/程序.wasm', 输出: () => {}, 张量线程数: 1});
const 实 = 能力[带型实现].诺节宿主, 文 = 值 => new TextEncoder().encode(值);
try {
  const 目录 = 文(实.诺节文件取得目录(文('读'))[1]);
  const 柄 = 文(实.诺节文件打开(目录, 文('疏文'), false)[1]);
  assert.deepEqual(Array.from(实.诺节文件读取(柄, 1n)[1]), [0xe7]);
  const 远 = 实.诺节文件定位读取(柄, BigInt(远偏), 3n);
  assert.equal(远[0], 0, 远[2]);
  assert.equal(new TextDecoder().decode(远[1]), '远');
  assert.deepEqual(Array.from(实.诺节文件读取(柄, 1n)[1]), [0x94]);
  assert.equal(实.诺节文件定位读取(柄, BigInt(远偏 + 3), 1n)[0], 9);
  assert.equal(实.诺节文件定位读取(柄, -1n, 1n)[0], 7);
  assert.equal(实.诺节文件定位读取(柄, 0n, -1n)[0], 7);
  assert.equal(实.诺节文件定位读取(柄, 0n, 0n)[1].length, 0);
  assert.equal(实.诺节文件关闭(柄)[0], 0);
  assert.equal(实.诺节文件定位读取(柄, 0n, 1n)[0], 3);
  console.log('共享定位读取完整偏移、原生导入桩与顺读位置验收通过');
} finally { 能力[能力清理](); 文件系统.rmSync(根, {recursive: true, force: true}); }
