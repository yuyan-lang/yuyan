// 汉语：用真实子进程管道验证中文行、空行、无换行末行与输出，异步Wasm边界另验。文言：以实子进程之管验中文行、空行、无新行之末行及出，异步Wasm边界别验。
import test from 'node:test';
import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {创建节点控制台} from './控制台.mjs';

if (process.argv.includes('--验收子程序')) {
  const 能 = 创建节点控制台();
  assert.deepEqual(await 能.读取('主控制台'), [0, '中文甲', '']);
  assert.deepEqual(await 能.读取('主控制台'), [0, '', '']);
  assert.deepEqual(await 能.读取('主控制台'), [0, '末行乙', '']);
  assert.deepEqual(await 能.读取('主控制台'), [4, '', '']);
  assert.deepEqual(await 能.写入('主控制台', '中文输出\n'), [0, '', '']);
  能.关闭();
} else {
  test('真实标准输入输出管道保持中文、空行与末行', () => {
    const 果 = spawnSync(process.execPath, [import.meta.filename, '--验收子程序'], {input: '中文甲\r\n\n末行乙', encoding: 'utf8', timeout: 10000});
    assert.equal(果.status, 0, 果.stderr);
    assert.ok(果.stdout.startsWith('中文输出\n'), 果.stdout);
  });
}
