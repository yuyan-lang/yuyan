// 汉语：真实PTY验收只驱动系统终端设置，尺寸业务由同源豫言原生服务完成。文言：实伪终端之验惟驱系统之设置，尺寸之事由同源豫言原生服务成之。
import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {existsSync} from 'node:fs';
import 路径 from 'node:path';
import {fileURLToPath} from 'node:url';
import {读取标准终端尺寸} from './终端尺寸.mjs';

const [模式, 服务路径] = process.argv.slice(2);
assert.ok(服务路径, '须传同源豫言原生终端尺寸服务');
process.env.YY_TERMINAL_SIZE_SERVICE = 路径.resolve(服务路径);
assert.ok(existsSync(process.env.YY_TERMINAL_SIZE_SERVICE), '同源终端尺寸原生服务尚未构建');
if (模式 === '实终端') {
  for (const [列, 行] of [[109, 37], [73, 19], [0, 0]]) {
    const 设 = spawnSync('stty', ['cols', String(列), 'rows', String(行)], {stdio: 'inherit'});
    assert.equal(设.status, 0);
    const 始 = performance.now();
    const 果 = 读取标准终端尺寸();
    const 耗时 = performance.now() - 始;
    assert.deepEqual(果, 列 ? [true, BigInt(列), BigInt(行)] : [false, 0n, 0n]);
    console.log(`终端尺寸 ${列}×${行}，单次原生转发 ${耗时.toFixed(3)} 毫秒`);
  }
} else {
  assert.deepEqual(读取标准终端尺寸(), [false, 0n, 0n], '管道标准输出须返回查询失败');
  const 本文件 = fileURLToPath(import.meta.url);
  const 命令 = [process.execPath, 本文件, '实终端', process.env.YY_TERMINAL_SIZE_SERVICE]
    .map(文 => "'" + 文.replaceAll("'", "'\\''") + "'").join(' ');
  // 汉语：script只提供PTY，业务查询与改尺寸分别由豫言服务及系统stty完成。Windows的PTY驱动留待后续版本。
  // 文言：script惟供伪终端，查尺寸、易尺寸各由豫言服务、系统stty成之。视窗之驱待后版。
  const 参数 = process.platform === 'darwin' ? ['-q', '/dev/null', '/bin/sh', '-c', 命令] : ['-q', '-e', '-c', 命令, '/dev/null'];
  const 果 = spawnSync('script', 参数, {encoding: 'utf8', timeout: 30000});
  process.stdout.write(果.stdout ?? '');
  assert.equal(果.status, 0, 果.stderr);
  assert.match(果.stdout, /终端尺寸 109×37/u);
  assert.match(果.stdout, /终端尺寸 73×19/u);
  assert.match(果.stdout, /终端尺寸 0×0/u);
  console.log('真实终端、窗口调整与未知尺寸验收通过');
}
