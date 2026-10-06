// 汉语：用正式启动器和实际输入管道核对公共继承输入接口，两个目录为已构建的节点发行。文言：以正式启动器与实输入之管核公继承输入接口，二目录皆已构节点发行。
import {spawnSync as 同步启动} from 'node:child_process';
import 路径 from 'node:path';
import assert from 'node:assert/strict';
const [父, 子] = process.argv.slice(2).map(目录 => 路径.resolve(目录, '启动.mjs'));
assert.ok(父 && 子, '须给父与子发行目录');
const 果 = 同步启动(process.execPath, [父, '--授权子程序', '子验收=' + 子, '--允许环境', '豫言验收'], {input: '运行子验收\n中文输入\n父续行\n', encoding: 'utf8', timeout: 120000});
assert.equal(果.status, 0, 果.stderr);
assert.ok(果.stdout.includes('公共继承输入接口实际调用通过'), 果.stdout);
process.stdout.write(果.stdout);
