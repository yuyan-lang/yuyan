// 汉语：用正式启动器和真实公共接口探针验证授权、中文输入输出、结束及未授权。文言：以正式启动器与实公共接口探针验授、中文入出、尽及未授。
import 路径 from 'node:path';
import {spawnSync as 启动} from 'node:child_process';
import assert from 'node:assert/strict';
const 入口 = 路径.join(路径.resolve(process.argv[2]), '启动.mjs');
for (const [参数, 输入, 期望] of [
  [['--授权控制台', '主控制台'], '中文甲\n', '公共输出：中文甲\n公共写入成功\n'],
  [['--授权控制台', '主控制台'], '', '输入结束\n'],
  [[], '未授输入\n', '公共读取失败\n'],
]) {
  const 果 = 启动(process.execPath, [入口, ...参数], {input: 输入, encoding: 'utf8', timeout: 30000});
  assert.equal(果.status, 0, 果.stderr);
  assert.equal(果.stdout, 期望);
}
console.log('正式公共控制台Wasm授权中文读写、输入结束与未授权拒绝通过');
