// 汉语：通过正式启动器验证原壳共享授权根与导航，不替换接口。文言：循正式启动器验原壳共授权根与导航，不易其接口。
import 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {spawnSync as 启动} from 'node:child_process';
import assert from 'node:assert/strict';

const 发行 = 路径.resolve(process.argv[2]);
const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy原壳导航-'));
try {
  文件系统.mkdirSync(路径.join(根, '子目录'));
  文件系统.writeFileSync(路径.join(根, '子目录', '正文.txt'), '中文导航正文');
  const 命令 = ['「文件」之「当前」', '「文件」之「列出」', '「文件」之「切换」于「子目录」', '「文件」之「当前」', '「文件」之「读取」于「正文.txt」', '「文件」之「建目录」于「新目录」', '「文件」之「切换」于「不存在」', '「文件」之「当前」', '「文件」之「读取」于「正文.txt」', '「文件」之「切换」于「..」', '「文件」之「当前」', '「退出」'];
  const 果 = 启动(process.execPath, [路径.join(发行, '启动.mjs'), '--授权目录', '桌面=' + 根], {input: 命令.join('\n') + '\n', encoding: 'utf8', timeout: 120000});
  assert.equal(果.status, 0, 果.stderr);
  assert.equal((果.stdout.match(/中文导航正文/g) || []).length, 2, 果.stdout);
  assert.equal((果.stdout.match(/壳> \/子目录/g) || []).length, 2, 果.stdout);
  assert.ok(果.stdout.includes('子目录') && 果.stdout.includes('错误：') && 果.stdout.includes('壳已退出'), 果.stdout);
  assert.ok(文件系统.statSync(路径.join(根, '子目录', '新目录')).isDirectory());
  const 只读 = 启动(process.execPath, [路径.join(发行, '启动.mjs'), '--授权只读目录', '桌面=' + 根], {input: '「文件」之「读取」于「子目录/正文.txt」\n「文件」之「建目录」于「拒绝目录」\n「退出」\n', encoding: 'utf8', timeout: 120000});
  assert.equal(只读.status, 0, 只读.stderr);
  assert.ok(只读.stdout.includes('中文导航正文') && 只读.stdout.includes('错误'), 只读.stdout);
  assert.ok(!文件系统.existsSync(路径.join(根, '拒绝目录')));
  console.log('正式原壳授权目录列表、导航、读取、失败保路径、相对建目录与只读拒写通过');
} finally {
  文件系统.rmSync(根, {recursive: true, force: true});
}
