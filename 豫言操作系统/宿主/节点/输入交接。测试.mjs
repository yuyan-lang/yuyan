// 汉语：真实管道依次交给父行读取、子程序和父后续读取，验证父端不预取子输入。文言：实管次授父行读、子程序与父续读，验父不先夺子之入。
import test from 'node:test';
import assert from 'node:assert/strict';
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import {spawnSync as 同步启动} from 'node:child_process';

test('父读命令、子读一行、父续读共享实际输入且不预读', () => {
  const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy输入交接-'));
  const 父 = 路径.join(根, '父.mjs'), 子 = 路径.join(根, '子.mjs');
  文件系统.writeFileSync(子, `import * as 文件系统 from 'node:fs'; const 字 = Buffer.alloc(1), 字们 = []; while (文件系统.readSync(0, 字, 0, 1, null)) { 字们.push(字[0]); if (字[0] === 10) break; } process.stdout.write(Buffer.from(字们));`);
  文件系统.writeFileSync(父, `import {创建能力, 带型实现, 能力清理} from ${JSON.stringify(new URL('./应用宿主.mjs', import.meta.url).href)};
    const 能 = 创建能力({授权: {目录: new Map(), 子程序: new Map([['子', {入口: ${JSON.stringify(子)}, 目录: ${JSON.stringify(根)}}]]), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: ${JSON.stringify(父)}});
    const 解 = new TextDecoder(), 编 = new TextEncoder();
    const 首 = 能[带型实现].标准库.尝试读取标准输入行();
    const 果 = 能[带型实现].诺节宿主.诺节运行继承输入子程序(编.encode('子'), [], []);
    const 尾 = 能[带型实现].标准库.尝试读取标准输入行();
    process.stdout.write(JSON.stringify([解.decode(首[1]), Number(果[0]), Number(果[1]), 解.decode(果[2]), 解.decode(尾[1])]));
    await 能[能力清理]();`);
  try {
    const 果 = 同步启动(process.execPath, [父], {input: '运行子程序\n中文输入\n父续行\n', encoding: 'utf8', timeout: 120000});
    assert.equal(果.status, 0, 果.stderr);
    assert.deepEqual(JSON.parse(果.stdout), ['运行子程序', 0, 0, '中文输入\n', '父续行']);
  } finally { 文件系统.rmSync(根, {recursive: true, force: true}); }
});
