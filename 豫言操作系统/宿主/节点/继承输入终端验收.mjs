// 汉语：真实PTY交互验收，依次输入父命令、子输入及不带换行的父续键；子首次读取标记用于核对标准模式。文言：实PTY交互之验，次输父命令、子输入与无换行之父续键；子初读之记用核常制。
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 系统 from 'node:os';
import assert from 'node:assert/strict';
import {创建能力, 带型实现, 能力清理} from './应用宿主.mjs';
const 根 = 文件系统.mkdtempSync(路径.join(系统.tmpdir(), 'yy终端交接-'));
const 子 = 路径.join(根, '子.mjs'), 标记 = 路径.join(根, 'yy首次读取');
文件系统.writeFileSync(子, `import * as 文件系统 from 'node:fs'; const 字=Buffer.alloc(1), 字们=[]; while(文件系统.readSync(0,字,0,1,null)){文件系统.writeFileSync(${JSON.stringify(标记)},'已读');字们.push(字[0]);if(字[0]===10)break;}process.stdout.write(Buffer.from(字们));`);
const 能 = 创建能力({授权: {目录: new Map(), 子程序: new Map([['子', {入口: 子, 目录: 根}]]), 源: new Set(), 环境: new Set()}, 应用参数: [], 程序路径: import.meta.filename});
try {
  const 标准 = 能[带型实现].标准库;
  assert.equal(标准.进入终端原始输入模式(), true, '须在真实终端运行');
  console.log('父命令>');
  let 文 = '';
  for (;;) { const [有键, 键] = 标准.读取终端按键(); if (!有键) continue; if (键 === '\r' || 键 === '\n') break; 文 += 键; }
  assert.equal(文, '父命令');
  console.log('子输入>；首次读取标记：' + 标记);
  const 果 = 能[带型实现].诺节宿主.诺节运行继承输入子程序(new TextEncoder().encode('子'), [], []);
  assert.equal(Number(果[0]), 0); assert.equal(Number(果[1]), 0);
  assert.equal(new TextDecoder().decode(果[2]), '子输入\n');
  console.log('父续键>（输入 x，不带换行）');
  let 尾;
  do { 尾 = 标准.读取终端按键(); } while (!尾[0]);
  assert.equal(尾[1], 'x');
  console.log('真实PTY父子输入及父原始模式恢复通过');
} finally { await 能[能力清理](); 文件系统.rmSync(根, {recursive: true, force: true}); }
