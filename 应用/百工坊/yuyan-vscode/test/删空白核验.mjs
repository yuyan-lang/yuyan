// 文言：辞之白无义，删之不当易色；逐字较原文与删白诸变体之 TextMate 类别。
// 汉语：豫言编译器忽略空格与换行，删去它们不应改变任何非空白字的颜色；本模块逐字比较原文与各删空白变体的 TextMate 类别。
import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {读取TextMate语法, TextMate逐字类别} from './语法高亮核验.mjs';

const 空白 = 字 => 字 === ' ' || 字 === '\n';

// 文言：删白而成「： 或 ：」者，词法实变，不在此核。
// 汉语：删空白后若拼出「：或：」，注释与转义的定界符真正改变，这类删除不核验。
const 拼成定界符 = (前, 后) => (前 === '「' && 后 === '：') || (前 === '：' && 后 === '」');

export const 删法们 = Object.freeze({
  全删: () => [],
  只删换行: (序, 段) => 段.filter(字 => 字 !== '\n'),
  只删空格: (序, 段) => 段.filter(字 => 字 !== ' '),
  删奇数段: (序, 段) => (序 % 2 ? [] : 段),
  删偶数段: (序, 段) => (序 % 2 ? 段 : [])
});

export function 删空白(源码, 删法) {
  const 字们 = Array.from(源码), 出 = [];
  let 序 = 0, 前字 = '';
  for (let i = 0; i < 字们.length;) {
    if (!空白(字们[i])) { 前字 = 字们[i]; 出.push(字们[i++]); continue; }
    let j = i;
    while (j < 字们.length && 空白(字们[j])) j++;
    const 段 = 字们.slice(i, j);
    let 新段 = 删法(序++, 段);
    if (新段.length === 0 && 拼成定界符(前字, 字们[j] ?? '')) 新段 = 段;
    出.push(...新段);
    i = j;
  }
  return 出.join('');
}

function 非空白类别(源码, 语法) {
  const {字们, 类别} = TextMate逐字类别(源码, 语法);
  const 结果 = [];
  let 行 = 1;
  for (let i = 0; i < 字们.length; i++) {
    if (字们[i] === '\n') 行++;
    if (!空白(字们[i])) 结果.push({字: 字们[i], 类: 类别[i], 行});
  }
  return 结果;
}

// 文言：返所异之段，各记删法、行、原文及前后之类。
// 汉语：返回颜色改变的连续段，记录删法、原文行号、原文与删前删后的类别。
export function 核验删空白(源码, 语法, 选用删法 = 删法们) {
  const 原 = 非空白类别(源码, 语法);
  const 差异 = [];
  for (const [删法名, 删法] of Object.entries(选用删法)) {
    const 变 = 非空白类别(删空白(源码, 删法), 语法);
    if (变.length !== 原.length) throw new Error(`删法 ${删法名} 改变了非空白字数`);
    for (let i = 0; i < 原.length;) {
      if (原[i].类 === 变[i].类) { i++; continue; }
      const 首 = i;
      while (i < 原.length && 原[i].类 !== 变[i].类) i++;
      const 段 = (列, 键) => [...new Set(列.slice(首, i).map(项 => 项[键]))].join('/');
      差异.push({删法: 删法名, 行: 原[首].行, 原文: 段(原, '字'), 删前: 段(原, '类'), 删后: 段(变, '类')});
    }
  }
  return 差异;
}

// 文言：用法：node 删空白核验.mjs 仓根 文件表 [第几片/共几片]；文件表每行一相对路径。
// 汉语：用法：node 删空白核验.mjs 仓根 文件表 [第几片/共几片]。文件表每行一个相对路径；分片从 0 起，便于多进程并行。
async function 主程序(根, 文件表, 分片 = '0/1') {
  const [第几, 共几] = 分片.split('/').map(Number);
  const 语法 = await 读取TextMate语法();
  const 文件们 = fs.readFileSync(文件表, 'utf8').split('\n').filter(Boolean)
    .filter((_, 序) => 序 % 共几 === 第几);
  const 汇总 = {文件数: 0, 有差文件数: 0, 差异段数: 0, 按删法: {}, 按类别: {}, 差异: []};
  for (const 文件 of 文件们) {
    const 差异 = 核验删空白(fs.readFileSync(path.join(根, 文件), 'utf8'), 语法);
    汇总.文件数++;
    if (差异.length) 汇总.有差文件数++;
    for (const 项 of 差异) {
      汇总.差异段数++;
      汇总.按删法[项.删法] = (汇总.按删法[项.删法] || 0) + 1;
      const 键 = `${项.删前} → ${项.删后}`;
      汇总.按类别[键] = (汇总.按类别[键] || 0) + 1;
      汇总.差异.push({文件, ...项});
    }
  }
  const 报告 = path.join(根, `yy删空白差异-${第几}.json`);
  fs.writeFileSync(报告, JSON.stringify(汇总, null, 1));
  process.stdout.write(`核验 ${汇总.文件数} 文件，有差 ${汇总.有差文件数}，差异 ${汇总.差异段数} 段；报告：${报告}\n`);
  for (const 项 of 汇总.差异.slice(0, 20)) {
    process.stdout.write(`${项.文件}:${项.行} 【${项.删法}】 ${JSON.stringify(项.原文)} ${项.删前} → ${项.删后}\n`);
  }
  if (汇总.差异段数) process.exitCode = 1;
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  主程序(process.argv[2], process.argv[3], process.argv[4]).catch(错误 => {
    process.stderr.write(`${错误.stack || 错误}\n`);
    process.exitCode = 2;
  });
}
