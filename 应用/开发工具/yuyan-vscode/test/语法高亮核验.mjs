// 文言：编器定实类，TextMate 定所见；惟同位同类乃合。
// 汉语：逐文件比较解析器语义标记与真实 TextMate 分词，保留原始 scope 供复查。
import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import oniguruma from 'vscode-oniguruma';
import textmate from 'vscode-textmate';

const 语义类别 = Object.freeze({
  结构操作符: '结构', 类型操作符: '类型', 控制操作符: '控制', 普通操作符: '操作',
  内建类型: '内建类型', 内建函数: '内建函数', 内建常量: '内建常量',
  绑定标识符: '标识', 引用标识符: '标识', 数值: '数值', 字符串: '字符串',
  注释: '注释', 结构终止符: '结构终止'
});

function 解析器字类(种类, 标记文字, 字) {
  if (种类 === '普通操作符' && /^[（）【】，]$/u.test(字)) return '标点';
  if (种类 === '普通操作符' && /^《[^《》]+》$/u.test(标记文字)) return '标签';
  return 语义类别[种类];
}

export function TextMate类别(scopes) {
  if (scopes.some(scope => scope.startsWith('comment.block.yuyan'))) return '注释';
  if (scopes.some(scope => scope.startsWith('string.quoted.other.yuyan'))) return '字符串';
  if (scopes.some(scope => scope.startsWith('support.type.builtin.yuyan'))) return '内建类型';
  if (scopes.some(scope => scope.startsWith('support.function.builtin.yuyan'))) return '内建函数';
  if (scopes.some(scope => scope.startsWith('constant.language.builtin.yuyan'))) return '内建常量';
  if (scopes.some(scope => scope.startsWith('constant.numeric.yuyan'))) return '数值';
  if (scopes.some(scope => scope.startsWith('entity.name.label.yuyan'))) return '标签';
  if (scopes.some(scope => scope.startsWith('markup.bold.structure.terminator.yuyan'))) return '结构终止';
  if (scopes.some(scope => scope.startsWith('markup.bold.structure.yuyan'))) return '结构';
  if (scopes.some(scope => scope.startsWith('storage.type.function.yuyan'))) return '类型';
  if (scopes.some(scope => scope.startsWith('keyword.control'))) return '控制';
  if (scopes.some(scope => scope.startsWith('keyword.operator'))) return '操作';
  if (scopes.some(scope => scope.startsWith('variable.other.yuyan') || scope.startsWith('punctuation.definition.variable.'))) return '标识';
  if (scopes.some(scope => scope.startsWith('punctuation.section.') || scope.startsWith('punctuation.separator.'))) return '标点';
  return '其他';
}

export async function 读取TextMate语法(文件 = new URL('../yuyan.tmGrammar.json', import.meta.url)) {
  const wasm = fs.readFileSync(fileURLToPath(import.meta.resolve('vscode-oniguruma/release/onig.wasm')));
  await oniguruma.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));
  const registry = new textmate.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: patterns => new oniguruma.OnigScanner(patterns),
      createOnigString: value => new oniguruma.OnigString(value)
    }),
    loadGrammar: async scope => scope === 'source.yuyan' ? JSON.parse(fs.readFileSync(文件, 'utf8')) : null
  });
  return registry.loadGrammar('source.yuyan');
}

function UTF16至字位(源码) {
  const 对照 = new Uint32Array(源码.length + 1);
  let 字位 = 0;
  for (let 码元 = 0; 码元 < 源码.length;) {
    const 宽 = 源码.codePointAt(码元) > 0xffff ? 2 : 1;
    for (let i = 0; i < 宽; i++) 对照[码元 + i] = 字位;
    码元 += 宽;
    字位++;
    对照[码元] = 字位;
  }
  return 对照;
}

export function TextMate逐字类别(源码, 语法) {
  const 字们 = Array.from(源码), 对照 = UTF16至字位(源码);
  const 类别 = Array(字们.length).fill('其他'), 作用域 = Array(字们.length).fill(null);
  let 状态 = textmate.INITIAL, 起点 = 0;
  for (const 行 of 源码.split('\n')) {
    const 结果 = 语法.tokenizeLine(行, 状态);
    状态 = 结果.ruleStack;
    for (const 词 of 结果.tokens) {
      const 始 = 对照[起点 + 词.startIndex], 终 = 对照[起点 + Math.min(词.endIndex, 行.length)];
      const 种类 = TextMate类别(词.scopes);
      for (let i = 始; i < 终; i++) { 类别[i] = 种类; 作用域[i] = 词.scopes; }
    }
    起点 += 行.length + 1;
  }
  return {字们, 类别, 作用域};
}

function 位置(行首们, 偏移) {
  let 左 = 0, 右 = 行首们.length;
  while (左 + 1 < 右) {
    const 中 = (左 + 右) >> 1;
    if (行首们[中] <= 偏移) 左 = 中; else 右 = 中;
  }
  return {行: 左 + 1, 列: 偏移 - 行首们[左] + 1};
}

export function 对照文件(源码, 标记们, 语法, 文件名) {
  const {字们, 类别, 作用域} = TextMate逐字类别(源码, 语法);
  const 行首们 = [0];
  for (let i = 0; i < 字们.length; i++) if (字们[i] === '\n') 行首们.push(i + 1);
  const 差异 = [];
  let 上一终点 = 0, 对照字数 = 0;
  for (const 标记 of 标记们) {
    const {开始, 结束, 种类} = 标记;
    const 标记文字 = 字们.slice(开始, 结束).join('');
    if (!语义类别[种类] || !Number.isSafeInteger(开始) || !Number.isSafeInteger(结束) ||
        开始 < 上一终点 || 结束 <= 开始 || 结束 > 字们.length) {
      throw new Error(`解析器标记范围或类别无效：${JSON.stringify(标记)}`);
    }
    上一终点 = 结束;
    for (let i = 开始; i < 结束; i++) if (字们[i] !== '\n') 对照字数++;
    for (let i = 开始; i < 结束;) {
      if (字们[i] === '\n') { i++; continue; }
      const 期望 = 解析器字类(种类, 标记文字, 字们[i]);
      if (类别[i] === 期望) { i++; continue; }
      const 错类 = 类别[i], 首 = i;
      while (i < 结束 && 字们[i] !== '\n' && 类别[i] === 错类 &&
        解析器字类(种类, 标记文字, 字们[i]) === 期望) i++;
      差异.push({文件: 文件名, ...位置(行首们, 首), 开始: 首, 结束: i,
        原文: 字们.slice(首, i).join(''), 解析器: 期望, TextMate: 错类, scopes: 作用域[首] || []});
    }
  }
  return {差异, 对照字数, 差异字数: 差异.reduce((总数, 项) => 总数 + 项.结束 - 项.开始, 0)};
}

async function 主程序(根, 记录路径, 豫言程序) {
  const 语法 = await 读取TextMate语法();
  const 超时毫秒 = Math.max(1000, Number(process.env.YY_GRAMMAR_TIMEOUT_MS) || 5000);
  const 缓存目录 = path.join(根, '.yybuild', '语法高亮核验');
  fs.mkdirSync(缓存目录, {recursive: true});
  const 解析器身份 = createHash('sha256')
    .update(fs.readFileSync(path.resolve(根, 豫言程序)))
    .update(fs.readFileSync(path.join(根, 'yy语法高亮上下文.txt')))
    .digest('hex');
  const 汇总 = {文件数: 0, 已解析: 0, 缓存命中: 0, 解析失败: [], 超时: [], 对照失败: [], 对照字数: 0, 差异数: 0, 差异字数: 0, 差异类别: {}, 差异: []};
  for (const 文件 of fs.readFileSync(记录路径, 'utf8').split('\n')) {
    if (!文件) continue;
    汇总.文件数++;
    if (汇总.文件数 % 25 === 0) process.stdout.write(`进度 ${汇总.文件数}\n`);
    let 源码;
    try { 源码 = fs.readFileSync(path.join(根, 文件), 'utf8'); }
    catch (错误) { 汇总.对照失败.push({文件, 错误: String(错误)}); continue; }
    const 缓存键 = createHash('sha256').update(解析器身份).update(文件).update(源码).digest('hex');
    const 缓存文件 = path.join(缓存目录, `${缓存键}.json`);
    let 输出;
    if (fs.existsSync(缓存文件)) {
      输出 = fs.readFileSync(缓存文件, 'utf8');
      汇总.缓存命中++;
    } else {
      const 解析 = spawnSync(豫言程序, ['文件', 根, 文件], {
        cwd: 根, encoding: 'utf8', timeout: 超时毫秒, maxBuffer: 32 * 1024 * 1024
      });
      if (解析.error?.code === 'ETIMEDOUT') {
        汇总.超时.push({文件, 毫秒: 超时毫秒});
        continue;
      }
      if (解析.error || 解析.status !== 0) {
        汇总.解析失败.push({文件, 错误: String(解析.error || 解析.stderr || `退出 ${解析.status}`)});
        continue;
      }
      if (fs.readFileSync(path.join(根, 文件), 'utf8') !== 源码) {
        汇总.对照失败.push({文件, 错误: '源码在解析期间发生变化'});
        continue;
      }
      输出 = 解析.stdout;
    }
    let 记录;
    try { 记录 = JSON.parse(输出); }
    catch (错误) { 汇总.对照失败.push({文件, 错误: `解析器输出不是 JSON：${String(错误)}`}); continue; }
    if (记录.源文件 !== 文件) { 汇总.对照失败.push({文件, 错误: '解析器返回的源文件不匹配'}); continue; }
    if (!fs.existsSync(缓存文件)) fs.writeFileSync(缓存文件, 输出);
    if (记录.错误 !== undefined) { 汇总.解析失败.push({文件, 错误: 记录.错误}); continue; }
    try {
      const 结果 = 对照文件(源码, 记录.标记, 语法, 文件);
      汇总.已解析++;
      汇总.对照字数 += 结果.对照字数;
      汇总.差异数 += 结果.差异.length;
      汇总.差异字数 += 结果.差异字数;
      for (const 差异 of 结果.差异) {
        const 键 = `${差异.解析器} → ${差异.TextMate}`;
        汇总.差异类别[键] = (汇总.差异类别[键] || 0) + 差异.结束 - 差异.开始;
        汇总.差异.push(差异);
      }
    } catch (错误) { 汇总.对照失败.push({文件, 错误: String(错误)}); }
  }
  const 输出 = path.join(根, 'yy语法高亮差异.json');
  fs.writeFileSync(输出, JSON.stringify(汇总, null, 2));
  process.stdout.write(`核验 ${汇总.文件数} 文件，解析成功 ${汇总.已解析}，缓存命中 ${汇总.缓存命中}，对照 ${汇总.对照字数} 字，差异 ${汇总.差异数} 段／${汇总.差异字数} 字，解析失败 ${汇总.解析失败.length}，超时 ${汇总.超时.length}，对照失败 ${汇总.对照失败.length}\n`);
  for (const 项 of 汇总.差异.slice(0, 20)) process.stdout.write(`${项.文件}:${项.行}:${项.列} ${JSON.stringify(项.原文)} 解析器=${项.解析器} TextMate=${项.TextMate}\n`);
  process.stdout.write(`完整报告：${输出}\n`);
  // 文言：未可析者置之，惟已析之色差及核验故障为败。
  // 汉语：解析失败只记入报告，不影响可解析文件的核验结果。
  if (汇总.差异数 || 汇总.对照失败.length) process.exitCode = 1;
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  主程序(process.argv[2], process.argv[3], process.argv[4]).catch(错误 => { process.stderr.write(`${错误.stack || 错误}\n`); process.exitCode = 2; });
}
