const fs = require('fs');
const path = require('path');
const { createRequire } = require('module');

const 仓库目录 = path.resolve(__dirname, '..');
const 扩展目录 = path.join(仓库目录, 'yuyan-vscode');
const 扩展依赖 = createRequire(path.join(扩展目录, 'package.json'));
const oniguruma = 扩展依赖('vscode-oniguruma');
const textmate = 扩展依赖('vscode-textmate');

const 语法路径 = path.join(扩展目录, 'yuyan.tmGrammar.json');
const 豫言代码块 = /<code data-language="yuyan">([\s\S]*?)<\/code>/g;

function 解码网页文字(文字) {
  return 文字
    .replace(/&quot;/g, '"')
    .replace(/&gt;/g, '>')
    .replace(/&lt;/g, '<')
    .replace(/&amp;/g, '&');
}

function 转义网页文字(文字) {
  return 文字
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

function 包含作用域(作用域们, 前缀) {
  return 作用域们.some(作用域 => 作用域.startsWith(前缀));
}

function 作用域类别(作用域们) {
  if (包含作用域(作用域们, 'constant.character.escape.')) return 'escape';
  if (包含作用域(作用域们, 'comment.')) return 'comment';
  if (包含作用域(作用域们, 'string.')) return 'string';
  if (包含作用域(作用域们, 'support.type.builtin.')) return 'builtin-type';
  if (包含作用域(作用域们, 'support.function.builtin.')) return 'builtin-function';
  if (包含作用域(作用域们, 'constant.language.builtin.')) return 'builtin-constant';
  if (包含作用域(作用域们, 'constant.numeric.')) return 'number';
  if (包含作用域(作用域们, 'markup.bold.structure.terminator.')) return 'structure-terminator';
  if (包含作用域(作用域们, 'markup.bold.structure.')) return 'structure';
  if (包含作用域(作用域们, 'storage.type.')) return 'type';
  if (包含作用域(作用域们, 'keyword.control.')) return 'control';
  if (包含作用域(作用域们, 'keyword.operator.')) return 'operator';
  if (包含作用域(作用域们, 'keyword.')) return 'operator';
  if (包含作用域(作用域们, 'entity.name.label.')) return 'label';
  if (包含作用域(作用域们, 'variable.other.')) return 'identifier';
  if (包含作用域(作用域们, 'punctuation.')) return 'punctuation';
  return undefined;
}

function 着色片段(文字, 类别) {
  const 已转义 = 转义网页文字(文字);
  return 类别 === undefined ? 已转义 : `<span class="tok-${类别}">${已转义}</span>`;
}

async function 加载语法() {
  const wasm = fs.readFileSync(扩展依赖.resolve('vscode-oniguruma/release/onig.wasm'));
  await oniguruma.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));

  const 注册表 = new textmate.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: 模式们 => new oniguruma.OnigScanner(模式们),
      createOnigString: 文字 => new oniguruma.OnigString(文字)
    }),
    loadGrammar: async 作用域名称 => {
      if (作用域名称 !== 'source.yuyan') return null;
      return JSON.parse(fs.readFileSync(语法路径, 'utf8'));
    }
  });

  return 注册表.loadGrammar('source.yuyan');
}

function 高亮源码(源码, 语法) {
  const 行们 = 源码.split('\n');
  let 规则栈 = textmate.INITIAL;
  const 结果行们 = [];

  for (const 行 of 行们) {
    const 结果 = 语法.tokenizeLine(行, 规则栈);
    规则栈 = 结果.ruleStack;
    const 片段们 = 结果.tokens.map(词 => {
      const 开始 = Math.min(词.startIndex, 行.length);
      const 结束 = Math.min(词.endIndex, 行.length);
      return 着色片段(行.slice(开始, 结束), 作用域类别(词.scopes));
    });
    结果行们.push(片段们.join(''));
  }

  return 结果行们.join('\n');
}

async function 高亮网页(网页) {
  const 语法 = await 加载语法();
  let 数量 = 0;
  const 结果 = 网页.replace(豫言代码块, (_完整匹配, 已转义源码) => {
    数量 += 1;
    return `<code data-language="yuyan">${高亮源码(解码网页文字(已转义源码), 语法)}</code>`;
  });
  if (数量 === 0) throw new Error('生成的网站中没有找到豫言源码代码块');
  return { 网页: 结果, 数量 };
}

async function 主函数() {
  const [, , 索引路径] = process.argv;
  if (索引路径 === undefined) {
    throw new Error('用法：node 工具/生成源码高亮.js <index.html>');
  }

  const 原网页 = fs.readFileSync(索引路径, 'utf8');
  const 结果 = await 高亮网页(原网页);
  const 临时路径 = `${索引路径}.源码高亮临时文件`;
  fs.writeFileSync(临时路径, 结果.网页);
  fs.renameSync(临时路径, 索引路径);
  process.stdout.write(`TextMate 已高亮 ${结果.数量} 个豫言源码文件\n`);
}

if (require.main === module) {
  主函数().catch(错误 => {
    process.stderr.write(`${错误.stack || 错误}\n`);
    process.exitCode = 1;
  });
}

module.exports = { 作用域类别, 高亮源码, 高亮网页, 加载语法 };
