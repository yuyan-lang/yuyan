// 文言：浏览器宿主支持清单与宿主实现表相合之验，不需浏览器与 Wasm 之产物：清单之式；适配不依浏览器所不供之平台包；书“宿主”之接口，其术皆在实现表；
//       平台接口包所声之函与实现表之字段一一相同；以最小之客器造宿主，表之每项皆得其术，有无悬栈皆可造带型之导入。
// 汉语：浏览器宿主支持清单（宿主支持清单.tsv）与宿主实现表（宿主.mjs 的 浏览器平台导入）的一致性单测，不需要浏览器与 Wasm 产物（CI 全树测试可直接跑）：
//   1. 清单格式：每行恰三列且不空，接口不重复；接口目录存在；清单文件存在且其接口名称是「豫言操作系统」加接口名；适配列是目录时，该适配目录存在。
//   2. 适配按依赖传递不得用到浏览器不提供的平台接口包（云工宿主、诺节宿主、系统库调用、安全外壳密码，以及其他带 。接口。豫 的非接口包），
//      依赖的其他操作系统接口也须在清单里。
//   3. 适配列写“宿主”的接口：清单里方向为“宿主”的函数，以（接口包名，函数名）在实现表里都找得到。
//   4. 平台接口包（浏览器宿主、中央张量宿主）：接口文件声明的函数与实现表的字段逐一相同；包还没有接口文件时跳过。
//   5. 用最小 Wasm 程序构造浏览器宿主：实现表每项都对得上能力表里的实现，有无 JSPI 都能造出带类型导入。
// 运行：node --test 豫言操作系统/宿主/浏览器/宿主支持清单。测试.mjs
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync, readdirSync, existsSync} from 'node:fs';
import {join, dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {浏览器平台导入, 创建浏览器宿主} from './宿主.mjs';

const 本目录 = dirname(fileURLToPath(import.meta.url));
const 仓根 = join(本目录, '..', '..', '..');
const 读 = 径 => readFileSync(join(仓根, 径), 'utf8');
const 有 = 径 => existsSync(join(仓根, 径));

// 文言：析清单：去行尾回车，略空行与 # 注行。汉语：解析清单：去掉行尾回车，跳过空行与 # 开头的注释行。
const 清单行们 = 读('豫言操作系统/宿主/浏览器/宿主支持清单.tsv').split('\n').map(行 => 行.replace(/\r$/u, ''))
  .filter(行 => 行 && !行.startsWith('#')).map(行 => 行.split('\t'));
const 清单接口们 = new Set(清单行们.map(([接口]) => 接口));

// 文言：遍 库/ 之包述，得名→包述之径。汉语：遍历 库/ 下的包描述，得到包名到包描述路径的表（跳过 *_v0、node_modules 与隐藏目录）。
const 包述表 = new Map();
const 遍 = 目录 => {
  for (const 项 of readdirSync(join(仓根, 目录), {withFileTypes: true})) {
    if (项.isDirectory()) {
      if (!/_v0$/u.test(项.name) && 项.name !== 'node_modules' && !项.name.startsWith('.')) 遍(join(目录, 项.name));
    } else if (项.name.endsWith('。包。豫')) {
      const 名 = /「名称」者『([^』]+)』/u.exec(读(join(目录, 项.name)))?.[1];
      if (名 && !包述表.has(名)) 包述表.set(名, join(目录, 项.name));
    }
  }
};
遍('库');
const 适配包述表 = new Map();
const 遍适配包 = 目录 => {
  for (const 项 of readdirSync(join(仓根, 目录), {withFileTypes: true})) {
    if (!项.isDirectory()) continue;
    const 子目录 = join(目录, 项.name);
    const 包述 = readdirSync(join(仓根, 子目录)).find(名 => 名.endsWith('适配。包。豫'));
    if (包述) {
      const 名 = /「名称」者『([^』]+)』/u.exec(读(join(子目录, 包述)))?.[1];
      if (名) 适配包述表.set(名, join(子目录, 包述));
    }
    遍适配包(子目录);
  }
};
遍适配包('豫言操作系统/适配');
const 依赖们 = 包述径 => [...(/「依赖」者\s*「列」【([^】]*)】/u.exec(读(包述径))?.[1] ?? '').matchAll(/『([^』]+)』/gu)].map(配 => 配[1]);
// 文言：着色器之接口篇供着色之译，不成宿主之导入，不计。汉语：着色器接口文件（。着色器。接口。豫）给着色语言翻译用，不编成宿主导入，不算。
const 接口文件们 = 目录 => (有(目录) ? readdirSync(join(仓根, 目录)).filter(名 => 名.endsWith('。接口。豫') && !名.endsWith('。着色器。接口。豫')) : []);

// 文言：平台接口包者，含接口之篇而非操作系统接口、非工具模之包；浏览器所供者惟实现表所列。
// 汉语：平台接口包：带 。接口。豫、又不是操作系统接口与工具模块（标准库、构建基础）的包，另加已知的几个；浏览器只提供实现表里的那些。
const 工具模块们 = new Set(['标准库', '构建基础']);
const 已知平台包们 = new Set(['云工宿主', '诺节宿主', '系统库调用', '安全外壳密码']);
const 浏览器所供 = new Set(Object.keys(浏览器平台导入));
const 是平台包 = 名 => 已知平台包们.has(名) ||
  (!工具模块们.has(名) && !名.startsWith('豫言操作系统') && 包述表.has(名) && 接口文件们(dirname(包述表.get(名))).length > 0);

test('清单格式：三列不空、接口不重复、文件与目录都在', () => {
  assert.ok(清单行们.length > 0, '清单没有内容');
  const 见 = new Set();
  for (const 行 of 清单行们) {
    assert.equal(行.length, 3, '清单的行须恰有三列：' + 行.join('⇥'));
    const [接口, 适配, 清单文件] = 行;
    assert.ok(接口 && 适配 && 清单文件, '清单的行有空列：' + 行.join('⇥'));
    assert.ok(!见.has(接口), '清单里接口重复：' + 接口);
    见.add(接口);
    assert.ok(有('豫言操作系统接口/' + 接口), '没有这个接口目录：' + 接口);
    assert.ok(有(清单文件), '清单文件不存在：' + 清单文件);
    assert.equal(JSON.parse(读(清单文件)).接口名称, '豫言操作系统' + 接口, '清单文件的接口名称与行不符：' + 清单文件);
    if (适配 !== '-' && 适配 !== '宿主') assert.ok(有('豫言操作系统/适配/' + 适配), '适配目录不存在：' + 适配);
  }
});

test('适配不用浏览器不提供的平台接口包，所依赖的接口也在清单里', () => {
  for (const [接口, 适配] of 清单行们) {
    if (适配 === '-' || 适配 === '宿主') continue;
    const 目录 = '豫言操作系统/适配/' + 适配;
    const 述 = readdirSync(join(仓根, 目录)).find(名 => 名.endsWith('。包。豫'));
    assert.ok(述, '适配目录没有包描述：' + 适配);
    const 待 = 依赖们(join(目录, 述)), 见 = new Set();
    while (待.length) {
      const 名 = 待.pop();
      if (见.has(名)) continue;
      见.add(名);
      if (适配包述表.has(名)) { 待.push(...依赖们(适配包述表.get(名))); continue; }
      if (名.startsWith('豫言操作系统')) {
        const 他接口 = 名.slice('豫言操作系统'.length);
        assert.ok(他接口 === 接口 || 清单接口们.has(他接口), `适配「${适配}」依赖的接口「${他接口}」不在浏览器宿主支持清单里`);
        continue;
      }
      assert.ok(!是平台包(名) || 浏览器所供.has(名), `适配「${适配}」（经依赖）用到浏览器不提供的平台接口包「${名}」`);
      if (包述表.has(名)) 待.push(...依赖们(包述表.get(名)));
    }
  }
});

test('写“宿主”的接口，其宿主函数都在实现表里', () => {
  for (const [, 适配, 清单文件] of 清单行们) {
    if (适配 !== '宿主') continue;
    const 清单 = JSON.parse(读(清单文件));
    for (const 函 of 清单.函数) {
      if (函.方向 !== '宿主') continue;
      assert.equal(typeof 浏览器平台导入[清单.接口名称]?.[函.函数], 'string', `实现表缺 ${清单.接口名称}.${函.函数}`);
    }
  }
});

test('平台接口包声明的函数与实现表的字段逐一相同', t => {
  for (const 包 of 浏览器所供) {
    const 文件们 = 包述表.has(包) ? 接口文件们(dirname(包述表.get(包))) : [];
    if (文件们.length === 0) { t.diagnostic(包 + ' 还没有接口文件（或包还没建），跳过'); continue; }
    const 目录 = dirname(包述表.get(包));
    const 声明 = 文件们.flatMap(名 => [...读(join(目录, 名)).matchAll(/^「([^」]+)」乃/gmu)].map(配 => 配[1])).sort();
    assert.deepEqual(声明, Object.keys(浏览器平台导入[包]).sort(), 包 + ' 的接口声明与实现表不一致');
  }
});

// 文言：手造一模：导入若干函数而附边界段，导出空之 _start。汉语：手工拼一个模块：导入若干函数（Wasm 类型都写成无参无果，JS 函数导入不核类型），附「豫言边界」段，导出空的 _start。
const 编码器 = new TextEncoder();
const 无号 = 数 => { const 字节 = []; do { let 字 = 数 & 127; 数 >>>= 7; if (数) 字 |= 128; 字节.push(字); } while (数); return 字节; };
const 名 = 文 => { const 字节 = [...编码器.encode(文)]; return [...无号(字节.length), ...字节]; };
const 段 = (号, 体) => [号, ...无号(体.length), ...体];
const 造程序 = 导入们 => new WebAssembly.Module(new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0,
  ...段(1, [1, 0x60, 0, 0]),
  ...段(2, [...无号(导入们.length), ...导入们.flatMap(([模, 字段]) => [...名(模), ...名(字段), 0, 0])]),
  ...段(3, [1, 0]),
  ...段(7, [1, ...名('_start'), 0, ...无号(导入们.length)]),
  ...段(10, [1, 2, 0, 0x0b]),
  ...段(0, [...名('豫言边界'), ...编码器.encode(导入们.map(([模, 字段, 签名]) => `导入\t${模}\t${字段}\t${签名}\n`).join(''))])]));
const 字段之 = 旧名 => {
  for (const [模, 字段们] of Object.entries(浏览器平台导入)) for (const [字段, 名] of Object.entries(字段们)) if (名 === 旧名) return [模, 字段];
  throw Error('实现表里没有对到 ' + 旧名 + ' 的字段');
};

test('用最小程序构造浏览器宿主：实现表每项都有实现，有无 JSPI 都能造带类型导入', async () => {
  // 文言：取异步、同步、中央张量各一二。汉语：挑几个：异步的请求文字，同步的文档句柄，中央张量的启用（异步）与读单精（同步、小数结果）。
  const 导入们 = [['豫言_浏览器_请求文字', '串，串，串→串'], ['豫言_浏览器_文档句柄', '→串'],
    ['豫言_中央张量_启用', '→组〔整，整，整，串〕'], ['豫言_中央张量_读单精', '整→小']].map(([旧名, 签名]) => [...字段之(旧名), 签名]);
  const 程序模块 = 造程序(导入们), 值桥模块 = 造程序([]);
  class 根类 extends EventTarget { getElementById() { return null; } }
  const 起 = () => 创建浏览器宿主({程序模块, 值桥模块, 根: new 根类(), 全局: globalThis, 路径: 'http://127.0.0.1/'}).完成;
  await 起();
  const 原悬 = WebAssembly.Suspending, 原承 = WebAssembly.promising;
  try {
    WebAssembly.Suspending = undefined;
    WebAssembly.promising = undefined;
    await 起();
  } finally {
    WebAssembly.Suspending = 原悬;
    WebAssembly.promising = 原承;
  }
});
