// 文言：验云工宿主支持清单与实现之表相符：清单之行、适配之依、平台接口之声与对照表、对照表与能表。
// 汉语：云工宿主支持清单与实现表的一致性单测：
//   一、宿主支持清单.tsv 每行三列、接口名不重复；适配目录与清单文件存在，清单的接口名称是“豫言操作系统”加接口名；
//   二、所列适配只依赖云工能提供的平台接口包，所依赖的豫言操作系统接口也都在清单里；
//   三、云工宿主包的声明都由物桥按字段实现，中央张量宿主的声明与 值桥.mjs 的对照表逐一对应；
//   四、物桥的值转换与成败结果；
//   五、程序没有类型化入口时，事件报错。
// 运行：node --test 豫言操作系统/宿主/云工/宿主支持清单。测试.mjs（Node 26，需 JSPI；全树测试自动发现）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readdirSync, readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {平台导入旧名} from './值桥.mjs';
import {创建云工宿主, 云工平台包} from './宿主.mjs';
import {创建物桥} from './物桥.mjs';

const 本目录 = path.dirname(fileURLToPath(import.meta.url));
const 仓根 = path.resolve(本目录, '../../..');
const 读 = 相对 => readFileSync(path.join(仓根, 相对), 'utf8');

// 文言：析清单之行，略空行与注行。汉语：解析宿主支持清单：跳过空行与 # 注释行，其余每行按制表符切成三列。
const 清单行们 = 读('豫言操作系统/宿主/云工/宿主支持清单.tsv').split('\n')
  .map(行 => 行.replace(/\r$/u, '')).filter(行 => 行 && !行.startsWith('#')).map(行 => 行.split('\t'));
const 清单接口们 = new Set(清单行们.map(([名]) => 名));

// 文言：去豫言之注（可嵌）。汉语：去掉豫言注释「：…：」（可以嵌套），免得注释里的文字被当成声明。
const 去注释 = 文 => {
  let 果 = '', 层 = 0;
  for (let 位 = 0; 位 < 文.length; 位++) {
    if (文.startsWith('「：', 位)) { 层++; 位++; continue; }
    if (层 > 0 && 文.startsWith('：」', 位)) { 层--; 位++; continue; }
    if (层 === 0) 果 += 文[位];
  }
  return 果;
};
// 文言：取接口文件中无体之声。汉语：取一个目录里各 。接口。豫 文件的无体声明名（行首的「名」乃）。
const 接口声明 = 目录 => {
  const 全 = path.join(仓根, 目录), 名们 = new Set();
  if (!existsSync(全)) return 名们;
  for (const 文件 of readdirSync(全).filter(名 => 名.endsWith('。接口。豫'))) {
    for (const 配 of 去注释(readFileSync(path.join(全, 文件), 'utf8')).matchAll(/^「([^」]+)」乃/gmu)) 名们.add(配[1]);
  }
  return 名们;
};
// 文言：析包描述之依赖。汉语：读包描述里「依赖」者「列」【…】 中的包名。
const 包依赖 = 包文 => {
  const 配 = 去注释(包文).match(/「依赖」者「列」【([^】]*)】/u);
  return 配 ? [...配[1].matchAll(/『([^』]+)』/gu)].map(项 => 项[1]) : [];
};
const 包描述文 = 目录 => {
  const 文件 = readdirSync(path.join(仓根, 目录)).find(名 => 名.endsWith('。包。豫'));
  assert.ok(文件, '目录里没有包描述：' + 目录);
  return 读(path.join(目录, 文件));
};

// 文言：平台接口包所在之目。汉语：云工实现的平台接口包所在目录。
const 平台包目录 = {云工宿主: '库/云工宿主', 中央张量宿主: '库/中央张量宿主'};
// 文言：凡平台接口包：对照表所收者，加浏览器宿主。汉语：已知的全部平台接口包：对照表收的五个，加浏览器宿主。
const 平台包们 = new Set([...Object.keys(平台导入旧名), '浏览器宿主']);

test('一、清单每行三列，接口名不重复，适配与清单文件存在且接口名称相符', () => {
  assert.ok(清单行们.length > 0);
  for (const 行 of 清单行们) {
    assert.equal(行.length, 3, '应恰有三列：' + 行.join(' | '));
    assert.ok(行.every(列 => 列.length > 0), '列不得为空：' + 行.join(' | '));
    const [接口, 适配, 清单文件] = 行;
    if (适配 !== '宿主' && 适配 !== '-') assert.ok(existsSync(path.join(仓根, '豫言操作系统/适配', 适配)), '适配目录不存在：' + 适配);
    assert.ok(existsSync(path.join(仓根, 清单文件)), '清单文件不存在：' + 清单文件);
    assert.equal(JSON.parse(读(清单文件)).接口名称, '豫言操作系统' + 接口, '清单的接口名称不符：' + 清单文件);
  }
  assert.equal(清单接口们.size, 清单行们.length, '接口名重复');
});

test('二、适配只依赖云工提供的平台接口包，所依赖的操作系统接口都在清单里', () => {
  for (const [接口, 适配] of 清单行们) {
    if (适配 === '宿主' || 适配 === '-') continue;
    for (const 依 of 包依赖(包描述文('豫言操作系统/适配/' + 适配))) {
      if (平台包们.has(依)) assert.ok(云工平台包.includes(依), `适配「${适配}」依赖云工不提供的平台接口包「${依}」`);
      const 配 = 依.match(/^豫言操作系统(.+)$/u);
      if (配 && !配[1].endsWith('适配')) assert.ok(清单接口们.has(配[1]), `适配「${适配}」依赖的接口「${配[1]}」不在清单里`);
    }
  }
});

// 文言：物桥之表，以字段为键。汉语：物桥的实现表（键为 物宿主。接口。豫 的函数名），用桩参数造一份只为取键与做单测。
const 物桥表 = 创建物桥({取全局: 名 => globalThis[名], 取绑定: () => { throw Error('无绑定'); }, 回调: async () => [0, [1]]});

test('三、平台接口包的声明都有实现：云工宿主由物桥按字段实现，中央张量宿主按对照表', () => {
  const 云工声明 = 接口声明(平台包目录.云工宿主);
  assert.ok(云工声明.size >= 17, '云工宿主接口文件的声明太少：' + 云工声明.size);
  assert.deepEqual([...云工声明].filter(名 => typeof 物桥表[名] !== 'function'), [], '云工宿主接口文件里有、物桥没有实现的函数');
  assert.deepEqual(Object.keys(物桥表).filter(名 => !云工声明.has(名)), [], '物桥里有、云工宿主接口文件里没有的函数');
  const 张量声明 = 接口声明(平台包目录.中央张量宿主), 表 = 平台导入旧名.中央张量宿主;
  assert.deepEqual([...张量声明].filter(名 => !Object.hasOwn(表, 名)), [], '中央张量宿主接口文件里有、对照表里没有的函数');
  assert.deepEqual(Object.keys(表).filter(名 => !张量声明.has(名)), [], '对照表里有、中央张量宿主接口文件里没有的函数');
});

test('四、物桥的值转换与结果：成功、失败、类型标签、新数组与新对象', async () => {
  const 文 = 串 => new TextEncoder().encode(串);
  assert.deepEqual(物桥表.云工取({甲: 3}, 文('甲')), [0, [3, 3n]]);
  assert.deepEqual(物桥表.云工取({甲: 1.5}, 文('甲')), [0, [4, 1.5]]);
  assert.deepEqual(物桥表.云工取({甲: '乙'}, 文('甲')), [0, [6, '乙']]);
  assert.deepEqual(物桥表.云工取({甲: null}, 文('甲')), [0, [0]]);
  const 败 = 物桥表.云工调({}, 文('无此法'), []);
  assert.equal(败[0], 1);
  assert.equal(败[1], 'TypeError');
  assert.equal(物桥表.云工类型([8, new Uint8Array(1)]), 'Uint8Array');
  assert.equal(物桥表.云工类型([0]), 'Null');
  assert.deepEqual(物桥表.云工新数组([[3, 1n], [6, 文('二')]]), [1, '二']);
  assert.deepEqual(物桥表.云工新对象([[文('键'), [2, true]]]), {键: true});
  assert.deepEqual(await 物桥表.云工候([8, Promise.resolve(7)]), [0, [3, 7n]]);
  assert.deepEqual(物桥表.云工取({}, 文('__proto__'))[0], 1);
});

test('五、程序没有类型化入口时，事件报“程序没有导出入口”', async () => {
  const 空值桥 = new WebAssembly.Module(new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0]));
  const 程序模块 = new WebAssembly.Module(new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0, 1, 4, 1, 0x60, 0, 0, 3, 2, 1, 0, 7, 10, 1, 6, ...new TextEncoder().encode('_start'), 0, 0, 10, 4, 1, 2, 0, 0x0b]));
  await assert.rejects(创建云工宿主({程序模块, 值桥模块: 空值桥}).scheduled({}, {}, {}), /程序没有导出入口/);
});
