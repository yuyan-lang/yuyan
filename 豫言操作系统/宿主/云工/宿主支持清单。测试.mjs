// 文言：验云工宿主支持清单与实现之表相符：清单之行、适配之依、平台接口之声与对照表、对照表与能表。
// 汉语：云工宿主支持清单与实现表的一致性单测：
//   一、宿主支持清单.tsv 每行三列、接口名不重复；适配目录与清单文件存在，清单的接口名称是“豫言操作系统”加接口名；
//   二、所列适配只依赖云工能提供的平台接口包，所依赖的豫言操作系统接口也都在清单里；
//   三、云工实现的平台接口包（云工宿主、中央张量宿主）在 。接口。豫 里声明的函数与 值桥.mjs 的对照表逐一对应；库还没改写成接口文件时，改为核对外调名都在表里；
//   四、对照表里的旧名在宿主能力表里都有实现：用一个导入全部这些函数的小 Wasm 跑一个事件，宿主缺实现时事件失败；
//   五、带类型路径接通：标量结果的导入能调，中央张量未启用时同步报错；
//   六、写“宿主”的接口（现在没有），其接口文件里的每个函数宿主都直接实现。
// 运行：node --test 豫言操作系统/宿主/云工/宿主支持清单。测试.mjs（Node 26，需 JSPI；全树测试自动发现）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readdirSync, readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {平台导入旧名} from './值桥.mjs';
import {创建云工宿主, 云工平台包} from './宿主.mjs';

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
// 文言：取一目诸篇之外调名。汉语：取一个目录（递归）里 。豫 文件的外调名，库改写前用来核对对照表。
const 外调名们 = (目录, 前缀们) => {
  const 名们 = new Set(), 全 = path.join(仓根, 目录);
  const 走 = 处 => {
    for (const 项 of readdirSync(处, {withFileTypes: true})) {
      const 径 = path.join(处, 项.name);
      if (项.isDirectory()) 走(径);
      else if (项.name.endsWith('。豫')) for (const 配 of readFileSync(径, 'utf8').matchAll(/《《外调》》名『([^』]+)』/gu)) if (前缀们.some(前 => 配[1].startsWith(前))) 名们.add(配[1]);
    }
  };
  if (existsSync(全)) 走(全);
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

test('三、平台接口包的声明与对照表逐一对应（库未改写时核对外调名）', () => {
  for (const 包 of 云工平台包) {
    const 表 = 平台导入旧名[包];
    assert.ok(表 && Object.keys(表).length > 0, '对照表缺包：' + 包);
    const 声明 = 接口声明(平台包目录[包]);
    if (声明.size > 0) {
      assert.deepEqual([...声明].filter(名 => !Object.hasOwn(表, 名)), [], `「${包}」接口文件里有、对照表里没有的函数`);
      assert.deepEqual(Object.keys(表).filter(名 => !声明.has(名)), [], `对照表里有、「${包}」接口文件里没有的函数`);
    }
    const 旧名们 = new Set(Object.values(表));
    const 前缀们 = 包 === '中央张量宿主' ? ['豫言_中央张量_'] : ['豫言_云工_'];
    for (const 目录 of [平台包目录[包], '豫言操作系统/适配']) {
      assert.deepEqual([...外调名们(目录, 前缀们)].filter(名 => !旧名们.has(名)), [], `「${目录}」里有对照表没有的外调名`);
    }
  }
});

// 文言：手造一模：导入诸函，_start 依次调之而弃其果，附边界段。汉语：手工拼一个模块：导入给定函数，_start 依次调用（有结果的丢弃），并附「豫言边界」段。
const 编码器 = new TextEncoder();
const 无号 = 数 => { const 字节 = []; do { let 字 = 数 & 127; 数 >>>= 7; if (数) 字 |= 128; 字节.push(字); } while (数); return 字节; };
const 名 = 文 => { const 字节 = [...编码器.encode(文)]; return [...无号(字节.length), ...字节]; };
const 段 = (号, 体) => [号, ...无号(体.length), ...体];
const 造模块 = (导入们, 调用们 = []) => {
  // 导入们：[{模, 字, 签, 果}]，果为结果的 Wasm 值类型字节（无结果则省）；调用们：要在 _start 里依次调用的导入序号。
  const 型们 = [[0x60, 0, 0], ...导入们.map(({果}) => [0x60, 0, ...(果 === undefined ? [0] : [1, 果])])];
  const 型段 = 段(1, [...无号(型们.length), ...型们.flat()]);
  const 导段 = 段(2, [...无号(导入们.length), ...导入们.flatMap(({模, 字}, 序) => [...名(模), ...名(字), 0, ...无号(序 + 1)])]);
  const 函段 = 段(3, [1, 0]);
  const 出段 = 段(7, [1, ...名('_start'), 0, ...无号(导入们.length)]);
  const 体 = [0, ...调用们.flatMap(序 => [0x10, ...无号(序), ...(导入们[序].果 === undefined ? [] : [0x1a])]), 0x0b];
  const 码段 = 段(10, [1, ...无号(体.length), ...体]);
  const 边界 = 导入们.map(({模, 字, 签}) => `导入\t${模}\t${字}\t${签}\n`).join('');
  const 自段 = 段(0, [...名('豫言边界'), ...编码器.encode(边界)]);
  return new WebAssembly.Module(new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0, ...型段, ...导段, ...函段, ...出段, ...码段, ...自段]));
};
// 文言：空模充值桥：标量之转不经值桥。汉语：用空模块充当值桥：本测只用标量与“元”，胶水不经值桥。
const 空值桥 = new WebAssembly.Module(new Uint8Array([0, 0x61, 0x73, 0x6d, 1, 0, 0, 0]));
const 跑定时事件 = 程序模块 => 创建云工宿主({程序模块, 值桥模块: 空值桥, 许可: {}, 输出: () => {}, 错误输出: () => {}}).scheduled({}, {}, null);
const i64 = 0x7e;

test('四、对照表里的旧名在宿主能力表里都有实现', async () => {
  const 导入们 = 云工平台包.flatMap(包 => Object.keys(平台导入旧名[包]).map(字 => ({模: 包, 字, 签: '→元'})));
  assert.ok(导入们.length >= 354, '云工宿主与中央张量宿主合计应不少于 354 个函数，实得 ' + 导入们.length);
  await 跑定时事件(造模块(导入们));
});

test('五、带类型路径接通：标量结果可调，中央张量未启用时同步报错', async () => {
  await 跑定时事件(造模块([{模: '云工宿主', 字: '云工当前Unix毫秒', 签: '→整', 果: i64}], [0]));
  await assert.rejects(跑定时事件(造模块([{模: '中央张量宿主', 字: '中央张量新境', 签: '→整', 果: i64}], [0])), /中央张量：尚未启用/);
});

test('六、写“宿主”的接口，其函数宿主都直接实现', () => {
  for (const [接口, 适配] of 清单行们) {
    if (适配 !== '宿主') continue;
    const 包名 = '豫言操作系统' + 接口, 表 = 平台导入旧名[包名] ?? {};
    assert.deepEqual([...接口声明('豫言操作系统接口/' + 接口)].filter(名 => !Object.hasOwn(表, 名)), [], `接口「${接口}」写了宿主，却有函数没有宿主实现`);
  }
});
