// 文言：路一之节点宿主：载一 WasmGC 之客，先核其清单与形，后以所授之能行之。汉语：路一的 Node 应用宿主：装载一个 WasmGC 应用，先核对接口清单与 Wasm 形状，再只以本次授予的能力运行它。
// 文言：此文兼为开发之入口与发行启动文件之源；构建器内联下列八模并嵌值桥与宿主支持清单，乃成单一启动文件。
// 汉语：本文件既可直接运行（开发用，旁读值桥与宿主支持清单），也是发行包“启动.mjs”的源码：构建器把下面标出的八个模块（边界胶水、三个共用模块、张量原语、
//       与浏览器宿主共用的显示与图形模块、Node 的显示与图形原语、系统库调用原语）内联进来，并把值桥字节与宿主支持清单嵌入“内嵌”常量，得到单个 JS 启动文件。
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 终端 from 'node:tty';
import {StringDecoder as 字节解码器} from 'node:string_decoder';
import 系统 from 'node:os';
import {createHash, randomFillSync} from 'node:crypto';
import {createRequire} from 'node:module';
import {spawnSync, spawnSync as 同步启动, spawn as 原生服务启动} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {Worker, MessageChannel, receiveMessageOnPort} from 'node:worker_threads';
// 〔内联起〕
import {创建子程序能力, 创建原生子程序桥} from './子程序.mjs';
import {创建任务采样} from './任务采样.mjs';
import {创建传输控制协议能力} from './传输控制协议.mjs';
import {创建传输控制协议授权} from './传输控制协议授权.mjs';
import {创建控制台能力} from '../浏览器/控制台.mjs';
import {创建终端输入} from './终端输入.mjs';
import {读取标准终端尺寸} from './终端尺寸.mjs';
import {创建文件能力, 登记目录授权} from './文件能力.mjs';
import {造边界导入, 造边界导出} from '../网页汇编/边界.mjs';
import {创建值桥, 文字, 精确小数, 小数表示, 理解小数, 随机整数, 处理器数量, 平台导入旧名} from '../云工/值桥.mjs';
import {创建物桥} from '../云工/物桥.mjs';
import {核对接口装载} from '../../装载/接口核对.mjs';
import {创建张量能力} from './张量.mjs';
import {创建显示面表, 创建图形能力, 状态码, 事件种类} from '../浏览器/图形.mjs';
import {创建节点图形能力} from './图形.mjs';
import {创建外部库能力} from './外部库.mjs';
// 〔内联止〕
// 〔内嵌起〕
const 内嵌 = null;
// 〔内嵌止〕

const 本目录 = 路径.dirname(fileURLToPath(import.meta.url));
const 发行清单格式 = '豫言节点发行清单';
// 文言：值桥单次交换之限同云工，逾则止；网络正文之限取与之等。汉语：值桥单次交换上限 16 MiB（与云工相同），网络请求与响应正文上限取同值。
const 交换上限 = 16 * 1024 * 1024;

class 装载失败 extends Error {}
const 须 = (条件, 消息) => {
  if (!条件) throw new 装载失败('豫言操作系统装载失败：' + 消息);
};

// 文言：客之退出以异常穿栈，宿主捕之而取其码。汉语：应用调用退出时抛出此异常穿过 Wasm 栈，由宿主捕获并取得退出码。
class 客体退出 extends Error {
  constructor(码) { super('客体退出'); this.退出码 = 码; }
}

// 文言：张量计算之线程数（兼主线）：缺省取本机可并行之数，上限二百五十六；--张量线程数 可易之，一则不起工作线程。
// 汉语：张量计算的线程数（含主线程）：缺省取 os.availableParallelism()，上限 256；宿主选项 --张量线程数 N 可调整（超过上限按上限计），取 1 时不创建工作线程。
const 张量线程上限 = 256;
// 文言：张量之后端名四；--张量后端 惟许此四者，以强指其后端，供验之用；缺则适配自择。
// 汉语：张量后端名共四个；测试用宿主选项 --张量后端 名 只接受这四个，强制张量计算适配用该后端（不可用时取得上下文失败）；缺省由适配自动选择。
const 张量后端名们 = ['中央处理器', '网页图算', '金属', '统算'];

// 文言：显示面之授：名=宽x高，宽高为窗之点数，正整数而不逾六万五千五百三十六。汉语：显示面授权写成 名称=宽x高（x 也可写 ×），宽高是 SDL 窗口的逻辑尺寸（点），
//       正整数且不超过 65536；高分屏上物理像素是它的倍数。待办事项：超过屏幕或 GPU 纹理上限的尺寸不另核对。
const 显示面尺寸上限 = 65536;

// 文言：宿主诸选在前，-- 或首个非选项之后皆归应用。汉语：宿主选项写在前面；遇到 -- 或第一个不认识的参数，其后全部交给应用。
const 取值选项 = new Set(['--程序', '--清单', '--值桥', '--授权目录', '--授权只读目录', '--允许源', '--允许环境', '--授权文件', '--张量线程数',
  '--张量后端', '--授权显示面', '--原生依赖目录', '--授权子程序', '--授权控制台']);
// 文言：环境之同义：YY_NODE_NATIVE_DIR 同 --原生依赖目录（选项优先），YY_NODE_DISPLAY_BACKGROUND=1 同 --显示面后台。
// 汉语：同义环境变量：YY_NODE_NATIVE_DIR 等同 --原生依赖目录（两者都给时以选项为准），YY_NODE_DISPLAY_BACKGROUND=1 等同 --显示面后台。
export function 解析宿主参数(参数, 当前目录 = process.cwd(), 环境 = process.env) {
  // 文言：终端浏览器之发行包默认通诸 HTTP(S) 源；余应用仍循逐源之授。汉语：终端浏览器发行包默认允许全部 HTTP(S) 来源，其他应用仍按来源授权。
  const 配置 = {程序: null, 清单: null, 值桥: null, 授权: {目录: new Map(), 子程序: new Map(), 源: new Set(), 环境: new Set(), 全部来源: 内嵌?.允许全部来源 === true}, 应用参数: [], 张量线程数: null, 张量后端: '',
    显示面: new Map(), 原生依赖目录: 环境.YY_NODE_NATIVE_DIR ? 路径.resolve(当前目录, 环境.YY_NODE_NATIVE_DIR) : null,
    显示面后台: 环境.YY_NODE_DISPLAY_BACKGROUND === '1', 允许系统库调用: false};
  const 授目录 = (文, 可写, 基准) => {
    登记目录授权(配置.授权.目录, 文, 可写, 基准);
  };
  const 允源 = 文 => {
    let 源;
    try { 源 = new URL(文); } catch { 须(false, '允许源不是网址：' + 文); }
    须(源.protocol === 'http:' || 源.protocol === 'https:', '允许源须为 http 或 https：' + 文);
    配置.授权.源.add(源.origin);
  };
  const 读授权文件 = 文件 => {
    const 全径 = 路径.resolve(当前目录, 文件), 基准 = 路径.dirname(全径);
    let 值;
    try { 值 = JSON.parse(文件系统.readFileSync(全径, 'utf8')); } catch (错) { 须(false, '授权文件无法读取：' + 错.message); }
    须(值 && typeof 值 === 'object' && !Array.isArray(值), '授权文件须为 JSON 对象');
    for (const 键 of Object.keys(值)) 须(['目录', '只读目录', '源', '环境'].includes(键), '授权文件含未知字段：' + 键);
    for (const [键, 可写] of [['目录', true], ['只读目录', false]]) {
      for (const [名, 径] of Object.entries(值[键] ?? {})) {
        须(typeof 径 === 'string', '授权文件的目录路径须为文字：' + 名);
        授目录(名 + '=' + 径, 可写, 基准);
      }
    }
    for (const 源 of 值.源 ?? []) 允源(String(源));
    for (const 名 of 值.环境 ?? []) 配置.授权.环境.add(String(名));
  };
  const 授显示面 = 文 => {
    const 配 = /^(.+)=([1-9][0-9]*)[x×]([1-9][0-9]*)$/u.exec(文);
    须(配, '显示面授权须写成 名称=宽x高：' + 文);
    const 宽 = Number(配[2]), 高 = Number(配[3]);
    须(宽 <= 显示面尺寸上限 && 高 <= 显示面尺寸上限, '显示面宽高不得超过 ' + 显示面尺寸上限 + '：' + 文);
    配置.显示面.set(配[1], {宽, 高});
  };
  for (let 序 = 0; 序 < 参数.length; 序++) {
    const 项 = 参数[序];
    if (项 === '--') { 配置.应用参数 = 参数.slice(序 + 1); break; }
    // 文言：无值之开关。汉语：不带值的开关选项。
    if (项 === '--显示面后台') { 配置.显示面后台 = true; continue; }
    // 文言：系统库之调能行任意原生之码，越诸授，故须明许。汉语：系统库调用能执行任意原生代码、绕过目录与网络等授权，所以须明确允许（见 外部库.mjs）。
    if (项 === '--允许系统库调用') { 配置.允许系统库调用 = true; continue; }
    if (!取值选项.has(项)) { 配置.应用参数 = 参数.slice(序); break; }
    须(序 + 1 < 参数.length, '宿主选项缺少值：' + 项);
    const 值 = 参数[++序];
    if (项 === '--程序') 配置.程序 = 路径.resolve(当前目录, 值);
    else if (项 === '--清单') 配置.清单 = 路径.resolve(当前目录, 值);
    else if (项 === '--值桥') 配置.值桥 = 路径.resolve(当前目录, 值);
    else if (项 === '--授权目录') 授目录(值, true, 当前目录);
    else if (项 === '--授权只读目录') 授目录(值, false, 当前目录);
    else if (项 === '--允许源') 允源(值);
    else if (项 === '--允许环境') 配置.授权.环境.add(值);
    else if (项 === '--授权控制台') { 须(值.length > 0, '控制台名称不能为空'); (配置.授权.控制台 ??= new Set()).add(值); }
    else if (项 === '--授权子程序') {
      const 位 = 值.indexOf('=');
      须(位 > 0 && 位 < 值.length - 1, '子程序授权须写成 名=启动文件路径');
      const 入口 = 路径.resolve(当前目录, 值.slice(位 + 1));
      配置.授权.子程序.set(值.slice(0, 位), {入口, 目录: 路径.dirname(入口)});
    }
    else if (项 === '--张量线程数') {
      须(/^[1-9][0-9]*$/u.test(值), '--张量线程数 须为正整数：' + 值);
      配置.张量线程数 = Math.min(Number(值), 张量线程上限);
    } else if (项 === '--张量后端') {
      须(张量后端名们.includes(值), '--张量后端 须为 ' + 张量后端名们.join('、') + ' 之一：' + 值);
      配置.张量后端 = 值;
    } else if (项 === '--授权显示面') 授显示面(值);
    else if (项 === '--原生依赖目录') 配置.原生依赖目录 = 路径.resolve(当前目录, 值);
    else 读授权文件(值);
  }
  return 配置;
}

// 文言：原生之依赖按需而载：先从启动文件之目，次从当前之目，末从原生依赖目录，皆依 Node 之 require 之法上溯 node_modules；皆不得则抛。
// 汉语：按需载入 npm 原生依赖（@kmamal/sdl、@kmamal/gpu）：依次从启动文件所在目录、当前目录、原生依赖目录（--原生依赖目录 或 YY_NODE_NATIVE_DIR）
//       按 Node 的 require 规则解析（逐级向上找 node_modules）；都找不到时抛出，显示与图形原语据此返回资源暂不可用。结果（含失败）按名字缓存。
export function 创建原生载入器(原生依赖目录 = null, 当前目录 = process.cwd()) {
  const 基准们 = [fileURLToPath(import.meta.url), 路径.join(当前目录, '豫言原生依赖.cjs')];
  if (原生依赖目录) 基准们.push(路径.join(原生依赖目录, '豫言原生依赖.cjs'));
  const 缓存 = new Map();
  return 名 => {
    if (!缓存.has(名)) {
      let 项 = null, 末因 = '';
      for (const 基准 of 基准们) {
        try { 项 = {模: createRequire(基准)(名)}; break; }
        catch (错) { 末因 = String(错?.message ?? 错).split('\n')[0]; }
      }
      缓存.set(名, 项 ?? {错: new Error('找不到或载不入原生依赖 ' + 名 + '（' + 末因 + '）；可用宿主选项 --原生依赖目录 或环境变量 YY_NODE_NATIVE_DIR 指定含 node_modules 的目录')});
    }
    const 项 = 缓存.get(名);
    if (项.错) throw 项.错;
    return 项.模;
  };
}

// 文言：能力之清理以符号为键，客不能以名调之。汉语：能力表里的清理函数用符号作键：值桥只按字符串名字分派，应用调用不到它。
export const 能力清理 = Symbol('能力清理');
// 文言：带型导入之实，以符号为键。汉语：带类型导入的实现表（导入模块「标准库」「构建基础」与下面的平台接口包，键为字段名）用符号作键，挂在能力表上。
export const 带型实现 = Symbol('带型实现');
// 文言：接回调入口之键。汉语：接上回调入口（云工宿主/执行云工回调）的函数，用符号作键，运行节点应用在实例建好后调用。
export const 接回调 = Symbol('接回调');

// 文言：平台接口包之带型导入，由旧名之实派生：小数之果去其壳，外部值之列展为数列；异步者标之。
// 汉语：平台接口包（云工宿主、诺节宿主、系统库调用、安全外壳密码）的带类型导入，由以旧原语名为键的实现派生，对照取 值桥.mjs 的 平台导入旧名。
//   与旧实现的差别只有两处：小数结果去掉 {小数} 外壳（张量取小数、外部库读小数）；系统库调用三个调用原语的参数是外部值的列，过边界后是 [[支序, 值], …]，
//   展成旧原语要的数值数组（支序 2 是无值，报错）。async 函数与按操作可能返回 Promise 的显示、图形标“异步”，胶水只给它们套 JSPI，其余同步直调。
//   本宿主没有实现的平台函数（云工宿主其余的、浏览器宿主、中央张量宿主等）不进表，由胶水给桩，调用时报“接口函数未绑定”。
const 平台包们 = ['云工宿主', '诺节宿主', '系统库调用', '安全外壳密码'];
const 另异步旧名 = new Set(['豫言_节点_显示', '豫言_节点_图形']);
const 小数果旧名 = new Set(['豫言_节点_张量取小数', '豫言_节点_外部库读小数']);
const 去壳 = 值 => Number(值?.小数 ?? 值);
const 展外部值 = 列 => 列.map(([序, 值]) => {
  if (序 === 2) throw Error('系统库调用：参数不能是无值');
  return 值;
});
const 外部调用收果 = {外部调用得整数原语: 果 => 果, 外部调用得小数原语: 去壳, 外部调用无返回原语: () => undefined};
export function 派生平台实现(旧表, 对照 = 平台导入旧名) {
  const 表 = {};
  for (const 模 of 平台包们) {
    const 实们 = {};
    for (const [字段, 旧名] of Object.entries(对照?.[模] ?? {})) {
      const 旧 = Object.hasOwn(旧表, 旧名) ? 旧表[旧名] : null;
      if (typeof 旧 !== 'function') continue;
      let 实 = 旧;
      if (模 === '系统库调用' && Object.hasOwn(外部调用收果, 字段)) {
        const 收 = 外部调用收果[字段];
        实 = (址, 签名, 参们) => 收(旧(址, 签名, 展外部值(参们)));
      } else if (小数果旧名.has(旧名)) {
        实 = (...参) => 去壳(旧(...参));
      } else if (旧.constructor?.name === 'AsyncFunction' || 另异步旧名.has(旧名)) {
        实 = (...参) => 旧(...参);
        实.异步 = true;
      }
      实们[字段] = 实;
    }
    表[模] = 实们;
  }
  return 表;
}

// 文言：宿主支持清单人工所守；发行时嵌入，开发时循表读诸适配之簿。汉语：宿主支持清单由人工维护（宿主支持清单.tsv）；发行启动文件里已嵌入，直接运行本文件时按表读取各适配的宿主提供清单。
export function 读取宿主支持() {
  if (内嵌) return 内嵌.宿主支持;
  const 仓根 = 路径.resolve(本目录, '../../..');
  const 支持 = {宿主提供: [], 应用提供: []};
  for (const 原行 of 文件系统.readFileSync(路径.join(本目录, '宿主支持清单.tsv'), 'utf8').split('\n')) {
    const 行 = 原行.replace(/\r$/u, '');
    if (!行 || 行.startsWith('#')) continue;
    const [接口, 适配, 清单文件] = 行.split('\t');
    须(接口 && 适配 && 清单文件, '宿主支持清单行格式不符：' + 行);
    const 项 = JSON.parse(文件系统.readFileSync(路径.join(仓根, 清单文件), 'utf8'));
    (适配 === '-' ? 支持.应用提供 : 支持.宿主提供).push(项);
  }
  return 支持;
}

const 正规化 = 值 => {
  if (Array.isArray(值)) return 值.map(正规化);
  if (值 && typeof 值 === 'object') return Object.fromEntries(
    Object.entries(值).sort(([左], [右]) => (左 < 右 ? -1 : 左 > 右 ? 1 : 0)).map(([键, 项]) => [键, 正规化(项)]));
  return 值;
};
const 同清单 = (甲, 乙) => JSON.stringify(正规化(甲)) === JSON.stringify(正规化(乙));
const 接口身份 = 项 => `${项?.接口所有者}/${项?.接口名称}/${项?.接口版本}`;

// 文言：装载之核：发行清单之式、程序摘要、客需诸约与宿主所供逐一相等、导入或为工具模块、平台接口包与所需接口包之带型导入、有启口；应用所供之入口亦须为宿主所识，且其术皆有导出。
// 汉语：装载核对：发行清单格式与程序摘要；应用要求的每个接口在 Node 宿主支持清单里有逐项相等的清单；Wasm 的导入只能是标准库、构建基础、平台接口包与应用要求的接口包的带类型导入，并导出 _start；应用提供的接口（如启动）也须是宿主认可的版本，且每个函数都有导出 接口名称/函数名。
export function 核对节点装载({程序模块, 程序字节, 清单, 宿主支持}) {
  须(清单 && typeof 清单 === 'object' && 清单.格式 === 发行清单格式 && 清单.格式版本 === 1, '发行清单格式不符');
  须(Array.isArray(清单.接口要求) && Array.isArray(清单.应用提供), '发行清单缺少接口要求或应用提供');
  if (清单.程序摘要 !== undefined) {
    const 摘要 = createHash('sha256').update(程序字节).digest('hex');
    须(清单.程序摘要 === 摘要, '发行清单的程序摘要与程序.wasm 不符');
  }
  核对接口装载({程序模块, 程序字节, 应用要求: 清单.接口要求, 宿主提供: 宿主支持.宿主提供, 宿主: '节点', 应用提供: 清单.应用提供});
  for (const 项 of 清单.应用提供) {
    须(Array.isArray(项?.函数) && 项.函数.length > 0 && 项.函数.every(函 => 函.方向 === '应用'), '应用提供的接口函数方向不符：' + 接口身份(项));
    须(宿主支持.应用提供.some(支 => 同清单(项, 支)), '宿主不认可应用提供的接口：' + 接口身份(项));
  }
  return true;
}

// 文言：值桥须与编译器同代；发行时嵌之，开发时先取命令所指，次取程序旁者，末取宿主旁者。汉语：值桥要与编译器同代：发行启动文件里已嵌入；直接运行时依次用 --值桥、程序旁的 值桥.wasm、本目录的 yy节点值桥接.wasm。
function 取值桥模块(配置, 程序路径) {
  if (内嵌) return new WebAssembly.Module(Uint8Array.from(内嵌.值桥));
  const 候选 = [配置.值桥, 路径.join(路径.dirname(程序路径), '值桥.wasm'), 路径.join(本目录, 'yy节点值桥接.wasm')];
  const 径 = 候选.find(项 => 项 && 文件系统.existsSync(项));
  须(径, '找不到值桥（可用 --值桥 指定）');
  return new WebAssembly.Module(文件系统.readFileSync(径));
}

// 文言：同步书出，遇暂不可写则稍候复书，保次序。汉语：同步写标准输出或错误；管道暂时写不进（EAGAIN）就短暂等待后重写，保证次序且不丢字节。
const 小候 = new Int32Array(new SharedArrayBuffer(4));
function 写出(号, 字节) {
  let 位 = 0;
  while (位 < 字节.length) {
    try { 位 += 文件系统.writeSync(号, 字节, 位, 字节.length - 位); }
    catch (错) {
      if (错.code === 'EAGAIN') { Atomics.wait(小候, 0, 0, 5); continue; }
      throw 错;
    }
  }
}
const 编码器 = new TextEncoder();
const 连字节 = (...段) => {
  const 总 = 段.reduce((和, 项) => 和 + 项.length, 0), 果 = new Uint8Array(总);
  let 位 = 0;
  for (const 项 of 段) { 果.set(项, 位); 位 += 项.length; }
  return 果;
};
const 换行 = Uint8Array.of(10);

// 文言：标准入按行读之，同原生 getline：去行尾之换行与回车；读尽而无余则返阴。汉语：按行同步读标准输入，语义同开发宿主：去掉行尾换行与回车，读尽且无剩余时返回（阴，空）。
function 创建行读者() {
  let 缓 = new Uint8Array(0), 已尽 = false;
  const 去行尾 = 行 => (行.length && 行[行.length - 1] === 13 ? 行.subarray(0, 行.length - 1) : 行).slice();
  return () => {
    for (;;) {
      const 位 = 缓.indexOf(10);
      if (位 >= 0) { const 行 = 缓.subarray(0, 位); 缓 = 缓.subarray(位 + 1); return [true, 去行尾(行)]; }
      if (已尽) {
        if (缓.length === 0) return [false, new Uint8Array(0)];
        const 行 = 缓; 缓 = new Uint8Array(0);
        return [true, 去行尾(行)];
      }
      // 汉语：只读当前行所需字节，不预取将交给子程序的后续输入；待办事项：长行读取效率另优化。文言：惟读当前行所需之字，不先取当授子程序之后入；待办事项：长行读效后优。
      const 块 = new Uint8Array(1);
      let 数 = 0;
      try { 数 = 文件系统.readSync(0, 块, 0, 块.length, null); }
      catch (错) {
        if (错.code === 'EAGAIN') { Atomics.wait(小候, 0, 0, 10); continue; }
        if (错.code !== 'EOF') throw 错;
      }
      if (数 === 0) 已尽 = true; else 缓 = 连字节(缓, 块.subarray(0, 数));
    }
  };
}

// 文言：小数之文三术，与云工、浏览器共之。汉语：小数文字的三个函数（精确小数、小数表示、理解小数）与云工、浏览器共用 ../云工/值桥.mjs 的实现。

// 文言：文件与网络之果以码归客：零成，余为基础错误之类；九为读尽。汉语：文件与网络原语用状态码返回：0 成功；1 未获授权、2 资源暂不可用、3 资源已失效、4 资源不存在、5 资源已存在、6 资源配额已尽、7 输入无效、8 宿主操作失败（附消息）；读文件时 9 表示读尽。
const 码 = Object.freeze({成功: 0, 未获授权: 1, 暂不可用: 2, 已失效: 3, 不存在: 4, 已存在: 5, 配额已尽: 6, 输入无效: 7, 操作失败: 8, 读尽: 9});
const 系统错码 = 错 => ({
  ENOENT: 码.不存在, ENOTDIR: 码.不存在, EEXIST: 码.已存在, EACCES: 码.未获授权, EPERM: 码.未获授权,
  EAGAIN: 码.暂不可用, EWOULDBLOCK: 码.暂不可用, EBUSY: 码.暂不可用, EISDIR: 码.输入无效, EINVAL: 码.输入无效,
  ENAMETOOLONG: 码.输入无效, ELOOP: 码.输入无效, EBADF: 码.已失效, ENOSPC: 码.配额已尽, EDQUOT: 码.配额已尽,
  EMFILE: 码.配额已尽, ENFILE: 码.配额已尽, EFBIG: 码.配额已尽
})[错?.code] ?? 码.操作失败;
const 错文 = 错 => String(错?.code ?? 错?.name ?? 'Error') + ': ' + String(错?.message ?? 错);
const 严格解码 = new TextDecoder('utf-8', {fatal: true});
const 空字节 = () => new Uint8Array(0);

// 文言：客可见之全局唯此表所列；process、fetch、Function 之属皆不与，令通用句柄桥不越授权。
// 汉语：通用句柄桥（云工同名原语）只能取得下表列出的全局对象与构造器；process、fetch、require、Function、WebAssembly 等不在表内，应用无法借句柄桥绕过文件、网络与环境的授权。
const 可用全局 = new Set([
  'Array', 'ArrayBuffer', 'BigInt', 'Blob', 'Boolean', 'CompressionStream', 'DataView', 'Date', 'DecompressionStream',
  'File', 'FormData', 'Headers', 'Intl', 'JSON', 'Map', 'Math', 'Number', 'RegExp', 'ReadableStream', 'Response', 'Set',
  'String', 'Symbol', 'TextDecoder', 'TextDecoderStream', 'TextEncoder', 'TextEncoderStream', 'TransformStream',
  'URL', 'URLSearchParams', 'Uint8Array', 'WritableStream', 'AbortController', 'AbortSignal', 'atob', 'btoa',
  'decodeURI', 'decodeURIComponent', 'encodeURI', 'encodeURIComponent', 'isFinite', 'isNaN', 'parseFloat', 'parseInt',
  'structuredClone', 'Object', 'Promise', 'Error', 'TypeError', 'RangeError', 'console', 'crypto', 'performance',
  'setTimeout', 'clearTimeout', 'setInterval', 'clearInterval', 'queueMicrotask'
]);

// 文言：路径惟相对目录权；空、首斜、空段、点、点点、零字节皆无效。汉语：文件路径只能是相对于目录权的 / 分隔路径；空串、以 / 开头、空段、.、..、零字节都算输入无效。

// 文言：诸能之表：标准运行时之原语、云工同名之通用原语、节点独有之文件、网络、张量、显示图形与系统库调用原语。汉语：能力表：标准库运行时原语、与云工同名同义的通用原语（供复用云工适配），
//       以及 Node 独有的文件、网络、张量、显示图形与系统库调用原语。显示与图形只在应用用到时才载入原生依赖；能力表的 能力清理 键在应用结束后关闭窗口与设备。
export function 创建能力({授权, 应用参数, 程序路径, 输出 = 写出, 张量线程数 = null, 张量后端 = '', 显示面 = new Map(), 原生依赖目录 = null, 显示面后台 = false,
  允许系统库调用 = false}) {
  const 读行 = 创建行读者();
  const 控制台 = 创建控制台能力(new Map(Array.from(授权.控制台 ?? [], 名 => [名, {读取行: () => {const 果 = 读行(); return [果[0], 文字(果[1])];}, 写文本: 文 => 输出(1, Buffer.from(文, 'utf8'))}])));
  const 资源 = new Map();
  // 文言：密码学之随机数成批取之，每号取二，免每号一召 getRandomValues（约六百纳秒）。汉语：密码学随机数成批取（每批 8192 个），每个号用两个；免得每个号都调一次 getRandomValues（约 600 纳秒）。
  let 随机池 = new Uint32Array(0), 随机位 = 0;
  const 取随机 = () => {
    if (随机位 >= 随机池.length) { 随机池 = new Uint32Array(8192); crypto.getRandomValues(随机池); 随机位 = 0; }
    return 随机池[随机位++];
  };
  const 登记资源 = 值 => {
    for (;;) {
      const 号 = String((取随机() & 0x1fffff) * 4294967296 + 取随机());
      if (!资源.has(号) && Number(号) > 2 ** 40) { 资源.set(号, 值); return 号; }
    }
  };
  // 文言：节点之 console 皆书于标准误而冠其级，同旧；标准出留与程序之文。
  // 汉语：诺节上「云工全局」取到的 console 一律写标准错误并冠级别（[调试] [错误] [信息] [日志] [警告]），与原来相同；标准输出留给程序自己的输出。
  const 级冠 = {debug: '[调试] ', error: '[错误] ', info: '[信息] ', log: '[日志] ', warn: '[警告] '};
  const 错误台 = Object.fromEntries(Object.entries(级冠).map(([级, 冠]) =>
    [级, (...参) => 输出(2, 编码器.encode(冠 + 参.map(项 => String(项)).join(' ') + '\n'))]));
  const 取全局 = 名 => {
    const 名称 = 文字(名);
    if (名称 === 'console') return 错误台;
    if (!可用全局.has(名称) || !(名称 in globalThis)) throw Error('节点宿主不开放此全局：' + 名称);
    return globalThis[名称];
  };
  const 取流字节 = 值 => {
    if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
    if (ArrayBuffer.isView(值)) return new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice();
    throw Error('可读流块不是字节');
  };
  const 取文件 = 号 => { const 项 = 资源.get(文字(号)); return 项?.种 === '文件' ? 项 : null; };

  // 文言：标准库之宿主服务，以导入模块「标准库」之字段名为键；无者为桩。汉语：标准库宿主服务（导入模块「标准库」，键为字段名），本宿主提供已登记服务，其余给桩（调用时报“接口函数未绑定”）；
  //   参数、结果是边界胶水的 JS 形（串为 Uint8Array，整为 BigInt，小为 Number，爻为布尔，列为数组）；语义见 ../标准库宿主.汉语.md。
  // 文言：今目录为实例所有；此宿主不许迁之，恒返启时之目。汉语：当前工作目录归实例所有；本宿主不支持切换，恒返回实例创建时的目录。
  const 当前目录 = process.cwd();
  const 终端输入 = 创建终端输入();
  const 任务采样 = 创建任务采样({名称: 路径.basename(程序路径)});
  const 子程序 = 创建子程序能力({程序: 授权.子程序, 环境: 授权.环境, 暂停输入: () => 终端输入.暂停()});
  const 服务候选 = [原生依赖目录, 路径.dirname(程序路径), 当前目录].filter(Boolean).map(目 => 路径.join(目, 'yy原生子程序服务.exe'));
  const 原生子程序 = 创建原生子程序桥({服务路径: 服务候选.find(径 => 文件系统.existsSync(径)), 程序: 授权.子程序, 环境: 授权.环境, 当前目录});
  const 网络核路径 = [process.env.YY传输控制共享, 路径.join(本目录, 'yy传输控制共享.wasm'), 路径.resolve('yy传输控制共享.wasm')].find(径 => 径 && 文件系统.existsSync(径));
  const 传输控制 = 创建传输控制协议能力({模块字节: 网络核路径 ? 文件系统.readFileSync(网络核路径) : null});
  const 网络授权 = 创建传输控制协议授权({授权, 原语: 传输控制.能力});
  const 标准库 = {
    ...网络授权.能力,
    传输控制协议_监听: (主机, 端口, 队长) => 网络授权.能力.传输控制协议_监听(文字(主机), 端口, 队长),
    传输控制协议_开始连接: (主机, 端口) => 网络授权.能力.传输控制协议_开始连接(文字(主机), 端口),
    数据报_交换: (服务器, 端口, 超时, 查询) => 网络授权.能力.数据报_交换(文字(服务器), 端口, 超时, 查询),
    // 汉语：本应用宿主的异步子进程启动留待后续版本，终止明确返回不支持。文言：此客宿主之异步子程启留待后版，止明还未支持。
    终止异步子进程: () => 58,
    查询异步子进程资源: () => [58, false, 0n, false, 0n],
    进入终端原始输入模式: () => 终端输入.进入(),
    退出终端原始输入模式: () => 终端输入.退出(),
    读取终端按键: () => 终端输入.读取(),
    获取命令行程序名: () => 程序路径,
    获取命令行参数: () => 应用参数,
    获取当前工作目录: () => 当前目录,
    // 文言：环境之读亦须授，未授者如不存。汉语：标准库读环境变量同样只见已授权的名称，未授权的名称当作不存在。
    获取环境变量: 名 => {
      const 名称 = 文字(名);
      return 授权.环境.has(名称) && Object.hasOwn(process.env, 名称) ? [true, process.env[名称]] : [false, ''];
    },
    获取当前纳秒时间: () => performance.now() * 1e6,
    获取当前本地日期时间字符串: () => {
      const 时 = new Date(), 补 = 值 => String(值).padStart(2, '0');
      return `${时.getFullYear()}-${补(时.getMonth() + 1)}-${补(时.getDate())} ${补(时.getHours())}:${补(时.getMinutes())}:${补(时.getSeconds())}`;
    },
    格式化当前本地日期时间: 格式 => {
      const 时 = new Date(), 补 = 值 => String(值).padStart(2, '0');
      const 表 = {'%Y': String(时.getFullYear()), '%m': 补(时.getMonth() + 1), '%d': 补(时.getDate()), '%H': 补(时.getHours()), '%M': 补(时.getMinutes()), '%S': 补(时.getSeconds()), '%%': '%'};
      return 文字(格式).replace(/%./gu, 项 => { if (!(项 in 表)) throw Error('未支持日期格式 ' + 项); return 表[项]; });
    },
    在线处理器数量: 处理器数量,
    运行于Windows: () => process.platform === 'win32',
    运行于MacOS: () => process.platform === 'darwin',
    运行于Linux: () => process.platform === 'linux',
    运行于豫言操作系统: () => false,
    标准输出是终端: () => 终端.isatty(1),
    标准输入是终端: () => 终端.isatty(0),
    尝试读取标准输入行: () => 读行(),
    打印行: 值 => { 输出(1, 连字节(值, 换行)); },
    打印字符串: 值 => { 输出(1, 值); },
    标准错误打印行: 值 => { 输出(2, 连字节(值, 换行)); },
    获取随机整数: 随机整数,
    获取随机小数: () => Math.random(),
    安全随机_字节串: 长 => {
      const 数值 = Number(长);
      if (!Number.isInteger(数值) || 数值 < 0 || 数值 > 1048576) throw Error('安全随机字节串：长度须在零至一兆之间');
      const 果 = new Uint8Array(数值);
      for (let 位 = 0; 位 < 数值; 位 += 65536) crypto.getRandomValues(果.subarray(位, Math.min(数值, 位 + 65536)));
      return 果;
    },
    小数转字符串: 小数表示,
    小数精确表示: 精确小数,
    字符串转小数: 理解小数,
    退出进程: 码值 => { throw new 客体退出(Number(码值)); }
  };
  // 文言：构建基础之宿主服务，惟可绘监视面板，恒阴。汉语：构建基础的宿主服务（导入模块「构建基础」）只给“可绘监视面板”（恒为阴），其余给桩。
  const 构建基础 = {可绘监视面板: () => false};

  // 文言：云工之通用原语以物桥行之，与云工宿主同一实现；绑定惟认 ENV，回调之入口行时乃接。
  // 汉语：云工宿主的通用原语（库/云工宿主/物宿主）用与云工宿主同一份物桥实现，云工适配（时间、随机数、日志、摘要、网址、规整、压缩、环境文字等）因此可在 Node 上运行；
  //   「云工绑定」只认 ENV（--允许环境 授权的环境变量），回调入口在实例建好后由 运行节点应用 接上。
  let 回调入口函 = null;
  const 云工物桥 = 创建物桥({
    取全局,
    取绑定: (类, 名) => {
      if (类 !== 'ENV' || !授权.环境.has(名)) throw Error('未授权的' + 类 + '绑定：' + 名);
      return Object.hasOwn(process.env, 名) ? process.env[名] : undefined;
    },
    回调: (号, 参们) => {
      if (!回调入口函) throw Error('程序没有回调入口 云工宿主/执行云工回调');
      return 回调入口函(BigInt(号), 参们);
    }
  });
  for (const 函 of Object.values(云工物桥)) if (函.constructor?.name === 'AsyncFunction') 函.异步 = true;

  // 文言：安全外壳密码之求SHA256，以旧名入能表。汉语：平台接口包「安全外壳密码」只实现求SHA256，按旧名放进能力表，由 派生平台实现 接上。
  const 标准 = {豫言_密码_SHA256: 内容 => new Uint8Array(createHash('sha256').update(内容).digest())};


  // 文言：文件之能：目录由宿主预授，路径不得越其界；柄号为不可推之随机数，宿主核其类与存亡。
  const 文件能力 = 创建文件能力({授权, 资源, 登记资源, 文字, 交换上限});
  const 节点文件 = 文件能力.原语;

  // 文言：出站之求：先核法、址、标头，次核授权之源，不随重定向；正文逾限则止。
  // 汉语：Node 网络原语：先核方法、绝对 http(s) 网址与标头（不合为输入无效），再核目标来源是否在 --允许源 里（否则未获授权）；不跟随重定向（3xx 跳转为宿主操作失败）；请求与响应正文各以 16 MiB 为上限，超出为资源配额已尽。
  const 可用方法 = new Set(['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS']);
  const 网络败 = (码值, 消息) => [码值, 0, '[]', 空字节(), 消息];
  const 节点网络 = {
    豫言_节点_网络正文上限: () => [交换上限, 交换上限],
    豫言_节点_网络发送: async (方法, 网址, 标头文, 正文) => {
      const 方法名 = 文字(方法);
      if (!可用方法.has(方法名)) return 网络败(码.输入无效, '方法无效：' + 方法名);
      let 目标;
      try { 目标 = new URL(文字(网址)); } catch { return 网络败(码.输入无效, '网址无效'); }
      if (目标.protocol !== 'http:' && 目标.protocol !== 'https:') return 网络败(码.输入无效, '网址须为 http 或 https');
      const 标头 = new Headers();
      try {
        const 列 = JSON.parse(文字(标头文));
        if (!Array.isArray(列)) throw Error('标头不是数组');
        for (const 对 of 列) {
          if (!Array.isArray(对) || 对.length !== 2 || typeof 对[0] !== 'string' || typeof 对[1] !== 'string') throw Error('标头项无效');
          标头.append(对[0], 对[1]);
        }
      } catch (错) { return 网络败(码.输入无效, '标头无效：' + 错文(错)); }
      if ((方法名 === 'GET' || 方法名 === 'HEAD') && 正文.length > 0) return 网络败(码.输入无效, 'GET 与 HEAD 请求不能带正文');
      if (!授权.全部来源 && !授权.源.has(目标.origin)) return 网络败(码.未获授权, '目标来源未获授权：' + 目标.origin);
      if (正文.length > 交换上限) return 网络败(码.配额已尽, '请求正文超过上限');
      let 回应;
      try {
        回应 = await fetch(目标, {method: 方法名, headers: 标头, body: 方法名 === 'GET' || 方法名 === 'HEAD' ? undefined : 正文, redirect: 'manual'});
      } catch (错) { return 网络败(码.操作失败, '传输失败：' + 错文(错?.cause ?? 错)); }
      if ([301, 302, 303, 307, 308].includes(回应.status)) {
        try { await 回应.body?.cancel(); } catch { /* 文言：弃之无害。汉语：取消失败不影响结果。 */ }
        return 网络败(码.操作失败, '不跟随重定向：HTTP ' + 回应.status);
      }
      const 块们 = [];
      let 总 = 0;
      if (回应.body) {
        const 读者 = 回应.body.getReader();
        try {
          for (;;) {
            const {done, value} = await 读者.read();
            if (done) break;
            总 += value.length;
            if (总 > 交换上限) {
              try { await 读者.cancel(); } catch { /* 文言：弃之无害。汉语：取消失败不影响结果。 */ }
              return 网络败(码.配额已尽, '响应正文超过上限');
            }
            块们.push(value);
          }
        } catch (错) { return 网络败(码.操作失败, '读取响应失败：' + 错文(错?.cause ?? 错)); }
      }
      return [码.成功, 回应.status, JSON.stringify([...回应.headers]), 连字节(...块们), ''];
    }
  };
  // 文言：张量之能：存储之限取本机物理内存；文件区段按位读，不动顺读之位；并行之线程数见上。汉语：张量计算与张量文件原语（见 张量.mjs）：存储上限取本机物理内存；由文件载入张量按位置读取（pread），不改变文件柄的顺序读取位置；注入 worker_threads 能力与线程数（取法见 张量线程上限 处）。
  const 节点张量 = 创建张量能力({
    码, 资源, 登记资源, 取文件, 文字, 错文, 存储上限: 系统.totalmem(), 交换上限,
    读文件区段: (描述符, 视图, 位置) => 文件系统.readSync(描述符, 视图, 0, 视图.length, 位置),
    文件字节数: 描述符 => 文件系统.fstatSync(描述符).size,
    线程: {Worker, MessageChannel, receiveMessageOnPort},
    线程数: 张量线程数 ?? Math.min(系统.availableParallelism(), 张量线程上限), 指定后端: 张量后端
  });
  // 文言：显示与图形之能：面由 --授权显示面 授之，原生之依赖初用乃载。汉语：显示与图形原语（见 图形.mjs）：显示面由 --授权显示面 授予；原生依赖第一次用到时才载入。
  const 节点图形 = 创建节点图形能力({
    创建显示面表, 创建图形能力, 状态码, 事件种类, 载入原生: 创建原生载入器(原生依赖目录), 授权显示面: 显示面, 后台: 显示面后台,
    写错误: 文 => 输出(2, 编码器.encode(文 + '\n')),
    文件系统, 路径分隔: 路径.sep, 平台: process.platform, 家目录: 系统.homedir(), 环境: process.env,
    内建字体文件: 路径.join(路径.dirname(路径.resolve(程序路径)), '字体.ttf')
  });
  // 文言：系统库之调，惟 --允许系统库调用 乃开；张量之存与视图可取，供金属等后端零抄。汉语：系统库调用原语（见 外部库.mjs）只在给了 --允许系统库调用 时可用；
  //       张量存储的原生地址与视图信息按张量句柄从资源表取（张量.mjs 的资源项：种 为 '张量'，存.缓 为 SharedArrayBuffer，另有 类、形、步、起），供金属等后端零拷贝。
  // 文言：旧版构建器不内联 外部库.mjs，启动文件中无此函，则惟报不可用。汉语：用旧版 yy双宿主构建 生成的 启动.mjs 没有内联 外部库.mjs（没有 创建外部库能力），此时只提供报告“不可用”的原语，其余照常。
  const 节点外部库 = typeof 创建外部库能力 !== 'function' ? {豫言_节点_外部库可用: () => false} : 创建外部库能力({文字, 允许: 允许系统库调用, 取张量: 号 => {
    const 项 = 资源.get(文字(号));
    return 项?.种 === '张量' ? 项 : null;
  }});
  const 旧表 = {...标准, ...节点文件, ...节点网络, ...节点张量, ...节点图形.原语, ...节点外部库,
    豫言_节点_控制台: async (操作, 名, 文) => 文字(操作) === '读取' ? 控制台.读取(文字(名)) : 文字(操作) === '写入' ? 控制台.写入(文字(名), 文字(文)) : [7, '', '控制台操作无效'],
    豫言_节点_运行子程序: (名, 参数, 输入, 环境项们) => 子程序.运行(文字(名), 参数.map(文字), 输入, 环境项们.map(项 => 项.map(文字))),
    豫言_节点_启动子程序: async (名, 参数, 输入, 环境项们) => 原生子程序.启动(文字(名), 参数.map(文字), 输入, 环境项们.map(项 => 项.map(文字))),
    豫言_节点_收取子程序: async 柄 => 原生子程序.收取(柄),
    豫言_节点_终止具名子程序: async 柄 => 原生子程序.终止(柄),
    豫言_节点_查询具名子程序资源: async 柄 => 原生子程序.资源(柄),
    豫言_节点_运行继承输入子程序: (名, 参数, 环境项们) => 子程序.运行(文字(名), 参数.map(文字), new Uint8Array(), 环境项们.map(项 => 项.map(文字)), true),
    豫言_节点_查询运行任务: () => {
      const 项 = 任务采样.采样();
      return [[项.标识, 项.名称, 项.范围, 项.采样微秒, 项.处理器万分比, 项.常驻字节, 项.堆已用字节]];
    }};
  // 文言：平台接口包之带型导入，由旧名之能表派生。汉语：平台接口包的带类型导入由上面以旧名为键的能力表派生，见 派生平台实现；旧名只是内部的键，不对外。
  return Object.freeze({[能力清理]: () => {网络授权.关闭(); 传输控制.关闭(); 文件能力.清理(); 原生子程序.关闭(); 控制台.关闭(); 终端输入.退出(); return 节点图形.清理();}, [接回调]: 函 => { 回调入口函 = 函; }, [带型实现]: {标准库, 构建基础, ...派生平台实现(旧表), 云工宿主: 云工物桥, 豫言操作系统控制台:{读取标准终端尺寸}, 豫言操作系统时间:{读取当前Unix毫秒: () => 传输控制.读取当前Unix毫秒()}}});
}

// 文言：以 JSPI 行客：诸能或同步或异步，客皆以常调用视之。汉语：用 JSPI 运行：能力可以同步返回，也可以返回 Promise（网络、摘要等），应用都按普通调用看待。
export async function 运行节点应用({程序模块, 值桥模块, 能力, 输出 = 写出}) {
  须(typeof WebAssembly.Suspending === 'function' && typeof WebAssembly.promising === 'function', '当前 Node 缺少 WebAssembly JSPI（需要 Node 26 或更新版本）');
  const 桥 = 创建值桥(值桥模块);
  // 文言：带型之导入依签名包之，同步者不套悬栈；无实现者给桩。汉语：带类型的导入由共用胶水按签名包装：同步实现直接调用，只有标了“异步”的才套 JSPI（标准库这些原语都是同步的）；没有实现的给桩。
  const 实例 = new WebAssembly.Instance(程序模块, 造边界导入(程序模块, 桥.原, 能力[带型实现] ?? {}));
  须(typeof 实例.exports._start === 'function', 'Wasm 缺少程序启动导出');
  // 文言：先接回调之入口，客于 _start 中亦得用之。汉语：先接上回调入口（应用接口导出 云工宿主/执行云工回调），程序在 _start 里登记的回调也能被调用。
  能力[接回调]?.(造边界导出(实例, 程序模块, 桥.原, {异步: true})['云工宿主/执行云工回调'] ?? null);
  try {
    // 文言：_start 先行静态之初置，乃召入口之函。汉语：_start 先做静态初始化，再调用入口函数「入口」（提案 00006）。
    await WebAssembly.promising(实例.exports._start)();
    return 0;
  } catch (错) {
    if (错 instanceof 客体退出) return 错.退出码;
    输出(2, 编码器.encode('豫言程序运行失败：' + (错?.stack ?? String(错)) + '\n'));
    return 1;
  }
}

// 文言：许系统库之调、程序用之而诺节未带 --experimental-ffi，则带之以同参重启此文，承三流，以其退码终；诺节不识此旗则不重启，原语自报不可用。
// 汉语：允许了系统库调用、程序用到系统库调用（导入模块「系统库调用」的带类型导入）而诺节没带 --experimental-ffi 时，
//       带上此旗以同样的参数重新启动本文件（继承标准流），以子进程的退出码退出；诺节不认此旗时不重启，系统库调用原语报告不可用。
const 外部库旗 = '--experimental-ffi';
const 用系统库调用 = 程序模块 => WebAssembly.Module.imports(程序模块).some(项 => 项.module === '系统库调用' || 项.name === '诺节文件定位读取');
const 须带外部库旗重启 = 程序模块 => process.allowedNodeEnvironmentFlags.has(外部库旗) && !process.execArgv.includes(外部库旗) &&
  !/(?:^|\s)--experimental-ffi(?:\s|$)/u.test(process.env.NODE_OPTIONS ?? '') && 用系统库调用(程序模块);
const 带外部库旗重启 = 参数 => {
  const 果 = spawnSync(process.execPath, [外部库旗, ...process.execArgv, fileURLToPath(import.meta.url), ...参数], {stdio: 'inherit'});
  if (果.error) throw 果.error;
  return 果.status ?? (果.signal ? 128 + (系统.constants.signals[果.signal] ?? 0) : 1);
};

// 文言：启：析参、载、核、行，而以客之退出码终。汉语：启动流程：解析宿主选项、读入程序与清单、装载核对、运行，进程退出码取应用的退出码；装载失败退出码为 3。
export async function 启动(参数 = process.argv.slice(2)) {
  try {
    const 配置 = 解析宿主参数(参数);
    const 程序路径 = 配置.程序 ?? 路径.join(本目录, '程序.wasm');
    const 清单路径 = 配置.清单 ?? 路径.join(路径.dirname(程序路径), '清单.json');
    let 程序字节, 清单, 程序模块;
    try { 程序字节 = 文件系统.readFileSync(程序路径); } catch (错) { 须(false, '无法读取程序：' + 错.message); }
    try { 清单 = JSON.parse(文件系统.readFileSync(清单路径, 'utf8')); } catch (错) { 须(false, '无法读取发行清单：' + 错.message); }
    try { 程序模块 = new WebAssembly.Module(程序字节); } catch (错) { 须(false, '程序不是有效的 Wasm：' + 错.message); }
    核对节点装载({程序模块, 程序字节, 清单, 宿主支持: 读取宿主支持()});
    // 文言：指金属或统算而未许系统库之调，则其不可用，取张量之境得资源暂不可用；先于标准错误示当加之选。
    // 汉语：给了 --张量后端 金属（或统算）却没给 --允许系统库调用 时该后端不可用，取得张量上下文返回资源暂不可用；先在标准错误提示需加该选项。
    if ((配置.张量后端 === '金属' || 配置.张量后端 === '统算') && !配置.允许系统库调用) {
      写出(2, 编码器.encode('提示：--张量后端 ' + 配置.张量后端 + ' 需同时给 --允许系统库调用，否则取得张量上下文返回资源暂不可用\n'));
    }
    if ((配置.允许系统库调用 || WebAssembly.Module.imports(程序模块).some(项 => 项.module === '诺节宿主' && 项.name === '诺节文件定位读取')) && 须带外部库旗重启(程序模块)) return 带外部库旗重启(参数);
    const 能力 = 创建能力({授权: 配置.授权, 应用参数: 配置.应用参数, 程序路径, 张量线程数: 配置.张量线程数, 张量后端: 配置.张量后端,
      显示面: 配置.显示面, 原生依赖目录: 配置.原生依赖目录, 显示面后台: 配置.显示面后台, 允许系统库调用: 配置.允许系统库调用});
    // 文言：客毕（含退出与败）则清其窗与设备，令进程得退。汉语：应用结束（含调用退出与运行失败）后关闭窗口与图形设备，进程才能退出。
    try { return await 运行节点应用({程序模块, 值桥模块: 取值桥模块(配置, 程序路径), 能力}); }
    finally { await 能力[能力清理]?.(); }
  } catch (错) {
    if (错 instanceof 装载失败 || String(错?.message).startsWith('豫言操作系统装载失败')) {
      写出(2, 编码器.encode(错.message + '\n'));
      return 3;
    }
    throw 错;
  }
}

// 文言：径行此文者即启；为他文所引者不启。汉语：作为入口运行时才启动；被测试等模块导入时只导出函数。
const 是入口 = (() => {
  try { return 文件系统.realpathSync(process.argv[1] ?? '') === 文件系统.realpathSync(fileURLToPath(import.meta.url)); }
  catch { return false; }
})();
if (是入口) process.exitCode = await 启动();
