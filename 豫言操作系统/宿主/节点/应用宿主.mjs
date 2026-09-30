// 文言：路一之节点宿主：载一 WasmGC 之客，先核其清单与形，后以所授之能行之。汉语：路一的 Node 应用宿主：装载一个 WasmGC 应用，先核对接口清单与 Wasm 形状，再只以本次授予的能力运行它。
// 文言：此文兼为开发之入口与发行启动文件之源；构建器内联下列七模并嵌值桥与宿主支持清单，乃成单一启动文件。
// 汉语：本文件既可直接运行（开发用，旁读值桥与宿主支持清单），也是发行包“启动.mjs”的源码：构建器把下面标出的七个模块（三个共用模块、张量原语、
//       与浏览器宿主共用的显示与图形模块、Node 的显示与图形原语、系统库调用原语）内联进来，并把值桥字节与宿主支持清单嵌入“内嵌”常量，得到单个 JS 启动文件。
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import 终端 from 'node:tty';
import 系统 from 'node:os';
import {createHash} from 'node:crypto';
import {createRequire} from 'node:module';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {Worker, MessageChannel, receiveMessageOnPort} from 'node:worker_threads';
// 〔内联起〕
import {创建值桥, 文字} from '../云工/值桥.mjs';
import {创建句柄表} from '../云工/句柄.mjs';
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
  '--张量后端', '--授权显示面', '--原生依赖目录']);
// 文言：环境之同义：YY_NODE_NATIVE_DIR 同 --原生依赖目录（选项优先），YY_NODE_DISPLAY_BACKGROUND=1 同 --显示面后台。
// 汉语：同义环境变量：YY_NODE_NATIVE_DIR 等同 --原生依赖目录（两者都给时以选项为准），YY_NODE_DISPLAY_BACKGROUND=1 等同 --显示面后台。
export function 解析宿主参数(参数, 当前目录 = process.cwd(), 环境 = process.env) {
  const 配置 = {程序: null, 清单: null, 值桥: null, 授权: {目录: new Map(), 源: new Set(), 环境: new Set()}, 应用参数: [], 张量线程数: null, 张量后端: '',
    显示面: new Map(), 原生依赖目录: 环境.YY_NODE_NATIVE_DIR ? 路径.resolve(当前目录, 环境.YY_NODE_NATIVE_DIR) : null,
    显示面后台: 环境.YY_NODE_DISPLAY_BACKGROUND === '1', 允许系统库调用: false};
  const 授目录 = (文, 可写, 基准) => {
    const 位 = 文.indexOf('=');
    须(位 > 0 && 位 < 文.length - 1, '目录授权须写成 名=路径：' + 文);
    配置.授权.目录.set(文.slice(0, 位), {路径: 路径.resolve(基准, 文.slice(位 + 1)), 可写});
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

// 文言：装载之核：发行清单之式、程序摘要、客需诸约与宿主所供逐一相等、唯一通桥与启口；应用所供之入口亦须为宿主所识。
// 汉语：装载核对：发行清单格式与程序摘要；应用要求的每个接口在 Node 宿主支持清单里有逐项相等的清单；Wasm 只有 yuyan:gc-host/v1.call 一个导入并导出 _start；应用提供的接口（如启动）也须是宿主认可的版本。
export function 核对节点装载({程序模块, 程序字节, 清单, 宿主支持}) {
  须(清单 && typeof 清单 === 'object' && 清单.格式 === 发行清单格式 && 清单.格式版本 === 1, '发行清单格式不符');
  须(Array.isArray(清单.接口要求) && Array.isArray(清单.应用提供), '发行清单缺少接口要求或应用提供');
  if (清单.程序摘要 !== undefined) {
    const 摘要 = createHash('sha256').update(程序字节).digest('hex');
    须(清单.程序摘要 === 摘要, '发行清单的程序摘要与程序.wasm 不符');
  }
  核对接口装载({程序模块, 程序字节, 应用要求: 清单.接口要求, 宿主提供: 宿主支持.宿主提供, 宿主: '节点'});
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
      const 块 = new Uint8Array(65536);
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

// 文言：小数之精确表示，同原生 %.17g。汉语：小数的精确文字表示，与原生运行时的 %.17g 一致（同开发宿主）。
const 数 = 值 => Number(值?.小数 ?? 值);
function 精确小数(值) {
  const 数字 = 数(值);
  if (Object.is(数字, -0)) return '-0';
  if (!Number.isFinite(数字)) return String(数字).toLowerCase().replace('infinity', 'inf');
  const [尾, 指数] = 数字.toExponential(16).split('e');
  const 幂 = Number(指数);
  if (幂 < -4 || 幂 >= 17) return 尾.replace(/\.?0+$/u, '') + 'e' + (幂 >= 0 ? '+' : '-') + String(Math.abs(幂)).padStart(2, '0');
  return 数字.toFixed(Math.max(0, 16 - 幂)).replace(/(\.\d*?)0+$/u, '$1').replace(/\.$/u, '');
}

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
  'structuredClone'
]);

// 文言：路径惟相对目录权；空、首斜、空段、点、点点、零字节皆无效。汉语：文件路径只能是相对于目录权的 / 分隔路径；空串、以 / 开头、空段、.、..、零字节都算输入无效。
function 合法相对路径(径) {
  if (!径 || 径.startsWith('/') || 径.includes('\u0000')) return false;
  // 文言：Windows 以反斜与冒号为界，恐越权，并拒之。汉语：Windows 会把反斜杠和冒号当作路径或流分隔，段内出现即拒绝。待办事项：其他平台是否也应拒绝，以求三平台完全一致。
  if (process.platform === 'win32' && /[\\:]/u.test(径)) return false;
  return 径.split('/').every(段 => 段 !== '' && 段 !== '.' && 段 !== '..');
}
const 在根内 = (根, 实) => {
  const 相对 = 路径.relative(根, 实);
  return 相对 === '' || (!路径.isAbsolute(相对) && 相对 !== '..' && !相对.startsWith('..' + 路径.sep));
};

// 文言：诸能之表：标准运行时之原语、云工同名之通用原语、节点独有之文件、网络、张量、显示图形与系统库调用原语。汉语：能力表：标准库运行时原语、与云工同名同义的通用原语（供复用云工适配），
//       以及 Node 独有的文件、网络、张量、显示图形与系统库调用原语。显示与图形只在应用用到时才载入原生依赖；能力表的 能力清理 键在应用结束后关闭窗口与设备。
export function 创建能力({授权, 应用参数, 程序路径, 输出 = 写出, 张量线程数 = null, 张量后端 = '', 显示面 = new Map(), 原生依赖目录 = null, 显示面后台 = false,
  允许系统库调用 = false}) {
  const 句柄 = 创建句柄表({上限: 1 << 20});
  const 读行 = 创建行读者();
  const 资源 = new Map();
  const 登记资源 = 值 => {
    for (;;) {
      const 缓 = new Uint32Array(2);
      crypto.getRandomValues(缓);
      const 号 = String((缓[0] & 0x1fffff) * 4294967296 + 缓[1]);
      if (!资源.has(号) && Number(号) > 2 ** 40) { 资源.set(号, 值); return 号; }
    }
  };
  const 取全局 = 名 => {
    const 名称 = 句柄.允名(文字(名));
    if (!可用全局.has(名称) || !(名称 in globalThis)) throw Error('节点宿主不开放此全局：' + 名称);
    return globalThis[名称];
  };
  const 取流字节 = 值 => {
    if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
    if (ArrayBuffer.isView(值)) return new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice();
    throw Error('可读流块不是字节');
  };
  const 取目录 = 号 => { const 项 = 资源.get(文字(号)); return 项?.种 === '目录' ? 项 : null; };
  const 取文件 = 号 => { const 项 = 资源.get(文字(号)); return 项?.种 === '文件' ? 项 : null; };

  const 标准 = {
    豫言_获取命令行程序名: () => 程序路径,
    豫言_获取命令行参数: () => [应用参数, 应用参数.length],
    豫言_获取当前工作目录: () => process.cwd(),
    // 文言：环境之读亦须授，未授者如不存。汉语：标准库读环境变量同样只见已授权的名称，未授权的名称当作不存在。
    豫言_获取环境变量: 名 => {
      const 名称 = 文字(名);
      return 授权.环境.has(名称) && Object.hasOwn(process.env, 名称) ? [true, process.env[名称]] : [false, ''];
    },
    豫言_获取当前纳秒时间: () => ({小数: performance.now() * 1e6}),
    豫言_获取当前本地日期时间字符串: () => {
      const 时 = new Date(), 补 = 值 => String(值).padStart(2, '0');
      return `${时.getFullYear()}-${补(时.getMonth() + 1)}-${补(时.getDate())} ${补(时.getHours())}:${补(时.getMinutes())}:${补(时.getSeconds())}`;
    },
    豫言_格式化当前本地日期时间: 格式 => {
      const 时 = new Date(), 补 = 值 => String(值).padStart(2, '0');
      const 表 = {'%Y': String(时.getFullYear()), '%m': 补(时.getMonth() + 1), '%d': 补(时.getDate()), '%H': 补(时.getHours()), '%M': 补(时.getMinutes()), '%S': 补(时.getSeconds()), '%%': '%'};
      return 文字(格式).replace(/%./gu, 项 => { if (!(项 in 表)) throw Error('未支持日期格式 ' + 项); return 表[项]; });
    },
    豫言_在线处理器数量: () => BigInt((globalThis.navigator?.hardwareConcurrency) ?? 1),
    豫言_运行于Windows: () => process.platform === 'win32',
    豫言_运行于MacOS: () => process.platform === 'darwin',
    豫言_运行于Linux: () => process.platform === 'linux',
    豫言_可绘监视面板: () => false,
    豫言_标准输出是终端: () => 终端.isatty(1),
    豫言_标准输入是终端: () => 终端.isatty(0),
    豫言_尝试读取标准输入行: () => 读行(),
    豫言_打印行: 值 => { 输出(1, 连字节(值, 换行)); },
    豫言_打印字符串: 值 => { 输出(1, 值); },
    豫言_标准错误打印行: 值 => { 输出(2, 连字节(值, 换行)); },
    豫言_打印通用值: (消息, 值) => {
      输出(2, 编码器.encode('[豫言通用值打印] ' + 文字(消息) + ': ' + JSON.stringify(值, (_, 项) => typeof 项 === 'bigint' ? String(项) : 项 instanceof Uint8Array ? 文字(项) : 项) + '\n'));
    },
    豫言_获取随机整数: 上界 => {
      const 界 = Number(上界);
      if (!(界 > 0)) throw Error('随机整数的上界须为正');
      return BigInt(Math.floor(Math.random() * 界));
    },
    豫言_获取随机小数: () => ({小数: Math.random()}),
    豫言_安全随机_字节串: 长 => {
      const 数值 = Number(长);
      if (!Number.isInteger(数值) || 数值 < 0 || 数值 > 1048576) throw Error('安全随机字节串：长度须在零至一兆之间');
      const 果 = new Uint8Array(数值);
      for (let 位 = 0; 位 < 数值; 位 += 65536) crypto.getRandomValues(果.subarray(位, Math.min(数值, 位 + 65536)));
      return 果;
    },
    豫言_密码_SHA256: 内容 => new Uint8Array(createHash('sha256').update(内容).digest()),
    豫言_字节转字符串: 值 => {
      if (值 < 0n || 值 > 255n) throw Error('字节值越界');
      return Uint8Array.of(Number(值));
    },
    豫言_字节串_空: () => new Uint8Array(0),
    豫言_字节串_长度: 值 => BigInt(值.length),
    豫言_字节串_取字节: (值, 序) => {
      if (序 < 0n || 序 >= BigInt(值.length)) throw Error('字节串取字节：序数越界');
      return BigInt(值[Number(序)]);
    },
    豫言_字节串_从字符串: 值 => 值.slice(),
    豫言_字节串_单字节: 值 => {
      if (值 < 0n || 值 > 255n) throw Error('构造单字节串：字节必须在零至二百五十五之间');
      return Uint8Array.of(Number(值));
    },
    豫言_字节串_拼接: (甲, 乙) => 连字节(甲, 乙),
    豫言_字节串_截取: (值, 起, 长) => {
      if (起 < 0n || 长 < 0n || 起 > BigInt(值.length) || 长 > BigInt(值.length) - 起) throw Error('截取字节串：范围越界');
      return 值.slice(Number(起), Number(起 + 长));
    },
    豫言_整数转小数: 值 => ({小数: Number(值)}),
    豫言_小数转整数: 值 => BigInt(Math.trunc(数(值))),
    豫言_整数加: (甲, 乙) => BigInt.asIntN(64, 甲 + 乙),
    豫言_整数乘: (甲, 乙) => BigInt.asIntN(64, 甲 * 乙),
    豫言_整数除: (甲, 乙) => 甲 / 乙,
    豫言_小数加: (甲, 乙) => ({小数: 数(甲) + 数(乙)}),
    豫言_小数减: (甲, 乙) => ({小数: 数(甲) - 数(乙)}),
    豫言_小数乘: (甲, 乙) => ({小数: 数(甲) * 数(乙)}),
    豫言_小数除: (甲, 乙) => ({小数: 数(甲) / 数(乙)}),
    豫言_字符串按字节在前: (甲, 乙) => {
      const 长 = Math.min(甲.length, 乙.length);
      for (let 位 = 0; 位 < 长; 位++) if (甲[位] !== 乙[位]) return 甲[位] < 乙[位];
      return 甲.length < 乙.length;
    },
    豫言_整数转字符串: 值 => String(值),
    豫言_字符串转整数: 值 => BigInt(文字(值).match(/^\s*[+-]?\d+/u)?.[0].trim() ?? '0'),
    豫言_小数转字符串: 值 => 数(值).toFixed(6),
    豫言_小数精确表示: 值 => 精确小数(值),
    豫言_字符串转小数: 值 => ({小数: Number(文字(值))}),
    豫言_源码数字名: 值 => /^[0-9-]+$/u.test(文字(值)),
    豫言_源码可用名: 值 => !/^[0-9-]+$/u.test(文字(值)) && !文字(值).startsWith('《《') && !文字(值).startsWith('：') && !文字(值).includes('」'),
    豫言_源码字符串表示: 值 => '『' + 文字(值).replace(/「：|』/gu, 字 => 字 === '』' ? '「：』：」' : '「：「：：」') + '』',
    豫言_退出进程: 码值 => { throw new 客体退出(Number(码值)); }
  };

  // 文言：与云工同名同义之通用原语，令云工诸适配无改而行于节点。汉语：以下原语与云工宿主同名、同语义（实现照录 豫言操作系统/宿主/云工/宿主.mjs），使时间、随机数、日志、摘要、网址、规整、压缩等云工适配不改一字即可在 Node 上运行；全局只开放“可用全局”表。
  const 云工通用 = {
    豫言_云工_当前Unix毫秒: () => Date.now(),
    豫言_云工_性能时刻文: () => String(performance.now()),
    豫言_云工_性能原点文: () => String(performance.timeOrigin),
    // 文言：三级之记皆入标准误，冠其级名；文字原样。汉语：日志写到标准错误，每条一行，冠以级别；文字本身原样输出。
    豫言_云工_控制台文字: (方法, 内容) => {
      const 名 = 文字(方法);
      const 冠 = {debug: '[调试] ', error: '[错误] ', info: '[信息] ', log: '[日志] ', warn: '[警告] '}[名];
      if (!冠) throw Error('控制台文字级别无效');
      输出(2, 连字节(编码器.encode(冠), 内容, 换行));
    },
    豫言_云工_全局句柄: 名 => 句柄.登记(取全局(名)),
    豫言_云工_读取属性: (号, 名) => JSON.stringify(句柄.出(句柄.取得(文字(号))[句柄.允名(文字(名))])),
    豫言_云工_设置对象属性: (号, 名, 值文) => {
      句柄.取得(文字(号))[句柄.允名(文字(名))] = 句柄.入(JSON.parse(文字(值文)));
    },
    豫言_云工_调用方法: async (号, 名, 参数文) => {
      const 对象 = 句柄.取得(文字(号));
      const 方法 = 对象[句柄.允名(文字(名))];
      if (typeof 方法 !== 'function') throw Error('宿主成员不是方法');
      return JSON.stringify(句柄.出(await Reflect.apply(方法, 对象, 句柄.参数(文字(参数文)))));
    },
    豫言_云工_调用方法安全: async (号, 名, 参数文) => {
      try {
        const 对象 = 句柄.取得(文字(号));
        const 方法 = 对象[句柄.允名(文字(名))];
        if (typeof 方法 !== 'function') throw Error('宿主成员不是方法');
        return [true, JSON.stringify(句柄.出(await Reflect.apply(方法, 对象, 句柄.参数(文字(参数文)))))];
      } catch (错) { return [false, JSON.stringify({名称: String(错?.name ?? 'Error'), 消息: String(错?.message ?? 错)})]; }
    },
    豫言_云工_调用方法原始: (号, 名, 参数文) => {
      const 对象 = 句柄.取得(文字(号));
      const 方法 = 对象[句柄.允名(文字(名))];
      if (typeof 方法 !== 'function') throw Error('宿主成员不是方法');
      return JSON.stringify(句柄.出(Reflect.apply(方法, 对象, 句柄.参数(文字(参数文)))));
    },
    豫言_云工_等待句柄: async 号 => JSON.stringify(句柄.出(await 句柄.取得(文字(号)))),
    豫言_云工_构造对象: (名, 参数文) => {
      const 构造 = 取全局(名);
      if (typeof 构造 !== 'function') throw Error('构造器不存在');
      return JSON.stringify(句柄.出(Reflect.construct(构造, 句柄.参数(文字(参数文)))));
    },
    豫言_云工_调用全局: async (名, 参数文) => {
      const 函数 = 取全局(名);
      if (typeof 函数 !== 'function') throw Error('全局函数不存在');
      return JSON.stringify(句柄.出(await Reflect.apply(函数, globalThis, 句柄.参数(文字(参数文)))));
    },
    豫言_云工_调用全局安全: async (名, 参数文) => {
      try {
        const 函数 = 取全局(名);
        if (typeof 函数 !== 'function') throw Error('全局函数不存在');
        return [true, JSON.stringify(句柄.出(await Reflect.apply(函数, globalThis, 句柄.参数(文字(参数文)))))];
      } catch (错) { return [false, JSON.stringify({名称: String(错?.name ?? 'Error'), 消息: String(错?.message ?? 错)})]; }
    },
    豫言_云工_网址编解码安全: (方法, 内容) => {
      const 名 = 文字(方法);
      if (!['encodeURI', 'encodeURIComponent', 'decodeURI', 'decodeURIComponent'].includes(名)) return [false, '方法不受支持'];
      try { return [true, globalThis[名](文字(内容))]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    豫言_云工_释放句柄: 号 => { 句柄.释放(文字(号)); },
    豫言_云工_句柄取字节: async 号 => {
      const 值 = 句柄.取得(文字(号));
      if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
      if (ArrayBuffer.isView(值)) return new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice();
      if (值 instanceof Blob) return new Uint8Array(await 值.arrayBuffer());
      throw Error('句柄不是二进制对象');
    },
    豫言_云工_字节成句柄: 内容 => 句柄.登记(内容.slice()),
    豫言_云工_编码UTF8: 内容 => 编码器.encode(文字(内容)),
    豫言_云工_解码文字: (标记, 严格, 略首, 内容) =>
      new TextDecoder(文字(标记), {fatal: Boolean(严格), ignoreBOM: Boolean(略首)}).decode(内容),
    豫言_云工_创建解码器: (标记, 严格, 略首) =>
      句柄.登记(new TextDecoder(文字(标记), {fatal: Boolean(严格), ignoreBOM: Boolean(略首)})),
    豫言_云工_续解码: (号, 内容) => 句柄.取得(文字(号)).decode(内容, {stream: true}),
    豫言_云工_终解码: 号 => 句柄.取得(文字(号)).decode(),
    豫言_云工_解Base64安全: 内容 => {
      try { return [true, Uint8Array.from(atob(文字(内容)), 字 => 字.charCodeAt(0))]; }
      catch { return [false, new Uint8Array(0)]; }
    },
    豫言_云工_物字造字节: (内容, 类别) => 句柄.登记(new Blob([内容.slice()], {type: 文字(类别)})),
    豫言_云工_物字造组合: (部件文, 选项文) => 句柄.登记(new Blob(句柄.参数(文字(部件文)), 句柄.入(JSON.parse(文字(选项文))))),
    豫言_云工_物字切片: (号, 起, 止, 类别) => 句柄.登记(句柄.取得(文字(号)).slice(Number(起), Number(止), 文字(类别))),
    豫言_云工_物字切片至尾: (号, 起, 类别) => 句柄.登记(句柄.取得(文字(号)).slice(Number(起), undefined, 文字(类别))),
    豫言_云工_物字原字: async 号 => new Uint8Array(await 句柄.取得(文字(号)).arrayBuffer()),
    豫言_云工_物字文字: async 号 => await 句柄.取得(文字(号)).text(),
    豫言_云工_物字流: 号 => 句柄.登记(句柄.取得(文字(号)).stream()),
    豫言_云工_压缩流创建: 格式 => 句柄.登记(new CompressionStream(文字(格式))),
    豫言_云工_解压流创建: 格式 => 句柄.登记(new DecompressionStream(文字(格式))),
    豫言_云工_压缩流读端: 号 => 句柄.登记(句柄.取得(文字(号)).readable),
    豫言_云工_压缩流写端: 号 => 句柄.登记(句柄.取得(文字(号)).writable),
    豫言_云工_打开可读流: 号 => 句柄.登记(句柄.取得(文字(号)).getReader()),
    豫言_云工_读取流块: async 号 => {
      const 结果 = await 句柄.取得(文字(号)).read();
      if (结果.done) return [true, new Uint8Array()];
      return [false, 取流字节(结果.value)];
    },
    豫言_云工_读取流块安全: async 号 => {
      try {
        const 结果 = await 句柄.取得(文字(号)).read();
        if (结果.done) return [true, '{"已终":true}'];
        return [true, JSON.stringify({已终: false, 字节句柄: 句柄.登记(取流字节(结果.value))})];
      } catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    豫言_云工_释放流读取器: 号 => {
      句柄.取得(文字(号)).releaseLock();
      句柄.释放(文字(号));
    },
    豫言_云工_密码随机识别: () => crypto.randomUUID(),
    豫言_云工_密码随机字节: 长度 => {
      const 数值 = Number(长度);
      if (!Number.isSafeInteger(数值) || 数值 < 0 || 数值 > 65536) throw Error('随机字节长度无效');
      return crypto.getRandomValues(new Uint8Array(数值));
    },
    豫言_云工_密码摘要: async (算法, 内容) => new Uint8Array(await crypto.subtle.digest(文字(算法), 内容)),
    豫言_云工_密码PBKDF2派生字节: async (口令, 盐, 轮数, 散列, 位数) => {
      const 基钥 = await crypto.subtle.importKey('raw', 口令, 'PBKDF2', false, ['deriveBits']);
      return new Uint8Array(await crypto.subtle.deriveBits({name: 'PBKDF2', salt: 盐, iterations: Number(轮数), hash: 文字(散列)}, 基钥, Number(位数)));
    },
    豫言_云工_密码HKDF派生字节: async (原钥, 盐, 用途, 散列, 位数) => {
      const 基钥 = await crypto.subtle.importKey('raw', 原钥, 'HKDF', false, ['deriveBits']);
      return new Uint8Array(await crypto.subtle.deriveBits({name: 'HKDF', salt: 盐, info: 用途, hash: 文字(散列)}, 基钥, Number(位数)));
    },
    // 文言：环境之授以 --允许环境 为准；未授者败，已授而阙或空者归阴。汉语：环境变量以 --允许环境 授权：只认种类 ENV；未授权的种类或名称抛出部署错误；已授权而缺失或为空时返回阴、读出空串。
    豫言_云工_授权绑定存在: (种类, 名) => {
      const 类 = 文字(种类), 称 = 文字(名);
      if (类 !== 'ENV' || !授权.环境.has(称)) throw Error('未授权的' + 类 + '绑定：' + 称);
      return Boolean(process.env[称]);
    },
    豫言_云工_环境文字: 名 => {
      const 称 = 文字(名);
      if (!授权.环境.has(称)) throw Error('未授权的ENV绑定：' + 称);
      if (!Object.hasOwn(process.env, 称)) throw Error('绑定不存在：' + 称);
      return process.env[称];
    }
  };

  // 文言：文件之能：目录由宿主预授，路径不得越其界；柄号为不可推之随机数，宿主核其类与存亡。
  // 汉语：Node 文件原语（新外调名）：目录权来自 --授权目录；路径先按规范校验，再求真实路径并确认仍在授权目录内（防符号链接逃逸）；句柄号是不可猜测的随机数，宿主核对种类与有效期。
  const 节点文件 = {
    豫言_节点_文件取得目录: 名 => {
      const 名称 = 文字(名);
      const 授 = 授权.目录.get(名称);
      if (!授) return [码.未获授权, '目录未获授权：' + 名称];
      let 根;
      try {
        根 = 文件系统.realpathSync(授.路径);
        if (!文件系统.statSync(根).isDirectory()) return [码.不存在, '授权路径不是目录：' + 名称];
      } catch (错) { return [系统错码(错), 错文(错)]; }
      return [码.成功, 登记资源({种: '目录', 根, 可写: 授.可写})];
    },
    豫言_节点_文件打开: (目录号, 相对, 要写) => {
      const 目录 = 取目录(目录号);
      if (!目录) return [码.已失效, '目录权无效或已失效'];
      let 径;
      try { 径 = 严格解码.decode(相对); } catch { return [码.输入无效, '路径不是有效的 UTF-8']; }
      if (!合法相对路径(径)) return [码.输入无效, '路径无效：' + 径];
      const 可写 = Boolean(要写);
      if (可写 && !目录.可写) return [码.未获授权, '目录只授读取'];
      let 实, 描述符;
      try { 实 = 文件系统.realpathSync(路径.join(目录.根, ...径.split('/'))); } catch (错) { return [系统错码(错), 错文(错)]; }
      // 文言：先求真径而后开之，其间可换链；待办事项：逐段 O_NOFOLLOW 以绝其隙。汉语：先求真实路径再打开，两步之间符号链接仍可能被替换；待办事项：逐段用 O_NOFOLLOW 打开以消除竞态。
      if (!在根内(目录.根, 实)) return [码.输入无效, '路径越出授权目录'];
      try { 描述符 = 文件系统.openSync(实, 可写 ? 文件系统.constants.O_WRONLY : 文件系统.constants.O_RDONLY); }
      catch (错) { return [系统错码(错), 错文(错)]; }
      try {
        if (!文件系统.fstatSync(描述符).isFile()) { 文件系统.closeSync(描述符); return [码.输入无效, '路径不是普通文件']; }
      } catch (错) { 文件系统.closeSync(描述符); return [系统错码(错), 错文(错)]; }
      return [码.成功, 登记资源({种: '文件', 描述符, 可写})];
    },
    豫言_节点_文件读取: (文件号, 上限) => {
      const 文件 = 取文件(文件号);
      if (!文件) return [码.已失效, 空字节(), '文件柄无效或已失效'];
      if (文件.可写) return [码.未获授权, 空字节(), '可写文件柄不可读'];
      const 限 = Number(上限);
      if (!(限 >= 0)) return [码.输入无效, 空字节(), '读取上限须非负'];
      if (限 === 0) return [码.成功, 空字节(), ''];
      const 缓 = new Uint8Array(Math.min(限, 交换上限));
      let 读数;
      try { 读数 = 文件系统.readSync(文件.描述符, 缓, 0, 缓.length, null); }
      catch (错) { return [系统错码(错), 空字节(), 错文(错)]; }
      return 读数 === 0 ? [码.读尽, 空字节(), ''] : [码.成功, 缓.slice(0, 读数), ''];
    },
    豫言_节点_文件写入: (文件号, 内容) => {
      const 文件 = 取文件(文件号);
      if (!文件) return [码.已失效, 0, '文件柄无效或已失效'];
      if (!文件.可写) return [码.未获授权, 0, '只读文件柄不可写'];
      if (内容.length === 0) return [码.成功, 0, ''];
      try {
        const 写数 = 文件系统.writeSync(文件.描述符, 内容);
        return 写数 > 0 ? [码.成功, 写数, ''] : [码.暂不可用, 0, '暂时不能写入'];
      } catch (错) { return [系统错码(错), 0, 错文(错)]; }
    },
    豫言_节点_文件关闭: 文件号 => {
      const 名 = 文字(文件号), 文件 = 取文件(名);
      if (!文件) return [码.已失效, '文件柄无效或已失效'];
      资源.delete(名);
      try { 文件系统.closeSync(文件.描述符); return [码.成功, '']; }
      catch (错) { return [码.操作失败, 错文(错)]; }
    }
  };

  // 文言：出站之求：先核法、址、标头，次核授权之源，不随重定向；正文逾限则止。
  // 汉语：Node 网络原语（新外调名）：先核方法、绝对 http(s) 网址与标头（不合为输入无效），再核目标来源是否在 --允许源 里（否则未获授权）；不跟随重定向（3xx 跳转为宿主操作失败）；请求与响应正文各以 16 MiB 为上限，超出为资源配额已尽。
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
      if (!授权.源.has(目标.origin)) return 网络败(码.未获授权, '目标来源未获授权：' + 目标.origin);
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
    写错误: 文 => 输出(2, 编码器.encode(文 + '\n'))
  });
  // 文言：系统库之调，惟 --允许系统库调用 乃开；张量之存可取其原生之址，供金属等后端零抄。汉语：系统库调用原语（见 外部库.mjs）只在给了 --允许系统库调用 时可用；
  //       张量存储的原生地址按张量句柄从资源表取（张量.mjs 的资源项：种 为 '张量'，存.缓 为 SharedArrayBuffer），供金属等后端零拷贝；
  //       给的是整块存储的起址，张量视图的起点与步长（项.起、项.步）另计。
  const 节点外部库 = 创建外部库能力({文字, 允许: 允许系统库调用, 取张量存储: 号 => {
    const 项 = 资源.get(文字(号));
    return 项?.种 === '张量' ? 项.存?.缓 ?? null : null;
  }});
  return Object.freeze({...标准, ...云工通用, ...节点文件, ...节点网络, ...节点张量, ...节点图形.原语, ...节点外部库, [能力清理]: 节点图形.清理});
}

// 文言：以 JSPI 行客：诸能或同步或异步，客皆以常调用视之。汉语：用 JSPI 运行：能力可以同步返回，也可以返回 Promise（网络、摘要等），应用都按普通调用看待。
export async function 运行节点应用({程序模块, 值桥模块, 能力, 输出 = 写出}) {
  须(typeof WebAssembly.Suspending === 'function' && typeof WebAssembly.promising === 'function', '当前 Node 缺少 WebAssembly JSPI（需要 Node 26 或更新版本）');
  const 桥 = 创建值桥(值桥模块);
  const 名称缓存 = new WeakMap();
  const 调用 = (名, 参) => {
    let 名称 = 名称缓存.get(名);
    if (名称 === undefined) { 名称 = 文字(桥.解(名)); 名称缓存.set(名, 名称); }
    if (!Object.hasOwn(能力, 名称)) throw Error('节点宿主未提供此能力：' + 名称);
    let 形参;
    if (名称 === '豫言_打印通用值') {
      const [消息, 值] = 桥.诊断参数(参);
      if (消息 === '模式匹配失败式') throw Error('豫言模式匹配失败：' + JSON.stringify(值, (_, 项) => typeof 项 === 'bigint' ? String(项) : 项 instanceof Uint8Array ? 文字(项) : 项));
      形参 = [消息, 值];
    } else {
      形参 = 桥.解(参);
      if (!Array.isArray(形参)) throw Error('宿主参数格式错误');
    }
    const 果 = 能力[名称](...形参);
    return 果 instanceof Promise ? 果.then(值 => 桥.编(值)) : 桥.编(果);
  };
  const 实例 = new WebAssembly.Instance(程序模块, {'yuyan:gc-host/v1': {call: new WebAssembly.Suspending(调用)}});
  须(typeof 实例.exports._start === 'function', 'Wasm 缺少程序启动导出');
  try {
    await WebAssembly.promising(实例.exports._start)();
    return 0;
  } catch (错) {
    if (错 instanceof 客体退出) return 错.退出码;
    输出(2, 编码器.encode('豫言程序运行失败：' + (错?.stack ?? String(错)) + '\n'));
    return 1;
  }
}

// 文言：许系统库之调、程序用之而诺节未带 --experimental-ffi，则带之以同参重启此文，承三流，以其退码终；诺节不识此旗则不重启，原语自报不可用。
// 汉语：允许了系统库调用、程序里有系统库调用的外调名（前缀 豫言_节点_外部库）而诺节没带 --experimental-ffi 时，带上此旗以同样的参数重新启动本文件
//       （继承标准流），以子进程的退出码退出；诺节不认此旗时不重启，系统库调用原语报告不可用。
const 外部库旗 = '--experimental-ffi';
const 须带外部库旗重启 = 程序字节 => process.allowedNodeEnvironmentFlags.has(外部库旗) && !process.execArgv.includes(外部库旗) &&
  !/(?:^|\s)--experimental-ffi(?:\s|$)/u.test(process.env.NODE_OPTIONS ?? '') && 程序字节.includes('豫言_节点_外部库');
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
    if (配置.允许系统库调用 && 须带外部库旗重启(程序字节)) return 带外部库旗重启(参数);
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
