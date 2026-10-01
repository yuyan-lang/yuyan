// 文言：桥唯传客值，毋决应用之事。汉语：值桥只转换 WasmGC 值，不承载业务规则。
// 文言：带型之导入经边界胶水；本目之 边界.mjs 于仓中转出网页汇编之本体，构建时以本体代之。汉语：带类型的导入经共用边界胶水；本目录的 边界.mjs 在仓库里转出 ../网页汇编/边界.mjs，构建器把胶水本体复制成产物里的 边界.mjs。
//   节点发行启动文件内联本文件时，先内联胶水，再去掉下面这行相对导入。
import {造边界导入, 造边界导出, 启动导出名} from './边界.mjs';
const 编码 = new TextEncoder();
const 解码 = new TextDecoder('utf-8', {ignoreBOM: true});
export const 文字 = 值 => 值 instanceof Uint8Array ? 解码.decode(值) : String(值);

// 文言：小数之文三术，诸宿主同义：精确者依 %.17g，表示者依 %f，理解者依 strtod。汉语：小数文字的三个函数，各宿主语义统一（见 ../标准库宿主.汉语.md）：
//   精确小数同 C 的 %.17g；小数表示同 C 的 %f（-0 写 -0.000000，≥1e21 展开整数，非有限写 nan、inf、-inf）；理解小数同 strtod（跳过空白取前缀，认 inf、infinity、nan，无可转得 0）。
const 数 = 值 => Number(值?.小数 ?? 值);
export function 精确小数(值) {
  const 数字 = 数(值);
  if (Object.is(数字, -0)) return '-0';
  if (!Number.isFinite(数字)) return String(数字).toLowerCase().replace('infinity', 'inf');
  const [尾, 指数] = 数字.toExponential(16).split('e');
  const 幂 = Number(指数);
  if (幂 < -4 || 幂 >= 17) return 尾.replace(/\.?0+$/u, '') + 'e' + (幂 >= 0 ? '+' : '-') + String(Math.abs(幂)).padStart(2, '0');
  return 数字.toFixed(Math.max(0, 16 - 幂)).replace(/(\.\d*?)0+$/u, '$1').replace(/\.$/u, '');
}
export function 小数表示(值) {
  const 数字 = 数(值);
  if (!Number.isFinite(数字)) return 精确小数(值);
  if (Object.is(数字, -0)) return '-0.000000';
  return Math.abs(数字) >= 1e21 ? BigInt(数字).toString() + '.000000' : 数字.toFixed(6);
}
export function 理解小数(值) {
  const 串 = 文字(值).trim(), 数字 = parseFloat(串);
  return /^[+-]?nan/iu.test(串) ? NaN : /^[+-]?inf/iu.test(串) ? (串.startsWith('-') ? -Infinity : Infinity) : Number.isNaN(数字) ? 0 : 数字;
}
// 文言：随机整数：界非正则止；以六十四位之安全随机取余，不保均布。汉语：随机整数：上界不大于零时中止；取 64 位安全随机数对上界取余，与标准库注释“不保证均匀”相符。
export function 随机整数(上界) {
  if (上界 <= 0n) throw Error('随机整数上界须大于零');
  const 组 = crypto.getRandomValues(new Uint32Array(2));
  return ((BigInt(组[0]) << 32n) | BigInt(组[1])) % 上界;
}
// 文言：可并行之数，不知则一。汉语：宿主可用的并行数，不知道时返回 1。
export const 处理器数量 = () => BigInt(Math.max(1, Math.trunc(Number(globalThis.navigator?.hardwareConcurrency)) || 1));
// 文言：客之退出以异常穿栈，宿主捕之：码零为成，余为败。汉语：应用调用退出进程时抛出此异常穿过 Wasm 栈，宿主捕获：退出码 0 算成功，其余算失败。
export class 豫言退出 extends Error {
  constructor(码) { super('豫言程序退出：' + String(码)); this.退出码 = 码; }
}

export function 创建值桥(模块) {
  const 桥 = new WebAssembly.Instance(模块).exports;
  const 留 = 长 => {
    if (!Number.isSafeInteger(长) || 长 < 0 || 长 > 16 * 1024 * 1024) throw Error('宿主交换数据超过上限');
    const 差 = 长 - 桥.memory.buffer.byteLength;
    if (差 > 0) 桥.memory.grow(Math.ceil(差 / 65536));
  };
  const 解 = (值, 深 = 0) => {
    if (深 > 64) throw Error('宿主值嵌套过深');
    switch (桥.kind(值)) {
      case 0: return null;
      case 1: case 4: return 桥.int(值);
      case 5: return 桥.float(值);
      case 2: {
        const 长 = 桥.bytes_len(值);
        留(长);
        桥.bytes_out(值);
        return new Uint8Array(桥.memory.buffer, 0, 长).slice();
      }
      case 3: return Array.from({length: 桥.tuple_len(值)}, (_, 序) => 解(桥.tuple_get(值, 序), 深 + 1));
      default: throw Error('未知豫言客值');
    }
  };
  const 编 = (值, 深 = 0) => {
    if (深 > 64) throw Error('宿主值嵌套过深');
    if (值 == null) return null;
    if (typeof 值 === 'boolean') return 桥.new_int(值 ? 1n : 0n);
    if (typeof 值 === 'bigint' || Number.isSafeInteger(值)) return 桥.new_int(BigInt(值));
    if (typeof 值 === 'string') 值 = 编码.encode(值);
    if (值 instanceof Uint8Array) {
      留(值.length);
      new Uint8Array(桥.memory.buffer, 0, 值.length).set(值);
      return 桥.bytes_in(值.length);
    }
    if (Array.isArray(值)) {
      const 组 = 桥.new_tuple(值.length);
      值.forEach((项, 序) => 桥.tuple_set(组, 序, 编(项, 深 + 1)));
      return 组;
    }
    if (typeof 值 === 'object' && Object.hasOwn(值, '小数')) return 桥.new_float(值.小数);
    throw Error('未知宿主值');
  };
  // 文言：异类值未必可由通用桥析；诊断仅尽力观之。汉语：模式失败时尝试读取打印参数，无法跨桥的代数值保留为占位说明。
  const 诊断参数 = 值 => {
    let 标签 = '未知诊断';
    let 内容 = '<豫言值不可经宿主值桥解码>';
    try { 标签 = 文字(解(桥.tuple_get(值, 0))); } catch { /* 保留标签占位。 */ }
    try { 内容 = 解(桥.tuple_get(值, 1)); } catch { /* 保留值占位。 */ }
    return [标签, 内容];
  };
  // 文言：原者，桥之本出，供边界胶水用之。汉语：原 是值桥实例的 exports，交给边界胶水读写带类型导入的值。
  return {编, 解, 诊断参数, 原: 桥};
}

// 文言：由新式之实派生旧通调之原语：小数之果包为 {小数}，列之果附其长。汉语：由新式实现派生旧 call 原语：小数结果包成 {小数}，列结果附上长度（旧形是“数组加长度”）。
const 旧小数果 = new Set(['获取当前纳秒时间', '获取随机小数', '字符串转小数']), 旧列果 = new Set(['获取命令行参数', '同步列出文件夹']);
export const 旧式原语 = 表 => Object.fromEntries(Object.entries(表).map(([名, 函]) => ['豫言_' + 名,
  旧小数果.has(名) ? (...参) => ({小数: 函(...参)}) : 旧列果.has(名) ? (...参) => { const 列 = 函(...参); return [列, 列.length]; } : 函]));

// 文言：仅具名之术得入客器。汉语：每次实例只开放调用者传入的具名能力。原语 是旧 call 的具名能力；标准库 可覆盖或补充导入模块「标准库」的实现（键为字段名）。
//   每次调用都新建一套闭包（云工每个事件一套，不共用“当前事件”状态）；签名解析按模块缓存在胶水里。
export function 创建豫言实例(程序模块, 值桥模块, 原语, {输出 = () => {}, 错误输出 = 文 => globalThis.console?.error?.(文), 参数 = [], 时限毫秒 = 30000, 标准库 = {}} = {}) {
  if (typeof WebAssembly.Suspending !== 'function' || typeof WebAssembly.promising !== 'function') {
    throw Error('宿主缺少 WebAssembly JSPI');
  }
  const 桥 = 创建值桥(值桥模块);
  const 截止 = performance.now() + 时限毫秒;
  // 文言：标准库之宿主服务，以导入模块「标准库」之字段名为键；无者为桩。汉语：标准库宿主服务（导入模块「标准库」，键为字段名），云工与浏览器提供这二十个，其余给桩；语义见 ../标准库宿主.汉语.md。
  const 标准库实现 = {
    获取命令行程序名: () => '/程序.wasm',
    获取命令行参数: () => 参数,
    获取当前工作目录: () => '/',
    // 文言：环境之名未授于客，皆如不存。汉语：本宿主不向标准库授予任何环境变量名，一律当作不存在。
    获取环境变量: () => [false, ''],
    获取当前纳秒时间: () => performance.now() * 1e6,
    运行于Windows: () => false,
    运行于MacOS: () => false,
    运行于Linux: () => false,
    标准输出是终端: () => false,
    标准输入是终端: () => false,
    尝试读取标准输入行: () => [false, ''],
    打印行: 值 => { 输出(文字(值) + '\n'); },
    打印字符串: 值 => { 输出(文字(值)); },
    // 文言：误出入宿主之误流；无独立之流者，书于日志之误级。汉语：标准错误写宿主的错误流；云工与浏览器没有独立错误流，缺省写日志的错误级（console.error）。
    标准错误打印行: 值 => { 错误输出(文字(值)); },
    小数转字符串: 小数表示,
    小数精确表示: 精确小数,
    字符串转小数: 理解小数,
    在线处理器数量: 处理器数量,
    获取随机整数: 随机整数,
    退出进程: 码 => { throw new 豫言退出(Number(码)); },
    ...标准库
  };
  const 内建 = {
    ...旧式原语(标准库实现),
    // 文言：模式不配之诊须得书，免其本因被未授权之报蔽。汉语：编译器在模式匹配失败时调用此原语，输出诊断值。
    豫言_打印通用值: (消息, 值) => {
      输出('[豫言通用值打印] ' + 文字(消息) + ': ' + JSON.stringify(值, (_, 项) => typeof 项 === 'bigint' ? String(项) : 项) + '\n');
      return null;
    },
    豫言_字节转字符串: 值 => { if (值 <= 0n || 值 > 255n) throw Error('字节值越界'); return Uint8Array.of(Number(值)); },
    豫言_字节串_空: () => new Uint8Array(),
    豫言_字节串_长度: 值 => BigInt(值.length),
    豫言_字节串_取字节: (值, 序) => {
      const 号 = Number(序);
      if (!Number.isSafeInteger(号) || 号 < 0 || 号 >= 值.length) throw Error('字节串索引越界');
      return BigInt(值[号]);
    },
    豫言_字节串_从字符串: 值 => 值.slice(),
    豫言_字节串_单字节: 值 => {
      if (值 < 0n || 值 > 255n) throw Error('字节值越界');
      return Uint8Array.of(Number(值));
    },
    豫言_字节串_拼接: (甲, 乙) => {
      const 果 = new Uint8Array(甲.length + 乙.length);
      果.set(甲);
      果.set(乙, 甲.length);
      return 果;
    },
    // 文言：截取以起点与长度，越界则止，同原生运行时。汉语：按起点与长度截取，越界报错，语义同原生运行时（字节串.c）。
    豫言_字节串_截取: (值, 起, 长) => {
      if (起 < 0n || 长 < 0n || 起 > BigInt(值.length) || 长 > BigInt(值.length) - 起) throw Error('截取字节串：范围越界');
      return 值.slice(Number(起), Number(起 + 长));
    },
    豫言_整数转小数: 值 => ({小数: Number(值)}),
    豫言_小数转整数: 值 => BigInt(Math.trunc(Number(值))),
    豫言_整数加: (甲, 乙) => BigInt.asIntN(64, 甲 + 乙),
    豫言_整数乘: (甲, 乙) => BigInt.asIntN(64, 甲 * 乙),
    豫言_整数除: (甲, 乙) => 甲 / 乙,
    豫言_小数加: (甲, 乙) => ({小数: 甲 + 乙}),
    豫言_小数减: (甲, 乙) => ({小数: 甲 - 乙}),
    豫言_小数乘: (甲, 乙) => ({小数: 甲 * 乙}),
    豫言_小数除: (甲, 乙) => ({小数: 甲 / 乙}),
    豫言_整数转字符串: 值 => String(值),
    豫言_字符串转整数: 值 => BigInt(文字(值).match(/^\s*[+-]?\d+/u)?.[0].trim() ?? '0'),
    豫言_源码数字名: 值 => /^[0-9-]+$/u.test(文字(值)),
    豫言_源码可用名: 值 => !/^[0-9-]+$/u.test(文字(值)) && !文字(值).startsWith('《《') && !文字(值).startsWith('：') && !文字(值).includes('」'),
    豫言_源码字符串表示: 值 => '『' + 文字(值).replace(/「：|』/gu, 字 => 字 === '』' ? '「：』：」' : '「：「：：」') + '』'
  };
  const 能力 = Object.freeze({...内建, ...原语});
  const 调用 = async (名, 参) => {
    if (performance.now() > 截止) throw Error('豫言执行超过时限');
    const 名称 = 文字(桥.解(名));
    if (!Object.hasOwn(能力, 名称)) throw Error('未授权宿主能力：' + 名称);
    if (名称 === '豫言_打印通用值') {
      const [消息, 值] = 桥.诊断参数(参);
      if (消息 === '模式匹配失败式') throw Error('豫言模式匹配失败：' + String(值));
      return 桥.编(await 能力[名称](消息, 值));
    }
    const 形参 = 桥.解(参);
    if (!Array.isArray(形参)) throw Error('宿主参数格式错误');
    return 桥.编(await 能力[名称](...形参));
  };
  // 文言：带型之导入亦于每调验其时限，同旧通调。汉语：带类型的导入同样在每次调用时检查墙钟时限（与旧 call 相同，见说明“每事件墙钟时限”）；时限无穷时不包。
  const 限时 = Number.isFinite(截止) ? 函 => Object.assign((...参) => {
    if (performance.now() > 截止) throw Error('豫言执行超过时限');
    return 函(...参);
  }, {异步: 函.异步}) : 函 => 函;
  const 带型导入 = 造边界导入(程序模块, 桥.原, {标准库: Object.fromEntries(Object.entries(标准库实现).map(([名, 函]) => [名, 限时(函)]))});
  const 实例 = new WebAssembly.Instance(程序模块, {
    ...带型导入,
    'yuyan:gc-host/v1': {call: new WebAssembly.Suspending(调用)},
    'yuyan:browser/v1': {
      check() { if (performance.now() > 截止) throw Error('豫言执行超过时限'); },
      fail(值) { throw Error(文字(桥.解(值))); }
    }
  });
  const 启动 = WebAssembly.promising(实例.exports._start);
  // 文言：退出码零为成，余为败。汉语：退出进程的码为 0 时正常结束本次运行，其余以“豫言程序退出：码”失败。
  const 运行 = async () => {
    try {
      await 启动();
      // 文言：_start 毕，应用实现启动之术者，乃调其导出。汉语：_start 之后，应用若实现了「启动程序」（导出 豫言操作系统启动/启动程序）就调用它；旧产物没有这个导出。
      const 启动导出 = 造边界导出(实例, 程序模块, 桥.原, {异步: true})[启动导出名];
      if (启动导出) await 启动导出();
    }
    catch (错) { if (!(错 instanceof 豫言退出) || 错.退出码 !== 0) throw 错; }
  };
  return {运行, 实例};
}
