// 文言：桥唯传客值，毋决应用之事。汉语：值桥只转换 WasmGC 值，不承载业务规则。
// 文言：带型之导入经边界胶水；本目之 边界.mjs 于仓中转出网页汇编之本体，构建时以本体代之。汉语：带类型的导入经共用边界胶水；本目录的 边界.mjs 在仓库里转出 ../网页汇编/边界.mjs，构建器把胶水本体复制成产物里的 边界.mjs。
// 文言：取之不得（旧部署之静态白名单无此文）则为空，无带型导入之程照行。汉语：用顶层 await 动态载入：取不到时（旧部署的静态白名单里还没有 边界.mjs）为空，没有带类型导入的程序照常运行，有带类型导入的程序明确报错。
const 边界胶水 = await import('./边界.mjs').catch(() => null);
const 有带型导入 = 模块 => WebAssembly.Module.imports(模块).some(项 => 项.kind === 'function' && 项.module !== 'yuyan:browser/v1');
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
  // 文言：原者，桥之本出，供边界胶水用之。汉语：原 是值桥实例的 exports，交给边界胶水读写带类型导入的值。
  return {解, 原: 桥};
}

// 文言：仅具名之术得入客器。汉语：每次实例只开放调用者传入的具名能力：标准库 可覆盖或补充导入模块「标准库」的实现（键为字段名）。
//   签名解析按模块缓存在胶水里。
// 文言：平台者，平台接口包之带型导入，形如 {包名: {函名: 术}}；术标异步者，有悬栈则套之，无则同步而调，得承诺乃拒。
// 汉语：平台 是平台接口包的带类型导入实现，形如 {模块名: {字段名: 函数}}，模块名即包名（如 浏览器宿主、中央张量宿主），字段名即接口文件里声明的函数名；
//   标“异步”的函数有 JSPI 时套 Suspending，没有 JSPI 时改为同步调用，真的返回 Promise 才报“缺少 JSPI”，所以只调同步能力的程序照常运行。
export function 创建豫言实例(程序模块, 值桥模块, {输出 = () => {}, 错误输出 = 文 => globalThis.console?.error?.(文), 参数 = [], 时限毫秒 = 30000, 标准库 = {}, 平台 = {}} = {}) {
  // 文言：无悬栈之客器可行纯同步豫言；遇异步能则明拒。汉语：没有 JSPI 时仍可运行只调用同步能力的豫言程序，异步调用会明确失败。
  const 可悬 = typeof WebAssembly.Suspending === 'function' && typeof WebAssembly.promising === 'function';
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
  // 文言：带型之导入每调验其时限。汉语：带类型的导入在每次调用时检查墙钟时限（见说明“每事件墙钟时限”）；时限无穷时不包。
  const 限时 = Number.isFinite(截止) ? 函 => Object.assign((...参) => {
    if (performance.now() > 截止) throw Error('豫言执行超过时限');
    return 函(...参);
  }, {异步: 函.异步}) : 函 => 函;
  // 文言：无悬栈者，异步之术改为同步而调：得承诺则明拒；毋使造导入之时即因无 Suspending 而败。
  // 汉语：没有 JSPI 时，标“异步”的实现改为同步调用：返回 Promise 就报“缺少 JSPI”，同步返回的照常使用；免得构造导入时因没有 Suspending 而整个程序起不来。
  const 去悬 = (名, 函) => {
    if (可悬 || !函.异步) return 函;
    return (...参) => {
      const 果 = 函(...参);
      if (果 !== null && typeof 果 === 'object' && typeof 果.then === 'function') {
        果.then(() => {}, () => {});
        throw Error('此浏览器缺少 JSPI，程序调用了异步宿主能力：' + 名);
      }
      return 果;
    };
  };
  const 备实现 = 表 => Object.fromEntries(Object.entries(表).map(([模, 字段们]) =>
    [模, Object.fromEntries(Object.entries(字段们).map(([字段, 函]) => [字段, 限时(去悬(模 + '.' + 字段, 函))]))]));
  if (!边界胶水 && 有带型导入(程序模块)) throw Error('程序有带类型的宿主导入，但取不到边界胶水 边界.mjs（须与 值桥.mjs 同目录发布）');
  const 带型导入 = 边界胶水 ? 边界胶水.造边界导入(程序模块, 桥.原, 备实现({...平台, 标准库: 标准库实现})) : {};
  const 实例 = new WebAssembly.Instance(程序模块, {
    ...带型导入,
    'yuyan:browser/v1': {
      check() { if (performance.now() > 截止) throw Error('豫言执行超过时限'); },
      fail(值) { throw Error(文字(桥.解(值))); }
    }
  });
  if (typeof 实例.exports._start !== 'function') throw Error('Wasm 缺少程序启动导出');
  const 启动 = 可悬 ? WebAssembly.promising(实例.exports._start) : async () => 实例.exports._start();
  // 文言：退出码零为成，余为败。汉语：退出进程的码为 0 时正常结束本次运行，其余以“豫言程序退出：码”失败。
  const 运行 = async () => {
    try {
      await 启动();
      // 文言：_start 毕，应用实现启动之术者，乃调其导出。汉语：_start 之后，应用若实现了「启动程序」（导出 豫言操作系统启动/启动程序）就调用它；旧产物没有这个导出。
      const 启动导出 = 边界胶水 ? 边界胶水.造边界导出(实例, 程序模块, 桥.原, {异步: 可悬})[边界胶水.启动导出名] : undefined;
      if (启动导出) await 启动导出();
    }
    catch (错) { if (!(错 instanceof 豫言退出) || 错.退出码 !== 0) throw 错; }
  };
  return {运行, 实例};
}
