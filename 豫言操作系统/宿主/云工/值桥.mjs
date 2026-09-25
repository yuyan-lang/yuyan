// 文言：桥唯传客值，毋决应用之事。汉语：值桥只转换 WasmGC 值，不承载业务规则。
const 编码 = new TextEncoder();
const 解码 = new TextDecoder();
export const 文字 = 值 => 值 instanceof Uint8Array ? 解码.decode(值) : String(值);

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
  return {编, 解, 诊断参数};
}

// 文言：仅具名之术得入客器。汉语：每次实例只开放调用者传入的具名能力。
export function 创建豫言实例(程序模块, 值桥模块, 原语, {输出 = () => {}, 参数 = [], 时限毫秒 = 30000} = {}) {
  if (typeof WebAssembly.Suspending !== 'function' || typeof WebAssembly.promising !== 'function') {
    throw Error('宿主缺少 WebAssembly JSPI');
  }
  const 桥 = 创建值桥(值桥模块);
  const 截止 = performance.now() + 时限毫秒;
  const 内建 = {
    豫言_获取命令行程序名: () => '/程序.wasm',
    豫言_获取命令行参数: () => [参数, 参数.length],
    豫言_获取当前工作目录: () => '/',
    豫言_获取环境变量: () => [false, ''],
    豫言_获取当前纳秒时间: () => ({小数: performance.now() * 1e6}),
    豫言_运行于Windows: () => false,
    豫言_运行于MacOS: () => false,
    豫言_运行于Linux: () => false,
    豫言_标准输出是终端: () => false,
    豫言_标准输入是终端: () => false,
    豫言_尝试读取标准输入行: () => [false, ''],
    豫言_打印行: 值 => { 输出(文字(值) + '\n'); return null; },
    豫言_打印字符串: 值 => { 输出(文字(值)); return null; },
    豫言_标准错误打印行: 值 => { 输出(文字(值) + '\n'); return null; },
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
    豫言_小数转字符串: 值 => Number(值).toFixed(6),
    豫言_小数精确表示: 值 => String(值),
    豫言_字符串转小数: 值 => ({小数: Number(文字(值)) || 0}),
    豫言_源码数字名: 值 => /^[0-9-]+$/u.test(文字(值)),
    豫言_源码可用名: 值 => !/^[0-9-]+$/u.test(文字(值)) && !文字(值).startsWith('《《') && !文字(值).startsWith('：') && !文字(值).includes('」'),
    豫言_源码字符串表示: 值 => '『' + 文字(值).replace(/「：|』/gu, 字 => 字 === '』' ? '「：』：」' : '「：「：：」') + '』',
    豫言_退出进程: 码 => { throw Error('豫言程序退出：' + String(码)); }
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
  const 实例 = new WebAssembly.Instance(程序模块, {
    'yuyan:gc-host/v1': {call: new WebAssembly.Suspending(调用)},
    'yuyan:browser/v1': {
      check() { if (performance.now() > 截止) throw Error('豫言执行超过时限'); },
      fail(值) { throw Error(文字(桥.解(值))); }
    }
  });
  return {运行: WebAssembly.promising(实例.exports._start), 实例};
}
