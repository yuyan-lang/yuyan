// 文言：宿主边界之共用胶水：读模之自定段「豫言边界」，依各带型导入之签名，互转 Wasm 之值与 JS 之值；节点、浏览器、云工三家共用。
// 汉语：宿主边界的共用胶水：读模块的自定义段「豫言边界」，按每个带类型导入的签名把 Wasm 值与 JS 值互相转换；节点、浏览器、云工三家宿主共用。
//   形与 JS 值的对应：整 ↔ BigInt（结果也收 Number、布尔）；小 ↔ Number；爻 ↔ 布尔（结果收任何真假值）；
//   串 ↔ Uint8Array（参数给副本；结果收字符串或 Uint8Array）；元 ↔ undefined；资 ↔ JS 对象本身；
//   组〔…〕、列〔…〕 ↔ 数组；变〔…〕 ↔ [支序, …载荷]，支序从零起。嵌在元组里的值经值桥模块读写。
//   规范见 应用/豫言编译器/文档/语言技术规范/网页汇编接口 网五。
// 用法：const 导入 = 造边界导入(模块, 桥, 实现表)；new WebAssembly.Instance(模块, {...导入, 'yuyan:gc-host/v1': {call}})。
//   桥是值桥模块（yy节点值桥接.wasm）实例的 exports；实现表形如 {模块名: {字段名: 函数}}；缺的导入给桩，调用时报“接口函数未绑定”。
//   实现函数带 异步=true 时（返回 Promise），套 WebAssembly.Suspending（JSPI）；没标异步却返回 Promise 时报错。

export const 边界段名 = '豫言边界';
export const 旧宿主模块 = 'yuyan:gc-host/v1';

const 编码器 = new TextEncoder(), 解码器 = new TextDecoder();

// 文言：解签名之文为形树。汉语：把签名文字解析成形：{种, 项|元素|支}。
export function 解析签名(文) {
  const 字 = [...文];
  let 位 = 0;
  const 看 = () => 字[位];
  const 吃 = 期 => { if (字[位] !== 期) throw Error(`边界签名有误（期待“${期}”）：${文}`); 位++; };
  const 读形列 = 止们 => {
    const 列 = [];
    if (止们.includes(看())) return 列;
    for (;;) { 列.push(读形()); if (看() === '，') { 位++; continue; } return 列; }
  };
  function 读形() {
    const 种 = 字[位++];
    switch (种) {
      case '整': case '小': case '爻': case '串': case '元': case '资': return {种};
      case '组': { 吃('〔'); const 项 = 读形列(['〕']); 吃('〕'); return {种, 项}; }
      case '列': { 吃('〔'); const 元素 = 读形(); 吃('〕'); return {种, 元素}; }
      case '变': {
        吃('〔'); const 支 = [];
        for (;;) { 支.push(读形列(['｜', '〕'])); if (看() === '｜') { 位++; continue; } break; }
        吃('〕'); return {种, 支};
      }
      default: throw Error(`边界签名里有未知的形“${种}”：${文}`);
    }
  }
  const 参 = 看() === '→' ? [] : 读形列(['→']);
  吃('→');
  const 果 = 读形();
  if (位 !== 字.length) throw Error(`边界签名末尾有多余文字：${文}`);
  return {参, 果, 文};
}

// 文言：读模之边界段，得 模\t字 → 签名 之表；无段者返空。汉语：读模块的「豫言边界」段，得“模块⇥字段”到签名的表；没有这一段时返回 null。
export function 读边界段(模块) {
  const 段们 = WebAssembly.Module.customSections(模块, 边界段名);
  if (段们.length === 0) return null;
  const 表 = new Map();
  for (const 段 of 段们) {
    for (const 行 of 解码器.decode(段).split('\n')) {
      const 列 = 行.split('\t');
      if (列[0] === '导入' && 列.length === 4) 表.set(列[1] + '\t' + 列[2], 解析签名(列[3]));
    }
  }
  return 表;
}

// 文言：是豫言之 GC 模否：有边界段，或导入旧宿主调用。汉语：是否豫言 WasmGC 程序：带「豫言边界」段，或导入旧的 yuyan:gc-host/v1（①之前的产物）。
export function 是豫言模块(模块) {
  return WebAssembly.Module.customSections(模块, 边界段名).length > 0 ||
    WebAssembly.Module.imports(模块).some(项 => 项.module === 旧宿主模块);
}

// 文言：造值桥之字节读写。汉语：经值桥读写字节串：桥内存不够就加页。
function 字节术(桥) {
  const 留 = 数 => { const 差 = 数 - 桥.memory.buffer.byteLength; if (差 > 0) 桥.memory.grow(Math.ceil(差 / 65536)); };
  return {
    读: 值 => { const 长 = 桥.bytes_len(值); 留(长); 桥.bytes_out(值); return new Uint8Array(桥.memory.buffer, 0, 长).slice(); },
    造: 值 => {
      const 字节 = typeof 值 === 'string' ? 编码器.encode(值) : 值 instanceof Uint8Array ? 值 : ArrayBuffer.isView(值) ? new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength) : 值 instanceof ArrayBuffer ? new Uint8Array(值) : null;
      if (字节 === null) throw TypeError('边界“串”须是字符串或字节：' + Object.prototype.toString.call(值));
      留(字节.length); new Uint8Array(桥.memory.buffer, 0, 字节.length).set(字节); return 桥.bytes_in(字节.length);
    },
  };
}

const 整值 = 值 => typeof 值 === 'bigint' ? 值 : BigInt(typeof 值 === 'boolean' ? (值 ? 1 : 0) : 值);

// 文言：嵌于元组之值：读与造。汉语：嵌在元组里的值（客体统一表示）与 JS 值互转，按形预先做成闭包。
function 嵌读器(形, 桥, 字节) {
  switch (形.种) {
    case '整': return 值 => 桥.int(值);
    case '小': return 值 => 桥.float(值);
    case '爻': return 值 => 桥.int(值) !== 0n;
    case '串': return 字节.读;
    case '元': return () => undefined;
    case '资': return 值 => 桥.ext_get(值);
    case '组': { const 读们 = 形.项.map(项 => 嵌读器(项, 桥, 字节)); return 值 => 读们.map((读, 序) => 读(桥.tuple_get(值, 序))); }
    case '列': { const 读 = 嵌读器(形.元素, 桥, 字节); return 值 => { const 长 = 桥.tuple_len(值), 果 = new Array(长); for (let 序 = 0; 序 < 长; 序++) 果[序] = 读(桥.tuple_get(值, 序)); return 果; }; }
    case '变': {
      const 支读们 = 形.支.map(支 => 支.map(项 => 嵌读器(项, 桥, 字节)));
      return 值 => { const 序 = Number(桥.int(桥.tuple_get(值, 0))); const 读们 = 支读们[序]; if (!读们) throw Error('边界变体的支序越界：' + 序); return [序, ...读们.map((读, 位) => 读(桥.tuple_get(值, 位 + 1)))]; };
    }
  }
  throw Error('未知边界形：' + 形.种);
}

function 嵌造器(形, 桥, 字节) {
  switch (形.种) {
    case '整': return 值 => 桥.new_int(整值(值));
    case '小': return 值 => 桥.new_float(Number(值));
    case '爻': return 值 => 桥.new_int(值 ? 1n : 0n);
    case '串': return 字节.造;
    case '元': return () => null;
    case '资': return 值 => 桥.new_ext(值);
    case '组': {
      const 造们 = 形.项.map(项 => 嵌造器(项, 桥, 字节));
      return 值 => { if (!Array.isArray(值) || 值.length !== 造们.length) throw TypeError(`边界“组”须是长 ${造们.length} 的数组`); const 组 = 桥.new_tuple(造们.length); 造们.forEach((造, 序) => 桥.tuple_set(组, 序, 造(值[序]))); return 组; };
    }
    case '列': {
      const 造 = 嵌造器(形.元素, 桥, 字节);
      return 值 => { const 项们 = Array.from(值); const 组 = 桥.new_tuple(项们.length); 项们.forEach((项, 序) => 桥.tuple_set(组, 序, 造(项))); return 组; };
    }
    case '变': {
      const 支造们 = 形.支.map(支 => 支.map(项 => 嵌造器(项, 桥, 字节)));
      return 值 => {
        const [序, ...载荷] = 值; const 造们 = 支造们[序];
        if (!造们 || 载荷.length !== 造们.length) throw TypeError('边界“变”须是 [支序, …载荷]，支序或载荷个数不对：' + 序);
        const 组 = 桥.new_tuple(造们.length + 1); 桥.tuple_set(组, 0, 桥.new_int(BigInt(序)));
        造们.forEach((造, 位) => 桥.tuple_set(组, 位 + 1, 造(载荷[位]))); return 组;
      };
    }
  }
  throw Error('未知边界形：' + 形.种);
}

// 文言：顶层之参与果。汉语：顶层参数（Wasm 值 → JS）与顶层结果（JS → Wasm 值）。标量不经值桥。
function 参读器(形, 桥, 字节) {
  switch (形.种) {
    case '整': case '小': case '资': return null;
    case '爻': return 值 => 值 !== 0;
    default: return 嵌读器(形, 桥, 字节);
  }
}

function 果造器(形, 桥, 字节) {
  switch (形.种) {
    case '整': return 整值;
    case '小': return Number;
    case '爻': return 值 => (值 ? 1 : 0);
    case '元': return () => undefined;
    case '资': return null;
    default: return 嵌造器(形, 桥, 字节);
  }
}

const 是承诺 = 值 => 值 !== null && typeof 值 === 'object' && typeof 值.then === 'function';

// 文言：依签名包实现之函。汉语：按签名包装一个实现函数；标量参数原样传，减少每次调用的开销。
export function 包装实现(签名, 实, 桥, 名 = '') {
  const 字节 = 字节术(桥);
  const 读们 = 签名.参.map(形 => 参读器(形, 桥, 字节));
  const 造 = 果造器(签名.果, 桥, 字节);
  const 全直 = 读们.every(读 => 读 === null);
  const 转参 = 全直 ? null : 参们 => 读们.map((读, 序) => (读 === null ? 参们[序] : 读(参们[序])));
  if (实.异步) {
    return new WebAssembly.Suspending(async (...参们) => { const 果 = await 实(...(转参 ? 转参(参们) : 参们)); return 造 ? 造(果) : 果; });
  }
  const 验 = 果 => { if (是承诺(果)) throw Error(`宿主函数 ${名} 返回了 Promise，却没有标“异步”`); return 果; };
  if (全直 && 造 === null) return (...参们) => 验(实(...参们));
  if (全直) return (...参们) => 造(验(实(...参们)));
  return (...参们) => { const 果 = 验(实(...转参(参们))); return 造 ? 造(果) : 果; };
}

// 文言：造带型导入之物：有实现者包之，无者给桩。汉语：为模块的全部带类型导入造导入对象：有实现的按签名包装，没有的给桩（调用时报“接口函数未绑定”）。旧的 yuyan:gc-host/v1 由宿主另行提供。
export function 造边界导入(模块, 桥, 实现表 = {}, 选项 = {}) {
  const 签名表 = 读边界段(模块) ?? new Map();
  const 导入 = {}, 未绑定 = [];
  for (const {module: 模, name: 字段, kind: 种} of WebAssembly.Module.imports(模块)) {
    if (种 !== 'function' || 模 === 旧宿主模块) continue;
    const 键 = 模 + '\t' + 字段, 签名 = 签名表.get(键), 实 = 实现表[模]?.[字段];
    if (!签名) throw Error(`导入缺少边界签名（模块没有「豫言边界」段中的这一行）：${模}.${字段}`);
    let 函;
    if (typeof 实 === 'function') 函 = 包装实现(签名, 实, 桥, 模 + '.' + 字段);
    else {
      未绑定.push(模 + '.' + 字段);
      函 = 选项.桩 ? 选项.桩(模, 字段, 签名) : () => { throw Error(`接口函数未绑定：${模}.${字段}`); };
    }
    (导入[模] ??= {})[字段] = 函;
  }
  Object.defineProperty(导入, '未绑定', {value: 未绑定, enumerable: false});
  return 导入;
}
