// 文言：文件藏于内存，外务止于所授。汉语：浏览器和测试共用的 WasmGC 宿主，不接触磁盘、网络或原生进程。
const 编码 = new TextEncoder(), 解码 = new TextDecoder();
export const 字节 = 值 => typeof 值 === "string" ? 编码.encode(值) : 值;
const 文 = 值 => 值 instanceof Uint8Array ? 解码.decode(值) : String(值);
const 数 = 值 => Number(值?.小数 ?? 值);
const 列 = 值 => [值, 值.length];
export function 规范路径(值) {
  const 段 = [];
  for (const 项 of 文(值).split("/")) {
    if (!项 || 项 === ".") continue;
    if (项 === "..") 段.pop(); else 段.push(项);
  }
  return "/" + 段.join("/");
}
export class 内存文件系统 {
  constructor(初值 = {}) {
    this.文件 = new Map(); this.时钟 = 1; this.总字节 = 0;
    for (const [名, 内容] of Object.entries(初值)) {
      this.写(名, 内容?.内容 ?? 内容);
      if (内容?.时间 !== undefined) { this.文件.get(规范路径(名)).时间 = 内容.时间; this.时钟 = Math.max(this.时钟, 内容.时间); }
    }
  }
  写(名, 内容) {
    名 = 规范路径(名); 内容 = 字节(内容).slice();
    const 总 = this.总字节 + 内容.length - (this.文件.get(名)?.内容.length ?? 0);
    if (总 > 128 * 1024 * 1024) throw Error("编译文件超过 128 MiB 上限");
    this.文件.set(名, { 内容, 时间: ++this.时钟 }); this.总字节 = 总;
  }
  读(名) {
    const 项 = this.文件.get(规范路径(名));
    if (!项) throw Error("文件不存在：" + 文(名));
    return 项.内容;
  }
  删(名) { 名 = 规范路径(名); this.总字节 -= this.文件.get(名)?.内容.length ?? 0; this.文件.delete(名); }
  是目录(名) { const 前 = 规范路径(名).replace(/\/$/, "") + "/"; return [...this.文件.keys()].some(径 => 径.startsWith(前)); }
  存在(名) { return this.文件.has(规范路径(名)) || this.是目录(名); }
  列目录(名) {
    const 前 = 规范路径(名).replace(/\/$/, "") + "/";
    return [...new Set([...this.文件.keys()].filter(径 => 径.startsWith(前)).map(径 => 径.slice(前.length).split("/")[0]))];
  }
}
export function 创建值桥(模块) {
  const 桥 = new WebAssembly.Instance(模块).exports;
  function 留(长) {
    if (长 > 128 * 1024 * 1024) throw Error("宿主交换数据过大");
    const 差 = 长 - 桥.memory.buffer.byteLength;
    if (差 > 0) 桥.memory.grow(Math.ceil(差 / 65536));
  }
  function 解(值, 深 = 0) {
    if (深 > 256) throw Error("宿主数据嵌套过深");
    switch (桥.kind(值)) {
      case 0: return null;
      case 1: case 4: return 桥.int(值);
      case 5: return { 小数: 桥.float(值) };
      case 2: { const 长 = 桥.bytes_len(值); 留(长); 桥.bytes_out(值); return new Uint8Array(桥.memory.buffer, 0, 长).slice(); }
      case 3: return Array.from({ length: 桥.tuple_len(值) }, (_, 序) => 解(桥.tuple_get(值, 序), 深 + 1));
      default: throw Error("未知客体类型");
    }
  }
  function 编(值, 深 = 0) {
    if (深 > 256) throw Error("宿主数据嵌套过深");
    if (值 == null) return null;
    if (typeof 值 === "boolean") return 桥.new_int(值 ? 1n : 0n);
    if (typeof 值 === "bigint" || typeof 值 === "number") return 桥.new_int(BigInt(值));
    if (typeof 值 === "string") 值 = 字节(值);
    if (值 instanceof Uint8Array) { 留(值.length); new Uint8Array(桥.memory.buffer, 0, 值.length).set(值); return 桥.bytes_in(值.length); }
    if (Array.isArray(值)) { const 组 = 桥.new_tuple(值.length); 值.forEach((项, 序) => 桥.tuple_set(组, 序, 编(项, 深 + 1))); return 组; }
    if (Object.hasOwn(值, "小数")) return 桥.new_float(值.小数);
    throw Error("未知宿主类型");
  }
  // 文言：原者，桥之本出，供边界胶水用之。汉语：原 是值桥实例的 exports，交给边界胶水读写带类型导入的值。
  return { 编, 解, 原: 桥 };
}
// 文言：小数之文三术，诸宿主同义：精确者依 %.17g，表示者依 %f，理解者依 strtod。汉语：小数文字的三个函数，各宿主语义统一（见 ../../标准库宿主.汉语.md）：
//   小数精确表示同 C 的 %.17g；小数表示同 C 的 %f（-0 写 -0.000000，≥1e21 展开整数，非有限写 nan、inf、-inf）；理解小数同 strtod（跳过空白取前缀，认 inf、infinity、nan，无可转得 0）。
export function 精确小数(值) {
  const 数字 = 数(值);
  if (Object.is(数字, -0)) return "-0";
  if (!Number.isFinite(数字)) return String(数字).toLowerCase().replace("infinity", "inf");
  const [尾, 指数] = 数字.toExponential(16).split("e"), 幂 = Number(指数);
  if (幂 < -4 || 幂 >= 17) return 尾.replace(/\.?0+$/, "") + "e" + (幂 >= 0 ? "+" : "-") + String(Math.abs(幂)).padStart(2, "0");
  return 数字.toFixed(Math.max(0, 16 - 幂)).replace(/(\.\d*?)0+$/, "$1").replace(/\.$/, "");
}
export function 小数表示(值) {
  const 数字 = 数(值);
  if (!Number.isFinite(数字)) return 精确小数(值);
  if (Object.is(数字, -0)) return "-0.000000";
  return Math.abs(数字) >= 1e21 ? BigInt(数字).toString() + ".000000" : 数字.toFixed(6);
}
export function 理解小数(值) {
  const 串 = 文(值).trim(), 数字 = parseFloat(串);
  return /^[+-]?nan/i.test(串) ? NaN : /^[+-]?inf/i.test(串) ? (串.startsWith("-") ? -Infinity : Infinity) : Number.isNaN(数字) ? 0 : 数字;
}
function 理解整数(值) {
  const 串 = 文(值).match(/^\s*[+-]?\d+/)?.[0];
  const 整 = 串 ? BigInt(串.trim()) : 0n;
  return 整 > 9223372036854775807n ? 9223372036854775807n : 整 < -9223372036854775808n ? -9223372036854775808n : 整;
}
// 文言：带型之导入，惟模有之乃载胶水；旧器旧客无之，不载亦行。汉语：有带类型导入的模块才动态载入共用胶水（本目录的 边界.mjs：仓库里转出 ../../网页汇编/边界.mjs，发布时由构建换成胶水本体）；旧编译器与旧产物只有 call，不载胶水也能运行。
let 边界胶水 = null;
const 取边界胶水 = () => 边界胶水 ??= import("./边界.mjs");
const 有带型导入 = 模块 => WebAssembly.Module.imports(模块).some(项 => 项.kind === "function" && 项.module !== "yuyan:gc-host/v1" && 项.module !== "yuyan:browser/v1");
// 文言：旧通调之果，小数包为 {小数}，列附其长。汉语：由新式实现派生旧 call 原语：小数结果包成 {小数}，列结果附上长度（旧形是“数组加长度”）。
const 旧小数果 = new Set(["获取当前纳秒时间", "获取随机小数", "字符串转小数"]), 旧列果 = new Set(["获取命令行参数", "同步列出文件夹"]);
const 旧式原语 = 表 => Object.fromEntries(Object.entries(表).map(([名, 函]) => ["豫言_" + 名,
  旧小数果.has(名) ? (...参) => ({ 小数: 函(...参) }) : 旧列果.has(名) ? (...参) => 列(函(...参)) : 函]));
export async function 执行模块(模块, 桥模块, 文件, 参数 = [], { 编译 = false, 报告 = () => {}, 原语扩展 = {}, 异步原语 = false, 时限 } = {}) {
  const 桥 = 创建值桥(桥模块);
  const 截止 = performance.now() + (时限 ?? (编译 ? 90000 : 5000));
  let 输出长 = 0, stdout = "", stderr = "";
  let 最近读文件 = "";
  const 输出 = (流, 值) => {
    const 内容 = 字节(值); 输出长 += 内容.length;
    if (输出长 > (编译 ? 2097152 : 65536)) throw Error(编译 ? "编译日志超过 2 MiB，已停止" : "输出超过 64 KB");
    const text = 文(内容); if (流 === "stdout") stdout += text; else stderr += text;
    报告({ type: "output", phase: 编译 ? "compile" : "run", stream: 流, text });
  };
  const 拼接 = (甲, 乙) => { const 果 = new Uint8Array(甲.length + 乙.length); 果.set(甲); 果.set(乙, 甲.length); return 果; };
  // 文言：标准库之宿主服务，以导入模块「标准库」之字段名为键。汉语：标准库宿主服务（导入模块「标准库」），键为字段名；参数、结果是边界胶水的 JS 形（串为 Uint8Array，整为 BigInt，小为 Number，爻为布尔，列为数组）。没有的给桩。
  const 标准库 = {
    获取命令行程序名: () => 编译 ? "/编译器.wasm" : "/程序.wasm",
    获取命令行参数: () => 参数,
    获取当前工作目录: () => "/",
    获取文件修改时间: 名 => BigInt(文件.文件.get(规范路径(名))?.时间 ?? 1),
    获取环境变量: () => [false, ""],
    // 文言：日志记时，取客地之刻。汉语：支持编译器实际使用的 strftime 日期时间字段，未知格式明确拒绝。
    格式化当前本地日期时间: 格式 => {const d=new Date(),pad=n=>String(n).padStart(2,'0'),fields={Y:String(d.getFullYear()),m:pad(d.getMonth()+1),d:pad(d.getDate()),H:pad(d.getHours()),M:pad(d.getMinutes()),S:pad(d.getSeconds()),'%':'%'};return 文(格式).replace(/%([%A-Za-z])/g,(_,k)=>{if(!(k in fields))throw Error('暂不支持日期格式：%'+k);return fields[k];});},
    获取当前纳秒时间: () => performance.now() * 1e6,
    获取随机整数: 上界 => {
      if (上界 <= 0n) throw Error("随机整数上界须大于零");
      const 组 = crypto.getRandomValues(new Uint32Array(2));
      return ((BigInt(组[0]) << 32n) | BigInt(组[1])) % 上界;
    },
    获取随机小数: () => Math.random(),
    同步读取文件: 名 => { 最近读文件 = 文(名); return 文件.读(名); },
    同步读取文件字节串: 名 => 文件.读(名),
    同步写入文件: (名, 内容) => 文件.写(名, 内容),
    同步写入文件字节串: (名, 内容) => 文件.写(名, 内容),
    同步删除文件: 名 => 文件.删(名),
    同步列出文件夹: 名 => [".", "..", ...文件.列目录(名)],
    路径存在: 名 => 文件.存在(名),
    路径是文件夹: 名 => 文件.是目录(名),
    路径是普通文件: 名 => 文件.文件.has(规范路径(名)),
    路径为符号链接: () => false,
    取得真实路径: 名 => { if (!文件.存在(名)) throw Error("文件不存在：" + 文(名)); return 规范路径(名); },
    路径可执行: () => false,
    查找可执行程序: () => [false, ""],
    在线处理器数量: () => 1,
    运行于Windows: () => false,
    运行于MacOS: () => false,
    运行于Linux: () => false,
    标准输出是终端: () => false,
    标准输入是终端: () => false,
    尝试读取标准输入行: () => [false, ""],
    打印行: 值 => 输出("stdout", 拼接(值, 字节("\n"))),
    标准错误打印行: 值 => 输出("stderr", 拼接(值, 字节("\n"))),
    打印字符串: 值 => 输出("stdout", 值),
    小数转字符串: 小数表示,
    小数精确表示: 精确小数,
    字符串转小数: 理解小数,
    退出进程: 码 => { const 错 = Error("程序退出"); 错.退出码 = 数(码); throw 错; }
  };
  // 文言：编器直书二进制，不假外器；客程亦不得行外部之进程。汉语：构建基础的宿主服务（导入模块「构建基础」）；编译器直接写出 Wasm 二进制，不需要外部组装进程，浏览器不提供任何通用进程执行能力。
  const 构建基础 = { 可绘监视面板: () => false };
  if (编译) 构建基础.存放包上下文 = 内容 => { 文件.写("/上下文/固定.上下文", 内容); return "/上下文/固定.上下文"; };
  // 文言：旧通调诸原语：标准库与构建基础者由上表派生，余者编器已内联或将为内建。汉语：旧 call 的原语：标准库与构建基础的由上面两表派生（语义相同）；其余是编译器已内联或第②步改成内建的旧名，旧产物仍会调用。
  const 原语 = {
    ...旧式原语(标准库),
    ...旧式原语(构建基础),
    豫言_字节转字符串: 值 => { if (值 <= 0n || 值 > 255n) throw Error("字节转字符串只接受一至二百五十五之间的整数"); return Uint8Array.of(数(值)); },
    // 文言：字节串之术一依原生运行时，越界则止，截取以起点与长度。汉语：字节串原语与原生运行时（字节串.c）语义一致：越界即报错，截取按起点与长度；编译器自身亦用之。
    豫言_字节串_空: () => new Uint8Array(),
    豫言_字节串_长度: 值 => BigInt(值.length),
    豫言_字节串_取字节: (值, 序) => { if (序 < 0n || 序 >= BigInt(值.length)) throw Error("字节串取字节：序数越界"); return BigInt(值[Number(序)]); },
    豫言_字节串_从字符串: 值 => 值,
    豫言_字节串_单字节: 值 => { if (值 < 0n || 值 > 255n) throw Error("构造单字节串：字节必须在零至二百五十五之间"); return Uint8Array.of(Number(值)); },
    豫言_字节串_拼接: (甲, 乙) => 拼接(甲, 乙),
    豫言_字节串_截取: (值, 起, 长) => {
      if (起 < 0n || 长 < 0n || 起 > BigInt(值.length) || 长 > BigInt(值.length) - 起) throw Error("截取字节串：范围越界");
      return 值.slice(Number(起), Number(起 + 长));
    },
    豫言_整数转小数: 值 => ({ 小数: 数(值) }),
    豫言_小数转整数: 值 => BigInt(Math.trunc(数(值))),
    豫言_整数加: (甲, 乙) => BigInt.asIntN(64, 甲 + 乙),
    豫言_整数乘: (甲, 乙) => BigInt.asIntN(64, 甲 * 乙),
    豫言_整数除: (甲, 乙) => 甲 / 乙,
    豫言_小数加: (甲, 乙) => ({ 小数: 数(甲) + 数(乙) }),
    豫言_小数减: (甲, 乙) => ({ 小数: 数(甲) - 数(乙) }),
    豫言_小数乘: (甲, 乙) => ({ 小数: 数(甲) * 数(乙) }),
    豫言_小数除: (甲, 乙) => ({ 小数: 数(甲) / 数(乙) }),
    豫言_整数转字符串: 值 => String(值),
    豫言_字符串转整数: 理解整数,
    豫言_源码数字名: 值 => /^[0-9-]+$/.test(文(值)),
    豫言_源码可用名: 值 => !/^[0-9-]+$/.test(文(值)) && !文(值).startsWith("《《") && !文(值).startsWith("：") && !文(值).includes("」"),
    豫言_源码字符串表示: 值 => "『" + 文(值).replace(/「：|』/g, 字 => 字 === "』" ? "「：』：」" : "「：「：：」") + "』"
  };
  // 文言：所授者可行，异步归桥续之。汉语：仅受信任宿主注入原语，JSPI 保持 D1/R2 调用期间的 Wasm 栈。
  Object.assign(原语, 原语扩展);
  const 调用 = (名, 参) => {
    const 名称 = 文(桥.解(名));
    if (!Object.hasOwn(原语, 名称)) throw Error("浏览器暂不支持此原语：" + 名称);
    const 果 = 原语[名称](...桥.解(参));
    return 果 instanceof Promise ? 果.then(值 => 桥.编(值)) : 桥.编(果);
  };
  try {
    // 文言：带型之导入依签名包之，无实现者给桩，调之乃报。汉语：带类型的导入由共用胶水按签名包装；不在上面两表里的给桩，调用时报“接口函数未绑定”。
    const 带型导入 = 有带型导入(模块) ? (await 取边界胶水()).造边界导入(模块, 桥.原, { 标准库, 构建基础 }) : {};
    const 实例 = new WebAssembly.Instance(模块, { ...带型导入, "yuyan:browser/v1": { check: () => {
      if (performance.now() >= 截止) throw Error("Wasm 执行超过时限");
    }, fail: 值 => { throw Error("未捕捉的豫言异常：\n" + 文(桥.解(值))); } }, "yuyan:gc-host/v1": { call: 异步原语 ? new WebAssembly.Suspending(调用) : 调用 } });
    // 文言：有独栈之能则用之。汉语：JSPI 在独立 Wasm 栈上执行，避免浏览器主栈较小导致编译器深调用溢出；无此 API 时保持同步引擎路径。
    if (typeof WebAssembly.promising === "function") await WebAssembly.promising(实例.exports._start)();
    else 实例.exports._start();
    // 文言：_start 毕，有边界段而应用实现启动之术者，乃调其导出。汉语：_start 之后，带「豫言边界」段的模块若实现了「启动程序」（导出 豫言操作系统启动/启动程序）就调用它；旧产物没有这个导出。
    const 胶水 = WebAssembly.Module.customSections(模块, "豫言边界").length > 0 ? await 取边界胶水() : null;
    const 启动导出 = 胶水 ? 胶水.造边界导出(实例, 模块, 桥.原, { 异步: typeof WebAssembly.promising === "function" })[胶水.启动导出名] : undefined;
    if (启动导出) await 启动导出();
    return { ok: true, stdout, stderr, exitCode: 0 };
  } catch (错) {
    if (错.退出码 !== 0) 报告({ type: "diagnostic", text: 最近读文件 + "\n" + (错.stack ?? 错.message) });
    return { ok: 错.退出码 === 0, stdout, stderr, exitCode: 错.退出码 ?? 1, ...(错.退出码 === undefined ? { error: 错.message } : {}) };
  }
}
