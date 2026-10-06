// 文言：文件藏于内存，外务止于所授。汉语：浏览器和测试共用的 WasmGC 宿主，不接触磁盘、网络或原生进程。
const 编码 = new TextEncoder(), 解码 = new TextDecoder();
export const 字节 = 值 => typeof 值 === "string" ? 编码.encode(值) : 值;
const 文 = 值 => 值 instanceof Uint8Array ? 解码.decode(值) : String(值);
const 数 = 值 => Number(值?.小数 ?? 值);
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
  // 文言：原者，桥之本出，供边界胶水用之。汉语：原 是值桥实例的 exports，交给边界胶水读写带类型导入的值。
  return { 解, 原: 桥 };
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
// 文言：带型之导入，惟模有之乃载胶水；无之者不载亦行。汉语：有带类型导入的模块才动态载入共用胶水（本目录的 边界.mjs：仓库里转出 ../../网页汇编/边界.mjs，发布时由构建换成胶水本体）；没有带类型导入的模块不载胶水也能运行。
let 边界胶水 = null;
const 取边界胶水 = () => 边界胶水 ??= import("./边界.mjs");
// 文言：Safari 不得列导入（见 边界.mjs），则视边界段之有无。汉语：Safari 列不出导入表时（见 边界.mjs 的 模块导入），改看模块有没有「豫言边界」段。
const 有带型导入 = 模块 => {
  try { return WebAssembly.Module.imports(模块).some(项 => 项.kind === "function" && 项.module !== "yuyan:browser/v1"); }
  catch { return WebAssembly.Module.customSections(模块, "豫言边界").length > 0; }
};
export async function 执行模块(模块, 桥模块, 文件, 参数 = [], { 编译 = false, 报告 = () => {}, 时限 } = {}) {
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
  // 文言：此无程序之文可摘，以定名代其摘要；编器以之名缓存之目，发布时预编之官书缓存与客端所编同居一目。易编器则资源全换，旧缓存不相混。
  // 汉语：这里拿不到正在运行的 Wasm 文件，获取当前程序SHA256 返回固定占位「浏览器编译器」代替内容摘要；编译器用它命名缓存目录，发布时预编译的标准库缓存与浏览器内编译因此落在同一目录。换编译器时整套资源一起替换，旧缓存不会混用。
  const 构建基础 = { 可绘监视面板: () => false, 获取当前程序SHA256: () => "浏览器编译器" };
  if (编译) 构建基础.存放包上下文 = 内容 => { 文件.写("/上下文/固定.上下文", 内容); return "/上下文/固定.上下文"; };
  try {
    // 文言：带型之导入依签名包之，无实现者给桩，调之乃报。汉语：带类型的导入由共用胶水按签名包装；不在上面两表里的给桩，调用时报“接口函数未绑定”。
    const 带型导入 = 有带型导入(模块) ? (await 取边界胶水()).造边界导入(模块, 桥.原, { 标准库, 构建基础 }) : {};
    const 实例 = new WebAssembly.Instance(模块, { ...带型导入, "yuyan:browser/v1": { check: () => {
      if (performance.now() >= 截止) throw Error("Wasm 执行超过时限");
    }, fail: 值 => { throw Error("未捕捉的豫言异常：\n" + 文(桥.解(值))); } } });
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
