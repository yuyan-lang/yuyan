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
  return { 编, 解 };
}
function 精确小数(值) {
  const 数字 = 数(值);
  if (Object.is(数字, -0)) return "-0";
  if (!Number.isFinite(数字)) return String(数字).toLowerCase().replace("infinity", "inf");
  const [尾, 指数] = 数字.toExponential(16).split("e"), 幂 = Number(指数);
  if (幂 < -4 || 幂 >= 17) return 尾.replace(/\.?0+$/, "") + "e" + (幂 >= 0 ? "+" : "-") + String(Math.abs(幂)).padStart(2, "0");
  return 数字.toFixed(Math.max(0, 16 - 幂)).replace(/(\.\d*?)0+$/, "$1").replace(/\.$/, "");
}
function 小数表示(值) {
  const 数字 = 数(值);
  if (!Number.isFinite(数字)) return 精确小数(值);
  if (Object.is(数字, -0)) return "-0.000000";
  return Math.abs(数字) >= 1e21 ? BigInt(数字).toString() + ".000000" : 数字.toFixed(6);
}
function 理解整数(值) {
  const 串 = 文(值).match(/^\s*[+-]?\d+/)?.[0];
  const 整 = 串 ? BigInt(串.trim()) : 0n;
  return 整 > 9223372036854775807n ? 9223372036854775807n : 整 < -9223372036854775808n ? -9223372036854775808n : 整;
}
export async function 执行模块(模块, 桥模块, 文件, 参数 = [], { 编译 = false, 组装, 报告 = () => {} } = {}) {
  const 桥 = 创建值桥(桥模块);
  const 截止 = performance.now() + (编译 ? 90000 : 5000);
  let 输出长 = 0, stdout = "", stderr = "";
  let 最近读文件 = "";
  const 输出 = (流, 值) => {
    const 内容 = 字节(值); 输出长 += 内容.length;
    if (输出长 > 65536) throw Error("输出超过 64 KB");
    const text = 文(内容); if (流 === "stdout") stdout += text; else stderr += text;
    报告({ type: "output", phase: 编译 ? "compile" : "run", stream: 流, text });
  };
  const 拼接 = (甲, 乙) => { const 果 = new Uint8Array(甲.length + 乙.length); 果.set(甲); 果.set(乙, 甲.length); return 果; };
  const 原语 = {
    豫言_获取命令行程序名: () => 编译 ? "/编译器.wasm" : "/程序.wasm",
    豫言_获取命令行参数: () => 列(参数),
    豫言_获取当前工作目录: () => "/",
    豫言_获取文件修改时间: 名 => BigInt(文件.文件.get(规范路径(名))?.时间 ?? 1),
    豫言_获取环境变量: () => [false, ""],
    豫言_获取当前纳秒时间: () => ({ 小数: performance.now() * 1e6 }),
    豫言_获取随机整数: 上界 => {
      if (上界 <= 0n) throw Error("随机整数上界须大于零");
      const 组 = crypto.getRandomValues(new Uint32Array(2));
      return ((BigInt(组[0]) << 32n) | BigInt(组[1])) % 上界;
    },
    豫言_获取随机小数: () => ({ 小数: Math.random() }),
    豫言_同步读取文件: 名 => { 最近读文件 = 文(名); return 文件.读(名); },
    豫言_同步读取文件字节串: 名 => 文件.读(名),
    豫言_同步写入文件: (名, 内容) => 文件.写(名, 内容),
    豫言_同步写入文件字节串: (名, 内容) => 文件.写(名, 内容),
    豫言_同步删除文件: 名 => 文件.删(名),
    豫言_同步列出文件夹: 名 => 列([".", "..", ...文件.列目录(名)]),
    豫言_路径存在: 名 => 文件.存在(名),
    豫言_路径是文件夹: 名 => 文件.是目录(名),
    豫言_路径是普通文件: 名 => 文件.文件.has(规范路径(名)),
    豫言_路径为符号链接: () => false,
    豫言_取得真实路径: 名 => { if (!文件.存在(名)) throw Error("文件不存在：" + 文(名)); return 规范路径(名); },
    豫言_路径可执行: () => false,
    豫言_查找可执行程序: () => [false, ""],
    豫言_在线处理器数量: () => 1,
    豫言_运行于Windows: () => false,
    豫言_运行于MacOS: () => false,
    豫言_运行于Linux: () => false,
    豫言_可绘监视面板: () => false,
    豫言_标准输出是终端: () => false,
    豫言_标准输入是终端: () => false,
    豫言_尝试读取标准输入行: () => [false, ""],
    豫言_打印行: 值 => 输出("stdout", 拼接(值, 字节("\n"))),
    豫言_标准错误打印行: 值 => 输出("stderr", 拼接(值, 字节("\n"))),
    豫言_打印字符串: 值 => 输出("stdout", 值),
    豫言_字节转字符串: 值 => { if (值 <= 0n || 值 > 255n) throw Error("字节转字符串只接受一至二百五十五之间的整数"); return Uint8Array.of(数(值)); },
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
    豫言_小数转字符串: 小数表示,
    豫言_小数精确表示: 精确小数,
    豫言_字符串转小数: 值 => {
      const 串 = 文(值).trim(), 数字 = parseFloat(串);
      return { 小数: /^[+-]?nan/i.test(串) ? NaN : /^[+-]?inf/i.test(串) ? (串.startsWith("-") ? -Infinity : Infinity) : Number.isNaN(数字) ? 0 : 数字 };
    },
    豫言_源码数字名: 值 => /^[0-9-]+$/.test(文(值)),
    豫言_源码可用名: 值 => !/^[0-9-]+$/.test(文(值)) && !文(值).startsWith("《《") && !文(值).startsWith("：") && !文(值).includes("」"),
    豫言_源码字符串表示: 值 => "『" + 文(值).replace(/「：|』/g, 字 => 字 === "』" ? "「：』：」" : "「：「：：」") + "』",
    豫言_退出进程: 码 => { const 错 = Error("程序退出"); 错.退出码 = 数(码); throw 错; }
  };
  // 文言：编器可组装，客程不得借之。汉语：编译阶段只识别固定组装协议，不提供任何通用进程执行能力。
  if (编译) {
    原语.豫言_存放包上下文 = 内容 => { 文件.写("/上下文/固定.上下文", 内容); return "/上下文/固定.上下文"; };
    原语.豫言_同步运行子进程 = (名, 参) => {
      const 项 = 参[0].map(文);
      if (文(名) !== "./yy网页汇编宿主" || 项.length !== 3 || 项[0] !== "--组装") throw Error("浏览器不支持外部进程：" + 文(名));
      文件.写(项[2], 组装(文(文件.读(项[1])))); return true;
    };
    原语.豫言_同步运行子进程并获取输出 = (名, 参) => [原语.豫言_同步运行子进程(名, 参), "", ""];
  }
  try {
    const 实例 = new WebAssembly.Instance(模块, { "yuyan:browser/v1": { check: () => {
      if (performance.now() >= 截止) throw Error(编译 ? "编译超过九十秒时限" : "运行超过五秒时限");
    }, fail: 值 => { throw Error("未捕捉的豫言异常：\n" + 文(桥.解(值))); } }, "yuyan:gc-host/v1": { call: (名, 参) => {
      const 名称 = 文(桥.解(名));
      if (!Object.hasOwn(原语, 名称)) throw Error("浏览器暂不支持此原语：" + 名称);
      return 桥.编(原语[名称](...桥.解(参)));
    } } });
    // 文言：有独栈之能则用之。汉语：JSPI 在独立 Wasm 栈上执行，避免浏览器主栈较小导致编译器深调用溢出；无此 API 时保持同步引擎路径。
    if (typeof WebAssembly.promising === "function") await WebAssembly.promising(实例.exports._start)();
    else 实例.exports._start();
    return { ok: true, stdout, stderr, exitCode: 0 };
  } catch (错) {
    if (错.退出码 !== 0) 报告({ type: "diagnostic", text: 最近读文件 + "\n" + (错.stack ?? 错.message) });
    return { ok: 错.退出码 === 0, stdout, stderr, exitCode: 错.退出码 ?? 1, ...(错.退出码 === undefined ? { error: 错.message } : {}) };
  }
}
