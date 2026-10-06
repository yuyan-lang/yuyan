// 文言：构建之进程表，居主线程：一程一工作线程，客之所问（文件、子进程、摘要）于此答之；子进程之出与退码存于此，候父收之。
// 汉语：构建宿主的进程表，放在主线程。每个豫言程序（Wasm）是一个“进程”，占一个 Web Worker（见 工作线程.mjs）；客体的提问（文件、子进程、摘要）在这里答复。
//   子进程的输出与退出码存在这里，等父进程收取；语义照 Node 工具宿主的进程桥接（../../节点/进程桥接.cjs）：启动返回句柄（负数为错误码），
//   收取返回五项（首项 0 表示未结束），多路等待只认子进程句柄（掩码 1 = 已结束）。env 与 timeout 包装在这里拆开；
//   不是 Wasm 的程序只认几个内建命令（mkdir、rm、cp、mv、true、false），其余报不存在（-2），标准库随之得退出码 127。
import {规范路径, 文} from './文件系统.mjs';
import {编码值} from './编码.mjs';

const 编码器 = new TextEncoder();
const 块长 = 1 << 20;
const 输出上限 = 256 * 1024 * 1024;
const 末名 = 径 => 径.slice(径.lastIndexOf('/') + 1);
const 是网页汇编字节 = 内容 => 内容.length >= 4 && 内容[0] === 0 && 内容[1] === 0x61 && 内容[2] === 0x73 && 内容[3] === 0x6d;
const 十六进制 = 字节们 => Array.from(new Uint8Array(字节们), 值 => 值.toString(16).padStart(2, '0')).join('');
const 合并 = 块们 => { let 长 = 0; for (const 块 of 块们) 长 += 块.length; const 果 = new Uint8Array(长); let 位 = 0; for (const 块 of 块们) { 果.set(块, 位); 位 += 块.length; } return 果; };

// 文言：内建之令，行于文件系统而即毕。汉语：内建命令：直接作用在内存文件系统上并立即结束，返回退出码或 [退出码, 错误文字]。
const 内建命令 = {
  mkdir(参数, 目录, 系统) { for (const 路 of 参数.filter(项 => !项.startsWith('-'))) 系统.建目录(规范路径(路, 目录)); return 0; },
  rm(参数, 目录, 系统) {
    const 选项 = 参数.filter(项 => 项.startsWith('-')).join('');
    for (const 路 of 参数.filter(项 => !项.startsWith('-'))) {
      const 全 = 规范路径(路, 目录);
      if (系统.是目录(全)) { if (!/[rR]/.test(选项)) return [1, 'rm: 是目录：' + 路]; 系统.删全部(全); }
      else if (系统.是文件(全)) 系统.删文件(全);
      else if (!选项.includes('f')) return [1, 'rm: 不存在：' + 路];
    }
    return 0;
  },
  cp(参数, 目录, 系统) {
    const 路们 = 参数.filter(项 => !项.startsWith('-')).map(路 => 规范路径(路, 目录));
    if (路们.length < 2) return [1, 'cp: 缺少参数'];
    const 目标 = 路们.pop();
    for (const 源 of 路们) 系统.复制(源, 目标);
    return 0;
  },
  mv(参数, 目录, 系统) {
    const 路们 = 参数.filter(项 => !项.startsWith('-')).map(路 => 规范路径(路, 目录));
    if (路们.length < 2) return [1, 'mv: 缺少参数'];
    const 目标 = 路们.pop();
    for (const 源 of 路们) 系统.移动(源, 目标);
    return 0;
  },
  true: () => 0,
  false: () => 1,
};

export class 进程表 {
  constructor({文件系统, 桥模块, 工作线程网址, 处理器数}) {
    this.文件系统 = 文件系统;
    this.桥模块 = 桥模块;
    this.工作线程网址 = 工作线程网址;
    this.处理器数 = 处理器数 ?? (typeof navigator !== 'undefined' ? navigator.hardwareConcurrency : 4) ?? 4;
    this.记录们 = new Map();
    this.线程们 = [];
    this.下号 = 1024;
    this.模块缓存 = new Map();
    this.摘要缓存 = new Map();
    this.已停 = false;
  }

  // 文言：取其模，同径同时同长者复用之。汉语：编译程序的 Wasm 模块；同一路径、修改时间与长度相同就复用（模块可传给多个工作线程共享已编代码）。
  取模块(径) {
    const 内容 = this.文件系统.读(径), 时间 = this.文件系统.时间(径), 键 = 径 + '\t' + 时间 + '\t' + 内容.length;
    if (!this.模块缓存.has(键)) this.模块缓存.set(键, WebAssembly.compile(内容));
    return this.模块缓存.get(键);
  }

  取线程() {
    let 线 = this.线程们.find(项 => !项.忙);
    if (!线) {
      const 工 = new Worker(this.工作线程网址, {type: 'module'});
      const 控制 = new SharedArrayBuffer(16), 数据 = new SharedArrayBuffer(块长);
      线 = {工, 控制: new Int32Array(控制), 数据: new Uint8Array(数据), 忙: false, 号: 0, 待续: null, 待候: null};
      工.postMessage({种: '初始', 控制, 数据, 桥模块: this.桥模块});
      工.onmessage = ({data}) => this.收消息(线, data);
      工.onerror = 事件 => this.线程出错(线, 事件);
      this.线程们.push(线);
    }
    线.忙 = true;
    return 线;
  }

  新记录(父号, 程序名, 参数, 目录, 环境, 观察者) {
    const 记录 = {号: this.下号++, 父号, 程序名, 参数, 目录, 环境, 完成: false, 码: 0, 输出: [], 错误: [], 出长: 0, 错长: 0, 线: null, 观察者, 回调们: [], 定时: null};
    this.记录们.set(记录.号, 记录);
    return 记录;
  }

  // 文言：启一程：拆 env、timeout 之包装；Wasm 则派工作线程，内建则即行；皆非则返负二。汉语：启动程序，返回句柄（负数是错误码）。
  async 启动(程序, 参数们, 目录, 环境 = {}, 父号 = 0, 观察者 = null) {
    if (this.已停) return -125;
    if (this.记录们.size >= 4096) return -24;
    let 余 = [程序, ...参数们], 加环境 = {}, 限时 = 0;
    for (;;) {
      const 名 = 末名(余[0] ?? '');
      if (名 === 'env') {
        let 位 = 1;
        while (位 < 余.length && /^[A-Za-z_][A-Za-z0-9_]*=/.test(余[位])) { const 等 = 余[位].indexOf('='); 加环境[余[位].slice(0, 等)] = 余[位].slice(等 + 1); 位++; }
        余 = 余.slice(位); continue;
      }
      if (名 === 'timeout') {
        let 位 = 1;
        while (位 < 余.length && 余[位].startsWith('-')) 位 += 余[位] === '-k' || 余[位] === '-s' ? 2 : 1;
        限时 = parseFloat(余[位]) || 0; 余 = 余.slice(位 + 1); continue;
      }
      break;
    }
    const [实程序 = '', ...实参数] = 余, 合环境 = {...环境, ...加环境};
    const 候选 = 规范路径(实程序, 目录);
    const 是客体 = 实程序 !== '' && this.文件系统.是文件(候选) && (实程序.endsWith('.wasm') || 是网页汇编字节(this.文件系统.读(候选)));
    if (!是客体) {
      const 内建 = !实程序.includes('/') && Object.hasOwn(内建命令, 实程序) ? 内建命令[实程序] : null;
      if (!内建) return -2;
      const 记录 = this.新记录(父号, 实程序, 实参数, 目录, 合环境, 观察者);
      let 果;
      try { 果 = 内建(实参数, 目录, this.文件系统); } catch (错) { 果 = [1, 实程序 + ': ' + (错?.message ?? 错)]; }
      const [码, 错文] = Array.isArray(果) ? 果 : [果, ''];
      if (错文) this.收输出(记录, 2, 编码器.encode(错文 + '\n'));
      this.结束(记录, 码);
      return 记录.号;
    }
    const 记录 = this.新记录(父号, 候选, 实参数, 目录, 合环境, 观察者);
    let 模块;
    try { 模块 = await this.取模块(候选); }
    catch (错) { this.收输出(记录, 2, 编码器.encode('构建宿主：程序不能编译：' + 候选 + '：' + 错.message + '\n')); this.结束(记录, 126); return 记录.号; }
    if (this.已停) { this.结束(记录, 143); return 记录.号; }
    const 线 = this.取线程();
    线.号 = 记录.号; 记录.线 = 线;
    if (限时 > 0) 记录.定时 = setTimeout(() => this.终止(记录, 124), 限时 * 1000);
    线.工.postMessage({种: '运行', 号: 记录.号, 模块, 程序名: 候选, 参数: 实参数, 目录, 环境: 合环境, 处理器数: this.处理器数});
    return 记录.号;
  }

  收输出(记录, 流, 值) {
    const 长名 = 流 === 1 ? '出长' : '错长';
    if (记录[长名] + 值.length > 输出上限) { if (!记录.溢出) { 记录.溢出 = true; 记录.错误.push(编码器.encode('子进程输出超过 256 MiB 上限\n')); } return; }
    记录[长名] += 值.length;
    (流 === 1 ? 记录.输出 : 记录.错误).push(值);
    记录.观察者?.(流, 值);
  }

  结束(记录, 码) {
    if (记录.完成) return;
    记录.完成 = true;
    记录.码 = 记录.溢出 ? 125 : 码;
    clearTimeout(记录.定时);
    if (记录.线) { 记录.线.忙 = false; 记录.线.号 = 0; 记录.线 = null; }
    for (const 回调 of 记录.回调们.splice(0)) 回调(记录);
    this.唤候();
  }

  // 文言：强止一程：毁其线程（同步之客不能自止），以码结之。汉语：强行结束一个进程：销毁它的工作线程（同步执行的客体收不到消息，只能销毁），按给定退出码结束。
  终止(记录, 码) {
    const 线 = 记录.线;
    if (线) { 线.工.terminate(); this.线程们 = this.线程们.filter(项 => 项 !== 线); 记录.线 = null; }
    this.结束(记录, 码);
  }

  线程出错(线, 事件) {
    事件.preventDefault?.();
    const 记录 = this.记录们.get(线.号);
    线.工.terminate();
    this.线程们 = this.线程们.filter(项 => 项 !== 线);
    if (记录) { 记录.线 = null; this.收输出(记录, 2, 编码器.encode('构建宿主：工作线程出错：' + (事件.message ?? 事件) + '\n')); this.结束(记录, 1); }
  }

  收消息(线, 消息) {
    const 记录 = this.记录们.get(消息.号);
    if (消息.种 === '出') { if (记录) this.收输出(记录, 消息.流, 消息.值); return; }
    if (消息.种 === '毕') { if (记录) this.结束(记录, 消息.码); else { 线.忙 = false; 线.号 = 0; } return; }
    if (消息.种 === '续') return this.写续(线);
    if (消息.种 !== '调') return;
    let 值;
    try { 值 = this.处理(线, 记录, 消息.名, 消息.参); }
    catch (错) { return this.答(线, 错?.message ?? String(错), true); }
    if (值 instanceof Promise) 值.then(果 => this.答(线, 果), 错 => this.答(线, 错?.message ?? String(错), true));
    else this.答(线, 值);
  }

  答(线, 值, 是错 = false) {
    let 字节们;
    try { 字节们 = 是错 ? 编码器.encode(String(值)) : 编码值(值); }
    catch (错) { 字节们 = 编码器.encode(错.message); 是错 = true; }
    const 本块 = Math.min(字节们.length, 线.数据.length);
    线.数据.set(字节们.subarray(0, 本块));
    线.待续 = 本块 < 字节们.length ? {字节们, 位: 本块, 状态: 是错 ? 2 : 1} : null;
    线.控制[1] = 字节们.length;
    线.控制[2] = 本块;
    Atomics.store(线.控制, 0, 是错 ? 2 : 1);
    Atomics.notify(线.控制, 0);
  }

  写续(线) {
    const 续 = 线.待续;
    if (!续) return;
    const 本块 = Math.min(续.字节们.length - 续.位, 线.数据.length);
    线.数据.set(续.字节们.subarray(续.位, 续.位 + 本块));
    续.位 += 本块;
    if (续.位 >= 续.字节们.length) 线.待续 = null;
    线.控制[2] = 本块;
    Atomics.store(线.控制, 0, 续.状态);
    Atomics.notify(线.控制, 0);
  }

  事件(关注) {
    return 关注.map(([号, 掩码]) => { const 记录 = this.记录们.get(号); if (!记录) return 4; return 记录.完成 && (掩码 & 1) ? 1 : 0; });
  }

  唤候() {
    for (const 线 of this.线程们) {
      const 候 = 线.待候;
      if (!候) continue;
      const 果 = this.事件(候.关注);
      if (!果.some(Boolean)) continue;
      clearTimeout(候.定时);
      线.待候 = null;
      候.成([0, 果]);
    }
  }

  async 摘要(径) {
    const 内容 = this.文件系统.读(径), 键 = 径 + '\t' + this.文件系统.时间(径) + '\t' + 内容.length;
    if (!this.摘要缓存.has(键)) this.摘要缓存.set(键, crypto.subtle.digest('SHA-256', 内容).then(十六进制));
    return this.摘要缓存.get(键);
  }

  // 文言：同文用旧号，异文另立号（同 Node 工具宿主）。汉语：在当前目录的 .yybuild/豫构上下文 里逐字比较，相同就复用已有编号，否则新建下一个编号（同 Node 工具宿主；主线程单线程，不必加锁）。
  存放包上下文(目录, 内容) {
    const 根 = 规范路径('.yybuild/豫构上下文', 目录);
    for (let 号 = 1; ; 号++) {
      const 径 = 根 + '/' + 号 + '.上下文';
      if (!this.文件系统.是文件(径)) { this.文件系统.写(径, 内容); return 径; }
      const 旧 = this.文件系统.读(径);
      if (旧.length === 内容.length && 旧.every((值, 序) => 值 === 内容[序])) return 径;
    }
  }

  async 同步运行(父号, 程序, 参数, 目录, 环境) {
    const 号 = await this.启动(程序, 参数, 目录, 环境, 父号);
    if (号 < 0) return [127, new Uint8Array(0), 编码器.encode('启动子进程失败：' + 程序 + '，系统错误码：' + -号)];
    const 记录 = await this.候完成(号);
    this.记录们.delete(号);
    return [记录.码, 合并(记录.输出), 合并(记录.错误)];
  }

  候完成(号) {
    const 记录 = this.记录们.get(号);
    if (记录.完成) return Promise.resolve(记录);
    return new Promise(成 => 记录.回调们.push(成));
  }

  处理(线, 记录, 名, 参) {
    const 系统 = this.文件系统;
    switch (名) {
      case '读': return 系统.读(参[0]);
      case '写': 系统.写(参[0], 参[1]); return undefined;
      case '删': 系统.删文件(参[0]); return undefined;
      case '列': return 系统.列(参[0]);
      case '存在': return 系统.存在(参[0]);
      case '是目录': return 系统.是目录(参[0]);
      case '是文件': return 系统.是文件(参[0]);
      case '可执行': return 系统.是文件(参[0]) && (参[0].endsWith('.wasm') || 是网页汇编字节(系统.读(参[0])));
      case '真实路径': { const 径 = 规范路径(参[0]); if (!系统.存在(径)) throw Error('文件或目录不存在：' + 径); return 径; }
      case '时间': return 系统.时间(参[0]);
      case '规范': return 规范路径(参[0]);
      case '存放包上下文': return this.存放包上下文(参[0], 参[1]);
      case '摘要': return this.摘要(参[0]);
      case '启动': return this.启动(参[0], 参[1], 参[2], 参[3], 记录?.号 ?? 0);
      case '收取': {
        const 子 = this.记录们.get(参[0]);
        if (!子) throw Error('无效的异步子进程句柄');
        if (!子.完成) return [0, 0, 0, new Uint8Array(0), new Uint8Array(0)];
        this.记录们.delete(参[0]);
        return [1, 子.号, 子.码, 合并(子.输出), 合并(子.错误)];
      }
      case '等待': {
        const [关注, 超时] = 参;
        const 果 = this.事件(关注);
        if (果.some(Boolean) || 超时 === 0) return [0, 果];
        return new Promise(成 => {
          线.待候 = {关注, 成, 定时: 超时 < 0 ? null : setTimeout(() => { 线.待候 = null; 成([0, this.事件(关注)]); }, 超时)};
        });
      }
      case '同步运行': return this.同步运行(记录?.号 ?? 0, 参[0], 参[1], 参[2], 参[3]);
      default: throw Error('构建宿主：未知操作 ' + 名);
    }
  }

  // 文言：顶层行一程，候其毕，出随生随报。汉语：运行一个顶层程序直到结束；观察者随时收到输出（流 1 为标准输出、2 为标准错误，值为字节）。
  async 运行(程序, 参数, {目录 = '/', 环境 = {}, 观察者 = null} = {}) {
    const 号 = await this.启动(程序, 参数, 目录, 环境, 0, 观察者);
    if (号 < 0) return {码: 127, 输出: new Uint8Array(0), 错误: 编码器.encode('启动失败：' + 程序 + '，系统错误码：' + -号 + '\n')};
    const 记录 = await this.候完成(号);
    this.记录们.delete(号);
    return {码: 记录.码, 输出: 合并(记录.输出), 错误: 合并(记录.错误)};
  }

  停止() {
    this.已停 = true;
    for (const 线 of this.线程们) { 线.工.terminate(); if (线.待候) clearTimeout(线.待候.定时); }
    this.线程们 = [];
    for (const 记录 of this.记录们.values()) { 记录.线 = null; this.结束(记录, 143); }
  }
}
