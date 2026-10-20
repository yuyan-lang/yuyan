// 文言：构建之进程表，居主线程：一程一工作线程，客之所问（文件、子进程、摘要）于此答之；子进程之出与退码存于此，候父收之。
// 汉语：构建宿主的进程表，放在主线程。每个豫言程序（Wasm）是一个“进程”，占一个 Web Worker（见 工作线程.mjs）；客体的提问（文件、子进程、摘要）在这里答复。
//   子进程的输出与退出码存在这里，等父进程收取；语义照 Node 工具宿主的进程桥接（../../节点/进程桥接.cjs）：启动返回句柄（负数为错误码），
//   收取返回五项（首项 0 表示未结束），多路等待只认子进程句柄（掩码 1 = 已结束）。env 与 timeout 包装在这里拆开；
//   不是 Wasm 的程序只认几个内建命令（mkdir、rm、cp、mv、true、false），其余报不存在（-2），标准库随之得退出码 127。
//   父进程结束时收拾它没人收取的子任务记录：已结束的删去，运行中的改挂给初始任务（提案 00043 第六条）；「亲行」让子进程继承父进程的标准输入。
import {规范路径, 文} from './文件系统.mjs';
import {编码值} from './编码.mjs';

const 编码器 = new TextEncoder(), 解码器 = new TextDecoder();
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
  // 汉语：系统环境（提案 00043）：每个进程环境的缺省项（网页行在的全局设定，如 豫言行在=网页），进程自己的环境盖过它。文言：系统之环境：诸进程环境之缺省，己之环境掩之。
  constructor({文件系统, 桥模块, 工作线程网址, 处理器数, 系统环境 = {}}) {
    this.文件系统 = 文件系统;
    this.桥模块 = 桥模块;
    this.工作线程网址 = 工作线程网址;
    this.系统环境 = 系统环境;
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
      线 = {工, 控制: new Int32Array(控制), 数据: new Uint8Array(数据), 忙: false, 号: 0, 待续: null, 待候: null, 次数: 0, 闲时: null};
      工.postMessage({种: '初始', 控制, 数据, 桥模块: this.桥模块});
      工.onmessage = ({data}) => this.收消息(线, data);
      工.onerror = 事件 => this.线程出错(线, 事件);
      this.线程们.push(线);
    }
    clearTimeout(线.闲时);
    线.忙 = true;
    return 线;
  }

  新记录(父号, 程序名, 参数, 目录, 环境, 观察者) {
    const 记录 = {号: this.下号++, 父号, 程序名, 参数, 目录, 环境, 完成: false, 码: 0, 输出: [], 错误: [], 出长: 0, 错长: 0, 线: null, 观察者, 回调们: [], 定时: null, 无收: false, 不存: false};
    this.记录们.set(记录.号, 记录);
    // 文言：首立之务为初始之务，无父者退而挂之（提案〇〇〇四三）。汉语：第一个建的记录是初始任务，无父而失所依的子任务挂给它（提案 00043）。
    this.初始号 ??= 记录.号;
    return 记录;
  }

  // 文言：启一程：拆 env、timeout 之包装；Wasm 则派工作线程，内建则即行；皆非则返负二。承入（末项）者子即用此入，父之入也（亲行，提案〇〇〇四三之五）。
  // 汉语：启动程序，返回句柄（负数是错误码）。最后一个参数 承入 给了输入记录时，子进程立即用它当标准输入（亲行继承父进程的输入，提案 00043 第五条），不给就由 开入 另立。
  async 启动(程序, 参数们, 目录, 环境 = {}, 父号 = 0, 观察者 = null, 承入 = null) {
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
    const [实程序 = '', ...实参数] = 余, 合环境 = {...this.系统环境, ...环境, ...加环境};
    const 候选 = 规范路径(实程序, 目录);
    const 是客体 = 实程序 !== '' && this.文件系统.是文件(候选) && (实程序.endsWith('.wasm') || 是网页汇编字节(this.文件系统.读(候选)));
    if (!是客体) {
      const 内建 = !实程序.includes('/') && Object.hasOwn(内建命令, 实程序) ? 内建命令[实程序] : null;
      if (!内建) return -2;
      const 记录 = this.新记录(父号, 实程序, 实参数, 目录, 合环境, 观察者);
      if (承入) 记录.入 = 承入;
      let 果;
      try { 果 = 内建(实参数, 目录, this.文件系统); } catch (错) { 果 = [1, 实程序 + ': ' + (错?.message ?? 错)]; }
      const [码, 错文] = Array.isArray(果) ? 果 : [果, ''];
      if (错文) this.收输出(记录, 2, 编码器.encode(错文 + '\n'));
      this.结束(记录, 码);
      return 记录.号;
    }
    const 记录 = this.新记录(父号, 候选, 实参数, 目录, 合环境, 观察者);
    if (承入) 记录.入 = 承入;
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
    // 文言：亲行之直通者不积其出，惟转于外。汉语：亲行直通的子进程不积压输出，只转给外务（随出随显示，同原生继承标准流）。
    if (记录.不存) { 记录.观察者?.(流, 值); return; }
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
    if (记录.线) {
      const 线 = 记录.线;
      记录.线 = null;
      线.忙 = false; 线.号 = 0; 线.次数++;
      // 文言：如诺驿之工，四事而换，闲四分之一秒亦释之，毋积旧堆。汉语：与诺驿编译线程一样，每四次任务回收线程；空闲250毫秒也释放，避免跨任务保留Wasm堆高水位。
      if (线.次数 >= 4) this.回收线程(线);
      else 线.闲时 = setTimeout(() => { if (!线.忙) this.回收线程(线); }, 250);
    }
    for (const 回调 of 记录.回调们.splice(0)) 回调(记录);
    // 文言：父退则阖其交互之子之入，如管之写端随父而闭，子读尽乃自退（提案〇〇〇四三）；子之已毕而无人收者即除其录，未毕者改录于初始之务（或无所依则零），
    //   其毕也不复留于籍。汉语：父进程结束时关闭它的交互子程序的标准输入，如同管道写端随父进程关闭，子程序读到结束就自己退出（提案 00043）；
    //   同时收拾没人收取的子任务记录：已结束的直接删去，还在运行的改挂给初始任务（初始任务自己退出则改挂给零），它结束之后也不留在进程表里。
    for (const 子 of [...this.记录们.values()]) {
      if (子.父号 !== 记录.号) continue;
      if (子.入 && !子.完成) this.关入(子.号);
      if (子.完成) { this.记录们.delete(子.号); continue; }
      子.父号 = 记录.号 === this.初始号 ? 0 : this.初始号;
      子.无收 = true;
    }
    if (记录.无收) this.记录们.delete(记录.号);
    this.唤候();
  }

  // 文言：强止一程：毁其线程（同步之客不能自止），以码结之。汉语：强行结束一个进程：销毁它的工作线程（同步执行的客体收不到消息，只能销毁），按给定退出码结束。
  回收线程(线) {
    clearTimeout(线.闲时);
    线.工.terminate();
    this.线程们 = this.线程们.filter(项 => 项 !== 线);
  }

  终止(记录, 码) {
    const 线 = 记录.线;
    if (线) { this.回收线程(线); 记录.线 = null; }
    this.结束(记录, 码);
  }

  线程出错(线, 事件) {
    事件.preventDefault?.();
    const 记录 = this.记录们.get(线.号);
    this.回收线程(线);
    // 文言：录浏览器所授之错、栈与处；未授则明言之，毋以事件字样充其因。汉语：记录浏览器提供的错误、栈和位置；没有详情时明确说明，避免把事件对象当作根因。
    if (记录) {
      const 错误 = 事件.error;
      const 详情 = 错误?.stack || 事件.message || 错误?.message || '浏览器未提供错误详情';
      const 位置 = 事件.filename ? `\n位置：${事件.filename}:${事件.lineno ?? 0}:${事件.colno ?? 0}` : '';
      const 上下文 = `\n进程：${记录.号} · ${记录.程序名} · 事件：${事件.type ?? '未知'} · 线程任务数：${线.次数}`;
      记录.线 = null;
      this.收输出(记录, 2, 编码器.encode('构建宿主：工作线程出错：' + 详情 + 位置 + 上下文 + '\n'));
      this.结束(记录, 1);
    }
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

  // 文言：帛之页散：每页一段求沙二五六而相连（页长非正或逾一吉则四千九十六）。汉语：字节串按页长分段求沙二五六，各段 32 字节摘要依次相连（页长不在一到 2^30 之间时取 4096，同诺驿宿主）。
  async 页杂凑(字, 页) {
    const 步 = 页 > 0 && 页 <= 1073741824 ? 页 : 4096, 段 = 字.length === 0 ? 0 : Math.ceil(字.length / 步), 出 = new Uint8Array(段 * 32);
    for (let 序 = 0; 序 < 段; 序++) 出.set(new Uint8Array(await crypto.subtle.digest('SHA-256', 字.subarray(序 * 步, Math.min(字.length, (序 + 1) * 步)))), 序 * 32);
    return 出;
  }

  // 文言：同文用旧号，异文另立号（同 Node 工具宿主）。汉语：在当前目录的 .豫构缓存/豫构上下文 里逐字比较，相同就复用已有编号，否则新建下一个编号（同 Node 工具宿主；主线程单线程，不必加锁）。
  存放包上下文(目录, 内容) {
    const 根 = 规范路径('.豫构缓存/豫构上下文', 目录);
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

  // 文言：亲行：子承父之标准入而行（提案〇〇〇四三之五），父无入者子读即尽，与空入同。入尽、原始之制皆在父之入上，子读而父后读，如管道；子毕不阖父之入，交还于父。
  //   直通（末项）为阳且父有外务观察者时，子之出即转于外（随出随显，同原生之承标准流），不复积压，所还之出与误为空；为阴则出与误收而还之（同原生之捕输出）。
  // 汉语：亲行（同步运行并继承标准输入，提案 00043 第五条）：子进程读父进程的标准输入，父进程没有开输入时子进程一读即尽，与空输入相同；
  //   输入结束标志与原始模式都在父进程的输入上，子程序读、父程序随后读，如同管道；子程序结束不关闭父进程的输入，交还给它接着读。
  //   直通（最后一个参数）为阳且父进程有输出观察者（顶层的壳、桌面程序）时，子进程的输出随出随转给它（同原生继承标准流），不积压，返回的标准输出与标准错误为空；
  //   否则照旧收下输出与错误再返回。返回 [退出码, 标准输出, 标准错误]；启动失败返回负的错误码。
  async 亲行(父号, 程序, 参数, 目录, 环境, 直通 = false) {
    if (this.已停) return -125;
    const 父 = this.记录们.get(父号), 转出 = 直通 && Boolean(父?.观察者);
    const 号 = await this.启动(程序, 参数, 目录, 环境, 父号, 转出 ? 父.观察者 : null, 父?.入 ?? null);
    if (号 < 0) return 号;
    const 记录 = this.记录们.get(号);
    if (记录 && 转出) 记录.不存 = true;
    const 毕 = await this.候完成(号);
    this.记录们.delete(号);
    return 转出 ? [毕.码, new Uint8Array(0), new Uint8Array(0)] : [毕.码, 合并(毕.输出), 合并(毕.错误)];
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
      case '改名': 系统.改名(参[0], 参[1]); return undefined;
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
        const [出, 错] = this.取出(子);
        return [1, 子.号, 子.码, 出, 错];
      }
      // 文言：网页行在之诸务（提案〇〇〇四三第三步）：文件按区读写、列目带类、交互之子程序、增量之出、标准入与终端。
      // 汉语：网页行在用到的操作（提案 00043 第三步）：文件按区读写、带类型列目、交互子程序、增量取出、标准输入与终端。
      case '长度': return this.文件系统.读(参[0]).length;
      case '读区': { const 字 = this.文件系统.读(参[0]); return 字.slice(Math.min(参[1], 字.length), Math.min(参[1] + 参[2], 字.length)); }
      case '写区': return this.写区(参[0], 参[1], 参[2]);
      case '设长': { const 旧 = this.文件系统.读(参[0]), 新 = new Uint8Array(参[1]); 新.set(旧.subarray(0, Math.min(旧.length, 参[1]))); this.文件系统.写(参[0], 新); return undefined; }
      case '列详': return this.文件系统.列(参[0]).map(名 => { const 全 = 规范路径(名, 参[0]); return [名, this.文件系统.是目录(全) ? 1 : this.文件系统.是文件(全) ? 0 : 2]; });
      case '建目录': this.文件系统.建目录(参[0]); return undefined;
      case '删径': {
        if (this.文件系统.是目录(参[0])) { if (this.文件系统.列(参[0]).length) throw Error('目录非空：' + 参[0]); this.文件系统.删全部(参[0]); }
        else this.文件系统.删文件(参[0]);
        return undefined;
      }
      case '交互启动': return this.交互启动(记录?.号 ?? 0, ...参);
      case '候收': return this.候完成(参[0]).then(子 => { this.记录们.delete(参[0]); const [出, 错] = this.取出(子); return [子.码, 出, 错]; });
      case '取出': { const 子 = this.记录们.get(参[0]); return 子 ? this.取出(子) : [new Uint8Array(0), new Uint8Array(0)]; }
      case '送入': return this.送入(参[0], 参[1]) ? 0 : 8;
      case '关入': return this.关入(参[0]) ? 0 : 8;
      case '入素查': return !!this.记录们.get(参[0])?.入?.素;
      case '是终端': return !!记录?.入?.终端;
      case '入素': { const 入 = 记录?.入; if (!入?.终端) return false; 入.素 = true; return true; }
      case '出素': { if (记录?.入) 记录.入.素 = false; return undefined; }
      case '读键': return this.读键(记录, true);
      case '瞥键': return this.读键(记录, false);
      case '列役': return [...this.记录们.values()].filter(子 => !子.完成).map(子 => [子.号, 末名(子.程序名), '网页行在', Math.round(performance.now() * 1000), 0, 0, 0]);
      case '等待': {
        const [关注, 超时] = 参;
        const 果 = this.事件(关注);
        if (果.some(Boolean) || 超时 === 0) return [0, 果];
        return new Promise(成 => {
          线.待候 = {关注, 成, 定时: 超时 < 0 ? null : setTimeout(() => { 线.待候 = null; 成([0, this.事件(关注)]); }, 超时)};
        });
      }
      case '同步运行': return this.同步运行(记录?.号 ?? 0, 参[0], 参[1], 参[2], 参[3]);
      case '亲行': return this.亲行(记录?.号 ?? 0, 参[0], 参[1], 参[2], 参[3], 参[4]);
      case '读行': return this.读行(记录);
      case '页杂凑': return this.页杂凑(参[0], 参[1]);
      case '召回': { const 子 = this.记录们.get(参[0]); if (!子) return 8; this.终止(子, 143); return 0; }
      // 文言：辍役（同终端之 Ctrl-C，同裸机）：号正则断其务及诸子孙，号负则惟断负号之子孙；退码皆百三十。果零成，一无可断之子孙，八号不合。
      // 汉语：打断任务（同终端 Ctrl-C，语义同裸机）：号为正打断这个任务与它的全部子孙，号为负只打断任务 −号 的子孙；退出码都是 130。结果零成功，一没有可打断的子孙，八任务号不对。
      case '辍役': {
        const 根 = this.记录们.get(Math.abs(参[0]));
        if (!根 || 根.完成) return 8;
        const 子孙们 = [], 寻 = 父 => { for (const 记 of this.记录们.values()) if (记.父号 === 父 && !记.完成) { 子孙们.push(记); 寻(记.号); } };
        寻(根.号);
        for (const 记 of 子孙们.reverse()) this.终止(记, 130);
        if (参[0] > 0) { this.终止(根, 130); return 0; }
        return 子孙们.length ? 0 : 1;
      }
      default:
        // 文言：余务交外（网页行在之显示、字体、剪贴、器）。汉语：其余操作交给外务（网页行在提供的显示、字体、剪贴板、设备）。
        if (this.外务 && Object.hasOwn(this.外务, 名)) return this.外务[名](记录, ...参);
        throw Error('构建宿主：未知操作 ' + 名);
    }
  }

  // 文言：标准入（提案〇〇〇四三）：开入之进程有一字之缓；读行至换行而止，读键取一字（ESC 起之终端序取其全），入尽则言尽，否则候送入。终端者（桌面终端仿真之会话）可入原始之制。
  //   未开入者（构建之作业、非交互之子程序）读即尽。
  // 汉语：标准输入（提案 00043）：开了输入的进程有一个字节缓冲；读行读到换行为止（去掉行尾回车），读键取一个字符（ESC 开头的终端序列取整段，同诺驿终端输入），
  //   输入结束就返回“没有”，否则等送入。“终端”进程（桌面终端仿真起的会话）可进入原始模式。没开输入的（构建作业、非交互子程序）一读即结束。
  开入(号, {终端 = false} = {}) {
    const 记录 = this.记录们.get(号);
    if (记录) 记录.入 ??= {字: new Uint8Array(0), 尽: false, 候: null, 终端, 素: false};
  }
  // 文言：试足所候之读：有则答之。汉语：缓冲里的字节够了就答复等着的读取。
  试候(入) {
    if (!入.候) return;
    const 果 = 入.候.种 === '行' ? this.取行(入) : this.取键(入);
    if (果) { const 成 = 入.候.成; 入.候 = null; 成(果); }
  }
  取行(入) {
    const 位 = 入.字.indexOf(10);
    if (位 < 0 && !入.尽) return null;
    if (位 < 0 && 入.字.length === 0) return [false, ''];
    const 止 = 位 < 0 ? 入.字.length : 位;
    let 行 = 解码器.decode(入.字.subarray(0, 止));
    if (行.endsWith('\r')) 行 = 行.slice(0, -1);
    入.字 = 入.字.slice(位 < 0 ? 止 : 止 + 1);
    return [true, 行];
  }
  // 文言：取一键：一字之码（UTF-8 一至四字），或 ESC [ / ESC O 起至字母或波浪号止之全序。汉语：取一个键：一个 UTF-8 字符，或从 ESC [、ESC O 起到字母或 ~ 为止的整段序列。
  取键(入) {
    const 字 = 入.字;
    if (字.length === 0) return 入.尽 ? [false, ''] : null;
    const 首 = 字[0], 长 = 首 < 0x80 ? 1 : 首 >= 0xf0 ? 4 : 首 >= 0xe0 ? 3 : 首 >= 0xc0 ? 2 : 1;
    if (字.length < 长) return 入.尽 ? [false, ''] : null;
    let 止 = 长;
    if (首 === 0x1b && 字.length >= 2 && (字[1] === 0x5b || 字[1] === 0x4f)) {
      let 位 = 2;
      while (位 < 字.length && !/[A-Za-z~]/.test(String.fromCharCode(字[位]))) 位++;
      if (位 < 字.length) 止 = 位 + 1;
    }
    入.字 = 字.slice(止);
    return [true, 解码器.decode(字.subarray(0, 止))];
  }
  读行(记录) {
    const 入 = 记录?.入;
    if (!入) return [false, ''];
    return this.取行(入) ?? new Promise(成 => { 入.候 = {种: '行', 成}; });
  }
  读键(记录, 等) {
    const 入 = 记录?.入;
    if (!入) return [false, ''];
    return this.取键(入) ?? (等 ? new Promise(成 => { 入.候 = {种: '键', 成}; }) : [false, '']);
  }
  送入(号, 内容) {
    const 入 = this.记录们.get(号)?.入;
    if (!入 || 入.尽) return false;
    const 新 = typeof 内容 === 'string' ? 编码器.encode(内容) : 内容, 合 = new Uint8Array(入.字.length + 新.length);
    合.set(入.字); 合.set(新, 入.字.length);
    入.字 = 合;
    this.试候(入);
    return true;
  }
  关入(号) {
    const 入 = this.记录们.get(号)?.入;
    if (!入) return false;
    入.尽 = true;
    this.试候(入);
    return true;
  }
  // 文言：增量之出：自上次取后新生之出与误，取则移其位。汉语：增量输出：取出上次取过之后新产生的标准输出与标准错误，取后记下位置（收取完成时只交其余部分）。
  取出(记录) {
    const 出位 = 记录.已取出 ?? 0, 错位 = 记录.已取错 ?? 0;
    记录.已取出 = 记录.输出.length; 记录.已取错 = 记录.错误.length;
    return [合并(记录.输出.slice(出位)), 合并(记录.错误.slice(错位))];
  }
  // 文言：按位写卷：位为负一则续于末；不足则补零。汉语：在偏移处写入字节（偏移为 −1 表示接在末尾），文件不够长时中间补零；返回写入的字节数。
  写区(径, 位, 字) {
    const 旧 = this.文件系统.是文件(径) ? this.文件系统.读(径) : new Uint8Array(0);
    const 起 = 位 < 0 ? 旧.length : 位, 新 = new Uint8Array(Math.max(旧.length, 起 + 字.length));
    新.set(旧); 新.set(字, 起);
    this.文件系统.写(径, 新);
    return 字.length;
  }
  // 文言：起交互或非交互之子程序：交互者开入而以终端视之；非交互者先送所予之入而阖之。汉语：启动子程序：交互的打开标准输入并当作终端；非交互的先送入给定的输入字节再关闭输入。
  async 交互启动(父号, 程序, 参数, 目录, 环境, 输入, 交互) {
    const 号 = await this.启动(程序, 参数, 目录, 环境, 父号);
    if (号 < 0) return 号;
    this.开入(号, {终端: !!交互});
    if (输入?.length) this.送入(号, 输入);
    if (!交互) this.关入(号);
    return 号;
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
    for (const 线 of this.线程们) { this.回收线程(线); if (线.待候) clearTimeout(线.待候.定时); }
    this.线程们 = [];
    // 汉语：结束诸记录时会删去无人收取的子记录，故先取一份快照再逐个结束。文言：终诸录之时或除无人收之子录，故先取其列而后遍终之。
    for (const 记录 of [...this.记录们.values()]) { 记录.线 = null; this.结束(记录, 143); }
  }
}
