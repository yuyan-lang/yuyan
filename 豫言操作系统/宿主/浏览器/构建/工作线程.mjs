// 文言：构建之客线程：一线程一时行一程；客之所需（文件、子进程、摘要）皆问于主线程，以共享之存候其答而塞行，故客视之若同步。
// 汉语：构建宿主的客体工作线程：每个线程同一时刻运行一个豫言程序（一个“进程”）。客体要的文件、子进程、摘要都向主线程提问，
//   用 Atomics.wait 在共享内存上阻塞等答复，所以对客体来说这些宿主服务都是同步的（与 Node 工具宿主的进程桥接同一做法）。
//   页面须跨源隔离（crossOriginIsolated），才有 SharedArrayBuffer。标准库与构建基础的带类型导入经共用胶水 边界.mjs 接入。
import {造边界导入} from './边界.mjs';
import {解码值} from './编码.mjs';
import {造豫寓实现, 单元现况, 清声流} from './豫寓.mjs';

const 编码器 = new TextEncoder(), 解码器 = new TextDecoder();
const 文 = 值 => 值 instanceof Uint8Array ? 解码器.decode(值) : String(值);
const 数 = 值 => Number(值?.小数 ?? 值);
const 拼 = (甲, 乙) => { const 果 = new Uint8Array(甲.length + 乙.length); 果.set(甲); 果.set(乙, 甲.length); return 果; };
const 换行 = 编码器.encode('\n');

// 文言：小数之文三术，诸宿主同义（见 ../../标准库宿主.汉语.md）。汉语：小数文字的三个函数，与各宿主同义。
function 精确小数(值) {
  const 数字 = 数(值);
  if (Object.is(数字, -0)) return '-0';
  if (!Number.isFinite(数字)) return String(数字).toLowerCase().replace('infinity', 'inf');
  const [尾, 指数] = 数字.toExponential(16).split('e'), 幂 = Number(指数);
  if (幂 < -4 || 幂 >= 17) return 尾.replace(/\.?0+$/, '') + 'e' + (幂 >= 0 ? '+' : '-') + String(Math.abs(幂)).padStart(2, '0');
  return 数字.toFixed(Math.max(0, 16 - 幂)).replace(/(\.\d*?)0+$/, '$1').replace(/\.$/, '');
}
function 小数表示(值) {
  const 数字 = 数(值);
  if (!Number.isFinite(数字)) return 精确小数(值);
  if (Object.is(数字, -0)) return '-0.000000';
  return Math.abs(数字) >= 1e21 ? BigInt(数字).toString() + '.000000' : 数字.toFixed(6);
}
function 理解小数(值) {
  const 串 = 文(值).trim(), 数字 = parseFloat(串);
  return /^[+-]?nan/i.test(串) ? NaN : /^[+-]?inf/i.test(串) ? (串.startsWith('-') ? -Infinity : Infinity) : Number.isNaN(数字) ? 0 : 数字;
}
function 格式化时间(格式) {
  const 时 = new Date(), 补 = 数字 => String(数字).padStart(2, '0');
  const 表 = {'%Y': String(时.getFullYear()), '%m': 补(时.getMonth() + 1), '%d': 补(时.getDate()), '%H': 补(时.getHours()), '%M': 补(时.getMinutes()), '%S': 补(时.getSeconds()), '%%': '%'};
  return 文(格式).replace(/%./g, 项 => { if (!(项 in 表)) throw Error('未支持日期格式 ' + 项); return 表[项]; });
}

// 文言：单元元素段之长（同诺驿宿主）。汉语：单元元素段的长度，表先按它增长（同诺驿宿主的「元素数」）。
function 元素数(字) {
  let 位 = 8;
  const 读 = () => { let 值 = 0, 移 = 0, 字节; do { 字节 = 字[位++]; 值 += (字节 & 127) * 2 ** 移; 移 += 7; } while (字节 & 128); return 值; };
  while (位 < 字.length) {
    const 节 = 字[位++], 长 = 读(), 止 = 位 + 长;
    if (节 === 9) { 读(); if (读() !== 0) throw Error('单元元素段须为标志 0'); while (字[位] !== 0x0b) 位++; 位++; return 读(); }
    位 = 止;
  }
  return 0;
}

let 控制 = null, 数据 = null, 桥模块 = null;
const 模块缓存 = new Map();
// 文言：辍察之具（提案〇〇〇四三第六步）：诸句共用一断字；主线程置标，句中之察点呼宿主之断例，断例见标则抛断异。
// 汉语：打断检查点的用具（提案 00043 第六步）：各单元共用同一个标志字；主线程置位，句子里的检查点调用宿主给的打断例程，例程见到标志就抛出专用异常。
const 打断字 = new Int32Array(new SharedArrayBuffer(4));
const 造打断错 = () => { const 错 = new Error('已打断'); 错.打断 = true; return 错; };

// 文言：问于主线程而候其答；答长逾共享之区则分块续取。汉语：向主线程提问并阻塞等答复：控制字 0 置 0 后发消息，主线程写好答复后置 1（成）或 2（错）并唤醒；
//   答复比共享数据区长时分块，取完一块再发“续”取下一块。错时答复是错误文字，这里抛 JS 异常（与 Node 工具宿主读不存在的文件时一样，客体随之中止）。
function 问(号, 名, ...参) { return 传问(号, 名, [], ...参); }
// 文言：传问：并移所列之缓于主线程（像素之帧免其拷贝；移后本线程不复用之）；环形之缓（SharedArrayBuffer）则附于信，与主线程共之。
// 汉语：同「问」，另把所列的缓冲移交给主线程（像素帧免拷贝；移交后本线程不再用它们）；共享的环形缓冲（SharedArrayBuffer）附在消息上，
//   与主线程共用（它不能移交，只能共享）。
function 传问(号, 名, 传们, ...参) {
  Atomics.store(控制, 0, 0);
  const 移们 = [], 环们 = [];
  for (const 项 of 传们) (typeof SharedArrayBuffer === 'function' && 项 instanceof SharedArrayBuffer ? 环们 : 移们).push(项);
  self.postMessage({种: '调', 号, 名, 参, 环: 环们[0]}, 移们);
  for (;;) {
    while (Atomics.load(控制, 0) === 0) Atomics.wait(控制, 0, 0);
    const 状态 = Atomics.load(控制, 0), 总长 = 控制[1];
    const 果 = new Uint8Array(总长);
    let 已得 = 0;
    for (;;) {
      const 本块 = 控制[2];
      果.set(数据.subarray(0, 本块), 已得);
      已得 += 本块;
      if (已得 >= 总长) break;
      Atomics.store(控制, 0, 0);
      self.postMessage({种: '续', 号});
      while (Atomics.load(控制, 0) === 0) Atomics.wait(控制, 0, 0);
    }
    if (状态 === 2) throw Error(文(果));
    return 解码值(果);
  }
}

function 运行(任务) {
  const {号, 环境} = 任务;
  // 文言：一程既毕则弃其声流之表（行在已因其毕而阖之）。汉语：一个程序跑完就丢掉它的声流表（行在已随它的结束关掉了这些流）。
  清声流();
  // 文言：程名与参可暂易（装网汇单元时，提案〇〇〇四三）。汉语：程序名与命令行参数可临时替换（「装网汇单元」运行单元时，提案 00043）。
  let 程序名 = 任务.程序名, 参数 = 任务.参数;
  let 当前目录 = 任务.目录;
  const 径 = 值 => { const 串 = 文(值); return 串.startsWith('/') ? 串 : (当前目录 === '/' ? '' : 当前目录) + '/' + 串; };
  const 出 = (流, 值) => self.postMessage({种: '出', 号, 流, 值});
  const 退出 = 码 => { const 错 = Error('客体退出'); 错.退出码 = 数(码); throw 错; };
  const 子进程 = (名, 参) => [文(名), Array.from(参, 文), 当前目录, 环境];
  const 标准库 = {
    书: 值 => 出(1, 拼(值, 换行)),
    续书: 值 => 出(1, 值.slice()),
    诉: 值 => 出(2, 拼(值, 换行)),
    // 文言：读一行于主线程（页所送入；未开入者即尽，提案〇〇〇四三）。汉语：向主线程读一行标准输入（页面送入的；没开输入的进程立即得“结束”，提案 00043）。
    闻: () => 问(号, '读行'),
    // 文言：终端之事问于主线程：交互之子程序以终端视之（桌面终端之仿），余皆非终端（提案〇〇〇四三）。汉语：终端相关都问主线程：交互子程序（桌面终端仿真起的会话）当作终端，其余不是终端（提案 00043）。
    亲书: () => !!问(号, '是终端'),
    亲闻: () => !!问(号, '是终端'),
    听键: () => 问(号, '读键'),
    瞥键: () => 问(号, '瞥键'),
    入素: () => !!问(号, '入素'),
    出素: () => { 问(号, '出素'); },
    阅: 名 => 问(号, '读', 径(名)),
    阅帛: 名 => 问(号, '读', 径(名)),
    著: (名, 内容) => { 问(号, '写', 径(名), 内容.slice()); },
    著帛: (名, 内容) => { 问(号, '写', 径(名), 内容.slice()); },
    焚: 名 => { 问(号, '删', 径(名)); },
    更名: (旧, 新) => { 问(号, '改名', 径(旧), 径(新)); },
    列目: 名 => ['.', '..', ...问(号, '列', 径(名))],
    实有: 名 => 问(号, '存在', 径(名)),
    为箧: 名 => 问(号, '是目录', 径(名)),
    为卷: 名 => 问(号, '是文件', 径(名)),
    能行: 名 => 问(号, '可执行', 径(名)),
    为影: () => false,
    真径: 名 => 问(号, '真实路径', 径(名)),
    改时: 名 => BigInt(问(号, '时间', 径(名))),
    所居: () => 当前目录,
    迁: 名 => {
      const 目标 = 问(号, '规范', 径(名));
      if (!问(号, '是目录', 目标)) return [20n, '不是目录：' + 目标];
      当前目录 = 目标;
      return [0n, ''];
    },
    辞: 码 => 退出(码),
    自号: () => 程序名,
    受命: () => 参数,
    问俗: 名 => { const 键 = 文(名); return [Object.hasOwn(环境, 键), 环境[键] ?? '']; },
    访: 名 => { const 串 = 文(名); if (!串.includes('/')) return [false, '']; const 全 = 径(串); return 问(号, '可执行', 全) ? [true, 全] : [false, '']; },
    遣使: (名, 参) => BigInt(问(号, '启动', ...子进程(名, 参))),
    收使: 句柄 => 问(号, '收取', Number(句柄)),
    群候: (关注, 超时) => 问(号, '等待', 关注.map(([句柄, 掩码]) => [Number(句柄), Number(掩码)]), Number(超时)),
    咨: (名, 参) => { const 果 = 问(号, '同步运行', ...子进程(名, 参)); return [果[0] === 0, 果[1], 果[2]]; },
    成行: (名, 参) => 问(号, '同步运行', ...子进程(名, 参))[0] === 0,
    传行: (名, 参) => { const 果 = 问(号, '同步运行', ...子进程(名, 参)); 出(1, 果[1]); 出(2, 果[2]); return BigInt(果[0]); },
    // 文言：亲行承本程之标准入且承三流（提案〇〇〇四三之五）：子读本程之入，其出与误由宿主随出随显，毕则还退码（同原生之继承标准流）。
    // 汉语：亲行继承本进程的标准输入，三流都继承（提案 00043 第五条）：子进程读本进程的标准输入，输出与错误由宿主随出随显，结束后返回退出码。
    亲行: (名, 参) => { const 果 = 问(号, '亲行', ...子进程(名, 参), true); return BigInt(typeof 果 === 'number' ? (果 < 0 ? 127 : 果) : 果[0]); },
    寓轩窗: () => false,
    寓林檎: () => false,
    寓林纳: () => false,
    寓豫言: () => false,
    核数: () => BigInt(任务.处理器数 ?? navigator.hardwareConcurrency ?? 1),
    此刻: () => (performance.timeOrigin + performance.now()) * 1e6,
    今时: () => 格式化时间('%Y-%m-%d %H:%M:%S'),
    今时依式: 格式 => 格式化时间(格式),
    抽数: 上界 => {
      const 界 = BigInt(上界);
      if (界 <= 0n) throw Error('随机整数上界须大于零');
      const 组 = crypto.getRandomValues(new Uint32Array(2));
      return ((BigInt(组[0]) << 32n) | BigInt(组[1])) % 界;
    },
    抽小数: () => Math.random(),
    密抽帛: 长 => {
      const 数值 = Number(长);
      if (!Number.isInteger(数值) || 数值 < 0 || 数值 > 1048576) throw Error('安全随机字节串：长度须在零至一兆之间');
      const 果 = new Uint8Array(数值);
      for (let 起 = 0; 起 < 数值; 起 += 65536) crypto.getRandomValues(果.subarray(起, Math.min(数值, 起 + 65536)));
      return 果;
    },
    述小数: 小数表示,
    精述小数: 精确小数,
    识小数: 理解小数,
    // 文言：余诸东道之术（提案〇〇〇四三，同诺驿宿主之义）：闲块不知则负一；风俗为环境之块；召回经主线程止子；问使无其能则五十八；浏览器无 TCP 与多播，诸门报皆以负五十八还之。
    // 汉语：东道其余函数（提案 00043，语义同诺驿宿主）：空闲物理块数不知道时返回 −1；风俗是“名=值\0”相连的环境块；召回经主线程终止子进程；
    //   问使（子进程资源）没有这项能力时状态为 58；浏览器没有 TCP 与多播数据报，这些函数一律返回 −58（暂不可用）。待办事项：拼音（注音、注音首字）。
    闲块数: () => -1n,
    风俗: () => Object.entries(环境).map(([名, 值]) => 名 + '=' + 值 + '\0').join(''),
    召回: 句柄 => BigInt(问(号, '召回', Number(句柄))),
    问使: () => [58n, false, 0n, false, 0n],
    开门: () => [-58n, 0n], 叩门: () => [-58n, 0n], 入门: () => [-58n, 0n], 迎: () => [-58n, 0n], 门号: () => [-58n, 0n],
    听: () => [-58n, ''], 听帛: () => [-58n, new Uint8Array(0)], 续告: () => [-58n, 0n], 续告帛: () => [-58n, 0n], 守候: () => [-58n, 0n],
    勿滞: () => [-58n, undefined], 缄口: () => [-58n, undefined], 别: () => [-58n, undefined], 误由: () => '浏览器没有 TCP（暂不可用）',
    投书: () => [-58n, new Uint8Array(0)], 开播报: () => [-58n, -1n], 投报: () => [-58n, 0n], 收报: () => [-58n, 0n, 0n, new Uint8Array(0)],
  };
  const 构建基础 = {
    藏包境: 内容 => 问(号, '存放包上下文', 当前目录, 内容.slice()),
    可绘监工板: () => false,
    绘监工板: () => undefined,
    取今程序沙二五六: () => 问(号, '摘要', 任务.程序名),
    取卷沙二五六: 名 => 问(号, '摘要', 径(名)),
    // 文言：帛之页散（同诺驿宿主）：每页一段求沙二五六而相连；浏览器之摘要惟异步，故问于主线程。汉语：字节串按页分段求沙二五六后相连（同诺驿宿主）；浏览器只有异步摘要，所以交主线程算。
    帛页杂凑沙二五六: (字, 页) => 问(号, '页杂凑', 字.slice(), Number(页)),
    // 文言：装网汇单元（提案〇〇〇四三）：装一单元于本进程，暂以所予之参与程名行其 _start，返则复之；零为成，余印其由而返一，退出照传；为察点所断则返二。
    // 汉语：「装网汇单元」（提案 00043）：把一个单元装进本进程，暂以给定的参数与程序名运行它的 _start，返回后恢复；零为成功，装不进或抛出宿主错误时印出原因、返回一，退出照常往外传。
    //   被检查点打断时返回二（提案 00043 第六步）：不印错误，由会话印“已打断”。装句前清标，编句期间按下的打断不带到这一句。
    装网汇单元: (字, 参们, 名) => {
      const 旧名 = 程序名, 旧参 = 参数;
      程序名 = 文(名); 参数 = Array.from(参们, 文);
      try {
        Atomics.store(打断字, 0, 0);
        装一单元(字.slice(), 程序名).exports._start(); return 0;
      }
      catch (错) {
        if (错?.退出码 !== undefined) throw 错;
        if (错?.打断) { Atomics.store(打断字, 0, 0); return 2; }
        出(2, 编码器.encode('装网汇单元：' + (错?.stack ?? 错) + '\n')); return 1;
      }
      finally { 程序名 = 旧名; 参数 = 旧参; }
    },
    单元现况,
  };
  // 文言：豫寓诸接口（提案〇〇〇四三第三步）。汉语：豫寓（豫言操作系统接口）各接口的实现（提案 00043 第三步）。
  const 豫寓 = 造豫寓实现({问: (名, ...参) => 问(号, 名, ...参), 传问: (名, 传们, ...参) => 传问(号, 名, 传们, ...参), 文, 目: () => 当前目录});
  const 诸实现 = {豫章: 标准库, 营造: 构建基础, ...豫寓};
  // 文言：本进程之单元界：零号表、二异常之标与已装诸单元之出（同诺驿宿主）。汉语：本进程的单元环境：零号表、两个异常标签与已装单元的导出，各单元共用（同诺驿宿主）。
  let 单元界 = null;
  const 装一单元 = (字, 名) => {
    单元界 ??= {表: new WebAssembly.Table({element: 'anyfunc', initial: 0}), 标签: {escape: new WebAssembly.Tag({parameters: ['eqref', 'eqref']}), error: new WebAssembly.Tag({parameters: ['eqref']})}, 导出们: new Map()};
    const 模 = new WebAssembly.Module(字), 基 = 单元界.表.length;
    单元界.表.grow(元素数(字));
    const 单元 = {};
    for (const 导 of WebAssembly.Module.imports(模)) if (导.module === '单元') {
      if (!单元界.导出们.has(导.name)) throw Error('单元导入未找到：' + 导.name + '（' + 名 + '）');
      单元[导.name] = 单元界.导出们.get(导.name);
    }
    const 运行时 = {表: 单元界.表, 表基: new WebAssembly.Global({value: 'i32', mutable: false}, 基), escape: 单元界.标签.escape, error: 单元界.标签.error};
    // 文言：辍察之一导入（提案〇〇〇四三第六步）：断例，诸单元共之，例读断字，见标则抛。
    // 汉语：打断检查点的一个导入（提案 00043 第六步）：打断例程，各单元共用；例程读标志字，见标就抛。
    const 实例 = new WebAssembly.Instance(模, {...造边界导入(模, 桥, 诸实现), 运行时, 单元, 'yuyan:browser/v1': {interrupt: () => { if (Atomics.load(打断字, 0) !== 0) throw 造打断错(); }}});
    for (const [出名, 值] of Object.entries(实例.exports)) 单元界.导出们.set(出名, 值);
    return 实例;
  };
  const 桥 = new WebAssembly.Instance(桥模块).exports;
  const 模块 = 任务.模块;
  const 实例 = new WebAssembly.Instance(模块, {...造边界导入(模块, 桥, 诸实现), 'yuyan:browser/v1': {interrupt: () => { if (Atomics.load(打断字, 0) !== 0) throw 造打断错(); }}});
  // 文言：有独栈之能则用之，免深调之溢。汉语：有 JSPI 时在独立的 Wasm 栈上执行，避免工作线程的栈太小、编译器深递归溢出。
  const 异步 = typeof WebAssembly.promising === 'function';
  return async () => {
    if (异步) await WebAssembly.promising(实例.exports._start)(); else 实例.exports._start();
  };
}

self.onmessage = async ({data: 消息}) => {
  if (消息.种 === '初始') {
    控制 = new Int32Array(消息.控制);
    数据 = new Uint8Array(消息.数据);
    桥模块 = 消息.桥模块;
    self.postMessage({种: '打断内存', 缓冲: 打断字.buffer});
    return;
  }
  if (消息.种 !== '运行') return;
  const 号 = 消息.号;
  let 码 = 0;
  try {
    await 运行(消息)();
  } catch (错) {
    if (错?.退出码 !== undefined) 码 = 错.退出码;
    else {
      码 = 1;
      self.postMessage({种: '出', 号, 流: 2, 值: 编码器.encode('构建宿主：程序异常终止：' + (错?.stack ?? 错) + '\n')});
    }
  }
  self.postMessage({种: '毕', 号, 码});
};
