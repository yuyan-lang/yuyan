// 文言：构建之客线程：一线程一时行一程；客之所需（文件、子进程、摘要）皆问于主线程，以共享之存候其答而塞行，故客视之若同步。
// 汉语：构建宿主的客体工作线程：每个线程同一时刻运行一个豫言程序（一个“进程”）。客体要的文件、子进程、摘要都向主线程提问，
//   用 Atomics.wait 在共享内存上阻塞等答复，所以对客体来说这些宿主服务都是同步的（与 Node 工具宿主的进程桥接同一做法）。
//   页面须跨源隔离（crossOriginIsolated），才有 SharedArrayBuffer。标准库与构建基础的带类型导入经共用胶水 边界.mjs 接入。
import {造边界导入, 造边界导出, 启动导出名} from './边界.mjs';
import {解码值} from './编码.mjs';

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

let 控制 = null, 数据 = null, 桥模块 = null;
const 模块缓存 = new Map();

// 文言：问于主线程而候其答；答长逾共享之区则分块续取。汉语：向主线程提问并阻塞等答复：控制字 0 置 0 后发消息，主线程写好答复后置 1（成）或 2（错）并唤醒；
//   答复比共享数据区长时分块，取完一块再发“续”取下一块。错时答复是错误文字，这里抛 JS 异常（与 Node 工具宿主读不存在的文件时一样，客体随之中止）。
function 问(号, 名, ...参) {
  Atomics.store(控制, 0, 0);
  self.postMessage({种: '调', 号, 名, 参});
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
  const {号, 程序名, 参数, 环境} = 任务;
  let 当前目录 = 任务.目录;
  const 径 = 值 => { const 串 = 文(值); return 串.startsWith('/') ? 串 : (当前目录 === '/' ? '' : 当前目录) + '/' + 串; };
  const 出 = (流, 值) => self.postMessage({种: '出', 号, 流, 值});
  const 退出 = 码 => { const 错 = Error('客体退出'); 错.退出码 = 数(码); throw 错; };
  const 子进程 = (名, 参) => [文(名), Array.from(参, 文), 当前目录, 环境];
  const 标准库 = {
    打印行: 值 => 出(1, 拼(值, 换行)),
    打印字符串: 值 => 出(1, 值.slice()),
    标准错误打印行: 值 => 出(2, 拼(值, 换行)),
    尝试读取标准输入行: () => [false, ''],
    标准输出是终端: () => false,
    标准输入是终端: () => false,
    读取终端按键: () => [false, ''],
    进入终端原始输入模式: () => false,
    退出终端原始输入模式: () => undefined,
    同步读取文件: 名 => 问(号, '读', 径(名)),
    同步读取文件字节串: 名 => 问(号, '读', 径(名)),
    同步写入文件: (名, 内容) => { 问(号, '写', 径(名), 内容.slice()); },
    同步写入文件字节串: (名, 内容) => { 问(号, '写', 径(名), 内容.slice()); },
    同步删除文件: 名 => { 问(号, '删', 径(名)); },
    同步列出文件夹: 名 => ['.', '..', ...问(号, '列', 径(名))],
    路径存在: 名 => 问(号, '存在', 径(名)),
    路径是文件夹: 名 => 问(号, '是目录', 径(名)),
    路径是普通文件: 名 => 问(号, '是文件', 径(名)),
    路径可执行: 名 => 问(号, '可执行', 径(名)),
    路径为符号链接: () => false,
    取得真实路径: 名 => 问(号, '真实路径', 径(名)),
    获取文件修改时间: 名 => BigInt(问(号, '时间', 径(名))),
    获取当前工作目录: () => 当前目录,
    切换当前工作目录: 名 => {
      const 目标 = 问(号, '规范', 径(名));
      if (!问(号, '是目录', 目标)) return [20n, '不是目录：' + 目标];
      当前目录 = 目标;
      return [0n, ''];
    },
    退出进程: 码 => 退出(码),
    获取命令行程序名: () => 程序名,
    获取命令行参数: () => 参数,
    获取环境变量: 名 => { const 键 = 文(名); return [Object.hasOwn(环境, 键), 环境[键] ?? '']; },
    查找可执行程序: 名 => { const 串 = 文(名); if (!串.includes('/')) return [false, '']; const 全 = 径(串); return 问(号, '可执行', 全) ? [true, 全] : [false, '']; },
    启动异步子进程: (名, 参) => BigInt(问(号, '启动', ...子进程(名, 参))),
    尝试收取异步子进程: 句柄 => 问(号, '收取', Number(句柄)),
    异步_输入输出多路等待: (关注, 超时) => 问(号, '等待', 关注.map(([句柄, 掩码]) => [Number(句柄), Number(掩码)]), Number(超时)),
    同步运行子进程并获取输出: (名, 参) => { const 果 = 问(号, '同步运行', ...子进程(名, 参)); return [果[0] === 0, 果[1], 果[2]]; },
    同步运行子进程: (名, 参) => 问(号, '同步运行', ...子进程(名, 参))[0] === 0,
    同步运行子进程并传递输出: (名, 参) => { const 果 = 问(号, '同步运行', ...子进程(名, 参)); 出(1, 果[1]); 出(2, 果[2]); return BigInt(果[0]); },
    同步运行子进程并继承标准流: (名, 参) => { const 果 = 问(号, '同步运行', ...子进程(名, 参)); 出(1, 果[1]); 出(2, 果[2]); return BigInt(果[0]); },
    运行于Windows: () => false,
    运行于MacOS: () => false,
    运行于Linux: () => false,
    在线处理器数量: () => BigInt(任务.处理器数 ?? navigator.hardwareConcurrency ?? 1),
    获取当前纳秒时间: () => (performance.timeOrigin + performance.now()) * 1e6,
    获取当前本地日期时间字符串: () => 格式化时间('%Y-%m-%d %H:%M:%S'),
    格式化当前本地日期时间: 格式 => 格式化时间(格式),
    获取随机整数: 上界 => {
      const 界 = BigInt(上界);
      if (界 <= 0n) throw Error('随机整数上界须大于零');
      const 组 = crypto.getRandomValues(new Uint32Array(2));
      return ((BigInt(组[0]) << 32n) | BigInt(组[1])) % 界;
    },
    获取随机小数: () => Math.random(),
    安全随机_字节串: 长 => {
      const 数值 = Number(长);
      if (!Number.isInteger(数值) || 数值 < 0 || 数值 > 1048576) throw Error('安全随机字节串：长度须在零至一兆之间');
      const 果 = new Uint8Array(数值);
      for (let 起 = 0; 起 < 数值; 起 += 65536) crypto.getRandomValues(果.subarray(起, Math.min(数值, 起 + 65536)));
      return 果;
    },
    小数转字符串: 小数表示,
    小数精确表示: 精确小数,
    字符串转小数: 理解小数,
  };
  const 构建基础 = {
    存放包上下文: 内容 => 问(号, '存放包上下文', 当前目录, 内容.slice()),
    可绘监视面板: () => false,
    绘监视面板: () => undefined,
    获取当前程序SHA256: () => 问(号, '摘要', 程序名),
    获取文件SHA256: 名 => 问(号, '摘要', 径(名)),
  };
  const 桥 = new WebAssembly.Instance(桥模块).exports;
  const 模块 = 任务.模块;
  const 实例 = new WebAssembly.Instance(模块, 造边界导入(模块, 桥, {标准库, 构建基础}));
  // 文言：有独栈之能则用之，免深调之溢；_start 毕，应用实现启动之术者乃调之。汉语：有 JSPI 时在独立的 Wasm 栈上执行，避免工作线程的栈太小、编译器深递归溢出；
  //   _start 之后，应用若实现了「启动程序」（导出 豫言操作系统启动/启动程序）就调用它。
  const 异步 = typeof WebAssembly.promising === 'function';
  return async () => {
    if (异步) await WebAssembly.promising(实例.exports._start)(); else 实例.exports._start();
    const 导出 = 造边界导出(实例, 模块, 桥, {异步});
    if (导出[启动导出名]) await 导出[启动导出名]();
  };
}

self.onmessage = async ({data: 消息}) => {
  if (消息.种 === '初始') {
    控制 = new Int32Array(消息.控制);
    数据 = new Uint8Array(消息.数据);
    桥模块 = 消息.桥模块;
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
