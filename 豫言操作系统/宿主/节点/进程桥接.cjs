// 文言：主线程候子进程，客线程惟递其请。汉语：只桥接系统进程与完成事件，任务图及编译算法仍在豫言客体内执行。
'use strict';
const {spawn} = require('node:child_process');
const {receiveMessageOnPort} = require('node:worker_threads');
const {constants} = require('node:os');
const 网 = require('node:net');

function 接管进程(端口, 信号缓冲, 编译线程 = null) {
  const 信号 = new Int32Array(信号缓冲), 记录们 = new Map(), 套接字们 = new Map();
  let 下号 = 1024, 待候 = null, 已关 = false;
  const 上限 = 256 * 1024 * 1024;
  function 回答(消息) {
    if (已关) return;
    端口.postMessage(消息);
    Atomics.store(信号, 0, 1);
    Atomics.notify(信号, 0);
  }
  // 文言：地址不可解者，其码与系统之误异，以负二万为基别之（同原生）。汉语：地址解析失败（如 ENOTFOUND）没有对应的 errno，按原生运行时的约定记为 -20000 加偏移。
  function 错误码(错) { return constants.errno[错.code] !== undefined ? -constants.errno[错.code] : /^EAI_|ENOTFOUND/.test(错.code ?? '') ? -20002 : -5; }
  // 文言：套接字之事：可读者，有所收、已终、有误或有待接之连；可写者，已连或已败。汉语：套接字就绪事件（位同 poll 的换算）：可读 = 有已收数据、对端已结束、出错或（监听者）有待接受的连接；可写 = 已连上或连接已失败；错误 4；挂断 8。
  function 套接字事件(记录, 掩码) {
    let 事 = 0;
    if (记录.种 === '听') return ((掩码 & 1) && 记录.待接.length ? 1 : 0) | (记录.错 ? 4 : 0);
    if ((掩码 & 1) && (记录.入长 > 0 || 记录.结束 || 记录.错 || 记录.连错)) 事 |= 1;
    if ((掩码 & 2) && (记录.已连 || 记录.错 || 记录.连错)) 事 |= 2;
    if (记录.错 || 记录.连错) 事 |= 4;
    if (记录.结束) 事 |= 8;
    return 事;
  }
  function 事件(关注) {
    return 关注.map(([号, 掩码]) => {
      const 套记 = 套接字们.get(Number(号));
      if (套记) return 套接字事件(套记, Number(掩码));
      const 记录 = 记录们.get(Number(号));
      if (!记录) return 4;
      return 记录.完成 && (Number(掩码) & 1) ? 1 : 0;
    });
  }
  function 唤候(到期 = false) {
    if (!待候) return;
    const 果 = 事件(待候.关注);
    if (!到期 && !果.some(Boolean)) return;
    clearTimeout(待候.定时);
    const 单 = 待候.单;
    待候 = null;
    回答({值: 单 ? [0, 果[0]] : [0, [果, 果.length]]});
  }
  // 文言：连之记：所收积于入，逾上限则暂停其流，读而减之乃复。汉语：连接记录：收到的数据积在“入”里，超过上限就暂停读取，读走一半后恢复；对端结束、出错与连上都唤醒等待者。
  function 新连接记录(套, 已连) {
    const 记录 = {种: '连', 套, 入: [], 入长: 0, 已连, 连错: 0, 结束: false, 错: 0};
    套.on('data', 块 => {记录.入.push(块); 记录.入长 += 块.length; if (记录.入长 > 上限) 套.pause(); 唤候();});
    套.on('connect', () => {记录.已连 = true; 唤候();});
    套.on('end', () => {记录.结束 = true; 唤候();});
    套.on('error', 错 => {if (记录.已连) 记录.错 = 错误码(错); else 记录.连错 = 错误码(错); 唤候();});
    套.on('close', () => {记录.结束 = true; 唤候();});
    return 记录;
  }
  function 取出(记录, 最大) {
    const 全 = Buffer.concat(记录.入), 取 = 全.subarray(0, 最大), 余 = 全.subarray(取.length);
    记录.入 = 余.length ? [Buffer.from(余)] : [];
    记录.入长 = 余.length;
    if (记录.套.isPaused() && 记录.入长 < 上限 / 2) 记录.套.resume();
    return Buffer.from(取);
  }
  // 文言：传输控制协议之术，皆不塞：候则返一，闭则返二，败则返负之误码。汉语：TCP 操作（与原生运行时同义，均不阻塞）：返回（状态，值），状态 0 成功、1 暂需等待、2 连接已关闭、负数为 -errno；句柄与子进程共用编号空间，多路等待可一并关注。
  async function 网络(术, 参) {
    const 空 = Buffer.alloc(0);
    if (术 === '网监听') {
      const [址, 端口号, 队长] = 参;
      if (!(端口号 >= 0 && 端口号 <= 65535) || !(队长 >= 0)) return [-22, -1];
      const 服 = 网.createServer({allowHalfOpen: true});
      const 记录 = {种: '听', 服, 待接: [], 错: 0};
      return await new Promise(成 => {
        服.once('error', 错 => 成([错误码(错), -1]));
        服.listen({host: 址 || undefined, port: 端口号, backlog: 队长}, () => {
          服.removeAllListeners('error');
          服.on('error', 错 => {记录.错 = 错误码(错); 唤候();});
          服.on('connection', 套 => {记录.待接.push(新连接记录(套, true)); 唤候();});
          const 号 = 下号++;
          套接字们.set(号, 记录);
          成([0, 号]);
        });
      });
    }
    if (术 === '网开始连接') {
      const [主机, 端口号] = 参;
      if (!主机 || !(端口号 >= 0 && 端口号 <= 65535)) return [-22, -1];
      const 号 = 下号++;
      套接字们.set(号, 新连接记录(网.connect({host: 主机, port: 端口号, allowHalfOpen: true}), false));
      return [1, 号];
    }
    const 号 = Number(参[0]), 记录 = 套接字们.get(号);
    if (!记录) return 术 === '网读取' ? [-9, 空] : [-9, 术 === '网本地端口' ? -1 : 0];
    if (术 === '网完成连接') return [记录.连错 || (记录.已连 ? 0 : 1), 号];
    if (术 === '网接受') {
      if (记录.种 !== '听') return [-22, -1];
      if (!记录.待接.length) return [1, -1];
      const 新号 = 下号++;
      套接字们.set(新号, 记录.待接.shift());
      return [0, 新号];
    }
    if (术 === '网读取') {
      const 最大 = 参[1];
      if (记录.种 !== '连' || !(最大 >= 0)) return [-22, 空];
      if (最大 === 0) return [0, 空];
      if (记录.入长 > 0) return [0, 取出(记录, 最大)];
      if (记录.错) return [记录.错, 空];
      if (记录.结束) return [2, 空];
      return [1, 空];
    }
    if (术 === '网写入') {
      const 内容 = Buffer.from(参[1]), 起 = 参[2];
      if (记录.种 !== '连' || !(起 >= 0 && 起 <= 内容.length)) return [-22, 0];
      if (起 === 内容.length) return [0, 0];
      if (记录.错 || 记录.连错) return [记录.错 || 记录.连错, 0];
      if (!记录.已连) return [1, 0];
      // 待办事项：Node 的套接字把写入全部缓冲，不会短写，也不按对端速度背压。
      记录.套.write(内容.subarray(起));
      return [0, 内容.length - 起];
    }
    if (术 === '网等待') {
      const [, 关注, 超时] = 参;
      if (!(关注 >= 1 && 关注 <= 3) || !(超时 >= -1 && 超时 <= 2147483647)) return [-22, 0];
      const 事 = 套接字事件(记录, 关注);
      if (事 || 超时 === 0) return [0, 事];
      待候 = {关注: [[号, 关注]], 单: true, 定时: 超时 < 0 ? null : setTimeout(() => 唤候(true), 超时)};
      return undefined;
    }
    if (术 === '网本地端口') {
      const 口 = 记录.种 === '听' ? 记录.服.address()?.port : 记录.套.localPort;
      return 口 === undefined ? [-57, -1] : [0, 口];
    }
    if (术 === '网无延迟') {if (记录.种 === '连') 记录.套.setNoDelay(!!参[1]); return [0, null];}
    if (术 === '网关闭写入') {if (记录.种 === '连') 记录.套.end(); return [0, null];}
    if (术 === '网关闭') {
      套接字们.delete(号);
      if (记录.种 === '听') {记录.服.close(); for (const 待 of 记录.待接) 待.套.destroy();}
      else 记录.套.destroySoon();
      return [0, null];
    }
    throw Error('未知网络操作');
  }
  async function 执行(请) {
    const [术, ...参] = 请;
    if (术.startsWith('网')) return await 网络(术, 参);
    if (术 === '启动') {
      if (记录们.size >= 4096) return -24;
      const [程序, 参数, 客体, 目录, 环境, 限时, 宽限] = 参;
      // 文言：子进程启于客之今目录；env、timeout 之包装已由客拆出，环境并入，限时由此计之。汉语：子进程在客实例记录的当前目录里启动。env 与 timeout 包装已在客线程拆出：附加环境并入子进程环境；有时限时由这里计时，到时发 SIGTERM（有宽限则宽限后再发 SIGKILL），退出码记为 124。带包装的任务不走内部编译线程池。
      const 包装 = !!环境 || 限时 > 0;
      const 工 = (包装 ? null : 编译线程?.(程序, 参数, 客体, 目录)) ?? spawn(程序, 参数, {cwd: 目录, stdio: ['ignore', 'pipe', 'pipe'], shell: false,
        env: 客体 ? {...process.env, ...(环境 ?? {}), YY_NODE_REPEAT: '1'} : process.env});
      const 记录 = {工, 完成: false, 码: 0, 输出: [], 错误: [], 出长: 0, 错长: 0, 溢出: false, 超时: false};
      const 定时们 = [];
      if (限时 > 0) 定时们.push(setTimeout(() => {
        记录.超时 = true;
        工.kill('SIGTERM');
        if (宽限 > 0) 定时们.push(setTimeout(() => 工.kill('SIGKILL'), 宽限 * 1000));
      }, 限时 * 1000));
      function 收(块, 是错) {
        const 长名 = 是错 ? '错长' : '出长';
        if (记录[长名] + 块.length > 上限) {
          记录.溢出 = true;
          工.kill('SIGKILL');
          return;
        }
        记录[长名] += 块.length;
        记录[是错 ? '错误' : '输出'].push(块);
      }
      工.stdout.on('data', 块 => 收(块, false));
      工.stderr.on('data', 块 => 收(块, true));
      const 号 = 下号++;
      return await new Promise(成 => {
        工.once('spawn', () => {记录们.set(号, 记录); 成(号);});
        工.once('error', 错 => {记录.码 = 127; 记录.错误.push(Buffer.from(错.message)); 成(错误码(错));});
        // 文言：二出俱阖乃告毕。汉语：以 close 而非 exit 标记完成，避免丢失管道末尾输出。
        工.once('close', (码, 杀信号) => {
          记录.完成 = true;
          定时们.forEach(clearTimeout);
          记录.码 = 记录.溢出 ? 125 : 记录.超时 ? 124 : (码 ?? (杀信号 ? 128 + (constants.signals[杀信号] ?? 0) : 127));
          if (记录.溢出) 记录.错误.push(Buffer.from('子进程输出超过 256 MiB 上限'));
          唤候();
        });
      });
    }
    // 汉语：仅终止本实例登记的子进程，记录保留至收取；结束的进程幂等成功。文言：惟止今实例所录之子程，记留至收之；既毕者复请亦成。
    if (术 === '终止') {
      const 号 = Number(参[0]), 记录 = 记录们.get(号);
      if (!记录) return 8;
      if (记录.完成) return 0;
      try { return 记录.工.kill('SIGKILL') ? 0 : 29; }
      catch { return 29; }
    }
    if (术 === '收取') {
      const 号 = Number(参[0]), 记录 = 记录们.get(号);
      if (!记录) throw Error('无效的异步子进程句柄');
      if (!记录.完成) return [0, 0, 0, Buffer.alloc(0), Buffer.alloc(0)];
      记录们.delete(号);
      return [1, 记录.工.pid, 记录.码, Buffer.concat(记录.输出), Buffer.concat(记录.错误)];
    }
    if (术 === '等待') {
      const [关注, 超时] = 参;
      if (!Number.isInteger(超时) || 超时 < -1 || 超时 > 2147483647 ||
          关注.some(([号, 掩码]) => !Number.isSafeInteger(Number(号)) || Number(号) < 0 || Number(号) > 2147483647 ||
            !Number.isInteger(Number(掩码)) || Number(掩码) < 1 || Number(掩码) > 3)) return [-22, [[], 0]];
      const 果 = 事件(关注);
      if (果.some(Boolean) || 超时 === 0) return [0, [果, 果.length]];
      待候 = {关注, 定时: 超时 < 0 ? null : setTimeout(() => 唤候(true), 超时)};
      return undefined;
    }
    throw Error('未知进程桥接操作');
  }
  端口.on('message', 请 => {
    执行(请).then(值 => {if (值 !== undefined) 回答({值});}, 错 => 回答({错误: 错.message}));
  });
  return () => {
    已关 = true;
    if (待候) clearTimeout(待候.定时);
    待候 = null;
    for (const 记录 of 记录们.values()) if (!记录.完成) 记录.工.kill('SIGKILL');
    记录们.clear();
    for (const 记录 of 套接字们.values()) {
      if (记录.种 === '听') {记录.服.close(); for (const 待 of 记录.待接) 待.套.destroy();}
      else 记录.套.destroy();
    }
    套接字们.clear();
    端口.close();
  };
}

function 客体请求(端口, 信号缓冲) {
  const 信号 = new Int32Array(信号缓冲);
  function 还字节(值) {
    if (值 instanceof Uint8Array) return Buffer.from(值);
    return Array.isArray(值) ? 值.map(还字节) : 值;
  }
  return (...请) => {
    Atomics.store(信号, 0, 0);
    端口.postMessage(请);
    while (Atomics.load(信号, 0) === 0) Atomics.wait(信号, 0, 0);
    const 信 = receiveMessageOnPort(端口);
    if (!信) throw Error('进程桥接通知缺少响应');
    if (信.message.错误) throw Error(信.message.错误);
    return 还字节(信.message.值);
  };
}
module.exports = {接管进程, 客体请求};
