// 文言：主线程候子进程，客线程惟递其请。汉语：只桥接系统进程与完成事件，任务图及编译算法仍在豫言客体内执行。
'use strict';
const {spawn} = require('node:child_process');
const {receiveMessageOnPort} = require('node:worker_threads');
const {constants} = require('node:os');

function 接管进程(端口, 信号缓冲, 编译线程 = null) {
  const 信号 = new Int32Array(信号缓冲), 记录们 = new Map();
  let 下号 = 1024, 待候 = null, 已关 = false;
  const 上限 = 256 * 1024 * 1024;
  function 回答(消息) {
    if (已关) return;
    端口.postMessage(消息);
    Atomics.store(信号, 0, 1);
    Atomics.notify(信号, 0);
  }
  function 错误码(错) { return -(constants.errno[错.code] ?? 5); }
  function 事件(关注) {
    return 关注.map(([号, 掩码]) => {
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
    待候 = null;
    回答({值: [0, [果, 果.length]]});
  }
  async function 执行(请) {
    const [术, ...参] = 请;
    if (术 === '启动') {
      if (记录们.size >= 4096) return -24;
      const [程序, 参数, 客体] = 参;
      const 工 = 编译线程?.(程序, 参数, 客体) ?? spawn(程序, 参数, {stdio: ['ignore', 'pipe', 'pipe'], shell: false,
        env: 客体 ? {...process.env, YY_NODE_REPEAT: '1'} : process.env});
      const 记录 = {工, 完成: false, 码: 0, 输出: [], 错误: [], 出长: 0, 错长: 0, 溢出: false};
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
          记录.码 = 记录.溢出 ? 125 : (码 ?? (杀信号 ? 128 + (constants.signals[杀信号] ?? 0) : 127));
          if (记录.溢出) 记录.错误.push(Buffer.from('子进程输出超过 256 MiB 上限'));
          唤候();
        });
      });
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
