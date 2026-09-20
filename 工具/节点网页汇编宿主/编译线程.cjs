// 文言：客实例各立，机器码共用；限次复用其工，不复用其客。汉语：默认有界复用线程与 GC 堆容量，每项任务仍有新 Wasm 实例、值桥和宿主状态；设 YY_NODE_COMPILER_REUSE=0 恢复独立线程。
'use strict';
const 文件 = require('node:fs'), 路径 = require('node:path');
const {EventEmitter} = require('node:events');
const {Worker, MessageChannel} = require('node:worker_threads');
const {接管进程} = require('./进程桥接.cjs');
const {PassThrough} = require('node:stream');

function 建立编译线程(宿主文件, 编译器路径, 引擎参数) {
  const 输入 = 路径.resolve(编译器路径), 模块们 = new Map();
  const 复用 = process.env.YY_NODE_COMPILER_REUSE !== '0', 空闲 = [], 诸工 = new Set();
  const 空闲上限 = require('node:os').availableParallelism();
  let 已关 = false;
  function 新池工() {
    const 工 = new Worker(宿主文件, {workerData:{复用线程:true}, execArgv:[], resourceLimits:{stackSizeMb:128}, stdout:true, stderr:true});
    const 项 = {工, 当前:null, 退事:null, 已退:false, 次数:0, 闲时:null};
    诸工.add(项);
    工.on('message', 消息 => {
      const 事 = 项.当前;
      if (!事) return;
      if (消息.种 === '出') (消息.号 === 1 ? 事.桥.stdout : 事.桥.stderr).write(Buffer.from(消息.值));
      else if (消息.种 === '毕') {
        项.当前 = null;
        事.清理();
        事.桥.stdout.end(); 事.桥.stderr.end();
        // 文言：出之流阖，方告其毕。汉语：PassThrough 的 end 事件确认所有输出已交付上层后，才按原进程桥接约定发 close。
        // 文言：四事而换其工，毋久蓄大堆。汉语：限制每个线程的任务次数，回收跨任务累积的 GC 堆高水位；不复用 Wasm 实例或数组内容。
        项.次数++;
        if (!已关 && !项.已退 && 项.次数 < 4 && 空闲.length < 空闲上限) {
          事.退码 = 消息.码; 事.告毕();
          空闲.push(项);
          // 文言：闲过四分之一秒，释其工。汉语：依赖图收窄时释放闲置堆，避免末级代码生成同时负担全部工作线程的高水位。
          项.闲时 = setTimeout(() => {项.已退 = true;const 位=空闲.indexOf(项);if(位>=0)空闲.splice(位,1);项.工.terminate().catch(() => {});},250);
          项.闲时.unref();
        }
        else {
          // 文言：欲换工，待其真退乃释任务槽。汉语：回收路径等待 Worker exit，防止旧堆尚未释放就提前启动替代任务、突破调度器的并发内存预算。
          项.退事 = {事, 码:消息.码};
          项.工.terminate().catch(() => {});
        }
      }
    });
    for (const [流, 号] of [[工.stdout,1],[工.stderr,2]]) 流.on('data', 值 => {
      if (项.当前) (号 === 1 ? 项.当前.桥.stdout : 项.当前.桥.stderr).write(值);
    });
    工.once('error', 错 => {
      项.已退 = true;
      if (项.当前) 项.当前.桥.stderr.write(Buffer.from(String(错.stack ?? 错)+'\n'));
    });
    工.once('exit', 码 => {
      clearTimeout(项.闲时);
      项.已退 = true; 诸工.delete(项);
      if (项.退事) {const {事,码:任务码}=项.退事;项.退事=null;事.退码=任务码;事.告毕();}
      if (项.当前) {
        const 事 = 项.当前; 项.当前 = null; 事.清理();
        事.桥.stdout.end(); 事.桥.stderr.end(); 事.退码 = 码 || 1; 事.告毕();
      }
    });
    return 项;
  }
  function 复用启动(数据, 端口, 清理) {
    while (空闲.length && 空闲.at(-1).已退) 空闲.pop();
    const 项 = 空闲.pop() ?? 新池工();
    clearTimeout(项.闲时); 项.闲时 = null;
    const 桥 = new EventEmitter();
    桥.pid = process.pid; 桥.stdout = new PassThrough(); 桥.stderr = new PassThrough();
    const 事 = {桥, 清理, 退码:null, 出毕:false, 错毕:false, 已告:false};
    桥.kill = () => {if (项.当前 === 事) {项.已退 = true;项.工.terminate().catch(() => {});} return true;};
    事.告毕 = () => {
      if (事.退码 !== null && 事.出毕 && 事.错毕 && !事.已告) {事.已告 = true; 桥.emit('close', 事.退码, null);}
    };
    桥.stdout.once('end', () => {事.出毕 = true; 事.告毕();});
    桥.stderr.once('end', () => {事.错毕 = true; 事.告毕();});
    项.当前 = 事;
    queueMicrotask(() => {
      桥.emit('spawn');
      try {项.工.postMessage({...数据, 复用线程:true}, [端口]);}
      catch (错) {端口.close(); 桥.stderr.write(Buffer.from(String(错)+'\n')); 桥.kill();}
    });
    return 桥;
  }
  function 模块(名) {
    const 径 = 文件.realpathSync(名), 状态 = 文件.statSync(径, {bigint: true});
    const 身份 = `${状态.dev}:${状态.ino}:${状态.size}:${状态.mtimeNs}:${状态.ctimeNs}`;
    const 旧 = 模块们.get(径);
    if (旧?.身份 === 身份) return 旧.值;
    const 值 = new WebAssembly.Module(文件.readFileSync(径));
    模块们.set(径, {身份, 值});
    return 值;
  }
  function 启动(程序, 参数, 客体) {
    const 位 = 参数.indexOf(宿主文件);
    // 文言：惟编译器自派之工入此途。汉语：只匹配同一编译器的内部 worker 命令；一般 Wasm 程序与原生子进程仍使用真实进程。
    if (!客体 || 程序 !== process.execPath || 位 < 0 ||
        路径.resolve(参数[位 + 1]) !== 输入 || 参数[位 + 2] !== '--mode=worker') return null;
    if (已关) throw Error('编译线程宿主已经关闭');
    const {port1, port2} = new MessageChannel(), 信号 = new SharedArrayBuffer(4);
    const 清理 = 接管进程(port1, 信号, 启动);
    if (复用) {
      try {return 复用启动({参数:参数.slice(位+1),端口:port2,信号,编译线程:true,模块:模块(输入),桥模块:模块('yy节点值桥接.wasm'),引擎参数},port2,清理);}
      catch (错) {清理();port2.close();throw 错;}
    }
    let 工;
    try {
      工 = new Worker(宿主文件, {
        workerData: {参数: 参数.slice(位 + 1), 端口: port2, 信号, 编译线程: true,
          模块: 模块(输入), 桥模块: 模块('yy节点值桥接.wasm'), 引擎参数},
        transferList: [port2], resourceLimits: {stackSizeMb: 128},
        stdout: true, stderr: true, execArgv: []
      });
    } catch (错) {清理(); port2.close(); throw 错;}
    const 桥 = new EventEmitter();
    // 文言：此号确为宿主进程号，不伪造子进程。汉语：编译线程共享真实宿主 PID；任务句柄和编译任务名仍唯一。
    桥.pid = process.pid;
    桥.stdout = 工.stdout;
    桥.stderr = 工.stderr;
    桥.kill = () => {工.terminate().catch(() => {}); return true;};
    let 退码 = null, 出毕 = false, 错毕 = false, 已告 = false;
    function 告毕() {
      if (退码 !== null && 出毕 && 错毕 && !已告) {已告 = true; 桥.emit('close', 退码, null);}
    }
    工.once('online', () => 桥.emit('spawn'));
    工.once('error', 错 => 桥.emit('error', 错));
    工.stdout.once('end', () => {出毕 = true; 告毕();});
    工.stderr.once('end', () => {错毕 = true; 告毕();});
    工.once('exit', 码 => {清理(); 退码 = 码; 告毕();});
    return 桥;
  }
  启动.清理 = () => {已关 = true; 空闲.length = 0; for (const 项 of 诸工) {clearTimeout(项.闲时);项.工.terminate().catch(() => {});}};
  return 启动;
}
module.exports = {建立编译线程};
