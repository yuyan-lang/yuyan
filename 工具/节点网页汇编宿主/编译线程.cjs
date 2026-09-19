// 文言：诸工各立实例，惟机器码共用。汉语：仅复用编译器模块代码；每项任务的新线程、GC 堆、实例及宿主状态独立。
'use strict';
const 文件 = require('node:fs'), 路径 = require('node:path');
const {EventEmitter} = require('node:events');
const {Worker, MessageChannel} = require('node:worker_threads');
const {接管进程} = require('./进程桥接.cjs');

function 建立编译线程(宿主文件, 编译器路径, 引擎参数) {
  const 输入 = 路径.resolve(编译器路径), 模块们 = new Map();
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
    const {port1, port2} = new MessageChannel(), 信号 = new SharedArrayBuffer(4);
    const 清理 = 接管进程(port1, 信号, 启动);
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
  return 启动;
}
module.exports = {建立编译线程};
