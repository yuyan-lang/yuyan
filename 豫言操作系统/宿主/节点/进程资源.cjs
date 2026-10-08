// 汉语：常驻资源由共用豫言底层核心解释；此胶水只把系统库调用接到客体线性内存。文言：常驻资源，共用豫言底层之核释之；此胶水惟接系统库之调于客体线性之存。
'use strict';
const 文件 = require('node:fs');
const 路径 = require('node:path');
let 核 = undefined, 库 = null;
function 载入() {
  if (核 !== undefined) return 核;
  核 = null;
  if (process.platform !== 'darwin') return 核;
  const 原发 = process.emitWarning;
  let 外部;
  try {
    // 汉语：系统库调用缺失明确报告不可用；只拦外部函数接口实验警告。文言：系统库之调缺，则明告不可用；惟拦外部函之实验警。
    process.emitWarning = function (警告, ...余) {
      if (/\bFFI\b/u.test(String(警告?.message ?? 警告))) return;
      return 原发.call(this, 警告, ...余);
    };
    外部 = process.getBuiltinModule?.('node:ffi');
  } catch { return 核; }
  finally { process.emitWarning = 原发; }
  if (!外部) return 核;
  try {
    const 候选 = [process.env.YY进程资源共享, 路径.join(__dirname, 'yy进程资源共享.wasm'), 路径.resolve('yy稳定节点宿主', 'yy进程资源共享.wasm')];
    const 径 = 候选.find(项 => 项 && 文件.existsSync(项));
    if (!径) return 核;
    库 = new 外部.DynamicLibrary('/usr/lib/libSystem.B.dylib');
    const 调 = 库.getFunction('proc_pid_rusage', {arguments: ['int32', 'int32', 'pointer'], return: 'int32'});
    let 实例, 内存;
    实例 = new WebAssembly.Instance(new WebAssembly.Module(文件.readFileSync(径)), {苹果: {
      proc_pid_rusage: (号, 类, 址) => {
        const 缓 = new Uint8Array(内存.buffer, 址, 96);
        return 调(号, 类, 外部.getRawPointer(缓)) === 0 ? 0 : -1;
      }
    }});
    内存 = 实例.exports.内存;
    if (!(内存 instanceof WebAssembly.Memory)) return 核;
    核 = 实例.exports;
    process.env.YY进程资源共享 = 径;
  } catch { 核 = null; }
  return 核;
}
function 查询进程资源(号) {
  const 共核 = 载入();
  if (!共核) return [58, false, 0n, false, 0n];
  try {
    const 码 = Number(共核.采样(号));
    if (码 !== 0) return [29, false, 0n, false, 0n];
    const 字节 = BigInt(共核.读取高字() >>> 0) * 4294967296n + BigInt(共核.读取低字() >>> 0);
    return [0, true, 字节, false, 0n];
  } catch { return [29, false, 0n, false, 0n]; }
}
module.exports = {查询进程资源};
