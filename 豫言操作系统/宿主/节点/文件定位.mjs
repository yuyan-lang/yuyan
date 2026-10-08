// 汉语：诺节只绑定系统 pread 到自有豫言共享核心；完整偏移由共核组装。文言：诺节惟绑系统定位之读于自有豫言共核；全偏移由共核合之。
import * as 文件系统 from 'node:fs';
import 路径 from 'node:path';
import {fileURLToPath} from 'node:url';
let 核, 库;
function 载入() {
  if (核 !== undefined) return 核;
  核 = null;
  if (!['darwin', 'linux'].includes(process.platform)) return 核;
  try {
    const 外部 = process.getBuiltinModule?.('node:ffi');
    if (!外部) return 核;
    const 候选 = [process.env.YY文件定位共享, 路径.join(路径.dirname(fileURLToPath(import.meta.url)), 'yy文件定位共享.wasm'), 路径.resolve('yy文件定位共享.wasm')];
    const 径 = 候选.find(项 => 项 && 文件系统.existsSync(项));
    if (!径) return 核;
    库 = new 外部.DynamicLibrary(process.platform === 'darwin' ? '/usr/lib/libSystem.B.dylib' : 'libc.so.6');
    const 调 = 库.getFunction('pread', {arguments: ['int32', 'pointer', 'uint64', 'int64'], return: 'int64'});
    let 实例;
    const 接读 = (柄, 址, 长, 偏移) => Number(调(柄, 外部.getRawPointer(new Uint8Array(实例.exports.memory.buffer, 址, 长)), BigInt(长), 偏移));
    实例 = new WebAssembly.Instance(new WebAssembly.Module(文件系统.readFileSync(径)), {苹果: {pread: 接读}, 林纳克斯: {pread64: 接读}});
    核 = 实例.exports;
  } catch { 核 = null; }
  return 核;
}
export function 共享文件定位读取(描述符, 偏移, 长度) {
  const 共核 = 载入();
  if (!共核) return [2, new Uint8Array(), '文件定位共享核心或系统库调用不可用'];
  try {
    const 数 = 共核.定位读取(process.platform === 'darwin' ? 3 : 2, 描述符, 1024, 长度, Number(BigInt.asUintN(32, 偏移)), Number(偏移 >> 32n));
    if (数 < 0) return [8, new Uint8Array(), '文件定位读取失败'];
    return 数 === 0 ? [9, new Uint8Array(), ''] : [0, new Uint8Array(共核.memory.buffer, 1024, 数).slice(), ''];
  } catch (错) { return [8, new Uint8Array(), String(错.message ?? 错)]; }
}
