// 汉语：真实回环验证共享纯豫核心的监听、连接、任意字节传送及关闭。文言：以实回环核共用纯豫之监听、连接、任意字节之传与关闭。
import 断言 from 'node:assert/strict';
import 文件 from 'node:fs';
import {fileURLToPath as 文件路径} from 'node:url';
import {创建传输控制协议能力} from './传输控制协议.mjs';
const 路径 = process.argv[2] ?? 文件路径(new URL('./yy传输控制共享.wasm', import.meta.url));
const 模块字节 = 文件.readFileSync(路径);
console.log('共享模块导入', WebAssembly.Module.imports(new WebAssembly.Module(模块字节)));
const 网络 = 创建传输控制协议能力({模块字节});
if (process.platform !== 'darwin') {
  断言.equal(网络.可用, false);
  断言.equal(网络.能力.传输控制协议_监听('127.0.0.1', 0, 128)[0], -58);
} else {
  断言.equal(网络.可用, true);
  const 能 = 网络.能力, 柄们 = [];
  try {
    const 听 = 能.传输控制协议_监听('127.0.0.1', 0, 128);
    断言.equal(听[0], 0); 柄们.push(听[1]);
    const 端 = 能.传输控制协议_获取本地端口(听[1]);
    断言.equal(端[0], 0); 断言.ok(端[1] > 0);
    const 连 = 能.传输控制协议_开始连接('127.0.0.1', 端[1]);
    断言.ok(连[0] === 0 || 连[0] === 1); 柄们.push(连[1]);
    断言.equal(能.传输控制协议_等待(连[1], 2, 2000)[0], 0);
    断言.equal(能.传输控制协议_完成连接(连[1])[0], 0);
    断言.equal(能.传输控制协议_等待(听[1], 1, 2000)[0], 0);
    const 接 = 能.传输控制协议_接受(听[1]);
    断言.equal(接[0], 0); 柄们.push(接[1]);
    const 原字节 = Uint8Array.of(0, 255, 128, 65, 10);
    const 写 = 能.传输控制协议_从字节序数写入字节串(连[1], 原字节, 0);
    断言.equal(写[0], 0); 断言.equal(写[1], 原字节.length);
    断言.equal(能.传输控制协议_等待(接[1], 1, 2000)[0], 0);
    const 读 = 能.传输控制协议_读取(接[1], 64);
    断言.equal(读[0], 0); 断言.deepEqual(读[1], 原字节);
    断言.equal(能.传输控制协议_设置无延迟(连[1], true)[0], 0);
    断言.equal(能.传输控制协议_关闭写入(连[1])[0], 0);
  } finally {
    for (const 柄 of 柄们.reverse()) 断言.equal(能.传输控制协议_关闭(柄)[0], 0);
    网络.关闭();
  }
}
console.log('传输控制协议共享回环测试通过');
