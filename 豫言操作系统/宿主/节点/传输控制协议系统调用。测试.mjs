// 汉语：以真实林檎套接字核验系统入口与负 errno，网络算法由豫言核心的测试验证。文言：以实林檎套接字核系统入口与负 errno；网络之术，豫言核之试验之。
import 断言 from 'node:assert/strict';
import {创建传输控制协议系统调用} from './传输控制协议.mjs';
const 内存 = new WebAssembly.Memory({initial: 1});
const 系统 = 创建传输控制协议系统调用({取内存: () => 内存});
if (process.platform === 'darwin') {
  断言.equal(系统.可用, true);
  const 苹 = 系统.导入.林檎;
  const 柄 = 苹.建套接字(2, 1, 0);
  断言.ok(柄 >= 0);
  try {
    const 旧标 = 苹.文件控制(柄, 3, 0);
    断言.ok(旧标 >= 0);
    断言.equal(苹.文件控制(柄, 4, 旧标 | 4), 0);
    断言.equal(苹.文件控制(柄, 3, 0) & 4, 4);
    const 路径 = new TextEncoder().encode('/etc/hosts\0');
    new Uint8Array(内存.buffer, 2048, 路径.length).set(路径);
    const 文件柄 = 苹.相对打开(-2, 2048, 0, 0);
    断言.ok(文件柄 >= 0);
    try {断言.ok(苹.读(文件柄, 4096, 16) > 0);}
    finally {断言.equal(苹.关闭(文件柄), 0);}
    断言.ok(苹.设信号处理(13, 1) >= 0);
    断言.equal(苹.读(-1, 1024, 1), -9);
    断言.equal(苹.写(-1, 1024, 1), -9);
    断言.equal(苹.轮询(1024, 0, 0), 0);
  } finally {断言.equal(苹.关闭(柄), 0); 系统.关闭();}
} else {
  断言.equal(系统.可用, false);
  断言.equal(系统.状态, 58);
}
console.log('传输控制协议系统调用测试通过');
