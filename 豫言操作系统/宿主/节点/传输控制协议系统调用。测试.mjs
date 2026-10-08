// 汉语：以真实苹果套接字核验系统入口与负 errno，网络算法由豫言核心的测试验证。文言：以实苹果套接字核系统入口与负 errno；网络之术，豫言核之试验之。
import 断言 from 'node:assert/strict';
import {创建传输控制协议系统调用} from './传输控制协议.mjs';
const 内存 = new WebAssembly.Memory({initial: 1});
const 系统 = 创建传输控制协议系统调用({取内存: () => 内存});
if (process.platform === 'darwin') {
  断言.equal(系统.可用, true);
  const 苹 = 系统.导入.苹果;
  const 柄 = 苹.socket(2, 1, 0);
  断言.ok(柄 >= 0);
  try {
    断言.equal(苹.read(-1, 1024, 1), -9);
    断言.equal(苹.write(-1, 1024, 1), -9);
    断言.equal(苹.poll(1024, 0, 0), 0);
  } finally {断言.equal(苹.close(柄), 0); 系统.关闭();}
} else {
  断言.equal(系统.可用, false);
  断言.equal(系统.状态, 58);
}
console.log('传输控制协议系统调用测试通过');
