// 汉语：节点仅调用公共接口形状，实际设备操作由豫言原生服务完成。文言：节点惟调公共接口之形，器之事皆由豫言原生之服成之。
import {创建原生声音桥} from './子程序.mjs';
import {strict as 断言} from 'node:assert';
const 桥 = 创建原生声音桥({服务路径:process.argv[2]});
const 实 = 桥.实现;
try {
  const 开 = 实.辟声出(48000n, 2n, 4096n);
  断言.equal(开[0], 0);
  const 流 = 开[1], 样 = new Uint8Array(19200);
  断言.deepEqual(实.写声样本(流, 样), [0]);
  断言.deepEqual(实.写声样本(流, 样), [0]);
  断言.ok(实.声出延迟(流) > 0n);
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 500);
  断言.equal(实.已播帧数(流), 9600n);
  断言.deepEqual(实.阖声出(流), [0]);
  断言.equal(实.已播帧数(流), -1n);
  console.log('节点转递豫言原生声音：排队、设备时钟与关闭通过');
} finally {桥.关闭();}
