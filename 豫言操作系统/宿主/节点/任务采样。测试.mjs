// 汉语：真实进程忙碌采样验证CPU差值与宿主内存口径，不验证窗口或公共ABI。文言：以实进程忙采验CPU之差与宿主内存之界，不验窗口或公ABI。
import test from 'node:test';
import assert from 'node:assert/strict';
import {创建任务采样} from './任务采样.mjs';

test('真实宿主进程处理器和内存采样', () => {
  const 采样器 = 创建任务采样({名称: '豫言桌面'});
  const 前 = 采样器.采样();
  const 止时 = performance.now() + 30;
  while (performance.now() < 止时) Math.sqrt(performance.now());
  const 后 = 采样器.采样();
  assert.equal(后.标识, String(process.pid));
  assert.equal(后.名称, '豫言桌面');
  assert.equal(后.范围, '宿主进程');
  assert.ok(后.采样微秒 > 前.采样微秒);
  assert.ok(后.处理器万分比 > 0);
  assert.ok(后.常驻字节 > 0 && 后.堆已用字节 > 0);
  assert.ok(后.常驻字节 >= 后.堆已用字节);
});
