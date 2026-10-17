import {test, after} from 'node:test';
import {mf, 跑, 观察} from './冒烟.mjs';
after(async () => { await mf.dispose(); });
const 睡 = ms => new Promise(r => setTimeout(r, ms));
test('探针：取消是否传到服务端 Worker（永不停歇的 ticker 流）', async () => {
  await (await mf.getWorker('svc')).fetch('https://o/__clear');
  const 果 = await 跑({op: 'svc', url: 'https://svc/ticker', read: 'cancel', timeout: 8000});
  console.log('  应用报告:', 果.文);
  const 甲 = await 观察('svc'); const 拉甲 = 甲.拉取?.['/ticker'];
  await 睡(600);
  const 乙 = await 观察('svc'); const 拉乙 = 乙.拉取?.['/ticker'];
  console.log('  取消观察:', JSON.stringify(乙.取消), ' 请求信号中止:', JSON.stringify(乙.信号中止), ' 取消后拉取数:', 拉甲, '→', 拉乙);
});
test('探针：期满后是否传到服务端', async () => {
  await (await mf.getWorker('svc')).fetch('https://o/__clear');
  const 果 = await 跑({op: 'svc', url: 'https://svc/ticker', read: 'chunks', timeout: 400});
  console.log('  应用报告:', 果.文.slice(-60));
  await 睡(600);
  const 乙 = await 观察('svc');
  console.log('  取消观察:', JSON.stringify(乙.取消), ' 请求信号中止:', JSON.stringify(乙.信号中止), ' 拉取数:', 乙.拉取?.['/ticker']);
});
