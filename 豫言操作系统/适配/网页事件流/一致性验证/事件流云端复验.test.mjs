// 网页事件流在真实 workerd（wrangler dev --local，Durable Object 内 durable-fetch）中的复验，走真实 HTTP。
// 前提：已按 说明 的“在本地 workerd 里复跑”起好 事件流一致性 应用（兼容标志含 enable_request_signal），环境变量 远端地址 指向它。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {取, 读日志, 睡, 等到, 流读取器} from './云端复验工具.mjs';

// 同一个持久对象的 D1 日志跨测试累积：每个测试先记基线，只看其后新增的记录。
async function 增量(...键们) {
  const 基线 = {};
  for (const 键 of 键们) 基线[键] = (await 读日志(键)).length;
  return async 键 => (await 读日志(键)).slice(基线[键]);
}
const 路 = (径, 参 = '') => encodeURI(径) + (参 ? '?' + 参 : '');
const 十六 = 值 => Buffer.from(值).toString('hex');

test('云端：响应头与帧序（真实 HTTP，chunked）', async () => {
  const 回 = await 取(路('/简单', 'n=5'));
  assert.equal(回.status, 200);
  assert.equal(回.headers.get('content-type'), 'text/event-stream; charset=utf-8');
  assert.equal(回.headers.get('cache-control'), 'no-store');
  assert.deepEqual(await new 流读取器(回).读完(), ['data: 0 豫言', 'data: 1 豫言', 'data: 2 豫言', 'data: 3 豫言', 'data: 4 豫言']);
});

test('云端：响应头先于程序结束；程序返回后未结束的流不会自行结束', async () => {
  const 增 = await 增量('不结束');
  const 回 = await 取(路('/不结束'));
  assert.equal(回.status, 200);
  const 读 = new 流读取器(回);
  assert.equal(await 读.下一帧(), 'data: 只有一帧');
  await 等到(async () => (await 增('不结束')).length === 1, {说明: '程序返回'});
  assert.equal((await 读.下一块(1500)).类型, '超时');
  await 读.取消('测试结束');
});

test('云端：64 KiB 块边界与大量块', async () => {
  let 读 = new 流读取器(await 取(路('/大块', '字=a&长=65536')));
  let 文 = await 读.全文();
  assert.equal(文.length, 65536 + 'data: 写入=0\n\n'.length);
  读 = new 流读取器(await 取(路('/大块', '字=a&长=65537')));
  assert.deepEqual(await 读.读完(), ['data: 异常=事件流单次写入超过 64 KiB']);
  读 = new 流读取器(await 取(路('/多块', '数=80&长=65536')));
  文 = await 读.全文(60000);
  assert.equal(文.length, 80 * 65536 + 2);
  for (const 序 of [0, 25, 26, 79]) assert.equal(文[序 * 65536], 'abcdefghijklmnopqrstuvwxyz'[序 % 26]);
  读 = new 流读取器(await 取(路('/多块', '数=3000&长=10')));
  文 = await 读.全文(60000);
  assert.equal(文.length, 30000 + 2);
});

test('云端：背压——读者停读时写入不会无限前进', async () => {
  const 增 = await 增量('写完', '背压结束');
  const 回 = await 取(路('/背压', '数=400&长=10000'));
  const 读 = new 流读取器(回);
  const 首 = await 读.下一块();
  assert.equal(首.类型, '块');
  await 睡(800);
  const 已写 = (await 增('写完')).length;
  console.log(`  [观察] 停读 800 毫秒后已写出 ${已写} 帧（每帧 10000 字节，共 400 帧）`);
  assert.ok(已写 < 400, '读者停读时不应写完全部');
  const 文 = await 读.全文();
  await 等到(async () => (await 增('背压结束')).length === 1, {说明: '背压结束'});
  assert.equal((await 增('写完')).length, 400);
  assert.ok(文.length + 首.字节.byteLength >= 400 * 10000);
});

async function 校验断开收尾(增) {
  await 等到(async () => (await 增('断开收尾')).length === 1, {超时: 20000, 说明: '断开后收尾'});
  assert.deepEqual(await 增('循环返回'), ['1']);
  assert.deepEqual(await 增('已断开'), ['真']);
  assert.deepEqual(await 增('结算'), ['完成']);
  assert.deepEqual(await 增('结束后写'), ['2']);
  assert.deepEqual(await 增('结束后已断开'), ['真']);
}
test('云端：客户端断开（fetch 中止）后写入返回 1，应用继续完成收尾', async () => {
  const 增 = await 增量('断开收尾', '循环返回', '已断开', '结算', '结束后写', '结束后已断开');
  const 控制 = new AbortController();
  const 回 = await 取(路('/断开'), {signal: 控制.signal});
  const 读 = new 流读取器(回);
  assert.equal(await 读.下一帧(), 'data: 0');
  assert.equal(await 读.下一帧(), 'data: 1');
  const 起 = Date.now();
  控制.abort();
  await 校验断开收尾(增);
  console.log(`  [观察] 从中止请求到应用记下“断开收尾”约 ${Date.now() - 起} 毫秒`);
});

test('云端：结束后语义', async () => {
  const 增 = await 增量('结束后收尾', '结束后写', '结束后已断开');
  const 读 = new 流读取器(await 取(路('/结束后')));
  assert.deepEqual(await 读.读完(), ['data: 甲']);
  await 等到(async () => (await 增('结束后收尾')).length === 1, {说明: '结束后收尾'});
  assert.deepEqual(await 增('结束后写'), ['2']);
  assert.deepEqual(await 增('结束后已断开'), ['假']);
});

test('云端：附加标头出现在线路上；Content-Type 不可覆盖', async () => {
  const 请求体 = 头列 => ({method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify({status: 201, headers: 头列.map(([名, 值]) => [十六(名), 十六(值)])})});
  let 回 = await 取('/%E6%A0%87%E5%A4%B4', 请求体([['X-Trace', 'abc-123'], ['Cache-Control', 'no-cache']]));
  assert.equal(回.status, 201);
  assert.equal(回.headers.get('x-trace'), 'abc-123');
  assert.equal(回.headers.get('cache-control'), 'no-cache');
  assert.equal(回.headers.get('content-type'), 'text/event-stream; charset=utf-8');
  assert.deepEqual(await new 流读取器(回).读完(), ['data: 标头已接受']);
  回 = await 取('/%E6%A0%87%E5%A4%B4', 请求体([['Content-Type', 'text/plain']]));
  assert.equal(回.status, 400);
  assert.match(await 回.text(), /事件流标头不允许应用附加：content-type/);
});

test('云端：多个事件流并发互不串扰', async () => {
  const 结果 = await Promise.all(Array.from({length: 12}, async () => new 流读取器(await 取(路('/简单', 'n=60'))).读完()));
  for (const 帧们 of 结果) { assert.equal(帧们.length, 60); 帧们.forEach((帧, 序) => assert.equal(帧, `data: ${序} 豫言`)); }
});

// 注意：wrangler 4.129.0 的 dev 代理把“响应流被中止”的未捕获异常当作致命错误，此测试之后 wrangler dev 会退出（本地开发工具的行为，
// 直接运行 workerd 不受影响），所以它放在文件末尾，且只核对客户端所见；应用侧“中止后写入返回 2、已断开为假、重复中止幂等”由 Node 测试核对。
test('云端：中止之后客户端所见（本地 workerd 的 HTTP/1.1：网络错误或正常结束均属平台行为）', async () => {
  const 回 = await 取(路('/中止'));
  const 读 = new 流读取器(回);
  assert.equal(await 读.下一帧(), 'data: 甲');
  assert.equal(await 读.下一帧(), 'data: 乙');
  const 块 = await 读.下一块(10000);
  console.log('  [观察] 中止后 HTTP/1.1 客户端读取结果：', 块.类型, String(块.错 ?? ''));
  assert.ok(块.类型 === '错' || 块.类型 === '终');
});
