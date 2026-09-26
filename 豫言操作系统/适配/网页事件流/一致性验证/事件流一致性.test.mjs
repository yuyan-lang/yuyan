// 网页事件流：真实 Wasm + Node 的一致性验证。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {载入应用, 创建模拟D1, 日志表, 读日志, 睡, 等到, 流读取器, 请求} from './工具.mjs';

const 新宿主 = await 载入应用();
async function 起(额外) {
  const {sql, DB} = 创建模拟D1(日志表);
  const 宿主 = 新宿主({D1: ['DB']}, 额外);
  const 调 = (路径, 选项) => 宿主.fetch(请求(路径, 选项), {DB});
  return {宿主, 调, sql, DB, 日志: 键 => 读日志(sql, 键), 关: () => sql.close()};
}
const 十六 = 值 => Buffer.from(值).toString('hex');
const 标头请求 = (状态, 头列) => 请求('/标头', {method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify({status: 状态, headers: 头列.map(([名, 值]) => [十六(名), 十六(值)])})});

test('响应头与帧序：状态、固定内容类型、默认无缓存，UTF-8 帧完整', async () => {
  const 环 = await 起();
  try {
    const 回 = await 环.调('/简单?n=5');
    assert.equal(回.status, 200);
    assert.equal(回.headers.get('content-type'), 'text/event-stream; charset=utf-8');
    assert.equal(回.headers.get('cache-control'), 'no-store');
    const 读 = new 流读取器(回);
    assert.deepEqual(await 读.读完(), ['data: 0 豫言', 'data: 1 豫言', 'data: 2 豫言', 'data: 3 豫言', 'data: 4 豫言']);
  } finally { 环.关(); }
});

test('响应头先于程序结束到达：提交响应头后程序继续运行', async () => {
  const 环 = await 起();
  try {
    const 回 = await 环.调('/不结束');
    assert.equal(回.status, 200);
    assert.deepEqual(环.日志('不结束'), [], '程序此时应阻塞在首次写入（无人读取）');
    const 读 = new 流读取器(回);
    assert.equal(await 读.下一帧(), 'data: 只有一帧');
    await 等到(() => 环.日志('不结束').length === 1, {说明: '程序返回'});
    await 读.取消('测试结束');
  } finally { 环.关(); }
});

test('收尾流：程序返回而未结束的流，宿主随事件结束而终止之', async () => {
  const 环 = await 起();
  try {
    const 读 = new 流读取器(await 环.调('/不结束'));
    assert.equal(await 读.下一帧(), 'data: 只有一帧');
    await 等到(() => 环.日志('不结束').length === 1, {说明: '程序返回'});
    const 再 = await 读.下一块(400);
    assert.ok(再.类型 === '终' || 再.类型 === '错', '事件结束后流应终止，实得：' + 再.类型);
    await 读.取消('测试结束');
  } finally { 环.关(); }
});

test('大量小块保序保内容', async () => {
  const 环 = await 起();
  try {
    const 回 = await 环.调('/多块?数=2000&长=10');
    const 读 = new 流读取器(回);
    const 文 = await 读.全文();
    let 期望 = '';
    for (let i = 0; i < 2000; i++) 期望 += 'abcdefghijklmnopqrstuvwxyz'[i % 26].repeat(10);
    assert.equal(文, 期望 + '完成');
    assert.ok(读.块数 >= 2000, `应保持逐块交付，实得 ${读.块数} 块`);
  } finally { 环.关(); }
});

test('64 KiB 块：恰好 65536 字节通过，65537 字节抛豫言异常，多字节按字节计', async () => {
  const 环 = await 起();
  try {
    let 读 = new 流读取器(await 环.调('/大块?字=a&长=65536'));
    let 文 = await 读.全文();
    assert.equal(文.length, 65536 + 'data: 写入=0\n\n'.length);
    assert.equal(文.slice(0, 65536), 'a'.repeat(65536));
    assert.ok(文.endsWith('data: 写入=0\n\n'));
    读 = new 流读取器(await 环.调('/大块?字=a&长=65537'));
    assert.deepEqual(await 读.读完(), ['data: 异常=事件流单次写入超过 64 KiB']);
    读 = new 流读取器(await 环.调('/大块?字=多&长=21845'));
    文 = await 读.全文();
    assert.equal(Buffer.byteLength(文), 65535 + Buffer.byteLength('data: 写入=0\n\n'));
    assert.ok(文.startsWith('豫'.repeat(21845)));
    读 = new 流读取器(await 环.调('/大块?字=多&长=21846'));
    assert.deepEqual(await 读.读完(), ['data: 异常=事件流单次写入超过 64 KiB']);
  } finally { 环.关(); }
});

test('多个 64 KiB 块的总量与内容完整', async () => {
  const 环 = await 起();
  try {
    const 读 = new 流读取器(await 环.调('/多块?数=120&长=65536'));
    const 文 = await 读.全文(60000);
    assert.equal(文.length, 120 * 65536 + '完成'.length);
    for (const 序 of [0, 1, 25, 26, 77, 119]) {
      const 字 = 'abcdefghijklmnopqrstuvwxyz'[序 % 26];
      assert.equal(文.slice(序 * 65536, 序 * 65536 + 3), 字.repeat(3));
      assert.equal(文[序 * 65536 + 65535], 字);
    }
    assert.ok(文.endsWith('完成'));
  } finally { 环.关(); }
});

test('背压：读者不读，写入不越过读者；读者读则写入随之前进', async () => {
  const 环 = await 起();
  try {
    const 回 = await 环.调('/背压?数=20&长=1000');
    const 已写 = () => 环.日志('写完').length;
    await 睡(150);
    assert.ok(已写() <= 1, `无人读取时不应写出多帧，实得 ${已写()}`);
    assert.deepEqual(环.日志('背压结束'), []);
    const 读 = new 流读取器(回);
    for (let 已读 = 1; 已读 <= 10; 已读++) {
      const 块 = await 读.下一块();
      assert.equal(块.类型, '块');
      await 睡(30);
      assert.ok(已写() <= 已读 + 1, `读 ${已读} 块后写入不应超过 ${已读 + 1}，实得 ${已写()}`);
    }
    await 读.全文();
    assert.equal(已写(), 20);
    assert.deepEqual(环.日志('背压结束'), ['完成']);
  } finally { 环.关(); }
});

async function 校验断开收尾(环) {
  await 等到(() => 环.日志('断开收尾').length === 1, {说明: '断开后收尾'});
  assert.deepEqual(环.日志('循环返回'), ['1']);
  assert.deepEqual(环.日志('已断开'), ['真']);
  assert.deepEqual(环.日志('结算'), ['完成']);
  assert.deepEqual(环.日志('结束后写'), ['2']);
  assert.deepEqual(环.日志('结束后已断开'), ['真'], '断开一旦观察到，结束后仍为真');
}

test('客户端断开（读端取消）：写入返回 1，应用继续完成收尾', async () => {
  const 环 = await 起();
  try {
    const 读 = new 流读取器(await 环.调('/断开'));
    assert.equal(await 读.下一帧(), 'data: 0');
    assert.equal(await 读.下一帧(), 'data: 1');
    await 读.取消('客户端走了');
    await 校验断开收尾(环);
  } finally { 环.关(); }
});

test('客户端断开（请求信号中止，写入正阻塞在背压上）：写入返回 1，应用继续收尾', async () => {
  const 环 = await 起();
  try {
    const 控制 = new AbortController();
    const 回 = await 环.调('/断开', {signal: 控制.signal});
    const 读 = new 流读取器(回);
    assert.equal(await 读.下一帧(), 'data: 0');
    await 睡(100);
    assert.deepEqual(环.日志('循环返回'), [], '此时应用阻塞在第二次写入');
    控制.abort();
    await 校验断开收尾(环);
  } finally { 环.关(); }
});

test('客户端断开（请求信号中止，读者仍在读）：下一次写入返回 1', async () => {
  const 环 = await 起();
  try {
    const 控制 = new AbortController();
    const 读 = new 流读取器(await 环.调('/断开', {signal: 控制.signal}));
    let 停 = false;
    const 后台 = (async () => { while (!停) { const 块 = await 读.下一块(200); if (块.类型 === '终' || 块.类型 === '错') break; } })();
    await 睡(60);
    控制.abort();
    await 校验断开收尾(环);
    停 = true;
    await 后台;
  } finally { 环.关(); }
});

test('空闲连接的断开也能被发现：只轮询“已断开”，读端取消或请求信号中止后都为真', async () => {
  for (const 方式 of ['读端取消', '请求信号中止']) {
    const 环 = await 起();
    try {
      const 控制 = new AbortController();
      const 读 = new 流读取器(await 环.调('/仅查断开', {signal: 控制.signal}));
      assert.equal(await 读.下一帧(), 'data: 开始');
      await 睡(120);
      assert.deepEqual(环.日志('轮询次数'), [], '应用在轮询，尚未见断开：' + 方式);
      if (方式 === '读端取消') await 读.取消('走了'); else 控制.abort();
      await 等到(() => 环.日志('仅查收尾').length === 1, {说明: '发现断开：' + 方式});
      assert.ok(Number(环.日志('轮询次数')[0]) >= 1 && Number(环.日志('轮询次数')[0]) < 100);
      assert.deepEqual(环.日志('轮询已断开'), ['真']);
    } finally { 环.关(); }
  }
});

test('中止：读者见到网络错误而非正常结束；中止后写入返回 2，已断开为假，重复中止与结束幂等', async () => {
  const 环 = await 起();
  try {
    const 读 = new 流读取器(await 环.调('/中止'));
    assert.equal(await 读.下一帧(), 'data: 甲');
    assert.equal(await 读.下一帧(), 'data: 乙');
    const 块 = await 读.下一块();
    assert.equal(块.类型, '错');
    assert.equal(块.错 instanceof Error ? 块.错.message : 块.错, '上游失败');
    await 等到(() => 环.日志('中止收尾').length === 1, {说明: '中止收尾'});
    assert.deepEqual(环.日志('中止后写'), ['2']);
    assert.deepEqual(环.日志('中止后已断开'), ['假']);
  } finally { 环.关(); }
});

test('结束：流正常终止；结束后写入返回 2，已断开为假，重复结束与中止幂等', async () => {
  const 环 = await 起();
  try {
    const 读 = new 流读取器(await 环.调('/结束后'));
    assert.deepEqual(await 读.读完(), ['data: 甲']);
    await 等到(() => 环.日志('结束后收尾').length === 1, {说明: '结束后收尾'});
    assert.deepEqual(环.日志('结束后写'), ['2']);
    assert.deepEqual(环.日志('结束后已断开'), ['假']);
  } finally { 环.关(); }
});

test('无效流柄与重复开始抛可捕获的豫言异常', async () => {
  const 环 = await 起();
  try {
    assert.match(await (await 环.调('/坏柄')).text(), /事件流柄无效/);
    const 读 = new 流读取器(await 环.调('/双开始'));
    assert.deepEqual(await 读.读完(), ['data: 本次事件已经开始过事件流回应']);
  } finally { 环.关(); }
});

test('流柄只在创建它的事件内有效：另一事件（含同号句柄）用之得豫言异常', async () => {
  const 环 = await 起();
  try {
    const 甲 = new 流读取器(await 环.调('/给柄'));
    const 柄 = (await 甲.下一帧()).replace(/^data: /, '');
    assert.match(柄, /^[0-9a-f-]{13}:\d+$/, '柄是不透明字符串：实例标记与句柄号');
    const 乙 = new 流读取器(await 环.调('/用柄?柄=' + encodeURIComponent(柄)));
    assert.deepEqual(await 乙.读完(), ['data: 事件流柄无效：不是本次事件创建的事件流']);
    assert.deepEqual(await 甲.读完(), []);
  } finally { 环.关(); }
});

test('非 HTTP 事件开始事件流回应抛可捕获的豫言异常', async () => {
  const 环 = await 起();
  try {
    await 环.宿主.scheduled({cron: '* * * * *', scheduledTime: Date.now()}, {DB: 环.DB}, {waitUntil() {}});
    assert.deepEqual(环.日志('定时事件'), ['事件流只能回应 HTTP 事件（fetch、durable-fetch、service-fetch）']);
  } finally { 环.关(); }
});

test('多个事件流并发互不串扰', async () => {
  const 环 = await 起();
  try {
    const 数 = 8;
    const 回们 = await Promise.all(Array.from({length: 数}, () => 环.调('/简单?n=100')));
    const 结果 = await Promise.all(回们.map(回 => new 流读取器(回).读完()));
    for (const 帧们 of 结果) {
      assert.equal(帧们.length, 100);
      帧们.forEach((帧, 序) => assert.equal(帧, `data: ${序} 豫言`));
    }
  } finally { 环.关(); }
});

test('也可在持久对象事件（durable-fetch）中使用', async () => {
  const 环 = await 起();
  try {
    const 回 = await 环.宿主.durableFetch(请求('/简单?n=3'), {DB: 环.DB}, {});
    assert.deepEqual(await new 流读取器(回).读完(), ['data: 0 豫言', 'data: 1 豫言', 'data: 2 豫言']);
  } finally { 环.关(); }
});

test('也可在 service-fetch 事件中使用；慢读者只阻塞它自己的事件', async () => {
  const 环 = await 起();
  try {
    const 慢 = await 环.调('/背压?数=20&长=1000');
    await 睡(50);
    const 回 = await 环.宿主.serviceFetch(请求('/简单?n=3'), {DB: 环.DB}, {});
    assert.deepEqual(await new 流读取器(回).读完(), ['data: 0 豫言', 'data: 1 豫言', 'data: 2 豫言']);
    assert.deepEqual(await new 流读取器(await 环.调('/简单?n=2')).读完(), ['data: 0 豫言', 'data: 1 豫言'], '别的流照常完成');
    assert.deepEqual(环.日志('背压结束'), [], '慢读者那条流仍在等读者');
    await new 流读取器(慢).全文();
    await 等到(() => 环.日志('背压结束').length === 1, {说明: '慢流读完后收尾'});
  } finally { 环.关(); }
});

// ——标头校验——
const 接受 = async (环, 状态, 头列) => {
  const 回 = await 环.宿主.fetch(标头请求(状态, 头列), {DB: 环.DB});
  assert.equal(回.status, 状态, '应接受：' + JSON.stringify(头列).slice(0, 80));
  assert.deepEqual(await new 流读取器(回).读完(), ['data: 标头已接受']);
  return 回;
};
const 拒绝 = async (环, 状态, 头列, 模式) => {
  const 回 = await 环.宿主.fetch(标头请求(状态, 头列), {DB: 环.DB});
  const 文 = await 回.text();
  assert.equal(回.status, 400, '应拒绝：' + JSON.stringify(头列).slice(0, 80) + ' 得 ' + 回.status + ' ' + 文);
  assert.match(文, 模式);
};

test('附加标头：可用标头、覆盖 Cache-Control、边界长度与数量', async () => {
  const 环 = await 起();
  try {
    let 回 = await 接受(环, 200, [['X-Trace', 'abc-123'], ['X-Accel-Buffering', 'no']]);
    assert.equal(回.headers.get('x-trace'), 'abc-123');
    assert.equal(回.headers.get('x-accel-buffering'), 'no');
    assert.equal(回.headers.get('cache-control'), 'no-store');
    assert.equal(回.headers.get('content-type'), 'text/event-stream; charset=utf-8');
    回 = await 接受(环, 200, [['cache-control', 'no-cache']]);
    assert.equal(回.headers.get('cache-control'), 'no-cache', '应用可覆盖默认的 Cache-Control');
    回 = await 接受(环, 200, [['X-Q', 'a"b\\c']]);
    assert.equal(回.headers.get('x-q'), 'a"b\\c', '引号与反斜杠在 JSON 转义后原样到达线路');
    回 = await 接受(环, 201, [['X-Tab', 'a\tb c']]);
    assert.equal(回.headers.get('x-tab'), 'a\tb c');
    await 接受(环, 299, []);
    await 接受(环, 200, [['x'.repeat(64), 'v']]);
    await 接受(环, 200, [['X-Long', 'v'.repeat(8192)]]);
    await 接受(环, 200, Array.from({length: 32}, (_, 序) => ['X-H' + 序, String(序)]));
    await 接受(环, 200, [["!#$%&'*+-.^_`|~AZaz09", 'ok']]);
  } finally { 环.关(); }
});

test('附加标头：状态越界、204/205、禁用名、非 token 名、非法值、过多与重复', async () => {
  const 环 = await 起();
  try {
    for (const 状态 of [199, 100, 300, 404, 500, 0, -1]) await 拒绝(环, 状态, [], /事件流响应状态不在 200 至 299/);
    for (const 状态 of [204, 205]) await 拒绝(环, 状态, [], /不得为 204 或 205/);
    for (const 名 of ['Content-Length', 'Transfer-Encoding', 'Connection', 'Keep-Alive', 'Upgrade', 'TE', 'Trailer', 'Content-Type', 'Content-Encoding', 'content-LENGTH', 'CONTENT-TYPE', 'Set-Cookie', 'set-cookie2'])
      await 拒绝(环, 200, [[名, '1']], /事件流标头不允许应用附加/);
    for (const 名 of ['', 'x'.repeat(65), 'Bad Name', 'Bad:Name', 'Bad(Name)', 'Bad/Name', 'Bad,Name', 'Bad"Name', 'Bad@Name', 'Bad[Name]', '头', 'a\u0000b', 'a\nb'])
      await 拒绝(环, 200, [[名, '1']], /事件流标头名无效/);
    for (const 值 of ['a\rb', 'a\nb', 'a\u0000b', 'a\u007fb', 'a\u001bb', 'a\u0080b', '豫', 'v'.repeat(8193)])
      await 拒绝(环, 200, [['X-A', 值]], /事件流标头值无效/);
    await 拒绝(环, 200, Array.from({length: 33}, (_, 序) => ['X-H' + 序, '1']), /附加标头超过 32 项/);
    await 拒绝(环, 200, [['X-A', '1'], ['x-a', '2']], /事件流标头重复/);
  } finally { 环.关(); }
});

test('收尾流：事件超过墙钟时限时，宿主中止未结束的响应流而不让它悬挂', async () => {
  const 环 = await 起({执行配置: {事件时限毫秒: {'durable-fetch': 300}}});
  try {
    const 回 = await 环.宿主.durableFetch(请求('/断开'), {DB: 环.DB}, {});
    const 读 = new 流读取器(回);
    let 结果 = '仍在流出';
    for (;;) { const 块 = await 读.下一块(1500); if (块.类型 !== '块') { 结果 = 块.类型; break; } }
    assert.ok(结果 === '错' || 结果 === '终', '时限之后流应被终止，实得：' + 结果);
  } finally { 环.关(); }
});
