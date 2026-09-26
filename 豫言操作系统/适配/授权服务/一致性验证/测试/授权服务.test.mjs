import {test, after} from 'node:test';
import assert from 'node:assert/strict';
import {编, 解, 睡, 造流, 切块, 拼接, 服务桩, 跑, 造宿主} from './桩.mjs';

// 保活：让期限计时器（AbortSignal.timeout 的定时器不占事件循环）在等待期间不致令进程提前退出。
const 保活 = setInterval(() => {}, 1000);
after(() => clearInterval(保活));
const 块们于 = 文 => [...文.matchAll(/〔([^〕]*)〕/g)].map(m => m[1]);
const 终于 = 文 => { const m = 文.match(/‖状态(\d+):([^‖]*)$/s); return m ? [Number(m[1]), m[2]] : null; };
const 可见 = 文 => 文.replaceAll('\r', '<CR>').replaceAll('\n', '<LF>').replaceAll('\t', '<TAB>');
const sse帧 = ['你', '好', '，', '世界'].map(字 => 'data: ' + JSON.stringify({choices: [{delta: {content: 字}}]}) + '\n\n');
const sse全文 = sse帧.join('') + 'data: [DONE]\n\n';

const 行为 = (路径, 请求, 桩, 记录) => {
  const 观察 = (桩.观察[路径] = {});
  switch (路径) {
    case '/sse5': return new Response(造流(切块(编.encode(sse全文), 7), {观察}), {status: 200, headers: {'content-type': 'text/event-stream', 'x-yuyan-model': 'm1'}});
    case '/sse5crlf': return new Response(造流(切块(编.encode(sse全文.replaceAll('\n', '\r\n')), 5), {观察}), {status: 200, headers: {'content-type': 'text/event-stream'}});
    case '/gated': {
      const 块们 = [sse帧[0], sse帧[1], sse帧[2], sse帧[3] + 'data: [DONE]\n\n'];
      let i = 0;
      return new Response(new ReadableStream({
        async pull(c) { if (i > 0) await 桩.等滴答到(i - 1); if (i < 块们.length) { c.enqueue(编.encode(块们[i++])); } else c.close(); },
        cancel() { 观察.已取消 = true; }
      }, {highWaterMark: 0}), {status: 200});
    }
    case '/utf8': {
      const 字节 = Uint8Array.of(0xEF, 0xBB, 0xBF, 0xE7, 0x94, 0xB2, 0xF0, 0x9F, 0x98, 0x80, 0xFF, 0x80, 0x78, 0xE8, 0xB1);
      桩.utf8字节 = 字节;
      return new Response(造流([字节.slice(0, 2), 字节.slice(2, 4), 字节.slice(4, 7), 字节.slice(7, 8), 字节.slice(8, 10), 字节.slice(10, 11), 字节.slice(11, 13), 字节.slice(13, 14), 字节.slice(14)], {观察}), {status: 200});
    }
    case '/big': { const 文 = '豫言云'.repeat(40) + 'ab'; return new Response(造流([编.encode(文)], {观察}), {status: 200}); }
    case '/slow': return new Response(造流(['甲'], {末尾: 'hang', 观察}), {status: 200});
    case '/hang': return new Response(造流([], {末尾: 'hang', 观察}), {status: 200});
    case '/errmid': return new Response(造流(['丙'], {末尾: 'error', 观察}), {status: 200});
    case '/late': return 睡(700).then(() => new Response('迟到', {status: 200}));
    case '/late2': return new Promise((_, 拒) => { 请求.signal.addEventListener('abort', () => 拒(请求.signal.reason)); });
    case '/nobody': return new Response(null, {status: 204});
    case '/status': return new Response(造流(['{"error":"忙"}'], {观察}), {status: 502, headers: {'content-type': 'application/x-ndjson', 'x-yuyan-model': 'm2', 'set-cookie': 'a=1'}});
    case '/redirect': return new Response('moved', {status: 302, headers: {location: '/elsewhere'}});
    case '/json': { const 文 = JSON.stringify({答: '豫言'.repeat(50)}); 桩.json文 = 文; return new Response(造流([文.slice(0, 20), 文.slice(20)], {观察}), {status: 200, headers: {'content-length': String(编.encode(文).length)}}); }
    case '/cl': return new Response(造流(['0123456789'], {观察}), {status: 200, headers: {'content-length': '5000'}});
    case '/nolen': return new Response(造流(Array.from({length: 20}, (_, i) => String.fromCharCode(97 + i).repeat(500)), {观察}), {status: 200});
    case '/exact': return new Response(造流(['a'.repeat(400), 'b'.repeat(600)], {观察}), {status: 200});
    case '/over1': return new Response(造流(['a'.repeat(400), 'b'.repeat(601)], {观察}), {status: 200});
    case '/rest': return new Response(造流(['甲乙丙', '丁戊己'], {观察}), {status: 200});
    default: return new Response('未知路径', {status: 404});
  }
};
const 造 = () => new 服务桩(行为);
const 运行 = (述, 桩, 宿主) => 跑({op: 'svc', timeout: 5000, ...述}, {SVC: 桩}, 宿主);

test('增量读取：5 帧 SSE 边读边累计字节（7 字节切块，跨块拆开汉字与帧）', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/sse5', read: 'sse'}, s);
  assert.equal(果.状态, 200);
  assert.match(果.文, /^状态码=200‖头=text\/event-stream‖文=你好，世界‖完成‖读次=\d+‖字节=\d+$/);
  assert.equal(Number(果.文.match(/字节=(\d+)/)[1]), 编.encode(sse全文).length);
  assert.ok(Number(果.文.match(/读次=(\d+)/)[1]) >= 5);
  assert.deepEqual(s.请求们.map(r => [r.方法, r.路径, r.有信号, r.转址]), [['GET', '/sse5', true, 'manual']]);
});

test('增量顺序：CRLF 分帧按序逐块出，末了状态 1（文字为空）', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/sse5crlf', read: 'chunks', max: 65536}, s);
  assert.equal(果.状态, 200);
  assert.deepEqual(终于(果.文), [1, '']);
  assert.equal(块们于(果.文).join(''), 可见(sse全文.replaceAll('\n', '\r\n')));
  assert.ok(块们于(果.文).length >= 5, '至少分成多块，说明不是整体读完');
});

test('读取是增量的：每读一块才放行下一块（缓冲整个正文会死锁）', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/gated', read: 'gated', binding: 'SVC'}, s);
  assert.equal(果.状态, 200);
  assert.deepEqual(块们于(果.文).map(块 => 块.includes('你') ? 0 : 块.includes('好') ? 1 : 块.includes('，') ? 2 : 3), [0, 1, 2, 3]);
  assert.match(果.文, /‖终:1‖块数=4$/);
  assert.deepEqual(s.滴答, [0, 1, 2, 3]);
});

test('UTF-8：BOM 去掉、汉字与表情跨块拼接、坏字节成 U+FFFD、末尾残缺序列补 U+FFFD', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/utf8', read: 'chunks', max: 65536}, s);
  assert.equal(果.状态, 200);
  const 期 = new TextDecoder().decode(s.utf8字节);
  assert.equal(期, '甲😀��x�');
  assert.equal(块们于(果.文).join(''), 期);
  assert.ok(!块们于(果.文).some(块 => 块.includes('�') && /^[�]$/.test(块) === false && 块.includes('甲�')), '不应把半个字符拆进下一块');
  assert.deepEqual(终于(果.文), [1, '']);
});

test('最大字节：单块过大时按码点边界切，每块 UTF-8 长度不超过上限且无半个字符', async () => {
  const 原文 = '豫言云'.repeat(40) + 'ab';
  for (const 最大 of [10, 7, 4, 3, 1, 100]) {
    const s = 造();
    const 果 = await 运行({url: 'https://svc/big', read: 'chunks', max: 最大}, s);
    assert.equal(果.状态, 200, `max=${最大}`);
    const 块们 = 块们于(果.文);
    assert.equal(块们.join(''), 原文, `max=${最大}`);
    const 上限 = Math.max(最大, 4);
    for (const 块 of 块们) { assert.ok(编.encode(块).length <= 上限, `max=${最大} 块过长：${块.length}`); assert.ok(!块.includes('�')); }
    assert.deepEqual(终于(果.文), [1, '']);
  }
});

test('超时：读流途中期满返回状态 3，上游收到取消（TimeoutError）', async () => {
  const s = 造();
  const t = Date.now();
  const 果 = await 运行({url: 'https://svc/slow', read: 'chunks', timeout: 300}, s);
  assert.equal(果.状态, 200);
  assert.deepEqual(块们于(果.文), ['甲']);
  assert.deepEqual(终于(果.文), [3, '超时']);
  assert.ok(Date.now() - t < 2500);
  await 睡(30);
  assert.deepEqual(s.观察['/slow'].取消, ['TimeoutError']);
});

test('超时：上游迟迟不出块，读取即返回状态 3', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/hang', read: 'chunks', timeout: 250}, s);
  assert.deepEqual(块们于(果.文), []);
  assert.deepEqual(终于(果.文), [3, '超时']);
  await 睡(30);
  assert.deepEqual(s.观察['/hang'].取消, ['TimeoutError']);
});

test('超时：等响应头期满——服务尊重信号则得阴与原因；不尊重信号则迟到之答亦按超时处理', async () => {
  const s = 造();
  const 甲 = await 运行({url: 'https://svc/late2', read: 'chunks', timeout: 200}, s);
  assert.match(甲.文, /^请求失败:授权服务请求失败或超时：TimeoutError/);
  const 乙 = await 运行({url: 'https://svc/late', read: 'chunks', timeout: 200}, s);
  assert.equal(乙.文, '请求失败:授权服务请求超时');
});

test('读取错误：流中途 controller.error 返回状态 2 与原因', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/errmid', read: 'chunks'}, s);
  assert.deepEqual(块们于(果.文), ['丙']);
  assert.deepEqual(终于(果.文), [2, 'Error: 上游炸了']);
});

test('取消：上游观察到取消；再读得状态 3（已取消），取消幂等', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/slow', read: 'cancel', timeout: 5000}, s);
  assert.equal(果.文, '状态码=200‖头=‖首块状态0:甲‖再读状态3:已取消‖又读状态3:已取消');
  await 睡(30);
  assert.deepEqual(s.观察['/slow'].取消, ['已取消']);
});

test('有限读取：正常读完并与 Content-Length 一致', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/json', read: 'limited', limit: 4096}, s);
  assert.match(果.文, /^状态码=200‖头=‖有限状态0:字节\d+:/);
  assert.equal(果.文.split('字节')[1].split(':').slice(1).join(':'), s.json文);
});

test('有限读取：Content-Length 超限则一字不读并取消（先看声明长度）', async () => {
  const s = 造();
  const 果 = await 运行({url: 'https://svc/cl', read: 'limited', limit: 1000}, s);
  assert.match(果.文, /‖有限状态1:字节0:$/);
  await 睡(30);
  assert.equal(s.观察['/cl'].拉取, 0);
  assert.deepEqual(s.观察['/cl'].取消, ['正文声明长度超过上限，已取消']);
});

test('有限读取：无声明长度时流式累计超限则取消；恰在上限不算超限，多一字节即超限', async () => {
  const s = 造();
  const 甲 = await 运行({url: 'https://svc/nolen', read: 'limited', limit: 1000}, s);
  assert.match(甲.文, /‖有限状态1:字节0:$/);
  await 睡(30);
  assert.deepEqual(s.观察['/nolen'].取消, ['正文超过上限，已取消']);
  assert.ok(s.观察['/nolen'].送出字节 <= 3000, '取消后上游不再被拉取，至多预读两三块：' + s.观察['/nolen'].送出字节);
  const 乙 = await 运行({url: 'https://svc/exact', read: 'limited', limit: 1000}, s);
  assert.match(乙.文, /‖有限状态0:字节1000:a{400}b{600}$/);
  const 丙 = await 运行({url: 'https://svc/over1', read: 'limited', limit: 1000}, s);
  assert.match(丙.文, /‖有限状态1:字节0:$/);
});

test('有限读取：读尽后再读得空文字成功；首块后接有限读取得余文', async () => {
  const s = 造();
  const 甲 = await 运行({url: 'https://svc/rest', read: 'limitedtwice', limit: 100}, s);
  assert.match(甲.文, /‖首次状态0:字节18‖再次状态0:字节0$/);
  const 乙 = await 运行({url: 'https://svc/rest', read: 'mixed', max: 65536, limit: 100}, s);
  assert.match(乙.文, /‖首块0:甲乙丙‖余文状态0:丁戊己$/);
});

test('响应头与状态：读取标头值不区分大小写、缺失为空；旧函数读取状态码；无正文直接读尽', async () => {
  const s = 造();
  const 甲 = await 运行({url: 'https://svc/status', read: 'none', header: 'X-Yuyan-Model'}, s);
  assert.equal(甲.文, '状态码=502‖头=m2');
  const 乙 = await 运行({url: 'https://svc/status', read: 'none', header: 'x-absent'}, s);
  assert.equal(乙.文, '状态码=502‖头=');
  const 丙 = await 运行({url: 'https://svc/nobody', read: 'chunks'}, s);
  assert.equal(丙.文, '状态码=204‖头=‖块=‖状态1:');
  const 丁 = await 运行({url: 'https://svc/redirect', read: 'chunks', header: 'location'}, s);
  assert.equal(丁.文, '状态码=302‖头=/elsewhere‖块=〔moved〕‖状态1:', '不跟随重定向，3xx 原样交给应用');
});

test('旧函数创建的响应柄可用于新读取函数', async () => {
  const s = 造();
  const 果 = await 跑({op: 'svcold', url: 'https://svc/sse5', read: 'chunks', max: 65536}, {SVC: s});
  assert.equal(块们于(果.文).join(''), 可见(sse全文));
  assert.deepEqual(终于(果.文), [1, '']);
});

test('请求形态：方法、固定标头、任意字符正文（含引号、反斜线、换行、制表、表情）逐字节送达；信号与 manual', async () => {
  const s = new 服务桩((路径, 请求, 桩, 记) => new Response('{}'));
  const 正文 = '甲"乙\\丙\n丁\t戊\r\n😀 末尾';
  const 果 = await 跑({op: 'svc', url: 'https://svc/echo?a=1', method: 'POST', body: 正文, headers: [['Content-Type', 'application/json'], ['accept', 'text/event-stream'], ['X-Yuyan-Request-ID', '0123abcd-0123-4abc-8def-0123456789ab']], read: 'none', timeout: 5000}, {SVC: s});
  assert.equal(果.状态, 200, 果.文);
  const 记 = s.请求们[0];
  assert.equal(记.方法, 'POST'); assert.equal(记.查询, '?a=1'); assert.equal(记.有信号, true); assert.equal(记.转址, 'manual');
  assert.deepEqual(记.头, {'content-type': 'application/json', accept: 'text/event-stream', 'x-yuyan-request-id': '0123abcd-0123-4abc-8def-0123456789ab'});
  assert.equal(解.decode(记.正文), 正文);
  assert.deepEqual([...记.正文], [...编.encode(正文)]);
});

test('大正文：恰 8 MiB 通过，多一字节被拒', async () => {
  const s = new 服务桩(() => new Response('{}'));
  const 甲 = await 跑({op: 'svc', url: 'https://svc/echo', method: 'POST', bodyPow: 23, read: 'none', timeout: 20000}, {SVC: s});
  assert.equal(甲.状态, 200, 甲.文.slice(0, 200));
  assert.equal(s.请求们[0].正文.length, 8388608);
  const 乙 = await 跑({op: 'svc', url: 'https://svc/echo', method: 'POST', bodyPow: 23, bodyExtra: 'b', read: 'none'}, {SVC: s});
  assert.equal(乙.状态, 400); assert.match(乙.文, /授权服务请求正文超过 8 MiB/);
  assert.equal(s.请求们.length, 1, '超限不得发出请求');
});

test('参数违规抛事故且不发请求', async () => {
  const s = 造();
  const 试 = async (述, 期) => { const 果 = await 运行({url: 'https://svc/echo', read: 'none', ...述}, s); assert.equal(果.状态, 400, JSON.stringify(述)); assert.match(果.文, 期, JSON.stringify(述)); };
  await 试({method: 'PUT'}, /授权服务方法无效/);
  await 试({method: 'get'}, /授权服务方法无效/);
  await 试({timeout: 0}, /超时毫秒须在 1 至 900000/);
  await 试({timeout: 900001}, /超时毫秒须在 1 至 900000/);
  await 试({method: 'GET', body: 'x'}, /GET 不可带正文/);
  await 试({binding: ''}, /授权服务绑定名为空/);
  await 试({headers: [['Authorization', 'Bearer x']]}, /授权服务固定标头不允许/);
  await 试({headers: [['Origin', 'https://x.test']]}, /授权服务固定标头不允许/);
  await 试({headers: [['Cookie', 'a=b']]}, /授权服务固定标头不允许/);
  await 试({headers: [['X-Other', 'v']]}, /授权服务固定标头不允许/);
  await 试({headers: [['X-Yuyan-', 'v']]}, /授权服务固定标头不允许/);
  await 试({headers: [['Accept', 'a'], ['accept', 'b']]}, /授权服务标头重复/);
  await 试({headers: [['Bad Name', 'v']]}, /授权服务固定标头名无效/);
  await 试({headers: [['', 'v']]}, /授权服务固定标头名无效/);
  await 试({headers: [['Accept', ' 前导空格']]}, /授权服务标头值无效/);
  await 试({headers: [['Accept', '尾随空格 ']]}, /授权服务标头值无效/);
  await 试({headers: [['Accept', '含换行\nx']]}, /授权服务标头值无效/);
  await 试({headers: [['Accept', '含制表\tx']]}, /授权服务标头值无效/);
  await 试({headers: [['Accept', '非ASCII汉字']]}, /授权服务标头值无效/);
  await 试({headers: [['Accept', 'a'.repeat(513)]]}, /授权服务标头值无效/);
  await 试({headers: Array.from({length: 17}, (_, i) => ['X-Yuyan-' + i, 'v'])}, /授权服务标头过多/);
  await 试({url: 'http://svc/x'}, /授权服务网址无效/);
  await 试({url: 'https://user:pw@svc/x'}, /授权服务网址无效/);
  await 试({url: 'https://svc/x y'}, /含控制字符、空白或反斜线/);
  await 试({url: 'https://svc\\@evil/x'}, /含控制字符、空白或反斜线/);
  await 试({url: 'svc/x'}, /授权服务网址无效/);
  await 试({url: ''}, /网址长度须在 1 至 2048/);
  await 试({url: 'https://svc/' + 'a'.repeat(2050)}, /网址长度须在 1 至 2048/);
  assert.equal(s.请求们.length, 0, '违规一律不得发出请求');
});

test('未授权或不存在的服务绑定：宿主报部署错误', async () => {
  const s = 造();
  await assert.rejects(运行({url: 'https://svc/echo', binding: 'OTHER', read: 'none'}, s), /未授权的SERVICE绑定：OTHER/);
});

test('无效响应柄：新读取函数一律抛事故，不接受伪造或他处的柄', async () => {
  const s = 造();
  const 果 = await 跑({op: 'badhandle', handle: '999999'}, {SVC: s});
  assert.equal(果.状态, 200);
  const 段们 = 果.文.split('‖');
  assert.match(段们[0], /授权服务响应柄无效/); assert.match(段们[1], /授权服务响应柄无效/);
  assert.match(段们[2], /网页上游响应柄无效/); assert.match(段们[3], /命名持久服务响应柄无效/);
  const 果二 = await 跑({op: 'badhandle', handle: '1'}, {SVC: s});
  assert.match(果二.文, /授权服务响应柄无效/, '别处的句柄号（如入站请求）同样被拒');
});

test('旧的原流直通仍可用：请求授权服务转正文并回应入站以原答', async () => {
  const s = new 服务桩(async (路径, 请求) => new Response('回声:' + 解.decode(new Uint8Array(await 请求.arrayBuffer())), {status: 201, headers: {'x-yuyan-model': 'p'}}));
  const 回 = await 造宿主().fetch(new Request('https://x.test/svcpass', {method: 'POST', headers: {'content-type': 'text/plain'}, body: '直通的正文'}), {SVC: s});
  assert.equal(回.status, 201); assert.equal(回.headers.get('x-yuyan-model'), 'p');
  assert.equal(await 回.text(), '回声:直通的正文');
});

test('边角：无正文响应的有限读取得空文字成功；POST 空正文照发；Content-Length 非数字不影响读取', async () => {
  const s = new 服务桩((路径, 请求) => 路径 === '/badcl' ? new Response(造流(['abc'], {观察: {}}), {status: 200, headers: {'content-length': '3'}}) : new Response(路径 === '/nobody' ? null : '{}', {status: 路径 === '/nobody' ? 204 : 200}));
  const 甲 = await 运行({url: 'https://svc/nobody', read: 'limited', limit: 10}, s);
  assert.equal(甲.文, '状态码=204‖头=‖有限状态0:字节0:');
  const 乙 = await 运行({url: 'https://svc/echo', method: 'POST', body: '', read: 'none'}, s);
  assert.equal(乙.状态, 200, 乙.文);
  assert.equal(s.请求们.at(-1).方法, 'POST'); assert.equal(s.请求们.at(-1).正文.length, 0);
  const 丙 = await 运行({url: 'https://svc/badcl', read: 'limited', limit: 10}, s);
  assert.equal(丙.文, '状态码=200‖头=‖有限状态0:字节3:abc');
});
