// 文言：以真 Wasm 与模拟队列、模拟批次验消息队列适配。汉语：加载已构建的“消息队列一致性”产物；生产者用类实例模拟 Queue.send/sendBatch，消费者用类实例模拟 MessageBatch 与 Message，逐条记录 ack/retry。
// 用法见同目录说明：在私有暂存根目录执行 `node --test <本文件>`，产物根目录由环境变量 YY_DIST_ROOT 指定（默认 ./dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const 产物 = pathToFileURL(path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '消息队列一致性') + '/');
const {创建云工宿主} = await import(new URL('宿主.mjs', 产物));
const 程序模块 = await WebAssembly.compile(await readFile(new URL('程序.wasm', 产物)));
const 值桥模块 = await WebAssembly.compile(await readFile(new URL('值桥.wasm', 产物)));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {QUEUE: ['PAGE_QUEUE', 'OTHER_QUEUE']}});

// 文言：平台外壳于启动之前核对应用之要求与宿主之所供；此测同其所核。汉语：与生成的 入口.mjs 启动时相同，用 接口核对.mjs 核对应用要求与宿主支持清单。
const {核对接口装载} = await import(new URL('接口核对.mjs', 产物));
const 应用要求 = JSON.parse(await readFile(new URL('接口要求组.json', 产物), 'utf8'));
const 宿主提供 = JSON.parse(await readFile(new URL('宿主提供组.json', 产物), 'utf8'));
test('装载前的接口核对通过，且应用要求含本接口', () => {
  核对接口装载({程序模块, 应用要求, 宿主提供, 宿主: '云工'});
  assert.ok(应用要求.some(项 => 项.接口名称 === '豫言操作系统消息队列' && 项.接口版本 === '0.1.0'), '应用要求里应有本接口 0.1.0');
  assert.ok(宿主提供.some(项 => 项.接口名称 === '豫言操作系统消息队列' && 项.接口版本 === '0.1.0'), '宿主支持清单里应有本接口 0.1.0');
});

// ───────────────────────────── 生产者 ─────────────────────────────
const 无失败 = Symbol('无失败');
class 模拟队列 {
  constructor() { this.单发 = []; this.批发 = []; this.失败 = 无失败; }
  async send(体, 选项) { if (this.失败 !== 无失败) throw this.失败; this.单发.push({体, 选项}); }
  async sendBatch(诸信, 选项) { if (this.失败 !== 无失败) throw this.失败; this.批发.push({诸信, 选项}); }
}
const 投 = async (径, 正文, {队列 = new 模拟队列(), 绑定 = 'PAGE_QUEUE', 延迟 = 0, 环境} = {}) => {
  const 查询 = new URLSearchParams({b: 绑定, d: String(延迟)});
  const 回 = await 宿主.fetch(new Request(`https://x.test${径}?${查询}`, {method: 'POST', body: Buffer.from(正文, 'utf8')}), 环境 ?? {PAGE_QUEUE: 队列, OTHER_QUEUE: new 模拟队列()});
  return {状态: 回.status, 文: new TextDecoder('utf-8', {ignoreBOM: true}).decode(await 回.arrayBuffer()), 队列};
};
const 单 = (正文, 选项) => 投('/send', 正文, 选项);
const 批 = (正文, 选项) => 投('/batch', 正文, 选项);
const 应单败 = async (正文, 消息, 选项) => {
  const r = await 单(正文, 选项);
  assert.equal(r.状态, 400, r.文);
  assert.equal(r.文, '失败：' + 消息, JSON.stringify(正文).slice(0, 100));
  assert.equal(r.队列.单发.length + r.队列.批发.length, 0, '失败时不得投递');
};
const 应批败 = async (正文, 消息, 选项) => {
  const r = await 批(正文, 选项);
  assert.equal(r.状态, 400, r.文);
  assert.equal(r.文, '失败：' + 消息, JSON.stringify(正文).slice(0, 100));
  assert.equal(r.队列.单发.length + r.队列.批发.length, 0, '失败时不得投递任何消息');
};

test('单条投递：与原型一致的页面任务，选项固定 contentType=json，无延迟时不带 delaySeconds', async () => {
  const 任务 = {kind: 'page', url: '/release/0123456789abcdef0123456789abcdef?lang=han', version: 'persistent-reader-8', refresh: true};
  const r = await 单(JSON.stringify(任务));
  assert.equal(r.状态, 200);
  assert.equal(r.文, '成功');
  assert.deepEqual(r.队列.单发, [{体: 任务, 选项: {contentType: 'json'}}]);
  assert.equal(Object.hasOwn(r.队列.单发[0].选项, 'delaySeconds'), false);
});

test('延迟秒数：0 不带，1 与 43200 通过，越界失败且不投递', async () => {
  for (const 延迟 of [1, 30, 43200]) {
    const r = await 单('{"a":1}', {延迟});
    assert.equal(r.文, '成功');
    assert.deepEqual(r.队列.单发[0].选项, {contentType: 'json', delaySeconds: 延迟});
  }
  for (const 延迟 of [-1, -43200, 43201, 1000000, Number.MAX_SAFE_INTEGER]) await 应单败('{"a":1}', '延迟秒数须在 0 至 43200 之间', {延迟});
});

test('消息体可以是任意 JSON 值，经宿主规范化后逐值到达 send', async () => {
  const 样本 = [
    ['"文字"', '文字'], ['123', 123], ['-1.5e3', -1500], ['true', true], ['false', false], ['null', null], ['[]', []], ['{}', {}],
    ['  {\n "a" : [ 1 , 2 ,\t{ "b" : "c" } ] ,\r\n "d":null }  ', {a: [1, 2, {b: 'c'}], d: null}],
    ['{"中":"文","emoji":"😀","q":"a\\"b","s":"a\\\\b","n":"a\\nb","u":"\\u0041\\u00e9\\u4e2d","c":"\\u0001\\u001f"}', {中: '文', emoji: '😀', q: 'a"b', s: 'a\\b', n: 'a\nb', u: 'Aé中', c: '\u0001\u001f'}],
    ['"\\ud83d\\ude00"', '😀'],
    ['{"a":{"b":{"c":{"d":{"e":[[[[["deep"]]]]]}}}}}', {a: {b: {c: {d: {e: [[[[['deep']]]]]}}}}}],
    ['{"__proto__":{"x":1},"constructor":"c","prototype":2}', JSON.parse('{"__proto__":{"x":1},"constructor":"c","prototype":2}')],
    ['{"a":"$句柄","b":"{\\"$句柄\\":\\"1\\"}","$":1,"$x":2,"$ 句柄":3}', {a: '$句柄', b: '{"$句柄":"1"}', $: 1, $x: 2, '$ 句柄': 3}],
    ['1e2', 100], ['1E-7', 1e-7], ['0.1', 0.1], ['-0', 0], ['12345678901234567890', 12345678901234567000],
  ];
  for (const [正文, 期] of 样本) {
    const r = await 单(正文);
    assert.equal(r.文, '成功', 正文);
    assert.deepEqual(r.队列.单发[0].体, 期, 正文);
    assert.equal(Object.is(r.队列.单发[0].体, -0), false);
  }
});

test('宿主记号不得出现在消息体：$句柄、$未定义、$大整数、$数字，含转义拼写与无穷大', async () => {
  const 坏 = [
    '{"$句柄":"1"}', '{"$句柄":"12"}', '[{"$句柄":"1"}]', '{"a":{"$句柄":"1"}}', '{ "$句柄" : "1" }', '{"\\u0024句柄":"1"}', '{"\\u0024\\u53e5\\u67c4":"1"}', '{"$\\u53e5\\u67c4":"1"}',
    '{"$未定义":true}', '{"$大整数":"12345678901234567890"}', '{"$数字":"NaN"}', '{"$数字":"Infinity"}', '1e999', '-1e999', '[1e999]', '{"a":1e999}',
    '{"$句柄":"1","x":2}',
  ];
  for (const 正文 of 坏) await 应单败(正文, '消息体含有不属于 JSON 的值或宿主专用记号');
  for (const 正文 of 坏.filter(项 => 项.startsWith('{') || 项.startsWith('['))) {
    await 应批败(`[{"body":${正文}}]`, '批量消息含有不属于 JSON 的值或宿主专用记号');
  }
});

test('不是 JSON 的消息体被拒绝', async () => {
  for (const 正文 of ['', ' ', '\n', '{', '{"a":1', '{\'a\':1}', 'undefined', 'NaN', 'Infinity', '[1,]', '{"a":1}x', '{"a":1}{"b":2}', '\u0001', '{"a":"\u0001"}', '{"a":"\n"}', 'tru', '01', '.5', '{"a":}', '\uFEFF{}', '{"a":1,}', '"a', '[1 2]']) {
    await 应单败(正文, '消息体不是有效的 JSON 文字');
  }
});

test('消息体大小：128000 字节整通过，多一字节失败；以规范化后的 UTF-8 字节计', async () => {
  const 满 = '"' + 'x'.repeat(127998) + '"';
  assert.equal(Buffer.byteLength(满), 128000);
  const r = await 单(满);
  assert.equal(r.文, '成功');
  assert.equal(r.队列.单发[0].体.length, 127998);
  await 应单败('"' + 'x'.repeat(127999) + '"', '消息体超过 128000 字节');
  const 汉满 = '"' + '豫'.repeat(42666) + '"';
  assert.equal(Buffer.byteLength(汉满), 128000);
  assert.equal((await 单(汉满)).文, '成功');
  await 应单败('"' + '豫'.repeat(42667) + '"', '消息体超过 128000 字节');
  await 应单败('"' + '豫'.repeat(42666) + 'a"', '消息体超过 128000 字节');
  // 输入文字本身也不得超过 128000 字节（含空白），即使规范化后更短
  await 应单败(' '.repeat(128001) + '1', '消息体超过 128000 字节');
  // \u 转义规范化后变短：输入 6 字节一个字符，输入超限先失败
  await 应单败('"' + '\\u4e2d'.repeat(21334) + '"', '消息体超过 128000 字节');
});

test('嵌套深度：至多 24 层，更深者失败且不投递', async () => {
  const 嵌 = 层 => '['.repeat(层) + '1' + ']'.repeat(层);
  const 嵌对象 = 层 => '{"a":'.repeat(层) + '1' + '}'.repeat(层);
  for (const 层 of [1, 12, 24]) {
    assert.equal((await 单(嵌(层))).文, '成功', String(层));
    assert.equal((await 单(嵌对象(层))).文, '成功', String(层));
    assert.equal((await 批(`[{"body":${嵌(层)}}]`)).文, '成功', String(层));
  }
  for (const 层 of [25, 26, 32, 33, 40, 200]) {
    await 应单败(嵌(层), '消息体嵌套超过 24 层');
    await 应单败(嵌对象(层), '消息体嵌套超过 24 层');
  }
  for (const 层 of [25, 26, 31, 33, 40, 200]) await 应批败(`[{"body":${嵌(层)}}]`, '批量消息嵌套超过 24 层');
  // 字符串里的括号不算层数
  assert.equal((await 单('"' + '['.repeat(100) + '{"' + '"'.length + '"}"')).状态, 400);
  assert.equal((await 单(JSON.stringify('['.repeat(100) + '{'.repeat(100)))).文, '成功');
});

test('平台拒绝：异常消息保留平台原文，含引号、反斜线、换行与中文', async () => {
  const 消息们 = ['Queue is full', '队列已满："a"\\b\nc\t中文 😀', 'x'.repeat(5000), ''];
  for (const 消息 of 消息们) {
    const 队列 = new 模拟队列();
    队列.失败 = new Error(消息);
    const r = await 单('{"a":1}', {队列});
    assert.equal(r.状态, 400);
    assert.equal(r.文, '失败：队列发送失败：' + 消息);
  }
  for (const 失败 of ['字符串拒绝', null, undefined, {message: '对象消息'}, Object.assign(new TypeError('类型误'), {name: 'TypeError'})]) {
    const 队列 = new 模拟队列();
    队列.失败 = 失败;
    const r = await 单('[1]', {队列});
    assert.equal(r.状态, 400, String(失败));
    assert.ok(r.文.startsWith('失败：队列发送失败：'), r.文);
  }
});

test('绑定名：空名由适配拒绝；未授权与未配置是部署错误，宿主中止请求', async () => {
  await 应单败('{"a":1}', '队列绑定名不得为空', {绑定: ''});
  await 应批败('[{"body":1}]', '队列绑定名不得为空', {绑定: ''});
  await assert.rejects(单('{"a":1}', {绑定: 'NOT_LISTED'}), /未授权的QUEUE绑定：NOT_LISTED/);
  await assert.rejects(批('[{"body":1}]', {绑定: 'NOT_LISTED'}), /未授权的QUEUE绑定：NOT_LISTED/);
  await assert.rejects(单('{"a":1}', {环境: {}}), /绑定不存在：PAGE_QUEUE/);
  const 他队列 = new 模拟队列(), 主队列 = new 模拟队列();
  await 单('{"to":"other"}', {绑定: 'OTHER_QUEUE', 环境: {PAGE_QUEUE: 主队列, OTHER_QUEUE: 他队列}});
  assert.equal(他队列.单发.length, 1);
  assert.equal(主队列.单发.length, 0);
});

test('批量投递：形状、延迟、顺序与选项', async () => {
  const 诸体 = [{kind: 'page', url: '/a?x=1&y=2', version: 'v', refresh: false}, [1, 'a,b]', {x: '}'}], '中文😀', null, 7, true, {}, []];
  const 项们 = 诸体.map((体, 序) => 序 % 2 ? {body: 体, delaySeconds: 序 * 10} : {body: 体});
  const r = await 批(JSON.stringify(项们));
  assert.equal(r.文, '成功');
  assert.equal(r.队列.批发.length, 1);
  assert.equal(r.队列.批发[0].选项, undefined);
  assert.deepEqual(r.队列.批发[0].诸信, 诸体.map((体, 序) => 序 % 2 ? {body: 体, contentType: 'json', delaySeconds: 序 * 10} : {body: 体, contentType: 'json'}));
  // delaySeconds 在 body 之前，或为 0，或写成 1e1 之类的整数
  const r2 = await 批('[{"delaySeconds":5,"body":"甲"},{"delaySeconds":0,"body":"乙"},{"body":"丙","delaySeconds":1e1},{"delaySeconds":43200,"body":{"delaySeconds":9}}]');
  assert.deepEqual(r2.队列.批发[0].诸信, [
    {body: '甲', contentType: 'json', delaySeconds: 5}, {body: '乙', contentType: 'json'}, {body: '丙', contentType: 'json', delaySeconds: 10}, {body: {delaySeconds: 9}, contentType: 'json', delaySeconds: 43200},
  ]);
});

test('批量投递的拆分：body 内的括号、逗号、引号、转义与嵌套不会被误拆', async () => {
  const 诸体 = ['],[', '{"body":1}', '[{"body":[1,2]},{"body":3}]', '\\', '\\"', '"', ',', '{', '}', '[', '}{', '","delaySeconds":5,"body":"', '"body":', '中,文]}', '{"a":"b\\"c,d]e"}', ['x', ['y', ['z', {w: '}"]'}]]]];
  const r = await 批(JSON.stringify(诸体.map(body => ({body}))));
  assert.equal(r.文, '成功');
  assert.deepEqual(r.队列.批发[0].诸信.map(信 => 信.body), 诸体);
  // 随机嵌套结构
  let 种子 = 12345;
  const 随 = () => (种子 = (Math.imul(种子, 1664525) + 1013904223) >>> 0) / 4294967296;
  const 原子 = ['a', 'b"c', 'd\\e', ']', '[', '}', '{', ',', ':', '"', '中', '😀', '', 0, 1, -2.5, true, false, null];
  const 造 = 深 => {
    const 选 = 随();
    if (深 <= 0 || 选 < 0.4) return 原子[Math.floor(随() * 原子.length)];
    if (选 < 0.7) return Array.from({length: Math.floor(随() * 4)}, () => 造(深 - 1));
    return Object.fromEntries(Array.from({length: Math.floor(随() * 4)}, (_, i) => ['k' + i + 原子[Math.floor(随() * 原子.length)], 造(深 - 1)]));
  };
  for (let 轮 = 0; 轮 < 60; 轮++) {
    const 体们 = Array.from({length: 1 + Math.floor(随() * 8)}, () => 造(5));
    const 项们2 = 体们.map(body => 随() < 0.3 ? {delaySeconds: Math.floor(随() * 43201), body} : {body});
    const 结果 = await 批(JSON.stringify(项们2));
    assert.equal(结果.文, '成功', JSON.stringify(项们2).slice(0, 200));
    assert.deepEqual(结果.队列.批发[0].诸信, 项们2.map(项 => ({body: 项.body, contentType: 'json', ...(项.delaySeconds ? {delaySeconds: 项.delaySeconds} : {})})));
  }
});

test('批量投递的限额与形状：条数、各项字段、延迟、大小；全验通过才投，否则一条不投', async () => {
  const 项 = 数 => JSON.stringify(Array.from({length: 数}, (_, i) => ({body: i})));
  assert.equal((await 批(项(1))).文, '成功');
  const 百 = await 批(项(100));
  assert.equal(百.文, '成功');
  assert.equal(百.队列.批发[0].诸信.length, 100);
  await 应批败('[]', '批量消息不得为空');
  await 应批败(项(101), '批量消息不得超过 100 条');
  await 应批败('{"body":1}', '批量消息须为 JSON 数组');
  await 应批败('"x"', '批量消息须为 JSON 数组');
  await 应批败('1', '批量消息须为 JSON 数组');
  await 应批败('null', '批量消息须为 JSON 数组');
  await 应批败('[1]', '批量消息的每一项须为 JSON 对象');
  await 应批败('["x"]', '批量消息的每一项须为 JSON 对象');
  await 应批败('[null]', '批量消息的每一项须为 JSON 对象');
  await 应批败('[[]]', '批量消息的每一项须为 JSON 对象');
  await 应批败('[{}]', '批量消息项缺少 body');
  await 应批败('[{"delaySeconds":5}]', '批量消息项缺少 body');
  await 应批败('[{"body":1,"contentType":"text"}]', '批量消息项只允许 body 与 delaySeconds 两个字段');
  await 应批败('[{"body":1,"extra":true}]', '批量消息项只允许 body 与 delaySeconds 两个字段');
  await 应批败('[{"body":1},{"body":2,"x":0}]', '批量消息项只允许 body 与 delaySeconds 两个字段');
  await 应批败('[{"Body":1}]', '批量消息项缺少 body');
  for (const 坏 of ['-1', '43201', '1.5', '"5"', 'null', 'true', '[5]', '1e21', '100000', '{"a":1}']) {
    await 应批败(`[{"body":1,"delaySeconds":${坏}}]`, '批量消息项的 delaySeconds 须为 0 至 43200 的整数');
  }
  await 应批败('[{"body":1},', '批量消息不是有效的 JSON 文字');
  await 应批败('', '批量消息不是有效的 JSON 文字');
  await 应批败('[{"body":undefined}]', '批量消息不是有效的 JSON 文字');
  // 单体 128000 字节与合计 256000 字节
  const 满体 = '"' + 'x'.repeat(127998) + '"';
  assert.equal((await 批(`[{"body":${满体}},{"body":${满体}}]`)).文, '成功');
  await 应批败(`[{"body":"${'x'.repeat(127999)}"}]`, '批量消息体超过 128000 字节');
  await 应批败(`[{"body":${满体}},{"body":${满体}},{"body":1}]`, '批量消息体合计超过 256000 字节');
  await 应批败(`[{"body":"${'y'.repeat(200000)}"},{"body":"${'y'.repeat(100000)}"}]`, '批量消息体超过 128000 字节');
  const 三体 = '"' + 'z'.repeat(90000) + '"';
  await 应批败(`[{"body":${三体}},{"body":${三体}},{"body":${三体}}]`, '批量消息体合计超过 256000 字节');
  // 输入文字上限 1 MiB
  await 应批败(' '.repeat(1048577) + '[]', '批量消息文字超过 1 MiB');
});

test('批量投递失败：平台拒绝时异常带原文；不是 JSON 值的项在校验阶段拒绝', async () => {
  const 队列 = new 模拟队列();
  队列.失败 = new Error('batch too large: "x"\n中文');
  const r = await 批('[{"body":1}]', {队列});
  assert.equal(r.状态, 400);
  assert.equal(r.文, '失败：队列批量发送失败：batch too large: "x"\n中文');
});

test('并发的生产者事件互不干扰', async () => {
  const 队列们 = Array.from({length: 20}, () => new 模拟队列());
  await Promise.all(队列们.map((队列, 序) => 序 % 2 ? 单(JSON.stringify({序}), {队列}) : 批(JSON.stringify([{body: {序}}, {body: 序, delaySeconds: 序}]), {队列})));
  队列们.forEach((队列, 序) => {
    if (序 % 2) assert.deepEqual(队列.单发, [{体: {序}, 选项: {contentType: 'json'}}]);
    else assert.deepEqual(队列.批发[0].诸信, [{body: {序}, contentType: 'json'}, ...(序 ? [{body: 序, contentType: 'json', delaySeconds: 序}] : [{body: 序, contentType: 'json'}])]);
  });
});

// ───────────────────────────── 消费者 ─────────────────────────────
class 模拟消息 {
  constructor(标识, 体, 尝试 = 1, 时间 = new Date(1758801600123)) {
    this.id = 标识; this.attempts = 尝试; this.timestamp = 时间; this.调用 = [];
    if (体 !== undefined) this.body = 体;
  }
  ack() { this.调用.push(['ack']); }
  retry(选项) { this.调用.push(选项 === undefined ? ['retry'] : ['retry', 选项]); }
}
class 模拟批次 {
  constructor(队列名, 诸信) { this.queue = 队列名; this.messages = 诸信; this.批调用 = []; }
  ackAll() { this.批调用.push('ackAll'); }
  retryAll(选项) { this.批调用.push(['retryAll', 选项]); }
}
const 模拟上下文 = () => ({承诺: [], waitUntil(p) { this.承诺.push(p); }});
// 文言：暂换 console.log 以收应用所记。汉语：运行一次 queue 事件并返回期间 console.log 的输出行。
const 消费 = async 批次 => {
  const 原 = console.log, 行们 = [];
  console.log = (...参数) => { 行们.push(参数.join(' ')); };
  const 上下文 = 模拟上下文();
  try { await 宿主.queue(批次, {}, 上下文); }
  finally { console.log = 原; }
  await Promise.all(上下文.承诺);
  return 行们;
};

test('读取队列批次：queue、id、attempts、timestamp（毫秒）、body 的形状与值', async () => {
  const 诸信 = [
    new 模拟消息('m1', {kind: 'page', url: '/release/x?lang=han', version: 'v8', refresh: true}, 1, new Date(1758801600123)),
    new 模拟消息('m2', [1, 'a', null, {b: false}], 4, new Date(0)),
    new 模拟消息('m3', '文字体', 10, new Date(-86400000)),
    new 模拟消息('m4', 42, 1, 1700000000000),
    new 模拟消息('m5', null, 2, new Date(8640000000000000)),
    new 模拟消息('m6', true, 3, new Date(1)),
  ];
  const 行 = await 消费(new 模拟批次('log', 诸信));
  assert.equal(行.length, 1);
  const 批 = JSON.parse(行[0]);
  assert.deepEqual(Object.keys(批), ['queue', 'messages']);
  assert.equal(批.queue, 'log');
  assert.equal(批.messages.length, 6);
  批.messages.forEach((信, 序) => assert.deepEqual(Object.keys(信), ['id', 'attempts', 'timestamp', 'body'], String(序)));
  assert.deepEqual(批.messages.map(信 => 信.id), ['m1', 'm2', 'm3', 'm4', 'm5', 'm6']);
  assert.deepEqual(批.messages.map(信 => 信.attempts), [1, 4, 10, 1, 2, 3]);
  assert.deepEqual(批.messages.map(信 => 信.timestamp), [1758801600123, 0, -86400000, 1700000000000, 8640000000000000, 1]);
  assert.deepEqual(批.messages.map(信 => 信.body), [诸信[0].body, [1, 'a', null, {b: false}], '文字体', 42, null, true]);
  assert.ok(诸信.every(信 => 信.调用.length === 0), '读取批次不得确认或重试任何消息');
});

test('读取队列批次：空批次、队列名与消息标识里的特殊字符', async () => {
  assert.deepEqual(JSON.parse((await 消费(new 模拟批次('log', [])))[0]), {queue: 'log', messages: []});
  const 标识们 = ['', 'a"b', 'a\\b', '中文😀', 'a\nb\tc', 'a\u0001\u001f\u007f', '\u2028\u2029', '{"$句柄":"1"}', 'x'.repeat(2000)];
  const 行 = await 消费(new 模拟批次('log', 标识们.map(标识 => new 模拟消息(标识, 1))));
  assert.deepEqual(JSON.parse(行[0]).messages.map(信 => 信.id), 标识们);
  for (const 名 of ['log', 'log"q\\', 'log中文😀', 'log\nx\ty', 'log\u0001\u001f', 'log\u2028', 'log' + 'n'.repeat(500)]) {
    const 批 = JSON.parse((await 消费(new 模拟批次(名, [new 模拟消息('a', 1)])))[0]);
    assert.equal(批.queue, 名, JSON.stringify(名));
  }
});

test('读取队列批次：大消息体（约 128 KB）与一百条消息', async () => {
  const 大 = 'x'.repeat(131000);
  const 行 = await 消费(new 模拟批次('log', [new 模拟消息('big', {大})]));
  assert.equal(JSON.parse(行[0]).messages[0].body.大.length, 131000);
  const 诸信 = Array.from({length: 100}, (_, 序) => new 模拟消息('id-' + 序, {序, 文: '中文'.repeat(序)}, 序 + 1, new Date(1758801600000 + 序)));
  const 批 = JSON.parse((await 消费(new 模拟批次('log', 诸信)))[0]);
  assert.equal(批.messages.length, 100);
  批.messages.forEach((信, 序) => assert.deepEqual(信, {id: 'id-' + 序, attempts: 序 + 1, timestamp: 1758801600000 + 序, body: {序, 文: '中文'.repeat(序)}}));
});

test('读取队列批次：一千条消息也不耗尽宿主句柄', async () => {
  const 诸信 = Array.from({length: 1000}, (_, 序) => new 模拟消息('m' + 序, 序));
  const 批 = JSON.parse((await 消费(new 模拟批次('log', 诸信)))[0]);
  assert.equal(批.messages.length, 1000);
  assert.equal(批.messages[999].body, 999);
});

test('消息体不是 JSON 值时读取批次失败（Date、Map、Set、二进制、undefined、BigInt、NaN、Infinity 与其嵌套）', async () => {
  const 坏体们 = [new Date(), new Map([[1, 2]]), new Set([1]), new Uint8Array([1, 2, 3]), new ArrayBuffer(4), undefined, 10n, NaN, Infinity, -Infinity, {a: new Date()}, [1, new Map()], {a: [undefined]}, new (class 自定 { constructor() { this.x = 1; } })(), Symbol.for('s')];
  for (const 体 of 坏体们) {
    const 行 = await 消费(new 模拟批次('err', [new 模拟消息('ok', 1), new 模拟消息('bad', 体)]));
    assert.equal(行.length, 1, String(体));
    assert.equal(行[0], '捕获：队列消息正文不是 JSON 值：序号 1', String(体));
  }
});

test('消息体嵌套超过宿主桥限制（32 层）时宿主中止本次事件，32 层以内可读', async () => {
  const 嵌 = 层 => { let 值 = 1; for (let i = 0; i < 层; i++) 值 = [值]; return 值; };
  const 行 = await 消费(new 模拟批次('log', [new 模拟消息('ok', 嵌(30))]));
  assert.deepEqual(JSON.parse(行[0]).messages[0].body, 嵌(30));
  await assert.rejects(消费(new 模拟批次('log', [new 模拟消息('deep', 嵌(40))])), /宿主结果嵌套过深/);
});

test('确认与重试：逐条动作，延迟选项，序号越界与延迟越界抛出可捕获异常', async () => {
  const 造 = 数 => Array.from({length: 数}, (_, 序) => new 模拟消息('m' + 序, 序));
  let 诸信 = 造(4), 行;
  行 = await 消费(new 模拟批次('plan:ards', 诸信));
  assert.deepEqual(诸信.map(信 => 信.调用), [[['ack']], [['retry']], [['retry', {delaySeconds: 30}]], []]);
  assert.deepEqual(行, []);
  诸信 = 造(3);
  行 = await 消费(new 模拟批次('plan:yxz', 诸信));
  assert.deepEqual(诸信.map(信 => 信.调用), [[['retry', {delaySeconds: 43200}]], [], []]);
  assert.deepEqual(行, ['捕获：队列消息序号越界', '捕获：队列消息序号越界']);
  诸信 = 造(2);
  行 = await 消费(new 模拟批次('plan:nl', 诸信));
  assert.deepEqual(诸信.map(信 => 信.调用), [[], []]);
  assert.deepEqual(行, ['捕获：延迟秒数须在 0 至 43200 之间', '捕获：延迟秒数须在 0 至 43200 之间']);
});

test('消费者不使用批次级确认与重试；程序正常结束而无显式调用即视为成功', async () => {
  const 诸信 = Array.from({length: 3}, (_, 序) => new 模拟消息('m' + 序, 序));
  const 批次 = new 模拟批次('plan:sss', 诸信);
  const 行 = await 消费(批次);
  assert.deepEqual(行, []);
  assert.deepEqual(批次.批调用, []);
  assert.ok(诸信.every(信 => 信.调用.length === 0));
  const 全动作 = new 模拟批次('plan:ardyy', Array.from({length: 5}, (_, 序) => new 模拟消息('n' + 序, 序)));
  await 消费(全动作);
  assert.deepEqual(全动作.批调用, []);
});

test('程序在队列事件里抛出未捕获异常：宿主把失败交还平台，此前已发出的确认调用不被撤销', async () => {
  const 诸信 = [new 模拟消息('a', 1), new 模拟消息('b', 2), new 模拟消息('c', 3)];
  const 批次 = new 模拟批次('plan:ats', 诸信);
  const 上下文 = 模拟上下文();
  await assert.rejects(宿主.queue(批次, {}, 上下文));
  assert.deepEqual(诸信.map(信 => 信.调用), [[['ack']], [], []]);
  assert.deepEqual(批次.批调用, [], '适配不代平台整批重试，由平台按异常处理');
  assert.equal(上下文.承诺.length, 1);
  await 上下文.承诺[0];
});

test('非队列事件里调用消费函数得到可捕获的豫言异常', async () => {
  for (const 径 of ['/consume', '/ack', '/retry']) {
    const 回 = await 宿主.fetch(new Request('https://x.test' + 径), {});
    assert.equal(回.status, 200, 径);
    assert.equal(await 回.text(), '捕获：当前事件不是队列事件', 径);
  }
});

test('复现包管理服务的页面任务消费者：版本不符确认、失败按 min(300, 15×2^min(尝试,4)) 秒退避重试、成功确认，并与日志接口组合', async () => {
  const 任务 = (体, 尝试 = 1) => new 模拟消息('t' + Math.random().toString(16).slice(2, 8), {version: 'persistent-reader-8', ...体}, 尝试);
  const 诸信 = [
    任务({kind: 'page', url: '/release/a', refresh: true}),
    new 模拟消息('old', {kind: 'page', version: 'persistent-reader-7', fail: true}, 3),
    任务({kind: 'release', id: 'x', fail: true}, 1),
    任务({kind: 'release', id: 'x', fail: true}, 2),
    任务({kind: 'release', id: 'x', fail: true}, 3),
    任务({kind: 'release', id: 'x', fail: true}, 4),
    任务({kind: 'release', id: 'x', fail: true}, 5),
    任务({kind: 'release', id: 'x', fail: true}, 9),
    任务({kind: 'weird'}, 2),
    任务({kind: 'release', id: 'y'}, 4),
  ];
  const 原 = {info: console.info, error: console.error}, 记 = {info: [], error: []};
  console.info = (...参数) => 记.info.push(参数.join(' '));
  console.error = (...参数) => 记.error.push(参数.join(' '));
  try { await 宿主.queue(new 模拟批次('page-render', 诸信), {}, 模拟上下文()); }
  finally { console.info = 原.info; console.error = 原.error; }
  assert.deepEqual(诸信.map(信 => 信.调用), [
    [['ack']], [['ack']], [['retry', {delaySeconds: 30}]], [['retry', {delaySeconds: 60}]], [['retry', {delaySeconds: 120}]],
    [['retry', {delaySeconds: 240}]], [['retry', {delaySeconds: 240}]], [['retry', {delaySeconds: 240}]], [['retry', {delaySeconds: 60}]], [['ack']],
  ]);
  assert.deepEqual(记.info, ['持久页面完成 page', '持久页面完成 release']);
  assert.equal(记.error.length, 7);
  assert.equal(记.error[0], '持久页面生成失败：处理失败：release');
  assert.equal(记.error[6], '持久页面生成失败：未知任务');
});

test('生产投递之后立即消费：两类事件各用各的 Wasm 实例，互不影响', async () => {
  const 队列 = new 模拟队列();
  const 投结果 = await 单('{"kind":"page","n":1}', {队列});
  assert.equal(投结果.文, '成功');
  const 体 = 队列.单发[0].体;
  const 行 = await 消费(new 模拟批次('log', [new 模拟消息('r1', 体)]));
  assert.deepEqual(JSON.parse(行[0]).messages[0].body, {kind: 'page', n: 1});
});

test('并发的队列事件互不干扰', async () => {
  const 批次们 = Array.from({length: 16}, (_, 序) => new 模拟批次('plan:' + 'ard'.repeat(2), Array.from({length: 6}, (_, 号) => new 模拟消息(`${序}-${号}`, {序, 号}))));
  await Promise.all(批次们.map(批次 => 消费(批次)));
  for (const 批次 of 批次们) assert.deepEqual(批次.messages.map(信 => 信.调用), [[['ack']], [['retry']], [['retry', {delaySeconds: 30}]], [['ack']], [['retry']], [['retry', {delaySeconds: 30}]]]);
});
