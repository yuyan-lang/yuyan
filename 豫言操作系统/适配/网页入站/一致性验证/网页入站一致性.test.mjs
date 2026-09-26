// 网页入站 0.8.0 一致性测试：真实 Wasm + Node 宿主。
// 复跑：在私有暂存目录（含 dist/）里 `node --test <本文件>`；产物位置可用环境变量 产物根 指定（默认 <当前目录>/dist）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {createHash, randomBytes} from 'node:crypto';
import {pathToFileURL} from 'node:url';
import path from 'node:path';

const 产物 = path.resolve(process.env.产物根 ?? path.join(process.cwd(), 'dist'), '网页入站一致性');
const {创建云工宿主} = await import(pathToFileURL(path.join(产物, '宿主.mjs')).href);
const 程序模块 = await WebAssembly.compile(await readFile(path.join(产物, '程序.wasm')));
const 值桥模块 = await WebAssembly.compile(await readFile(path.join(产物, '值桥.wasm')));
const 许可 = JSON.parse(await readFile(path.join(产物, '许可.json'), 'utf8'));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可});
const 上下文 = {waitUntil(承诺) { Promise.resolve(承诺).catch(() => {}); }};

const 摘 = 字节 => createHash('sha256').update(字节).digest('hex');
const 编 = 文 => Buffer.from(文, 'utf8');
const 空摘 = 摘(Buffer.alloc(0));
const MiB = 1024 * 1024;
// 可控读取流：块列为字节数组；记录中留下拉取次数、取消原因；可在指定序号处报错。
function 流(块列, 记录 = {}, {出错于 = -1, 取消抛 = false} = {}) {
  let 序 = 0;
  记录.拉 = 0;
  return new ReadableStream({
    pull(控制器) {
      记录.拉++;
      if (序 === 出错于) { 控制器.error(new Error('源流故障')); return; }
      if (序 < 块列.length) 控制器.enqueue(块列[序++] instanceof Uint8Array ? 块列[序 - 1] : Uint8Array.from(块列[序 - 1]));
      else 控制器.close();
    },
    cancel(原因) { 记录.取消 = 原因; if (取消抛) throw new Error('取消也失败'); }
  }, {highWaterMark: 0});
}
const 切块 = (字节, 大小) => { const 列 = []; for (let i = 0; i < 字节.length; i += 大小) 列.push(字节.subarray(i, Math.min(字节.length, i + 大小))); return 列; };
async function 调(路径, {方法 = 'POST', 体, 头 = {}, 环境 = {}, 请求} = {}) {
  let 请 = 请求;
  if (!请) {
    const 初 = {method: 方法, headers: 头};
    if (体 !== undefined) { 初.body = 体; if (体 instanceof ReadableStream) 初.duplex = 'half'; }
    请 = new Request('https://x.test' + 路径, 初);
  }
  const 回 = await 宿主.fetch(请, 环境, 上下文);
  return {状态: 回.status, 文: await 回.text(), 头: 回.headers};
}
// 报告格式：前缀|字节数|SHA-256|不超过 512 字节时的十六进制
function 报(果) {
  assert.equal(果.状态, 200, '应用不应失败：' + 果.文);
  const 段 = 果.文.split('|');
  return {前缀: 段[0], 字节数: Number(段[1]), 摘要: 段[2], 十六: 段[3], 文本: 段[3] ? Buffer.from(段[3], 'hex').toString('utf8') : ''};
}
function 错(果) {
  assert.equal(果.状态, 400, '应得到可捕获的失败：' + 果.文);
  const r = 报({状态: 200, 文: 果.文});
  assert.equal(r.前缀, 'err');
  return r.文本;
}

const 体报 = (状态, 字节, 含十六 = true) => ({前缀: String(状态), 字节数: 字节.length, 摘要: 摘(字节), ...(含十六 && 字节.length <= 512 ? {十六: 字节.toString('hex')} : {})});
function 断言体(果, 状态, 字节, 说明) {
  const r = 报(果);
  const 期望 = 体报(状态, 字节);
  assert.equal(r.前缀, 期望.前缀, (说明 ?? '') + ' 状态');
  assert.equal(r.字节数, 期望.字节数, (说明 ?? '') + ' 字节数');
  assert.equal(r.摘要, 期望.摘要, (说明 ?? '') + ' 摘要');
  if (期望.十六 !== undefined) assert.equal(r.十六, 期望.十六, (说明 ?? '') + ' 十六进制');
}

test('入站标头存在：出现即为阳，含空值，名不分大小写；非法名失败', async () => {
  const 头 = {'X-Empty': '', 'X-Val': 'v', 'X_Odd.Name~1': '1'};
  const 问 = async 名 => (await 调('/has?name=' + encodeURIComponent(名), {方法: 'GET', 头})).文;
  assert.equal(await 问('X-Empty'), '阳');
  assert.equal(await 问('x-empty'), '阳');
  assert.equal(await 问('X-VAL'), '阳');
  assert.equal(await 问('X-Absent'), '阴');
  assert.equal(await 问('X_Odd.Name~1'), '阳');
  assert.equal(await 问('x_odd.name~1'), '阳');
  for (const 坏 of ['', 'bad name', 'a:b', 'a\r\nb', '名', 'a b', 'x'.repeat(257)]) {
    const 果 = await 调('/has?name=' + encodeURIComponent(坏), {方法: 'GET', 头});
    assert.match(错(果), /入站标头名称无效/, JSON.stringify(坏));
  }
  assert.equal(await 问('x'.repeat(256)), '阴');
});

test('旧函数回归：方法、路径、标头、查询、来源、主机名、原查询、同源网址', async () => {
  assert.equal((await 调('/method', {方法: 'PUT', 体: 'x'})).文, 'PUT');
  assert.equal((await 调('/rawpath')).文, '/rawpath');
  assert.equal((await 调('', {请求: new Request('https://X.test/a%2Fb/%E4%BD%A0?op=/rawpath#f')})).文, '/a%2Fb/%E4%BD%A0');
  assert.equal(报(await 调('', {请求: new Request('https://x.test/plain?op=/decpath')})).文本, '/plain');
  assert.equal(报(await 调('', {请求: new Request('https://x.test/a%2Fb/%E4%BD%A0?op=/decpath')})).文本, '/a/b/你');
  assert.match(错(await 调('', {请求: new Request('https://x.test/%FF?op=/decpath')})), /URI|malformed|decode/i);
  assert.equal(报(await 调('/hdr?name=x-a', {头: {'X-A': 'v1'}})).文本, 'v1');
  assert.equal(报(await 调('/hdr?name=x-none')).文本, '');
  assert.equal(报(await 调('/query?name=k&k=v%20w&k=2', {方法: 'GET'})).文本, 'v w');
  assert.equal(报(await 调('/query?name=z', {方法: 'GET'})).文本, '');
  assert.equal((await 调('/qhas?name=e&e=', {方法: 'GET'})).文, '阳');
  assert.equal((await 调('/qhas?name=zz', {方法: 'GET'})).文, '阴');
  assert.equal((await 调('/origin', {请求: new Request('https://Host.Example:8443/origin')})).文, 'https://host.example:8443');
  assert.equal((await 调('/host', {请求: new Request('https://Host.Example:8443/host')})).文, 'host.example');
  assert.equal((await 调('/rawquery?a=1&b=%20', {方法: 'GET'})).文, '?a=1&b=%20');
  assert.equal((await 调('/rawquery', {方法: 'GET'})).文, '');
  const 网 = async (路径, 设, 删) => {
    const 网址 = 'https://x.test/mkurl?op=/mkurl&path=' + encodeURIComponent(路径) + '&set=' + encodeURIComponent(JSON.stringify(设)) + '&del=' + encodeURIComponent(JSON.stringify(删)) + '&keep=1&drop=2&a=old&a=old2';
    const 果 = await 调('', {请求: new Request(网址)});
    const 期 = new URL(网址);
    if (路径) 期.pathname = 路径;
    for (const k of 删) 期.searchParams.delete(k);
    for (const [k, v] of 设) 期.searchParams.set(k, v);
    return {果, 期: 期.href};
  };
  for (const [路径, 设, 删] of [['/new', [['a', 'x y']], ['drop']], ['', [], []], ['/p/中文', [['k', '值&=?#'], ['e', '']], ['keep', 'nope']], ['/', [['a', '1'], ['b', '2']], ['a']]]) {
    const {果, 期} = await 网(路径, 设, 删);
    assert.equal(果.文, 期, JSON.stringify([路径, 设, 删]));
  }
  for (const 坏 of ['//evil', '/a\\b', '/a?b', '/a#b', '/a\u0001b', 'x']) {
    assert.match(错((await 网(坏, [], [])).果), /同源改写路径无效/, JSON.stringify(坏));
  }
});

test('读取入站事件种类：fetch、service-fetch、durable-fetch', async () => {
  const 造 = () => new Request('https://x.test/kind');
  assert.equal(await (await 宿主.fetch(造(), {}, 上下文)).text(), 'fetch');
  assert.equal(await (await 宿主.serviceFetch(造(), {}, 上下文)).text(), 'service-fetch');
  assert.equal(await (await 宿主.durableFetch(造(), {}, {})).text(), 'durable-fetch');
});

for (const [名, 路径] of [['字节', '/body/bytes'], ['宽松文字', '/body/text'], ['严格文字', '/body/strict']]) {
  test(`${名}：分块成功、恰好上限与超一字节`, async () => {
    const 数据 = Buffer.from('abcdefgh');
    let 果 = await 调(路径 + '?limit=8', {体: 流([[97, 98, 99], [100, 101, 102], [103, 104]])});
    断言体(果, 0, 数据, '恰好上限');
    const 记 = {};
    果 = await 调(路径 + '?limit=7', {体: 流([[97, 98, 99], [100, 101, 102], [103, 104], [105]], 记)});
    断言体(果, 1, Buffer.alloc(0), '流式超限');
    assert.equal(记.拉, 3, '读到越界块即停，不再多读');
    assert.equal(记.取消, '入站正文超过上限', '超限须取消读取流');
    果 = await 调(路径 + '?limit=1', {体: 流([[97]])});
    断言体(果, 0, Buffer.from('a'), '上限一');
    果 = await 调(路径 + '?limit=1', {体: 流([[97], [98]])});
    断言体(果, 1, Buffer.alloc(0), '上限一超');
  });

  test(`${名}：上限参数越界失败，取值端点可用`, async () => {
    for (const 限 of ['0', '-1', '16777217', 'abc', '']) {
      const 消息 = 错(await 调(路径 + '?limit=' + 限, {体: 'abc'}));
      assert.match(消息, /入站正文字节上限须在 1 至 16777216/, 限);
    }
    断言体(await 调(路径 + '?limit=16777216', {体: 'abc'}), 0, Buffer.from('abc'), '上限 16 MiB');
    断言体(await 调(路径 + '?limit=1', {体: 'a'}), 0, Buffer.from('a'), '上限 1');
  });

  test(`${名}：无正文、已被读取、已被锁定与空正文`, async () => {
    for (const 方法 of ['GET', 'HEAD']) 断言体(await 调(路径 + '?limit=10', {方法}), 2, Buffer.alloc(0), 方法);
    断言体(await 调(路径 + '?limit=10', {方法: 'POST'}), 2, Buffer.alloc(0), 'POST 无体');
    断言体(await 调(路径 + '?limit=10', {方法: 'DELETE'}), 2, Buffer.alloc(0), 'DELETE 无体');
    断言体(await 调(路径 + '?limit=10', {体: 流([])}), 0, Buffer.alloc(0), '空流');
    断言体(await 调(路径 + '?limit=10', {体: ''}), 0, Buffer.alloc(0), '空串体');
    const 已读 = new Request('https://x.test' + 路径 + '?limit=10', {method: 'POST', body: 'abc'});
    await 已读.arrayBuffer();
    断言体(await 调('', {请求: 已读}), 3, Buffer.alloc(0), '已被读取');
    const 已锁 = new Request('https://x.test' + 路径 + '?limit=10', {method: 'POST', body: 'abc'});
    已锁.body.getReader();
    断言体(await 调('', {请求: 已锁}), 3, Buffer.alloc(0), '已被锁定');
    const 请 = new Request('https://x.test' + 路径 + '?limit=10', {method: 'POST', body: 'abc'});
    断言体(await 调('', {请求: 请}), 0, Buffer.from('abc'), '首次读取');
    断言体(await 调('', {请求: 请}), 3, Buffer.alloc(0), '再次读取');
  });

  test(`${名}：声明长度已超上限则不读正文；不可解析的声明长度交给流式计数`, async () => {
    let 记 = {};
    断言体(await 调(路径 + '?limit=10', {体: 流([[1, 2, 3]], 记), 头: {'content-length': '11'}}), 1, Buffer.alloc(0), '声明超限');
    assert.equal(记.拉, 0, '声明长度已超上限：完全不读流');
    assert.equal(记.取消, undefined);
    记 = {};
    断言体(await 调(路径 + '?limit=10', {体: 流([[1, 2, 3]], 记), 头: {'content-length': '10'}}), 0, Buffer.from([1, 2, 3]), '声明恰为上限');
    for (const 坏 of ['abc', '-5', '12, 12', ' ', '1.5', '']) {
      记 = {};
      断言体(await 调(路径 + '?limit=2', {体: 流([[1, 2, 3]], 记), 头: {'content-length': 坏}}), 1, Buffer.alloc(0), '坏声明长度 ' + JSON.stringify(坏));
      assert.equal(记.拉, 1, '坏声明长度不据以判限，而是读到越界块：' + JSON.stringify(坏));
    }
    记 = {};
    断言体(await 调(路径 + '?limit=2', {体: 流([[1, 2, 3]], 记), 头: {'content-length': '0000000000000000005'}}), 1, Buffer.alloc(0), '十九位声明长度视为超限');
    assert.equal(记.拉, 0);
    记 = {};
    断言体(await 调(路径 + '?limit=16777216', {体: 流([[1, 2, 3]], 记), 头: {'content-length': '99999999999999999999'}}), 1, Buffer.alloc(0), '二十位声明长度');
    assert.equal(记.拉, 0);
  });

  test(`${名}：流故障与取消故障`, async () => {
    const 消息 = 错(await 调(路径 + '?limit=100', {体: 流([[1, 2], [3]], {}, {出错于: 1})}));
    assert.match(消息, /读取入站正文失败：.*源流故障/);
    const 记 = {};
    断言体(await 调(路径 + '?limit=2', {体: 流([[1, 2], [3]], 记, {取消抛: true})}), 1, Buffer.alloc(0), '取消失败不应越界');
    assert.equal(记.取消, '入站正文超过上限');
  });

  test(`${名}：视图偏移、大量小块与整块`, async () => {
    const 大 = Uint8Array.from({length: 40}, (_, i) => i);
    const 视图 = new Uint8Array(大.buffer, 5, 10);
    断言体(await 调(路径 + '?limit=100', {体: 流([视图, new Uint8Array(大.buffer, 20, 3)])}), 0, Buffer.from([...大.subarray(5, 15), ...大.subarray(20, 23)]), '带偏移视图');
    const 可文 = n => Buffer.from(randomBytes(n).toString('base64').slice(0, n), 'ascii');
    const 小块 = 路径 === '/body/bytes' ? randomBytes(4096 * 4) : 可文(4096 * 4);
    断言体(await 调(路径 + '?limit=16777216', {体: 流(切块(小块, 4))}), 0, 小块, '4096 个四字节块（句柄不泄漏）');
    const 整 = 路径 === '/body/bytes' ? randomBytes(300000) : 可文(300000);
    断言体(await 调(路径 + '?limit=16777216', {体: 流([整])}), 0, 整, '单一大块');
  });
}

test('字节：十六兆字节恰好上限、超一字节、零字节与整块', async () => {
  const 满 = randomBytes(16 * MiB);
  let 果 = await 调('/body/bytes?limit=16777216', {体: 流(切块(满, 65536))});
  断言体(果, 0, 满, '16 MiB 分块');
  果 = await 调('/body/bytes?limit=16777216', {体: 流([满])});
  断言体(果, 0, 满, '16 MiB 整块');
  const 记 = {};
  果 = await 调('/body/bytes?limit=16777215', {体: 流(切块(满, 65536), 记)});
  断言体(果, 1, Buffer.alloc(0), '超一字节');
  assert.equal(记.拉, 256, '读到越界块为止');
  const 巨 = new Uint8Array(24 * MiB);
  const 记2 = {};
  果 = await 调('/body/bytes?limit=1048576', {体: 流([巨], 记2)});
  断言体(果, 1, Buffer.alloc(0), '巨块超限（不得整块拷入 Wasm）');
  assert.equal(记2.拉, 1);
  const 零 = Buffer.alloc(1000);
  断言体(await 调('/body/bytes?limit=1000', {体: 流([零])}), 0, 零, '全零字节');
});

test('大量小块不致栈溢出：十六 MiB 分成 16384 与 65536 块（字节），四 MiB 分成 16384 块（文字）', async () => {
  const 满 = randomBytes(16 * MiB);
  for (const 块 of [1024, 256]) 断言体(await 调('/body/bytes?limit=16777216', {体: 流(切块(满, 块))}), 0, 满, '块大小 ' + 块);
  const 文 = Buffer.from('汉a'.repeat(1 * MiB).slice(0, 2 * MiB), 'utf8');
  for (const 路 of ['/body/text', '/body/strict']) 断言体(await 调(路 + '?limit=16777216', {体: 流(切块(文, 256))}), 0, 文, 路 + ' 小块');
});

test('文字：宽松解码——分块多字节、BOM、非法序列、NUL', async () => {
  const 你 = Buffer.from('你好，世界🙂');
  const 断点 = [1, 2, 4, 7, 10];
  for (const 点 of 断点) 断言体(await 调('/body/text?limit=100', {体: 流([你.subarray(0, 点), 你.subarray(点)])}), 0, 你, '在字节 ' + 点 + ' 处断开');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xef, 0xbb, 0xbf, 104, 105]])}), 0, Buffer.from('hi'), 'BOM 剥除');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xef], [0xbb], [0xbf, 104]])}), 0, Buffer.from('h'), 'BOM 跨块剥除');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xe5], [0xa5, 0xbd, 0xef, 0xbb, 0xbf, 0x61]])}), 0, Buffer.from('好\ufeffa'), '流中间的 U+FEFF 保留，与首块是否残缺无关');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xef, 0xbb, 0xbf], [0xef, 0xbb, 0xbf, 0x61]])}), 0, Buffer.from('\ufeffa'), '只剥首个 BOM');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xef, 0xbb, 0xbf]])}), 0, Buffer.alloc(0), '正文只有 BOM');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xff, 0x41]])}), 0, Buffer.from('�A'), '孤立高位字节');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xed, 0xa0, 0x80]])}), 0, Buffer.from('���'), '代理对编码');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xe4, 0xbd]])}), 0, Buffer.from('�'), '末尾残缺序列');
  断言体(await 调('/body/text?limit=100', {体: 流([[97, 0, 98]])}), 0, Buffer.from('a\u0000b'), 'NUL 字节保留');
  断言体(await 调('/body/bytes?limit=100', {体: 流([[0xef, 0xbb, 0xbf, 104, 105]])}), 0, Buffer.from([0xef, 0xbb, 0xbf, 104, 105]), '字节接口不剥 BOM');
});

test('文字：严格解码——合法多字节通过，非法序列返回状态四', async () => {
  const 你 = Buffer.from('你好，世界🙂');
  for (const 点 of [1, 2, 4, 7, 10]) 断言体(await 调('/body/strict?limit=100', {体: 流([你.subarray(0, 点), 你.subarray(点)])}), 0, 你, '在字节 ' + 点 + ' 处断开');
  断言体(await 调('/body/strict?limit=100', {体: 流([[0xef, 0xbb, 0xbf, 104, 105]])}), 0, Buffer.from('hi'), 'BOM 剥除');
  断言体(await 调('/body/strict?limit=100', {体: 流([[97, 0, 98]])}), 0, Buffer.from('a\u0000b'), 'NUL 合法');
  for (const [名, 块列] of [['孤立高位字节', [[0xff]]], ['末尾残缺', [[0xe4, 0xbd]]], ['过长编码', [[0xc0, 0x80]]], ['代理对', [[0xed, 0xa0, 0x80]]], ['越界码点', [[0xf4, 0x90, 0x80, 0x80]]], ['跨块残缺', [[0xe4], [0xbd, 0x41]]], ['延续字节开头', [[0x80]]]]) {
    const 记 = {};
    断言体(await 调('/body/strict?limit=100', {体: 流(块列.concat([[65, 66]]), 记)}), 4, Buffer.alloc(0), 名);
  }
  const 记 = {};
  断言体(await 调('/body/strict?limit=100', {体: 流([[0xff], [65], [66], [67]], 记)}), 4, Buffer.alloc(0), '发现非法即停读');
  assert.equal(记.拉, 1);
  assert.equal(记.取消, '入站正文不是有效 UTF-8');
  断言体(await 调('/body/text?limit=100', {体: 流([[0xff], [65]])}), 0, Buffer.from('�A'), '同一输入宽松解码成功');
});

test('文字：大正文、整块分片与宽松解码膨胀上限', async () => {
  const 汉 = Buffer.from('汉'.repeat(1747626));
  for (const 路 of ['/body/text', '/body/strict']) {
    断言体(await 调(路 + '?limit=16777216', {体: 流(切块(汉, 65536))}), 0, 汉, 路 + ' 5 MiB 分块');
    断言体(await 调(路 + '?limit=16777216', {体: 流([汉])}), 0, 汉, 路 + ' 5 MiB 整块（分片解码）');
  }
  const 十兆 = Buffer.from('a'.repeat(10 * MiB));
  断言体(await 调('/body/text?limit=16777216', {体: 流([十兆])}), 0, 十兆, '10 MiB 单块');
  const 坏 = Buffer.alloc(3 * MiB, 0xff);
  const 三 = await 调('/body/text?limit=16777216', {体: 流(切块(坏, 65536))});
  断言体(三, 0, Buffer.from('�'.repeat(3 * MiB)), '3 MiB 非法字节膨胀为 9 MiB 仍成功');
  const 六 = await 调('/body/text?limit=16777216', {体: 流(切块(Buffer.alloc(6 * MiB, 0xff), 65536))});
  assert.match(错(六), /宽松解码后的文字超过 16 MiB/);
  断言体(await 调('/body/strict?limit=16777216', {体: 流(切块(Buffer.alloc(6 * MiB, 0xff), 65536))}), 4, Buffer.alloc(0), '严格解码立即拒绝');
});

test('读取入站癸象正文文：合法输入规范化，失败均可捕获且含原因', async () => {
  const 正 = 体 => 调('/body/json', {体});
  for (const [入, 出] of [['{"a":1,"b":[true,null,"x"]}', '{"a":1,"b":[true,null,"x"]}'], ['  {"甲":"乙"}  ', '{"甲":"乙"}'], ['﻿{"bom":1}', '{"bom":1}'], ['"\\u4e2d文"', '"中文"'], ['null', 'null'], ['[1, 2 , 3]', '[1,2,3]'], ['{"a":{"b":{"c":[]}}}', '{"a":{"b":{"c":[]}}}']]) {
    const r = 报(await 正(入));
    assert.equal(r.前缀, 'ok');
    assert.equal(r.文本, 出, JSON.stringify(入));
  }
  for (const [入, 模式] of [['{bad', /SyntaxError/], ['', /Unexpected end|JSON/], ['[1,2', /SyntaxError/], ['{"a":1}garbage', /SyntaxError/], ['undefined', /SyntaxError/]]) {
    assert.match(错(await 正(入)), new RegExp('^入站 JSON 正文无效：'), JSON.stringify(入));
    assert.match(错(await 正(入)), 模式, JSON.stringify(入));
  }
  assert.match(错(await 调('/body/json', {方法: 'GET'})), /入站请求没有 JSON 正文/);
  assert.match(错(await 调('/body/json', {方法: 'POST'})), /入站请求没有 JSON 正文/);
  const 深 = 'x'.repeat(34);
  assert.match(错(await 正('['.repeat(34) + ']'.repeat(34))), /嵌套过深/);
  assert.equal(报(await 正('['.repeat(33) + ']'.repeat(33))).前缀, 'ok');
  assert.match(错(await 正('\u0000')), /入站 JSON 正文无效/);
});

test('读取入站癸象正文文：2 MiB 上限与读取次序', async () => {
  const 整 = ' '.repeat(2 * MiB - 1) + '1';
  assert.equal(整.length, 2 * MiB);
  assert.equal(报(await 调('/body/json', {体: 整})).文本, '1', '恰好 2 MiB');
  assert.equal(报(await 调('/body/json', {体: ' '.repeat(2 * MiB - 2) + '12'})).文本, '12');
  assert.match(错(await 调('/body/json', {体: ' ' + 整})), /入站 JSON 正文超过 2 MiB/, '2 MiB 加一字节');
  const 记 = {};
  assert.match(错(await 调('/body/json', {体: 流([Buffer.alloc(2 * MiB, 32), Buffer.alloc(1, 32)], 记)})), /超过 2 MiB/);
  assert.equal(记.取消, '入站正文超过上限');
  const 甲 = 报(await 调('/body/textthenjson?limit=100', {体: '{"a":1}'}));
  assert.equal(甲.前缀, '0+');
  assert.equal(甲.文本, '入站请求正文已被读取');
  const 乙 = 报(await 调('/body/jsonthentext?limit=100', {体: '{"a":1}'}));
  assert.equal(乙.前缀, '3+');
  assert.equal(乙.文本, '{"a":1}');
  const 重复 = 报(await 调('/body/twice?limit=10', {体: 'abc'}));
  assert.equal(重复.前缀, '0,3');
});

test('读取入站有限癸象正文文（0.8.0）：规范化文字；状态 1 至 5 以返值表示且不抛异常', async () => {
  const 限 = (体, 限值 = 1000, 额外 = {}) => 调('/body/jsonlimit?limit=' + 限值, {体, ...额外});
  const 正 = async (入, 出, 限值) => { const r = 报(await 限(入, 限值)); assert.equal(r.前缀, '0', JSON.stringify(入)); assert.equal(r.文本, 出, JSON.stringify(入)); };
  await 正('{ "a" : 1 , "b" : [ 2 ] }', '{"a":1,"b":[2]}');
  await 正('{"a":1,"b":2,"a":3}', '{"a":3,"b":2}');
  await 正('{"b":1,"2":2,"1":3}', '{"1":3,"2":2,"b":1}');
  await 正('{"a":"\\u4e2d\\/"}', '{"a":"中/"}');
  await 正('{"a":"\\ud800"}', '{"a":"\\ud800"}');
  await 正('{"a":1e21}', '{"a":1e+21}');
  await 正('﻿{"bom":1}', '{"bom":1}');
  await 正('null', 'null'); await 正('[1,2]', '[1,2]'); await 正('  "x"  ', '"x"');
  await 正('{"名":"你好🙂"}', '{"名":"你好🙂"}');
  // 多字节字符跨块
  const 你 = Buffer.from('{"名":"你好🙂"}');
  for (const 点 of [3, 6, 8, 12, 15]) { const r = 报(await 调('/body/jsonlimit?limit=1000', {体: 流([你.subarray(0, 点), 你.subarray(点)])})); assert.equal(r.前缀, '0', '断点 ' + 点); assert.equal(r.文本, '{"名":"你好🙂"}'); }
  // 状态 5：任何 JSON 错误都不抛异常，也不使 Wasm 中止（畸形转义正是这一类）
  for (const 入 of ['{bad', '[1,2', '{"a":"\\x"}', '{"a":"\\u12"}', '{"a":"\\', '{"a":1}garbage', 'undefined', '\u0000', '{"a":01}', '{"a":1,}']) {
    const r = 报(await 限(入)); assert.equal(r.前缀, '5', JSON.stringify(入)); assert.ok(r.字节数 > 0, '应附原因：' + JSON.stringify(入));
  }
  assert.equal(报(await 限('['.repeat(34) + ']'.repeat(34))).前缀, '5', '嵌套过深');
  // 空正文的 POST：Node 给空流（状态 5：空文不是 JSON），真实 workerd 可能报无正文（状态 2）
  assert.ok(['2', '5'].includes(报(await 限('')).前缀));
  // 状态 4、2、3
  assert.equal(报(await 调('/body/jsonlimit?limit=100', {体: 流([[0xff]])})).前缀, '4');
  assert.equal(报(await 调('/body/jsonlimit?limit=100', {体: 流([[0x7b, 0x22, 0x61, 0x22, 0x3a, 0x22, 0xe4, 0xbd], [0x22, 0x7d]])})).前缀, '4', '跨块残缺序列');
  assert.equal(报(await 调('/body/jsonlimit?limit=100', {方法: 'GET'})).前缀, '2');
  assert.equal(报(await 调('/body/jsonlimittwice?limit=100', {体: '{"a":1}'})).前缀, '0,3');
  assert.equal(报(await 调('/body/textthenjsonlimit?limit=100', {体: '{"a":1}'})).前缀, '0+3');
  // 状态 1：恰好等于上限通过，多一字节超限；声明的 Content-Length 超限则一字不读；取消读取流
  assert.equal(报(await 限('{"a":1}', 7)).前缀, '0');
  assert.equal(报(await 限('{"a":1}', 6)).前缀, '1');
  const 记 = {};
  assert.equal(报(await 调('/body/jsonlimit?limit=10', {体: 流([Buffer.alloc(10, 32), Buffer.alloc(1, 32), Buffer.alloc(1, 32)], 记)})).前缀, '1');
  assert.equal(记.取消, '入站正文超过上限'); assert.equal(记.拉, 2, '读到越界块即停');
  const 记二 = {};
  assert.equal(报(await 调('/body/jsonlimit?limit=10', {体: 流([[49]], 记二), 头: {'content-length': '11'}})).前缀, '1');
  assert.equal(记二.拉, 0, '声明超限则一个字节也不读');
  // 上限范围：1 至 8388608
  assert.match(错(await 限('1', 0)), /入站 JSON 正文字节上限须在 1 至 8388608/);
  assert.match(错(await 限('1', 8388609)), /入站 JSON 正文字节上限须在 1 至 8388608/);
  assert.equal(报(await 限(' '.repeat(8 * MiB - 1) + '1', 8388608)).文本, '1', '恰 8 MiB');
  assert.equal(报(await 限(' '.repeat(8 * MiB) + '1', 8388608)).前缀, '1', '8 MiB 加一字节');
});

// 确定性伪随机（mulberry32），便于复现。
function 随机器(种子) { let a = 种子 >>> 0; return () => { a = (a + 0x6D2B79F5) >>> 0; let t = a; t = Math.imul(t ^ (t >>> 15), t | 1); t ^= t + Math.imul(t ^ (t >>> 7), t | 61); return ((t ^ (t >>> 14)) >>> 0) / 4294967296; }; }

test('差分模糊：随机标头名的 入站标头存在 与参考实现一致', async () => {
  const 随 = 随机器(20260930);
  const 池 = Array.from("abcXYZ019-_.~!#$%&'*+^`|", c => c).concat([' ', ':', ';', '"', '\\', '/', 'é', '你', '\u0001']);
  const 令牌正则 = /^[!#$%&'*+.^_`|~0-9A-Za-z-]+$/;
  let 合法数 = 0, 非法数 = 0;
  for (let i = 0; i < 1500; i++) {
    let 名 = '';
    const 长 = Math.floor(随() * 随() * 12);
    for (let j = 0; j < 长; j++) 名 += 池[Math.floor(随() * 池.length)];
    if (i % 31 === 0) 名 = 'h'.repeat([255, 256, 257][Math.floor(随() * 3)]);
    const 头 = new Headers();
    let 有 = false;
    if (令牌正则.test(名) && Buffer.byteLength(名) <= 256) { 头.set(名, 'v'); 有 = 随() < 0.5; }
    const 请 = new Request('https://x.test/has?name=' + encodeURIComponent(名), {headers: 有 ? 头 : {}});
    const 果 = await 调('', {请求: 请});
    if (令牌正则.test(名) && Buffer.byteLength(名) <= 256) { assert.equal(果.文, 有 ? '阳' : '阴', JSON.stringify(名)); 合法数++; }
    else { assert.match(错(果), /入站标头名称无效/, JSON.stringify(名)); 非法数++; }
  }
  assert.ok(合法数 > 100 && 非法数 > 100, `样本应两类都足够：合法 ${合法数}，非法 ${非法数}`);
});

test('差分模糊：随机 Content-Length 声明与上限的预判与参考实现一致', async () => {
  const 随 = 随机器(20261001);
  const 片 = ['0', '1', '2', '5', '9', '10', '00', '000', '12345678', '99999999999999999', '999999999999999999', '9999999999999999999', ' ', '-', '+', 'a', ',', '.', 'e', '\t'];
  let 预判数 = 0, 忽略数 = 0;
  for (let i = 0; i < 1500; i++) {
    let 声明 = '';
    const 段数 = 1 + Math.floor(随() * 3);
    for (let j = 0; j < 段数; j++) 声明 += 片[Math.floor(随() * 片.length)];
    const 限 = 1 + Math.floor(随() * 20);
    const 头 = new Headers();
    try { 头.set('content-length', 声明); } catch { continue; }
    const 实际值 = 头.get('content-length');
    const 记 = {};
    const 请 = new Request('https://x.test/body/bytes?limit=' + 限, {method: 'POST', headers: 头, body: 流([[1, 2, 3]], 记), duplex: 'half'});
    const 参考超限 = /^[0-9]{1,18}$/.test(实际值) ? BigInt(实际值) > BigInt(限) : /^[0-9]{19,}$/.test(实际值);
    const 果 = await 调('', {请求: 请});
    const 报告 = 报(果);
    if (参考超限) { assert.equal(报告.前缀, '1', JSON.stringify(实际值)); assert.equal(记.拉, 0, '预判超限不读流 ' + JSON.stringify(实际值)); 预判数++; }
    else { assert.equal(记.拉 > 0, true, '不可预判时应读流 ' + JSON.stringify(实际值)); assert.equal(报告.前缀, 限 >= 3 ? '0' : '1', JSON.stringify([实际值, 限])); 忽略数++; }
  }
  assert.ok(预判数 > 50 && 忽略数 > 50, `样本应两类都足够：预判 ${预判数}，忽略 ${忽略数}`);
});

// 被替换的 JS 原型的关键语义：账户读正文式的“逐块累计、超限即取消”，与 TextDecoder 整块解码。
async function 原型读正文(请求, 限) {
  if (!请求.body) return {状态: 2};
  const 读 = 请求.body.getReader();
  let 大小 = 0;
  const 片 = [];
  for (;;) {
    const {done, value} = await 读.read();
    if (done) break;
    大小 += value.length;
    if (大小 > 限) { await 读.cancel(); return {状态: 1};
    }
    片.push(value);
  }
  return {状态: 0, 字节: Buffer.concat(片)};
}
test('差分模糊：随机分块与上限下，字节读取与 JS 原型（逐块累计、超限即取消）一致', async () => {
  const 随 = 随机器(20261006);
  let 成 = 0, 超 = 0;
  for (let i = 0; i < 600; i++) {
    const 总 = Math.floor(随() * 80);
    const 数据 = Buffer.from(Array.from({length: 总}, () => Math.floor(随() * 256)));
    const 块 = [];
    for (let 位 = 0; 位 < 总;) { const 大小 = 1 + Math.floor(随() * 12); 块.push(数据.subarray(位, Math.min(总, 位 + 大小))); 位 += 大小; }
    const 限 = 1 + Math.floor(随() * 80);
    const 参考 = await 原型读正文(new Request('https://x.test/', {method: 'POST', body: 流(块.map(b => new Uint8Array(b))), duplex: 'half'}), 限);
    const 记 = {};
    const 果 = await 调('/body/bytes?limit=' + 限, {体: 流(块.map(b => new Uint8Array(b)), 记)});
    const 报告 = 报(果);
    assert.equal(报告.前缀, String(参考.状态 === 2 ? 0 : 参考.状态), `总 ${总} 限 ${限}`);
    if (参考.状态 === 0) { assert.equal(报告.摘要, 摘(参考.字节)); assert.equal(报告.字节数, 参考.字节.length); 成++; }
    else { 超++; assert.equal(记.取消, '入站正文超过上限'); }
  }
  assert.ok(成 > 100 && 超 > 100, `样本应两类都足够：成功 ${成}，超限 ${超}`);
});

test('差分模糊：随机字节的宽松与严格解码与 TextDecoder 整块解码一致（任意分块）', async () => {
  const 随 = 随机器(20261007);
  const 文池 = ['a', 'Z', ' ', '\n', '\u0000', 'é', '你', '好', '🙂', ' ', '�', '﻿'];
  let 合法 = 0, 非法 = 0;
  for (let i = 0; i < 600; i++) {
    let 字节 = Buffer.from(Array.from({length: Math.floor(随() * 6)}, () => 文池[Math.floor(随() * 文池.length)]).join(''), 'utf8');
    const 变异 = 随();
    if (变异 < 0.25) 字节 = Buffer.concat([字节, Buffer.from([Math.floor(随() * 256)])]);
    else if (变异 < 0.4 && 字节.length) { 字节 = Buffer.from(字节); 字节[Math.floor(随() * 字节.length)] = Math.floor(随() * 256); }
    else if (变异 < 0.5) 字节 = Buffer.concat([Buffer.from([0xef, 0xbb, 0xbf]), 字节]);
    else if (变异 < 0.6 && 字节.length) 字节 = 字节.subarray(0, 字节.length - 1);
    const 块 = [];
    for (let 位 = 0; 位 < 字节.length;) { const 大小 = 1 + Math.floor(随() * 4); 块.push(new Uint8Array(字节.subarray(位, Math.min(字节.length, 位 + 大小)))); 位 += 大小; }
    const 宽期 = new TextDecoder().decode(字节);
    let 严期; try { 严期 = new TextDecoder('utf-8', {fatal: true}).decode(字节); } catch { 严期 = null; }
    const 宽 = 报(await 调('/body/text?limit=100', {体: 流(块)}));
    assert.equal(宽.前缀, '0');
    assert.equal(宽.摘要, 摘(Buffer.from(宽期, 'utf8')), '宽松 ' + 字节.toString('hex'));
    const 严 = 报(await 调('/body/strict?limit=100', {体: 流(块)}));
    if (严期 === null) { assert.equal(严.前缀, '4', '严格拒绝 ' + 字节.toString('hex')); 非法++; }
    else { assert.equal(严.前缀, '0', '严格接受 ' + 字节.toString('hex')); assert.equal(严.摘要, 摘(Buffer.from(严期, 'utf8'))); 合法++; }
  }
  assert.ok(合法 > 100 && 非法 > 100, `样本应两类都足够：合法 ${合法}，非法 ${非法}`);
});

// 文言：旧读标头术遇非法名亦为可捕之事故，不使宿主 TypeError 越界。汉语：读取入站标头值 传入 a b、a:b、中文等非法名称时抛可捕获的豫言异常。
test('旧读取入站标头值：非法标头名可捕获地失败', async () => {
  for (const 坏 of ['', 'bad name', 'a:b', 'a\r\nb', '名', 'x'.repeat(257)]) {
    assert.match(错(await 调('/hdr?name=' + encodeURIComponent(坏))), /入站标头名称无效/, JSON.stringify(坏));
  }
});
