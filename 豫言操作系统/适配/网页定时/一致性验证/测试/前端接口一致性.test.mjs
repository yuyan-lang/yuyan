// 文言：以真实 Wasm 探针验网页定时、储存、导航、环境、请求、事件源、编译、应用八包：事之入列、限额、败之类、取消之义、背压不失。
// 汉语：JSDOM + 真实 Wasm 的一致性验收：探针应用通过“豫言测试命令/回复”消息调用八个新接口包的全部函数；
//       网络类测试用本机 node:http 服务器与 Node 的真实 fetch，同源页面 URL 指向该服务器。
import test from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import {readFile, readdir} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import {启动探针, 等待, 睡} from './夹具.mjs';

const 用页 = (选项, 函数) => async () => {
  const 页 = await 启动探针(选项);
  try { await 函数(页); } finally { await 页.关(); }
};
const 事件 = async 页 => JSON.parse(await 页.调('等待网页事件'));
const 失败 = (承诺, 期望) => assert.rejects(承诺, 期望);
const 存在 = 文 => (文.startsWith('1|') ? [true, 文.slice(2)] : [false, 文.slice(2)]);

test('接口函数全部被探针覆盖', async () => {
  const 接口根 = (process.env.接口根 ?? fileURLToPath(new URL('../../../../../豫言操作系统接口/', import.meta.url))).replace(/\/?$/u, '/');
  const 探针源 = await readFile(process.env.探针源 ?? fileURLToPath(new URL('../应用/前端接口探针/入口。豫', import.meta.url)), 'utf8');
  for (const 包 of ['网页定时', '网页储存', '网页导航', '网页环境', '网页请求', '网页事件源', '网页编译', '网页应用']) {
    const 文件 = (await readdir(接口根 + 包)).find(名 => 名.endsWith('。接口。豫'));
    const 源 = await readFile(接口根 + 包 + '/' + 文件, 'utf8');
    const 名们 = [...源.matchAll(/^「([^」]+)」乃/gmu)].map(项 => 项[1]);
    assert.ok(名们.length >= 1, 包);
    for (const 名 of 名们) assert.ok(探针源.includes('「' + 名 + '」于') || 探针源.includes('『' + 名 + '』'), `探针未覆盖 ${包}.${名}`);
  }
});

// ---------------------------------------------------------------------------
// 一、网页定时
// ---------------------------------------------------------------------------
test('网页定时：一次定时到期投递“定时”事件，带定时号、标记与时刻，之后消失', 用页({}, async 页 => {
  const 号 = Number(await 页.调('启动一次定时', 30, '甲'));
  assert.ok(号 >= 1);
  const 事 = await 事件(页);
  assert.equal(事.类型, '定时');
  assert.equal(事.订阅号, 号);
  assert.equal(事.名称, '定时');
  assert.equal(事.详情.种类, '一次');
  assert.equal(事.详情.标记, '甲');
  assert.ok(Number.isInteger(事.详情.时刻) && Math.abs(事.详情.时刻 - Date.now()) < 5000);
  // 一次定时到期后名额已释放，取消返回阴
  assert.equal(await 页.调('取消定时', 号), 'false');
  await 睡(80);
  assert.equal(页.宿主.状态().各类积压.定时 ?? 0, 0);
}));

test('网页定时：定时号单调递增；周期定时不积压，取走后继续投递', 用页({}, async 页 => {
  const 甲 = Number(await 页.调('启动重复定时', 20, '乙'));
  const 乙 = Number(await 页.调('启动一次定时', 100000, '远'));
  assert.ok(乙 > 甲);
  await 睡(250); // 应用未取事件：只应积压一个
  assert.equal(页.宿主.状态().各类积压.定时, 1);
  const 事一 = await 事件(页);
  assert.equal(事一.订阅号, 甲);
  assert.equal(事一.详情.种类, '重复');
  const 事二 = await 事件(页); // 取走后继续投递
  assert.equal(事二.订阅号, 甲);
  assert.ok(事二.序 > 事一.序);
  assert.equal(await 页.调('取消定时', 甲), 'true');
  assert.equal(await 页.调('取消定时', 乙), 'true');
  assert.equal(await 页.调('取消定时', 乙), 'false');
}));

test('网页定时：取消后保证不再收到该号的事件，含已入列而未取者', 用页({}, async 页 => {
  const 号 = Number(await 页.调('启动一次定时', 20, '丙'));
  await 睡(80); // 事件已入列，未取
  assert.equal(页.宿主.状态().各类积压.定时, 1);
  assert.equal(await 页.调('取消定时', 号), 'true'); // 入列的事件被移除，故为阳
  assert.equal(页.宿主.状态().各类积压.定时, 0);
  const 号丁 = Number(await 页.调('启动一次定时', 30, '丁'));
  const 事 = await 事件(页);
  assert.equal(事.订阅号, 号丁, '取到的是后启动的定时，被取消的“丙”不再出现');
  assert.equal(事.详情.标记, '丁');
}));

test('网页定时：违规调用抛事故，超过 64 个定时器抛事故，取消后可再启动', 用页({}, async 页 => {
  await 失败(页.调('启动一次定时', -1, 'x'), /定时延时毫秒/);
  await 失败(页.调('启动一次定时', 2147483648, 'x'), /定时延时毫秒/);
  await 失败(页.调('启动重复定时', 9, 'x'), /定时间隔毫秒/);
  await 失败(页.调('启动一次定时', 10, 'x'.repeat(1025)), /标记/);
  const 号们 = [];
  for (let i = 0; i < 64; i++) 号们.push(Number(await 页.调('启动一次定时', 600000, '占位' + i)));
  await 失败(页.调('启动一次定时', 600000, '第六十五'), /上限/);
  assert.equal(await 页.调('取消定时', 号们[0]), 'true');
  assert.ok(Number(await 页.调('启动一次定时', 600000, '补位')) > 号们[63]);
}));

// ---------------------------------------------------------------------------
// 二、网页储存
// ---------------------------------------------------------------------------
test('网页储存：读写删与区域隔离，空值可区分于不存在', 用页({}, async 页 => {
  assert.deepEqual(存在(await 页.调('读取网页储存', '本地', '偏好')), [false, '']);
  assert.equal(await 页.调('写入网页储存', '本地', '偏好', '云端'), 'true');
  assert.deepEqual(存在(await 页.调('读取网页储存', '本地', '偏好')), [true, '云端']);
  assert.deepEqual(存在(await 页.调('读取网页储存', '会话', '偏好')), [false, ''], '会话与本地互不相通');
  assert.equal(await 页.调('写入网页储存', '会话', '草稿', ''), 'true');
  assert.deepEqual(存在(await 页.调('读取网页储存', '会话', '草稿')), [true, ''], '空字符串值是存在的');
  assert.equal(页.窗.sessionStorage.getItem('草稿'), '');
  assert.equal(页.窗.localStorage.getItem('偏好'), '云端');
  await 页.调('删除网页储存', '本地', '偏好');
  assert.deepEqual(存在(await 页.调('读取网页储存', '本地', '偏好')), [false, '']);
  await 页.调('删除网页储存', '本地', '偏好'); // 键不存在也不抛
  // 非 BMP 字符与换行原样往返
  const 值 = '甲😀\n乙\u0000丙'.replace('\u0000', '');
  await 页.调('写入网页储存', '本地', 'yy-cloud-draft:账户:项目:ai', 值);
  assert.deepEqual(存在(await 页.调('读取网页储存', '本地', 'yy-cloud-draft:账户:项目:ai')), [true, 值]);
}));

test('网页储存：违规调用抛事故；配额或禁用等运行时失败返回阴而不抛', async () => {
  const 页 = await 启动探针({窗扩展: 窗 => {
    const 原写 = 窗.Storage.prototype.setItem;
    窗.Storage.prototype.setItem = function (键, 值) {
      if (键 === '配额满') throw new 窗.DOMException('已满', 'QuotaExceededError');
      return 原写.call(this, 键, 值);
    };
  }});
  try {
    await 失败(页.调('读取网页储存', '临时', '键'), /区域无效/);
    await 失败(页.调('读取网页储存', '本地', ''), /不能为空/);
    await 失败(页.调('写入网页储存', '本地', 'k'.repeat(513), 'v'), /至多 512/);
    assert.equal(await 页.调('写入网页储存', '本地', 'k', 'v'.repeat(300000)), 'true');
    assert.equal(await 页.调('写入网页储存', '本地', '配额满', 'x'), 'false', '配额满：返回阴');
    assert.deepEqual(存在(await 页.调('读取网页储存', '本地', '配额满')), [false, '']);
  } finally { await 页.关(); }
});

// ---------------------------------------------------------------------------
// 三、网页环境
// ---------------------------------------------------------------------------
test('网页环境：本地时区偏移东为正，按该时刻的夏令时计；违规调用抛事故', 用页({}, async 页 => {
  const 偏移 = async 毫秒 => Number(await 页.调('读取本地时区偏移分钟', 毫秒));
  const 期望 = 毫秒 => -new Date(毫秒).getTimezoneOffset();
  for (const 毫秒 of [0, Date.UTC(2026, 0, 1), Date.UTC(2026, 6, 1), -1, 1790000000123]) assert.equal(await 偏移(毫秒), 期望(毫秒) + 0);
  await 失败(页.调('读取本地时区偏移分钟', 8640000000000001), /越界/);
}));

test('网页环境：读取页面饼按名精确匹配并原样返回，不存在为阴，名称不合规抛事故', 用页({}, async 页 => {
  页.文.cookie = 'yuyan_lang=wen; path=/';
  页.文.cookie = '别的=x; path=/';
  页.文.cookie = 'enc=a%20b; path=/';
  assert.deepEqual(存在(await 页.调('读取页面饼', 'yuyan_lang')), [true, 'wen']);
  assert.deepEqual(存在(await 页.调('读取页面饼', 'yuyan')), [false, '']);
  assert.deepEqual(存在(await 页.调('读取页面饼', '别的')), [true, 'x']);
  assert.deepEqual(存在(await 页.调('读取页面饼', 'enc')), [true, 'a%20b']);
  await 失败(页.调('读取页面饼', 'a b'), /饼名称无效/);
  await 失败(页.调('读取页面饼', ''), /饼名称无效/);
  await 失败(页.调('读取页面饼', 'a=b'), /饼名称无效/);
}));

// ---------------------------------------------------------------------------
// 四、网页导航
// ---------------------------------------------------------------------------
test('网页导航：压入与替换历史只改地址不触发 popstate；移动历史触发历史事件', 用页({url: 'https://yuyan-lang.org/cloud/?service=code'}, async 页 => {
  const 订阅 = await 页.调('订阅界面事件', '窗口', 'popstate', '{}');
  assert.ok(Number(订阅) > 0);
  await 页.调('压入历史', '/cloud/?project=1');
  assert.equal(页.窗.location.href, 'https://yuyan-lang.org/cloud/?project=1');
  assert.equal(页.宿主.状态().各类积压.历史 ?? 0, 0, 'pushState 不触发 popstate');
  await 页.调('替换历史', '/cloud/?project=2#锚');
  assert.equal(页.窗.location.search, '?project=2');
  assert.equal(页.窗.location.hash, '#%E9%94%9A');
  assert.equal(页.窗.history.length, 2, '替换历史不增加条数');
  await 页.调('移动历史', -1);
  const 事 = await 事件(页);
  assert.equal(事.类型, '历史');
  assert.equal(事.名称, 'popstate');
  assert.equal(事.订阅号, Number(订阅));
  assert.equal(事.详情.网址, 'https://yuyan-lang.org/cloud/?service=code');
  assert.equal(页.窗.location.search, '?service=code');
}));

test('网页导航：违规地址与非同源历史抛事故', 用页({url: 'https://yuyan-lang.org/cloud/'}, async 页 => {
  await 失败(页.调('前往网址', 'javascript:alert(1)'), /只允许 http 或 https/);
  await 失败(页.调('前往网址', 'data:text/html,x'), /只允许 http 或 https/);
  await 失败(页.调('替换当前网址', ''), /不能为空/);
  await 失败(页.调('前往网址', '/a\\b'), /控制字符或反斜杠/);
  await 失败(页.调('压入历史', 'https://别处.站/'), /同源/);
  await 失败(页.调('替换历史', 'https://别处.站/x'), /同源/);
  await 失败(页.调('移动历史', 0), /不能为 0/);
  await 失败(页.调('移动历史', 51), /越界/);
}));

// ---------------------------------------------------------------------------
// 五、网页请求（本机 node:http 服务器 + Node 真实 fetch，页面 URL 与服务器同源）
// ---------------------------------------------------------------------------
const 起服务 = async 处理 => {
  const 服务 = http.createServer(处理);
  await new Promise(完成 => 服务.listen(0, '127.0.0.1', 完成));
  const {port} = 服务.address();
  return {
    源: `http://127.0.0.1:${port}`,
    关: () => new Promise(完成 => { 服务.closeAllConnections?.(); 服务.close(完成); })
  };
};
const 用服务 = (处理, 选项, 函数) => async () => {
  const 服 = await 起服务(处理);
  const 页 = await 启动探针({url: 服.源 + '/cloud/', 网络: (...参) => fetch(...参), ...选项});
  try { await 函数(页, 服); } finally { await 页.关(); await 服.关(); }
};
const 读体 = 请求 => new Promise(完成 => { const 块 = []; 请求.on('data', 项 => 块.push(项)); 请求.on('end', () => 完成(Buffer.concat(块).toString('utf8'))); });

test('网页请求：GET 完成事件含状态、小写标头、正文、字节数、最终网址与标记', 用服务((请求, 响应) => {
  响应.writeHead(200, {'Content-Type': 'application/json; charset=utf-8', 'X-Custom': 'a', 'Cache-Control': 'no-store'});
  响应.end(JSON.stringify({用户: '张三', 表情: '😀'}));
}, {}, async (页, 服) => {
  const 号 = Number(await 页.调('发起网页请求', JSON.stringify({网址: '/cloud/api/session', 标记: '会话'})));
  assert.ok(号 >= 1);
  const 事 = await 事件(页);
  assert.equal(事.类型, '请求');
  assert.equal(事.订阅号, 号);
  assert.equal(事.名称, '完成');
  const 详 = 事.详情;
  assert.equal(详.成, true);
  assert.equal(详.状态, 200);
  assert.equal(详.状态文, 'OK');
  assert.equal(详.标记, '会话');
  assert.equal(详.网址, 服.源 + '/cloud/api/session');
  assert.match(详.标头['content-type'], /application\/json/);
  assert.equal(详.标头['cache-control'], 'no-store');
  assert.deepEqual(JSON.parse(详.正文), {用户: '张三', 表情: '😀'});
  assert.equal(详.字节数, Buffer.byteLength(详.正文));
}));

test('网页请求：任何 HTTP 状态都是“完成”；POST 带标头与正文；HEAD 无正文；不缓存', 用服务(async (请求, 响应) => {
  const 体 = await 读体(请求);
  if (请求.url === '/cloud/api/bad') { 响应.writeHead(404, {'Content-Type': 'application/json'}); 响应.end('{"error":"没有"}'); return; }
  响应.writeHead(200, {'Content-Type': 'text/plain'});
  响应.end(JSON.stringify({方法: 请求.method, 类型: 请求.headers['content-type'], 缓存: 请求.headers['cache-control'] ?? '', 体}));
}, {}, async (页, 服) => {
  await 页.调('发起网页请求', JSON.stringify({网址: '/cloud/api/bad'}));
  const 甲 = (await 事件(页)).详情;
  assert.equal(甲.成, true);
  assert.equal(甲.状态, 404);
  assert.equal(JSON.parse(甲.正文).error, '没有');
  await 页.调('发起网页请求', JSON.stringify({方法: 'POST', 网址: '/cloud/api/echo', 标头: {'Content-Type': 'application/json'}, 正文: '{"名":"乙😀"}', 缓存: '不缓存'}));
  const 乙 = JSON.parse((await 事件(页)).详情.正文);
  assert.equal(乙.方法, 'POST');
  assert.equal(乙.类型, 'application/json');
  assert.equal(乙.体, '{"名":"乙😀"}');
  assert.match(乙.缓存, /no-cache/);
  await 页.调('发起网页请求', JSON.stringify({方法: 'HEAD', 网址: '/cloud/api/echo'}));
  const 丙 = (await 事件(页)).详情;
  assert.equal(丙.成, true);
  assert.equal(丙.正文, '');
}));

test('网页请求：并发请求各有订阅号，结果按到达顺序入队；取消保证不再收到该号事件', 用服务((请求, 响应) => {
  const 延 = Number(new URL(请求.url, 'http://x').searchParams.get('d'));
  setTimeout(() => { 响应.writeHead(200, {'Content-Type': 'text/plain'}); 响应.end(请求.url); }, 延);
}, {}, async (页, 服) => {
  const 慢 = Number(await 页.调('发起网页请求', JSON.stringify({网址: '/x?d=300', 标记: '慢'})));
  const 中 = Number(await 页.调('发起网页请求', JSON.stringify({网址: '/x?d=150', 标记: '中'})));
  const 快 = Number(await 页.调('发起网页请求', JSON.stringify({网址: '/x?d=10', 标记: '快'})));
  const 弃 = Number(await 页.调('发起网页请求', JSON.stringify({网址: '/x?d=20', 标记: '弃'})));
  assert.equal(await 页.调('取消网页请求', 弃), 'true');
  assert.equal(await 页.调('取消网页请求', 弃), 'false', '已取消的号再取消返回阴');
  const 到达 = [];
  for (let i = 0; i < 3; i++) { const 事 = await 事件(页); 到达.push([事.订阅号, 事.详情.标记]); }
  assert.deepEqual(到达, [[快, '快'], [中, '中'], [慢, '慢']]);
  await 睡(80);
  assert.equal(页.宿主.状态().各类积压.请求 ?? 0, 0, '被取消的请求没有留下事件');
  assert.equal(页.宿主.状态().请求数, 0);
}));

test('网页请求：结果事件在队列里未取时取消，事件被移除并返回阳', 用服务((请求, 响应) => { 响应.end('好'); }, {}, async (页, 服) => {
  const 号 = Number(await 页.调('发起网页请求', JSON.stringify({网址: '/x'})));
  await 等待(() => 页.宿主.状态().各类积压.请求 === 1, '请求结果入队');
  assert.equal(await 页.调('取消网页请求', 号), 'true');
  assert.equal(页.宿主.状态().各类积压.请求 ?? 0, 0);
  const 号乙 = Number(await 页.调('发起网页请求', JSON.stringify({网址: '/x', 标记: '后'})));
  const 事 = await 事件(页);
  assert.equal(事.订阅号, 号乙);
}));

test('网页请求：网络错误、超时、正文过大、正文非 UTF-8 都是“失败”事件而不抛事故', 用服务((请求, 响应) => {
  const 路 = new URL(请求.url, 'http://x').pathname;
  if (路 === '/drop') { 请求.socket.destroy(); return; }
  if (路 === '/hang') return; // 永不响应
  if (路 === '/big') { 响应.writeHead(200); 响应.end('x'.repeat(500)); return; }
  if (路 === '/badutf8') { 响应.writeHead(200); 响应.end(Buffer.from([0x61, 0xff, 0xfe, 0x62])); return; }
  响应.end('好');
}, {}, async (页, 服) => {
  const 取失败 = async (请求体) => { await 页.调('发起网页请求', JSON.stringify(请求体)); const 事 = await 事件(页); assert.equal(事.名称, '失败'); assert.equal(事.详情.成, false); return 事.详情; };
  assert.equal((await 取失败({网址: '/drop', 标记: '甲'})).原因, '网络错误');
  const 超时 = await 取失败({网址: '/hang', 超时毫秒: 120, 标记: '乙'});
  assert.equal(超时.原因, '超时');
  assert.equal(超时.标记, '乙');
  assert.equal((await 取失败({网址: '/big', 正文上限: 100})).原因, '正文过大');
  assert.equal((await 取失败({网址: '/badutf8'})).原因, '正文不是有效UTF-8');
}));

test('网页请求：违规调用抛事故（非同源、方法、标头、正文、字段、并发上限）', 用服务((请求, 响应) => { /* 永不响应，占住并发 */ }, {}, async (页, 服) => {
  const 试 = (对象, 期望) => 失败(页.调('发起网页请求', typeof 对象 === 'string' ? 对象 : JSON.stringify(对象)), 期望);
  await 试('不是json', /不是有效 JSON/);
  await 试('[1]', /须为 JSON 对象/);
  await 试({网址: 'https://别处.站/x'}, /同源/);
  await 试({网址: '//别处.站/x'}, /站内路径/);
  await 试({网址: 'x'}, /站内路径/);
  await 试({网址: '/x\\y'}, /反斜杠/);
  await 试({网址: '/x', 方法: 'get'}, /方法/);
  await 试({网址: '/x', 方法: 'CONNECT'}, /方法/);
  await 试({网址: '/x', 正文: '体'}, /不能带正文/);
  await 试({网址: '/x', 方法: 'POST', 正文: 1}, /正文须为文字/);
  await 试({网址: '/x', 标头: {Cookie: 'a=b'}}, /被禁用/);
  await 试({网址: '/x', 标头: {'Sec-Fetch-Mode': 'x'}}, /被禁用/);
  await 试({网址: '/x', 标头: {'坏 名': 'x'}}, /标头名无效/);
  await 试({网址: '/x', 标头: {'X-甲': 'a\nb'}}, /标头名无效/);
  await 试({网址: '/x', 缓存: '永远'}, /缓存/);
  await 试({网址: '/x', 超时毫秒: 0}, /超时毫秒/);
  await 试({网址: '/x', 正文上限: 16777217}, /正文上限/);
  await 试({网址: '/x', 未知: 1}, /未知字段/);
  await 试({网址: '/x', 标记: 'a'.repeat(257)}, /标记/);
  // 并发上限：占满 32 个（服务器永不响应），第 33 个抛事故
  const 号们 = [];
  for (let i = 0; i < 32; i++) 号们.push(Number(await 页.调('发起网页请求', JSON.stringify({网址: '/hang' + i, 超时毫秒: 600000}))));
  await 试({网址: '/no33'}, /并发达到上限/);
  assert.equal(await 页.调('取消网页请求', 号们[0]), 'true');
  assert.ok(Number(await 页.调('发起网页请求', JSON.stringify({网址: '/refill', 超时毫秒: 600000}))) > 号们[31]);
}));

test('网页请求：阻塞变体返回响应 JSON，等待期间到达的事件不丢；网络失败不抛事故', 用服务((请求, 响应) => {
  if (请求.url === '/drop') { 请求.socket.destroy(); return; }
  setTimeout(() => { 响应.writeHead(200, {'Content-Type': 'text/plain'}); 响应.end('同步之果'); }, 150);
}, {}, async (页, 服) => {
  const 定时号 = Number(await 页.调('启动一次定时', 30, '期间'));
  const 结果 = JSON.parse(await 页.调('请求网页文字', JSON.stringify({网址: '/x', 标记: '同步'})));
  assert.equal(结果.成, true);
  assert.equal(结果.正文, '同步之果');
  assert.equal(结果.标记, '同步');
  const 事 = await 事件(页); // 阻塞等待期间到期的定时事件仍在队列里
  assert.equal(事.类型, '定时');
  assert.equal(事.订阅号, 定时号);
  const 失 = JSON.parse(await 页.调('请求网页文字', JSON.stringify({网址: '/drop'})));
  assert.equal(失.成, false);
  assert.equal(失.原因, '网络错误');
  assert.equal(页.宿主.状态().请求数, 0, '阻塞请求不留登记');
  await 失败(页.调('请求网页文字', JSON.stringify({网址: 'https://别处.站/'})), /同源/);
}));

// ---------------------------------------------------------------------------
// 六、网页事件源（SSE）
// ---------------------------------------------------------------------------
const 帧 = (项) => (项.id !== undefined ? `id: ${项.id}\n` : '') + (项.event ? `event: ${项.event}\n` : '') + (项.retry ? `retry: ${项.retry}\n` : '') +
  String(项.data).split('\n').map(行 => `data: ${行}`).join('\n') + '\n\n';
const 头SSE = {'Content-Type': 'text/event-stream; charset=utf-8', 'Cache-Control': 'no-cache'};
const 取事件源事件 = async (页, 号) => { const 事 = await 事件(页); assert.equal(事.类型, '事件流'); assert.equal(事.订阅号, 号); return 事; };

test('网页事件源：打开、成批消息（含事件名、事件号、多行数据）', 用服务((请求, 响应) => {
  响应.writeHead(200, 头SSE);
  响应.write(': 注释\n\n' + 帧({id: 1, data: '甲'}) + 帧({id: 2, event: 'x', data: '第一\n第二'}));
  // 保持连接
}, {}, async (页, 服) => {
  const 号 = Number(await 页.调('打开事件源', '/cloud/api/events', ''));
  assert.ok(号 >= 1);
  const 开 = await 取事件源事件(页, 号);
  assert.equal(开.名称, '打开');
  assert.deepEqual(开.详情, {重连: false});
  const 收 = [];
  while (收.length < 2) {
    const 事 = await 取事件源事件(页, 号);
    assert.equal(事.名称, '消息');
    assert.ok(事.详情.批.length >= 1 && 事.详情.批.length <= 256);
    收.push(...事.详情.批);
  }
  assert.deepEqual(收, [{事件号: '1', 事件名: 'message', 数据: '甲'}, {事件号: '2', 事件名: 'x', 数据: '第一\n第二'}]);
  assert.equal(await 页.调('关闭事件源', 号), 'true');
  assert.equal(await 页.调('关闭事件源', 号), 'false');
}));

test('网页事件源：流结束后自动带 Last-Event-ID 重连，服务器 retry 字段生效', async () => {
  const 见到 = [];
  let 次 = 0;
  const 服 = await 起服务((请求, 响应) => {
    见到.push(请求.headers['last-event-id'] ?? null);
    响应.writeHead(200, 头SSE);
    次++;
    if (次 === 1) { 响应.end(帧({id: 7, data: '一', retry: 120})); return; }
    响应.write(帧({id: 8, data: '二'}));
  });
  const 页 = await 启动探针({url: 服.源 + '/cloud/', 网络: (...参) => fetch(...参)});
  try {
    const 号 = Number(await 页.调('打开事件源', '/e', JSON.stringify({起始事件号: '5', 重连毫秒: 5000})));
    const 全 = [];
    const 起 = Date.now();
    while (!全.some(事 => 事.名称 === '打开' && 事.详情.重连 === true) || !全.some(事 => 事.名称 === '消息' && 事.详情.批.some(项 => 项.数据 === '二'))) {
      全.push(await 取事件源事件(页, 号));
      if (全.length > 20) throw Error('事件过多');
    }
    assert.ok(Date.now() - 起 < 3000, '重连间隔取服务器的 retry:120 而不是 5000');
    assert.deepEqual(见到, ['5', '7'], '首连用起始事件号，重连用最后事件号');
    const 名们 = 全.map(事 => 事.名称 + (事.详情.状态 ? ':' + 事.详情.状态 + ':' + 事.详情.原因 : ''));
    assert.deepEqual(名们.filter(名 => 名 !== '消息'), ['打开', '错误:重连中:服务端关闭', '打开']);
  } finally { await 页.关(); await 服.关(); }
});

test('网页事件源：HTTP 非 200 与内容类型不对是致命错误，已关闭后不再重连', async () => {
  let 请求数 = 0;
  const 服 = await 起服务((请求, 响应) => {
    请求数++;
    if (请求.url === '/404') { 响应.writeHead(404); 响应.end('无'); return; }
    响应.writeHead(200, {'Content-Type': 'text/html'}); 响应.end('<p>不是事件流</p>');
  });
  const 页 = await 启动探针({url: 服.源 + '/cloud/', 网络: (...参) => fetch(...参)});
  try {
    const 甲 = Number(await 页.调('打开事件源', '/404', ''));
    const 事甲 = await 取事件源事件(页, 甲);
    assert.equal(事甲.名称, '错误');
    assert.deepEqual(事甲.详情, {状态: '已关闭', 原因: 'HTTP 状态 404', HTTP状态: 404});
    const 乙 = Number(await 页.调('打开事件源', '/html', ''));
    const 事乙 = await 取事件源事件(页, 乙);
    assert.deepEqual(事乙.详情, {状态: '已关闭', 原因: '内容类型不是 text/event-stream'});
    await 睡(200);
    assert.equal(请求数, 2, '致命错误不重连');
    assert.equal(await 页.调('关闭事件源', 甲), 'false', '已因致命错误关闭且事件取尽');
  } finally { await 页.关(); await 服.关(); }
});

test('网页事件源：背压——应用不取事件时暂停读流，取走后五千条事件无一丢失、无一重复、保持顺序', async () => {
  let 写完 = false;
  let 已写 = 0;
  const 服 = await 起服务(async (请求, 响应) => {
    响应.writeHead(200, 头SSE);
    for (let i = 1; i <= 5000; i++) {
      const 空 = 响应.write(帧({id: i, data: JSON.stringify({序: i, 文: '填充'.repeat(50)})}));
      已写 = i;
      if (!空) await new Promise(完成 => 响应.once('drain', 完成));
    }
    写完 = true;
  });
  const 页 = await 启动探针({url: 服.源 + '/cloud/', 网络: (...参) => fetch(...参)});
  try {
    const 号 = Number(await 页.调('打开事件源', '/e', ''));
    await 睡(600); // 应用不取事件
    const 积压 = 页.宿主.状态().各类积压.事件流;
    assert.ok(积压 >= 1 && 积压 <= 12, `未取走的事件流事件应不超过约 8 批，实际 ${积压}`);
    assert.equal(页.宿主.状态().各类丢弃.事件流 ?? 0, 0);
    // 取走并核对
    const 见 = [];
    while (见.length < 5000) {
      const 事 = await 取事件源事件(页, 号);
      if (事.名称 !== '消息') continue;
      for (const 项 of 事.详情.批) 见.push(Number(项.事件号));
    }
    assert.equal(见.length, 5000);
    assert.deepEqual(见, Array.from({length: 5000}, (_, i) => i + 1));
    assert.equal(页.宿主.状态().各类丢弃.事件流 ?? 0, 0);
    await 等待(() => 写完, '服务器写完');
    assert.equal(已写, 5000);
  } finally { await 页.关(); await 服.关(); }
});

test('网页事件源：违规调用抛事故；第九个事件源抛事故；关闭后保证不再收到该号事件', 用服务((请求, 响应) => {
  响应.writeHead(200, 头SSE);
  const 钟 = setInterval(() => { try { 响应.write(帧({data: 'x'})); } catch { /* 连接已断 */ } }, 20);
  响应.on('close', () => clearInterval(钟));
  响应.on('error', () => {});
}, {}, async (页, 服) => {
  await 失败(页.调('打开事件源', 'https://别处.站/e', ''), /同源/);
  await 失败(页.调('打开事件源', 'e', ''), /站内路径/);
  await 失败(页.调('打开事件源', '/e', '不是json'), /不是有效 JSON/);
  await 失败(页.调('打开事件源', '/e', '{"未知":1}'), /未知字段/);
  await 失败(页.调('打开事件源', '/e', '{"重连毫秒":50}'), /重连毫秒/);
  await 失败(页.调('打开事件源', '/e', JSON.stringify({起始事件号: 'a\nb'})), /起始事件号无效/);
  const 号们 = [];
  for (let i = 0; i < 8; i++) 号们.push(Number(await 页.调('打开事件源', '/e', '')));
  await 失败(页.调('打开事件源', '/e', ''), /上限/);
  await 睡(80);
  assert.equal(await 页.调('关闭事件源', 号们[0]), 'true');
  assert.equal(页.宿主.状态().各类积压.事件流 > 0, true);
  // 关闭之后，队列里该号的事件被移除；取到的一定是别的号
  for (let i = 0; i < 5; i++) assert.notEqual((await 事件(页)).订阅号, 号们[0]);
  assert.ok(Number(await 页.调('打开事件源', '/e', '')) > 号们[7], '关闭一个后可再打开');
}));

// ---------------------------------------------------------------------------
// 七、网页编译（注入假的编译客户端）
// ---------------------------------------------------------------------------
const 假编译客户端 = (行为 = {}) => {
  const 记录 = {输入: [], 停止次数: 0};
  let 收尾 = null;
  return {
    记录,
    浏览器编译: (输入, 仅编译, 报告) => new Promise(完成 => {
      记录.输入.push([输入, 仅编译]);
      收尾 = 完成;
      (async () => {
        await 睡(5);
        for (const 事 of 行为.事件们 ?? []) 报告(事);
        if (行为.挂起) return; // 等停止
        await 睡(行为.耗时 ?? 5);
        完成(行为.结果 ?? {ok: true, stdout: '标准输出', stderr: '', artifact: {sha256: 'a'.repeat(64)}});
      })();
    }),
    停止编译: () => { 记录.停止次数++; 收尾?.({ok: false, phase: 'compile', error: '已停止'}); }
  };
};
const 取编译事件 = async (页, 号) => { const 事 = await 事件(页); assert.equal(事.类型, '编译'); assert.equal(事.订阅号, 号); return 事; };
const 取完整编译 = async (页, 号) => {
  const 进度 = [];
  for (;;) {
    const 事 = await 取编译事件(页, 号);
    if (事.名称 === '进度') { 进度.push(...事.详情.批); continue; }
    assert.equal(事.名称, '完成');
    return {进度, 完成: 事.详情};
  }
};

test('网页编译：进度成批、完成事件含全部字段；文件 JSON 原样交给客户端；不截断输出', async () => {
  const 事件们 = [{type: 'stage', phase: 'load', label: '正在加载'}];
  for (let i = 0; i < 100; i++) 事件们.push({type: 'output', stream: i % 2 ? 'stdout' : 'stderr', text: `正在编译：/库/${i}。豫\n`});
  事件们.push({type: 'diagnostic', text: '错误：某处'}, {type: '未知', text: '忽略'});
  const 客 = 假编译客户端({事件们, 结果: {ok: false, phase: 'compile', stdout: 'O'.repeat(20000), stderr: 'E', error: '类型错误', artifact: null}});
  const 页 = await 启动探针({编译客户端: 客});
  try {
    const 文件 = {'入口。豫': '「打印行」于『你好』。', '库/甲。豫': '甲😀'};
    const 号 = Number(await 页.调('启动本地编译', JSON.stringify(文件)));
    assert.ok(号 >= 1);
    const {进度, 完成} = await 取完整编译(页, 号);
    assert.deepEqual(客.记录.输入, [[{files: 文件}, true]]);
    assert.deepEqual(进度[0], {类: '阶段', 阶段: 'load', 标签: '正在加载'});
    const 输出们 = 进度.filter(项 => 项.类 === '输出');
    assert.equal(输出们.length, 100, '一百条输出一条不少');
    assert.deepEqual(输出们.map(项 => 项.文字), Array.from({length: 100}, (_, i) => `正在编译：/库/${i}。豫\n`), '顺序不变');
    assert.equal(输出们[0].流, 'stderr');
    assert.equal(输出们[1].流, 'stdout');
    assert.deepEqual(进度.at(-1), {类: '诊断', 文字: '错误：某处'});
    assert.equal(进度.length, 102, '未知类型的报告被忽略');
    assert.deepEqual(完成, {成: false, 标准输出: 'O'.repeat(20000), 标准错误: 'E', 错误: '类型错误', 阶段: 'compile', 产物摘要: ''});
  } finally { await 页.关(); }
});

test('网页编译：成功时产物摘要为客户端所给；同一时刻只有一个编译；完成后可再启动', async () => {
  const 客 = 假编译客户端({耗时: 60});
  const 页 = await 启动探针({编译客户端: 客});
  try {
    const 号 = Number(await 页.调('启动本地编译', '{"a。豫":"x"}'));
    await 失败(页.调('启动本地编译', '{"b。豫":"y"}'), /已有本地编译在运行/);
    const {完成} = await 取完整编译(页, 号);
    assert.deepEqual(完成, {成: true, 标准输出: '标准输出', 标准错误: '', 错误: '', 阶段: '', 产物摘要: 'a'.repeat(64)});
    const 号乙 = Number(await 页.调('启动本地编译', '{"b。豫":"y"}'));
    assert.ok(号乙 > 号);
    assert.equal((await 取完整编译(页, 号乙)).完成.成, true);
  } finally { await 页.关(); }
});

test('网页编译：停止本地编译返回阳并收到“已停止”的完成事件；没有编译时返回阴', async () => {
  const 客 = 假编译客户端({挂起: true});
  const 页 = await 启动探针({编译客户端: 客});
  try {
    assert.equal(await 页.调('停止本地编译'), 'false');
    const 号 = Number(await 页.调('启动本地编译', '{"a。豫":"x"}'));
    await 等待(() => 客.记录.输入.length === 1, '客户端已开始');
    assert.equal(await 页.调('停止本地编译'), 'true');
    const {完成} = await 取完整编译(页, 号);
    assert.equal(完成.成, false);
    assert.equal(完成.错误, '已停止');
    assert.equal(客.记录.停止次数, 1);
    assert.equal(await 页.调('停止本地编译'), 'false');
  } finally { await 页.关(); }
});

test('网页编译：客户端不可用时以完成事件报告；文件 JSON 违规抛事故；可用性检查浏览器能力', async () => {
  const 页 = await 启动探针({导入模块: async () => { throw Error('找不到模块'); }});
  try {
    assert.equal(await 页.调('本地编译可用'), 'false', 'JSDOM 没有 Worker 与 DecompressionStream');
    const 号 = Number(await 页.调('启动本地编译', '{"a。豫":"x"}'));
    const {完成} = await 取完整编译(页, 号);
    assert.equal(完成.成, false);
    assert.match(完成.错误, /^编译客户端不可用：找不到模块/);
    await 失败(页.调('启动本地编译', '不是json'), /不是有效 JSON/);
    await 失败(页.调('启动本地编译', '{}'), /至少需要一个文件/);
    await 失败(页.调('启动本地编译', '[1]'), /须为/);
    await 失败(页.调('启动本地编译', '{"a。豫":1}'), /内容须为文字/);
    await 失败(页.调('启动本地编译', JSON.stringify({['x'.repeat(513)]: 'y'})), /路径无效/);
  } finally { await 页.关(); }
  const 页乙 = await 启动探针({窗扩展: 窗 => { 窗.Worker = function () {}; 窗.DecompressionStream = function () {}; 窗.WebAssembly = {promising: () => {}}; }});
  try { assert.equal(await 页乙.调('本地编译可用'), 'true'); } finally { await 页乙.关(); }
});

test('网页编译：默认从页面基址目录加载 编译/客户端.mjs', async () => {
  const 加载过 = [];
  const 客 = 假编译客户端();
  const 页 = await 启动探针({url: 'https://yuyan-lang.org/cloud/?service=code', 导入模块: async 地址 => { 加载过.push(地址); return 客; }});
  try {
    const 号 = Number(await 页.调('启动本地编译', '{"a。豫":"x"}'));
    await 取完整编译(页, 号);
    assert.deepEqual(加载过, ['https://yuyan-lang.org/cloud/%E7%BC%96%E8%AF%91/%E5%AE%A2%E6%88%B7%E7%AB%AF.mjs']);
  } finally { await 页.关(); }
});

// ---------------------------------------------------------------------------
// 八、网页应用（注入假的模块加载器）
// ---------------------------------------------------------------------------
test('网页应用：加载同源入口模块、调用启动函数并等它就绪；同一路径不可重复；限 8 个', async () => {
  const 加载 = [];
  const 关闭了 = [];
  const 页 = await 启动探针({导入模块: async 地址 => {
    加载.push(地址);
    return {启动豫言浏览器应用: async () => ({就绪: 睡(30), 完成: new Promise(() => {}), 关闭: () => 关闭了.push(地址)})};
  }, url: 'https://yuyan-lang.org/cloud/?service=ai-buy'});
  try {
    const 起 = Date.now();
    await 页.调('启动页面应用', '/AI市场应用/入口.mjs');
    assert.ok(Date.now() - 起 >= 25, '等子应用就绪后才返回');
    assert.deepEqual(加载, ['https://yuyan-lang.org/AI%E5%B8%82%E5%9C%BA%E5%BA%94%E7%94%A8/%E5%85%A5%E5%8F%A3.mjs']);
    await 失败(页.调('启动页面应用', '/AI市场应用/入口.mjs'), /已启动/);
    for (let i = 1; i < 8; i++) await 页.调('启动页面应用', `/应用${i}/入口.mjs`);
    await 失败(页.调('启动页面应用', '/应用九/入口.mjs'), /上限/);
  } finally { await 页.关(); }
  assert.equal(关闭了.length, 8, '宿主关闭时子应用一并关闭');
});

test('网页应用：路径不合规、模块缺少启动函数、启动失败与就绪超时都抛事故', async () => {
  const 页 = await 启动探针({页面应用超时: 100, 导入模块: async 地址 => {
    if (地址.includes('%E7%BC%BA%E5%87%BD%E6%95%B0')) return {};
    if (地址.includes('%E6%97%A0%E6%B3%95%E5%8A%A0%E8%BD%BD')) throw Error('模块 404');
    if (地址.includes('%E5%86%99%E4%B8%8D%E5%87%BA')) return {启动豫言浏览器应用: async () => { throw Error('接口核对失败'); }};
    if (地址.includes('%E6%B0%B8%E4%B8%8D')) return {启动豫言浏览器应用: async () => ({就绪: new Promise(() => {}), 完成: new Promise(() => {}), 关闭() {}})};
    return {启动豫言浏览器应用: async () => ({就绪: Promise.resolve(), 完成: new Promise(() => {}), 关闭() {}})};
  }});
  try {
    for (const 坏 of ['', '入口.mjs', '/x/程序.wasm', '/入口.mjsx', '//x/入口.mjs', '/x/../入口.mjs', '/./入口.mjs', '/x/入口.mjs?a=1', '/x/入口.mjs#b', '/x\\y/入口.mjs', '/%2e%2e/入口.mjs', '/' + 'x'.repeat(520) + '/入口.mjs']) {
      await 失败(页.调('启动页面应用', 坏), /页面应用路径/, `应拒绝：${坏.slice(0, 30)}`);
    }
    await 失败(页.调('启动页面应用', '/缺函数/入口.mjs'), /没有导出 启动豫言浏览器应用/);
    await 失败(页.调('启动页面应用', '/无法加载/入口.mjs'), /模块 404/);
    await 失败(页.调('启动页面应用', '/写不出/入口.mjs'), /接口核对失败/);
    await 失败(页.调('启动页面应用', '/永不就绪/入口.mjs'), /未就绪/);
    // 失败的路径可以再试（前面的失败没有占位）；成功一次即可
    await 页.调('启动页面应用', '/可用/入口.mjs');
  } finally { await 页.关(); }
});
