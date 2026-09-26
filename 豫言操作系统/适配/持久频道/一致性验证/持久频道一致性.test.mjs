// 持久频道：真实 Wasm + Node 的一致性验证。
// 用同一个宿主对象上并发的多个 durable-fetch 事件模拟同一个持久对象实例的多个事件；
// 宿主对象上的频道注册表随该对象存续（与真实 Durable Object 的作用域一致）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {载入应用, 创建模拟D1, 日志表, 读日志, 睡, 等到, 流读取器, 请求} from './工具.mjs';

const 新宿主 = await 载入应用();
const 事件表 = 'CREATE TABLE 事件(id INTEGER PRIMARY KEY AUTOINCREMENT, 文 TEXT);';
async function 起() {
  const {sql, DB} = 创建模拟D1(日志表 + 事件表);
  const 宿主 = 新宿主({D1: ['DB']});
  const 状态 = {waitUntil() {}};
  const 调 = (路径, 选项) => 宿主.durableFetch(请求(路径, 选项), {DB}, 状态);
  const 文 = async (路径, 选项) => (await 调(路径, 选项)).text();
  const 流 = async 路径 => new 流读取器(await 调(路径));
  return {宿主, 调, 文, 流, sql, DB, 日志: 键 => 读日志(sql, 键), 关: () => sql.close()};
}
const 编 = encodeURIComponent;

test('跨事件订阅与发布：订阅之后发布的一定送达，事件结束后订阅数归零', async () => {
  const 环 = await 起();
  try {
    const 读 = await 环.流('/订阅转发?名=c&超时=3000&最多=3');
    assert.equal(await 环.文('/订阅数?名=c'), '1');
    assert.equal(await 环.文('/发布?名=c&文=' + 编('甲')), '1');
    assert.equal(await 环.文('/发布?名=c&文=' + 编('乙丙')), '1');
    assert.equal(await 环.文('/发布?名=c&文=' + 编('丁')), '1');
    assert.deepEqual(await 读.读完(), ['data: 消息:甲', 'data: 消息:乙丙', 'data: 消息:丁', 'data: 已满']);
    await 等到(async () => await 环.文('/订阅数?名=c') === '0', {说明: '订阅取消'});
    assert.equal(await 环.文('/发布?名=c&文=x'), '0', '无订阅时送达 0');
  } finally { 环.关(); }
});

test('订阅之前发布的不补发', async () => {
  const 环 = await 起();
  try {
    assert.equal(await 环.文('/发布?名=c&文=早'), '0');
    const 读 = await 环.流('/订阅转发?名=c&超时=300&最多=0');
    assert.equal(await 环.文('/发布?名=c&文=' + 编('晚')), '1');
    assert.deepEqual(await 读.读完(), ['data: 消息:晚', 'data: 超时']);
  } finally { 环.关(); }
});

test('多订阅者：每人按序各得一份，送达数等于订阅数', async () => {
  const 环 = await 起();
  try {
    const 读们 = await Promise.all([1, 2, 3].map(() => 环.流('/订阅转发?名=c&超时=5000&最多=200')));
    assert.equal(await 环.文('/订阅数?名=c'), '3');
    assert.equal(await 环.文('/发布多?名=c&前缀=m&数=200'), '600');
    for (const 读 of 读们) {
      const 帧们 = await 读.读完();
      assert.equal(帧们.length, 201);
      for (let i = 0; i < 200; i++) assert.equal(帧们[i], 'data: 消息:m' + i);
      assert.equal(帧们[200], 'data: 已满');
    }
  } finally { 环.关(); }
});

test('多个发布者并发：每个订阅内保序（同一发布者的消息相对次序不变），且无丢失', async () => {
  const 环 = await 起();
  try {
    const 读 = await 环.流('/订阅转发?名=c&超时=5000&最多=300');
    await Promise.all(['a', 'b', 'c'].map(前缀 => 环.文(`/发布多?名=c&前缀=${前缀}&数=100`)));
    const 帧们 = (await 读.读完()).filter(帧 => 帧.startsWith('data: 消息:'));
    assert.equal(帧们.length, 300);
    for (const 前缀 of ['a', 'b', 'c']) {
      const 序 = 帧们.filter(帧 => 帧.startsWith('data: 消息:' + 前缀)).map(帧 => Number(帧.slice(('data: 消息:' + 前缀).length)));
      assert.deepEqual(序, Array.from({length: 100}, (_, i) => i));
    }
  } finally { 环.关(); }
});

test('等待超时：到期返回状态 1，不被订阅数探测拉长或缩短', async () => {
  const 环 = await 起();
  try {
    let 结果 = (await 环.文('/等待?名=c&超时=200')).split('|');
    assert.equal(结果[0], '1');
    assert.equal(结果[1], '');
    assert.ok(Number(结果[2]) >= 190 && Number(结果[2]) < 800, '用时 ' + 结果[2]);
    // 探测持续 1.5 秒（远超等待时长）：若每次探测都重新计时，等待会被拉长到探测结束之后才返回
    const 等 = 环.文('/等待?名=t&超时=600');
    await 睡(60);
    let 等毕 = false, 停 = false;
    setTimeout(() => { 停 = true; }, 1500);
    const 探测者 = (async () => { while (!停) { const 数 = await 环.文('/订阅数?名=t'); if (!等毕) assert.equal(数, '1'); await 睡(40); } })();
    结果 = (await 等).split('|');
    等毕 = true;
    await 探测者;
    assert.equal(结果[0], '1');
    assert.ok(Number(结果[2]) >= 570 && Number(结果[2]) < 1000, '探测不应改变超时长短，实得 ' + 结果[2]);
  } finally { 环.关(); }
});

test('等待超时参数越界抛豫言异常；零超时为立即轮询', async () => {
  const 环 = await 起();
  try {
    assert.equal(await 环.文('/等待?名=c&超时=-1'), '错误:持久频道等待超时须在 0 至 3600000 毫秒');
    assert.equal(await 环.文('/等待?名=c&超时=3600001'), '错误:持久频道等待超时须在 0 至 3600000 毫秒');
    const 零 = (await 环.文('/等待?名=c&超时=0')).split('|');
    assert.equal(零[0], '1');
    assert.ok(Number(零[2]) < 100);
    assert.equal(await 环.文('/大量?名=c&数=10'), '10|10', '同一事件内先发后取：零超时逐条取出已排队消息');
  } finally { 环.关(); }
});

test('关闭：已排队的消息先交付，随后各订阅得状态 2 与原因；同名频道可重新订阅', async () => {
  const 环 = await 起();
  try {
    const 甲 = await 环.流('/订阅转发?名=c&超时=5000&最多=0');
    const 乙 = await 环.流('/订阅转发?名=c&超时=5000&最多=0');
    assert.equal(await 环.文('/发布多?名=c&前缀=m&数=3'), '6');
    assert.equal(await 环.文('/关闭?名=c&因=' + 编('收摊')), '2');
    for (const 读 of [甲, 乙]) assert.deepEqual(await 读.读完(), ['data: 消息:m0', 'data: 消息:m1', 'data: 消息:m2', 'data: 终:收摊']);
    assert.equal(await 环.文('/订阅数?名=c'), '0');
    assert.equal(await 环.文('/关闭?名=nochan&因=x'), '0');
    const 新 = await 环.流('/订阅转发?名=c&超时=300&最多=0');
    assert.equal(await 环.文('/订阅数?名=c'), '1');
    assert.equal(await 环.文('/发布?名=c&文=' + 编('新代')), '1');
    assert.deepEqual(await 新.读完(), ['data: 消息:新代', 'data: 超时']);
  } finally { 环.关(); }
});

test('取消订阅：幂等；取消后再等待返回状态 2；订阅数随之减少', async () => {
  const 环 = await 起();
  try {
    assert.equal(await 环.文('/取消后?名=c'), '2:订阅已取消|2:订阅已取消|0');
  } finally { 环.关(); }
});

test('订阅柄：无效柄与另一个事件的柄都抛豫言异常', async () => {
  const 环 = await 起();
  try {
    assert.equal(await 环.文('/坏柄?柄=xyz'), '持久频道订阅柄无效：不是本次事件创建的订阅|持久频道订阅柄无效：不是本次事件创建的订阅');
    const 甲 = await 环.流('/给柄?名=c');
    const 柄 = (await 甲.下一帧()).replace(/^data: /, '');
    assert.match(柄, /^[0-9a-f-]{13}:\d+$/);
    // 另一事件自己也有订阅（句柄号可能相同），拿甲的柄仍然无效
    assert.equal(await 环.文('/用柄?名=d&柄=' + 编(柄)), '持久频道订阅柄无效：不是本次事件创建的订阅');
    assert.deepEqual(await 甲.读完(), []);
  } finally { 环.关(); }
});

test('每频道订阅上限 256：第 257 个订阅抛可捕获的豫言异常', async () => {
  const 环 = await 起();
  try {
    assert.equal(await 环.文('/订阅上限?名=c&次数=300'), '256|256|持久频道订阅数已达上限 256|0');
  } finally { 环.关(); }
});

test('每持久对象频道上限 64：第 65 个频道由宿主拒绝', async () => {
  const 环 = await 起();
  try {
    assert.equal(await 环.文('/频道上限?前缀=p&数=64'), '64');
  } finally { 环.关(); }
  const 环2 = await 起();
  try {
    await assert.rejects(环2.调('/频道上限?前缀=p&数=65'), /云工广播频道达到上限/);
  } finally { 环2.关(); }
});

test('积压超过 1 MiB 的订阅被断开：状态 2 与原因，之后不再送达', async () => {
  const 环 = await 起();
  try {
    const 文 = await 环.文('/积压?名=c&数=20&长=65536');
    const [送达, 状态, 原因, 再状态, 订数] = 文.split('|');
    const 序列 = 送达.split(',').map(Number);
    assert.equal(序列.length, 20);
    const 首零 = 序列.indexOf(0);
    assert.ok(首零 > 10 && 首零 < 20, '前若干条送达，随后积压超限而不再送达：' + 送达);
    assert.ok(序列.slice(0, 首零).every(值 => 值 === 1) && 序列.slice(首零).every(值 => 值 === 0));
    assert.equal(状态, '2');
    assert.equal(原因, '订阅积压超过 1 MiB，已被断开');
    assert.equal(再状态, '2');
    assert.equal(订数, '0');
  } finally { 环.关(); }
});

test('一个订阅内收发上万条消息不耗尽宿主句柄', async () => {
  const 环 = await 起();
  try {
    assert.equal(await 环.文('/大量?名=c&数=5000'), '5000|5000');
  } finally { 环.关(); }
});

test('文字大小与内容规则：0 与 65536 字节可发，65537 与 0xFF 起首被拒', async () => {
  const 环 = await 起();
  try {
    const 读 = await 环.流('/订阅转发?名=c&超时=3000&最多=4');
    assert.equal(await 环.文('/发布大?名=c&字=a&长=65536'), '1');
    assert.equal(await 环.文('/发布大?名=c&字=多&长=21845'), '1');
    assert.equal(await 环.文('/发布大?名=c&字=空&长=0'), '1');
    assert.equal(await 环.文('/发布大?名=c&字=a&长=65537'), '错误:持久频道文字超过 65536 字节');
    assert.equal(await 环.文('/发布大?名=c&字=多&长=21846'), '错误:持久频道文字超过 65536 字节');
    assert.equal(await 环.文('/发布大?名=c&字=高&长=1'), '错误:持久频道文字不得以字节 0xFF 起首');
    assert.equal(await 环.文('/发布?名=c&文=' + 编('尾')), '1');
    const 帧们 = await 读.读完();
    assert.equal(帧们[0], 'data: 消息:' + 'a'.repeat(65536));
    assert.equal(帧们[1], 'data: 消息:' + '豫'.repeat(21845));
    assert.equal(帧们[2], 'data: 消息:');
    assert.equal(帧们[3], 'data: 消息:尾');
  } finally { 环.关(); }
});

test('消息按字节原样往返：NUL、控制字节、换行、代理对外的多字节字符都不被截断或改写', async () => {
  const 环 = await 起();
  try {
    const 读 = await 环.流('/订阅转发?名=c&超时=3000&最多=2');
    const 甲 = 'a\u0000b\u0001c\u007fd';
    const 乙 = '第一行\n第二行\r\n😀豫言';
    assert.equal(await 环.文('/发布?名=c&文=' + 编(甲)), '1');
    assert.equal(await 环.文('/发布?名=c&文=' + 编(乙)), '1');
    const 帧们 = await 读.读完();
    assert.equal(帧们[0], 'data: 消息:' + 甲);
    assert.equal(帧们[1] + '\n\n', 'data: 消息:' + 乙 + '\n\n');
  } finally { 环.关(); }
});

test('频道名规则：1 至 64 字节，限 A-Z a-z 0-9 . _ : -', async () => {
  const 环 = await 起();
  try {
    const 错 = '错误:持久频道名';
    for (const 名 of ['', 'a b', '中', 'a/b', 'a\\b', 'a,b', 'x'.repeat(65), 'a?b']) {
      assert.ok((await 环.文('/订阅数?名=' + 编(名))).startsWith(错), '应拒绝名 ' + JSON.stringify(名));
      assert.ok((await 环.文('/发布?名=' + 编(名) + '&文=x')).startsWith(错));
      assert.ok((await 环.文('/关闭?名=' + 编(名) + '&因=x')).startsWith(错));
    }
    for (const 名 of ['a', 'A.b_c:d-e', 'x'.repeat(64), '0']) {
      assert.equal(await 环.文('/订阅数?名=' + 编(名)), '0');
    }
    assert.equal(await 环.文('/订阅数?名=' + 编('x'.repeat(65))), '错误:持久频道名须为 1 至 64 字节');
    assert.equal(await 环.文('/订阅数?名=' + 编('a b')), '错误:持久频道名只许 A-Z a-z 0-9 . _ : -');
  } finally { 环.关(); }
});

test('只能在持久对象事件内使用：普通 fetch 事件得到可捕获的豫言异常', async () => {
  const 环 = await 起();
  try {
    const 回 = await 环.宿主.fetch(请求('/普通事件'), {DB: 环.DB});
    assert.equal(await 回.text(), '持久频道只能在持久对象事件内使用');
  } finally { 环.关(); }
});

test('频道属于宿主对象（即持久对象实例）：不同实例互不相通', async () => {
  const 甲 = await 起();
  const 乙 = await 起();
  try {
    const 读 = await 甲.流('/订阅转发?名=c&超时=300&最多=0');
    assert.equal(await 乙.文('/发布?名=c&文=x'), '0');
    assert.equal(await 乙.文('/订阅数?名=c'), '0');
    assert.equal(await 甲.文('/订阅数?名=c'), '1');
    assert.deepEqual(await 读.读完(), ['data: 超时']);
  } finally { 甲.关(); 乙.关(); }
});

// ——先订阅、再回放、再合并实时（以事件序号去重）——
const 追加 = (环, 文) => 环.文('/追加?文=' + 编(文));
function 序号们(帧们) { return 帧们.filter(帧 => 帧.startsWith('id: ')).map(帧 => Number(帧.split('\n')[0].slice(4))); }

test('先订阅再回放：订阅与读头之间到来的事件恰好出现一次，其后的实时事件继续送达', async () => {
  const 环 = await 起();
  try {
    for (let i = 1; i <= 5; i++) await 追加(环, '事件' + i);
    const 等流 = 环.调('/事件流?after=0&时限=900&窗口=250');
    await 等到(async () => await 环.文('/订阅数?名=events') === '1', {说明: '订阅建立'});
    // 窗口内到来的事件：既会被回放读到，又会通过订阅送达，必须去重
    for (let i = 6; i <= 8; i++) await 追加(环, '事件' + i);
    const 回 = await 等流;
    const 读 = new 流读取器(回);
    await 睡(50);
    for (let i = 9; i <= 10; i++) await 追加(环, '事件' + i);
    const 帧们 = await 读.读完();
    assert.deepEqual(序号们(帧们), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    帧们.filter(帧 => 帧.startsWith('id: ')).forEach(帧 => { const [头, 数据] = 帧.split('\n'); assert.equal(数据, 'data: 事件' + 头.slice(4)); });
    assert.ok(帧们.includes(': connected') && 帧们.at(-1) === 'data: 结束');
    await 等到(() => 环.日志('事件流收尾').length === 1, {说明: '收尾'});
    assert.equal(环.日志('事件流收尾')[0], '8|8|3', '头=8，回放 8 条，窗口内的 3 条实时重复被略去');
  } finally { 环.关(); }
});

test('从中间序号续读：只回放 after 之后的事件', async () => {
  const 环 = await 起();
  try {
    for (let i = 1; i <= 6; i++) await 追加(环, '事件' + i);
    const 读 = await 环.流('/事件流?after=4&时限=200');
    const 帧们 = await 读.读完();
    assert.deepEqual(序号们(帧们), [5, 6]);
  } finally { 环.关(); }
});

test('重连凭 Last-Event-ID：只回放该序号之后的事件（浏览器 EventSource 的重连方式）', async () => {
  const 环 = await 起();
  try {
    for (let i = 1; i <= 6; i++) await 追加(环, '事件' + i);
    const 回 = await 环.宿主.durableFetch(请求('/事件流?时限=200', {headers: {'Last-Event-ID': '3'}}), {DB: 环.DB}, {waitUntil() {}});
    assert.deepEqual(序号们(await new 流读取器(回).读完()), [4, 5, 6]);
  } finally { 环.关(); }
});

test('并发追加与多个不同时刻开始的事件流：每个流恰好各见每个事件一次', async () => {
  const 环 = await 起();
  try {
    const 追加者 = 号 => (async () => { for (let i = 0; i < 25; i++) await 追加(环, `事件${号 * 100 + i}`); })();
    const 全体追加 = Promise.all([1, 2, 3, 4].map(追加者));
    const 读们 = [];
    for (const 窗口 of [0, 5, 20]) { 读们.push(环.流(`/事件流?after=0&时限=700&窗口=${窗口}`)); await 睡(15); }
    await 全体追加;
    for (const 读 of await Promise.all(读们)) {
      const 帧们 = await 读.读完();
      const 号们 = 序号们(帧们);
      assert.equal(号们.length, 100, `流应见 100 个事件，实得 ${号们.length}`);
      assert.deepEqual([...号们].sort((甲, 乙) => 甲 - 乙), Array.from({length: 100}, (_, i) => i + 1), '无缺无重');
    }
  } finally { 环.关(); }
});

test('收尾流：事件结束时宿主自动取消本事件遗留的订阅', async () => {
  const 环 = await 起();
  try {
    assert.equal(await 环.文('/订阅不取消?名=leak'), '已订阅，未取消');
    assert.equal(await 环.文('/订阅数?名=leak'), '0', '事件已结束，遗留的订阅应被回收');
  } finally { 环.关(); }
});
