// 文言：以真 Wasm 与模拟邮件绑定验邮件发送适配。汉语：加载已构建的“邮件发送一致性”产物；邮件绑定是类实例，记录每次 send 的参数，并可按需同步抛出、异步拒绝。
// 用法见同目录说明：在私有暂存根目录执行 `node --test <本文件>`，产物根目录由环境变量 YY_DIST_ROOT 指定（默认 ./dist）。
// 注意：适配依赖宿主原语 豫言_云工_授权绑定存在（现由 宿主.mjs 提供，见适配说明）；产物里的 宿主.mjs 缺少它时本文件直接失败。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const 产物 = pathToFileURL(path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '邮件发送一致性') + '/');
const 宿主源码 = await readFile(new URL('宿主.mjs', 产物), 'utf8');
if (!宿主源码.includes('豫言_云工_授权绑定存在')) {
  throw new Error('宿主.mjs 缺少原语 豫言_云工_授权绑定存在：请按 适配/邮件发送/说明.汉语.md 的“宿主原语”一节补到 豫言操作系统/宿主/云工/宿主.mjs 后重新构建');
}
const {创建云工宿主} = await import(new URL('宿主.mjs', 产物));
const 程序模块 = await WebAssembly.compile(await readFile(new URL('程序.wasm', 产物)));
const 值桥模块 = await WebAssembly.compile(await readFile(new URL('值桥.wasm', 产物)));
const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {EMAIL: ['EMAIL']}});

// 文言：平台外壳于启动之前核对应用之要求与宿主之所供；此测同其所核。汉语：与生成的 入口.mjs 启动时相同，用 接口核对.mjs 核对应用要求与宿主支持清单。
const {核对接口装载} = await import(new URL('接口核对.mjs', 产物));
const 应用要求 = JSON.parse(await readFile(new URL('接口要求组.json', 产物), 'utf8'));
const 宿主提供 = JSON.parse(await readFile(new URL('宿主提供组.json', 产物), 'utf8'));
test('装载前的接口核对通过，且应用要求含本接口', () => {
  核对接口装载({程序模块, 应用要求, 宿主提供, 宿主: '云工'});
  assert.ok(应用要求.some(项 => 项.接口名称 === '豫言操作系统邮件发送' && 项.接口版本 === '0.1.0'), '应用要求里应有本接口 0.1.0');
  assert.ok(宿主提供.some(项 => 项.接口名称 === '豫言操作系统邮件发送' && 项.接口版本 === '0.1.0'), '宿主支持清单里应有本接口 0.1.0');
});

const 令牌 = 'tok-9f8e7d6c5b4a39281706';
const 收件正常 = 'user@example.com';
const 发件正常 = 'noreply@yuyan-lang.org';

class 模拟邮件 {
  constructor() { this.已发 = []; }
  async send(信) { this.已发.push(信); return {messageId: 'm-' + this.已发.length}; }
}
class 同步抛出邮件 {
  constructor(误) { this.已发 = []; this.误 = 误; }
  send(信) { this.已发.push(信); throw this.误; }
}
class 异步拒绝邮件 {
  constructor(误) { this.已发 = []; this.误 = 误; }
  async send(信) { this.已发.push(信); await new Promise(完成 => setTimeout(完成, 5)); throw this.误; }
}

const 发 = async ({绑定 = 'EMAIL', 发件 = 发件正常, 收件 = 收件正常, 主题 = '主题', 正文 = '正文', 环境 = {EMAIL: new 模拟邮件()}} = {}) => {
  const 查询 = new URLSearchParams({b: 绑定, from: 发件, to: 收件, subject: 主题});
  const 回 = await 宿主.fetch(new Request('https://x.test/?' + 查询, {method: 'POST', body: Buffer.from(正文, 'utf8')}), 环境);
  return {状态: 回.status, 文: new TextDecoder('utf-8', {ignoreBOM: true}).decode(await 回.arrayBuffer()), 邮件: 环境.EMAIL};
};
const 应败 = async (参数, 原因, 说明 = '') => {
  const 邮件 = new 模拟邮件();
  const r = await 发({...参数, 环境: {EMAIL: 邮件}});
  assert.equal(r.文, '失败：' + 原因, 说明 || JSON.stringify(参数).slice(0, 200));
  assert.equal(邮件.已发.length, 0, '校验失败不得调用平台：' + 说明);
};
const 应成 = async (参数, 说明 = '') => {
  const 邮件 = new 模拟邮件();
  const r = await 发({...参数, 环境: {EMAIL: 邮件}});
  assert.equal(r.文, '成功', 说明 + ' ' + JSON.stringify(参数).slice(0, 200));
  assert.equal(邮件.已发.length, 1);
  return 邮件.已发[0];
};

test('成功路径：send 收到恰为 {from,to,subject,text} 的对象，内容逐字相同', async () => {
  const 邮件 = new 模拟邮件();
  const 正文 = '打开以下链接即可自动完成邮箱验证：\nhttps://x.example/个人#verify=' + 令牌 + '\n\n链接 60 分钟内有效。\t制表 "引号" \\反斜线\\ \r\n第三行 😀 \u2028 \u007f';
  const r = await 发({主题: '豫言：验证邮箱', 正文, 环境: {EMAIL: 邮件}});
  assert.equal(r.文, '成功');
  assert.equal(邮件.已发.length, 1);
  assert.deepEqual(Object.keys(邮件.已发[0]).sort(), ['from', 'subject', 'text', 'to']);
  assert.deepEqual(邮件.已发[0], {from: 发件正常, to: 收件正常, subject: '豫言：验证邮箱', text: 正文});
});

test('合法输入的边界：地址 254 字节、主题 200 字节、非 ASCII 地址、空主题与空正文', async () => {
  const 长址 = 'x'.repeat(254 - '@example.com'.length) + '@example.com';
  assert.equal(Buffer.byteLength(长址), 254);
  assert.equal((await 应成({收件: 长址})).to, 长址);
  assert.equal((await 应成({发件: 长址})).from, 长址);
  const 汉址 = '用'.repeat(83) + '@b.c';
  assert.equal(Buffer.byteLength(汉址), 253);
  assert.equal((await 应成({收件: 汉址})).to, 汉址);
  assert.equal((await 应成({收件: '用户@例子.中国'})).to, '用户@例子.中国');
  assert.equal((await 应成({收件: 'a+b_c.d-e@sub.example.co.uk'})).to, 'a+b_c.d-e@sub.example.co.uk');
  assert.equal((await 应成({收件: 'a@b'})).to, 'a@b');
  assert.equal((await 应成({收件: 'a@b\u{1F600}'})).to, 'a@b\u{1F600}');
  const 长题 = 'y'.repeat(200);
  assert.equal((await 应成({主题: 长题})).subject, 长题);
  const 汉题 = '题'.repeat(66);
  assert.equal(Buffer.byteLength(汉题), 198);
  assert.equal((await 应成({主题: 汉题})).subject, 汉题);
  assert.equal((await 应成({主题: '', 正文: ''})).subject, '');
  assert.equal((await 应成({主题: 'a\u2003b', 正文: 'ok'})).subject, 'a\u2003b', '主题中的普通空白类字符允许');
});

test('收件与发件地址：注入、空白、多收件人、特殊字符、长度均被拒绝且不调用平台', async () => {
  const 坏址 = [
    'a@b.c\r\nBcc: x@y.z', 'a@b.c\nBcc: x@y.z', 'a@b.c\rBcc: x@y.z', 'a@b.c\r\n', '\na@b.c', 'a@b.c\n',
    'a @b.c', ' a@b.c', 'a@b.c ', 'a@b.c\t', 'a@b\u3000.c', 'a@b\u00A0.c', 'a@b.c\u2028', 'a@b.c\u2029', '\uFEFFa@b.c', 'a@b.c\uFEFF', 'a\u200B@b.c', 'a@b.c\u202E', 'a@b.c\u200F', 'a@b.c\u2060', 'a@b.c\u205F', 'a@b.c\u1680', 'a@b.c\u0085', 'a@b.c\u2000', 'a@b.c\u200A', 'a@b.c\u202F',
    'a@b.c,d@e.f', 'a@b.c;d@e.f', 'a@b.c d@e.f', '"x"@b.c', 'Name<a@b.c>', '<a@b.c>', 'a@b.c>', 'a@b@c', '@b.c', 'a@', '@', 'a', '', 'ab', 'a@b(c)', 'a@b[c]', 'a\\@b.c', 'a:b@c.d', 'mailto:a@b.c', 'a@[1.2.3.4]',
    'a\u0000@b.c', 'a\u0001@b.c', 'a\u007f@b.c', 'a\u0080@b.c', 'a\u009f@b.c',
    'x'.repeat(255 - '@example.com'.length) + '@example.com', '用'.repeat(84) + '@b.c',
  ];
  for (const 址 of 坏址) {
    await 应败({收件: 址}, '收件地址无效', '收件 ' + JSON.stringify(址));
    await 应败({发件: 址}, '发件地址无效', '发件 ' + JSON.stringify(址));
  }
});

test('主题：CR、LF、控制字符、行分隔符与超长均被拒绝', async () => {
  const 坏题 = ['a\r\nBcc: x@y.z', 'a\nb', 'a\rb', '\r\n', 'a\tb', 'a\u0000b', 'a\u0001b', 'a\u001fb', 'a\u007fb', 'a\u0085b', 'a\u2028b', 'a\u2029b', 'y'.repeat(201), '题'.repeat(67)];
  for (const 题 of 坏题) await 应败({主题: 题}, '邮件主题无效', JSON.stringify(题));
});

test('正文：NUL 与其他控制字符被拒绝，制表、换行、回车允许；1 MiB 上限', async () => {
  for (const 坏 of ['a\u0000b', 'a\u0001b', 'a\u0008b', 'a\u000bb', 'a\u000cb', 'a\u000eb', 'a\u001fb']) await 应败({正文: 坏}, '邮件正文无效', JSON.stringify(坏));
  for (const 好 of ['a\tb', 'a\nb', 'a\rb', 'a\r\nb\r\n', '\u007f', '\u0085', '\u2028\u2029', '\uFEFF开头', '😀', '"\\"']) assert.equal((await 应成({正文: 好})).text, 好, JSON.stringify(好));
  const 满 = 'A'.repeat(1048576);
  const 始 = performance.now();
  assert.equal((await 应成({正文: 满})).text, 满);
  console.log('    1 MiB 正文发送耗时 ' + (performance.now() - 始).toFixed(0) + ' ms');
  await 应败({正文: 满 + 'A'}, '邮件正文无效');
  const 汉满 = '文'.repeat(349525) + 'a';
  assert.equal(Buffer.byteLength(汉满), 1048576);
  assert.equal((await 应成({正文: 汉满})).text, 汉满);
  await 应败({正文: '文'.repeat(349525) + 'ab'}, '邮件正文无效');
  const 多行 = ('第一行 "引号" \\ \t\r\n').repeat(40000);
  assert.ok(Buffer.byteLength(多行) < 1048576);
  const 始二 = performance.now();
  assert.equal((await 应成({正文: 多行})).text, 多行);
  console.log('    含转义字符的大正文发送耗时 ' + (performance.now() - 始二).toFixed(0) + ' ms');
});

// 文言：以 JS 重写同一规则作参照，与豫言之校验对拍；规则见规范之“校验”一节。汉语：JS 参照实现，逐码点核对地址、主题、正文规则。
const 特殊字 = new Set('"(),:;<>[]\\');
const 禁用址码点 = 码 => 码 <= 0x20 || (码 >= 0x7F && 码 <= 0xA0) || 码 === 0x1680 || (码 >= 0x2000 && 码 <= 0x200F) || (码 >= 0x2028 && 码 <= 0x202F) || (码 >= 0x205F && 码 <= 0x206F) || 码 === 0x3000 || 码 === 0xFEFF;
const 参照址合法 = 址 => {
  const 字节 = Buffer.byteLength(址, 'utf8');
  if (字节 < 3 || 字节 > 254) return false;
  const 诸字 = [...址];
  for (const 字 of 诸字) if (禁用址码点(字.codePointAt(0)) || 特殊字.has(字)) return false;
  const 艾特们 = 诸字.flatMap((字, 序) => 字 === '@' ? [序] : []);
  return 艾特们.length === 1 && 艾特们[0] > 0 && 艾特们[0] < 诸字.length - 1;
};
const 参照题合法 = 文 => Buffer.byteLength(文, 'utf8') <= 200 && [...文].every(字 => { const 码 = 字.codePointAt(0); return !(码 <= 0x1F || 码 === 0x7F || 码 === 0x85 || 码 === 0x2028 || 码 === 0x2029); });
const 参照文合法 = 文 => Buffer.byteLength(文, 'utf8') <= 1048576 && [...文].every(字 => { const 码 = 字.codePointAt(0); return !(码 <= 8 || 码 === 11 || 码 === 12 || (码 >= 14 && 码 <= 31)); });
const 试 = async (字段, 值) => {
  const 邮件 = new 模拟邮件();
  const r = await 发({[字段]: 值, 环境: {EMAIL: 邮件}});
  return r.文 === '成功';
};
const 分块并发 = async (诸项, 处理, 宽 = 40) => { for (let 起 = 0; 起 < 诸项.length; 起 += 宽) await Promise.all(诸项.slice(起, 起 + 宽).map(处理)); };

test('对拍：逐个码点作为地址、主题、正文的成分，豫言校验与 JS 参照一致', async () => {
  // 边界密集处逐点全扫，其余区段每 1009 点抽一，全部码点区间（含增补平面）都有覆盖。
  const 码点们 = [];
  const 密 = 码 => 码 <= 0x2FF || (码 >= 0x1660 && 码 <= 0x16A0) || (码 >= 0x1FF0 && 码 <= 0x2070) || (码 >= 0x2FF0 && 码 <= 0x3010) || (码 >= 0xFEF0 && 码 <= 0xFF10) || (码 >= 0xFFF0 && 码 <= 0xFFFF);
  for (let 码 = 0; 码 <= 0x10FFFF; 码++) if ((码 < 0xD800 || 码 > 0xDFFF) && (密(码) || 码 % 1009 === 0)) 码点们.push(码);
  码点们.push(0x10000, 0x1F600, 0xE0001, 0x10FFFF);
  await 分块并发(码点们, async 码 => {
    const 字 = String.fromCodePoint(码);
    for (const 址 of [`a${字}@b.c`, `a@b${字}.c`, `${字}a@b.c`, `a@b.c${字}`]) assert.equal(await 试('收件', 址), 参照址合法(址), `地址 U+${码.toString(16)} ${JSON.stringify(址)}`);
    if (码 <= 0x300 || (码 >= 0x2000 && 码 <= 0x2070) || 码 === 0xFEFF) {
      assert.equal(await 试('主题', `a${字}b`), 参照题合法(`a${字}b`), `主题 U+${码.toString(16)}`);
      assert.equal(await 试('正文', `a${字}b`), 参照文合法(`a${字}b`), `正文 U+${码.toString(16)}`);
    }
  });
});

test('对拍：随机字符串作为发件、收件、主题、正文（固定种子）', async () => {
  let 种子 = 20260925;
  const 随 = () => (种子 = (Math.imul(种子, 1664525) + 1013904223) >>> 0) / 4294967296;
  const 字母 = ['a', 'Z', '0', '@', '@', '.', '-', '_', '+', ' ', '\t', '\r', '\n', '\u0000', '\u001f', '\u007f', '\u0085', '\u00a0', '\u00a1', ',', ';', ':', '<', '>', '(', ')', '[', ']', '"', '\\', '用', '例', '😀', '\u200b', '\u200f', '\u2028', '\u2029', '\u202f', '\u2060', '\u206f', '\u3000', '\ufeff', 'é', 'ß', '!', '#', '%'];
  const 造 = (最长) => { const 长 = Math.floor(随() * 最长); let 文 = ''; for (let i = 0; i < 长; i++) 文 += 字母[Math.floor(随() * 字母.length)]; return 文; };
  const 样本 = Array.from({length: 1500}, () => 造(12));
  await 分块并发(样本, async 文 => {
    assert.equal(await 试('收件', 文), 参照址合法(文), '收件 ' + JSON.stringify(文));
    assert.equal(await 试('发件', 文), 参照址合法(文), '发件 ' + JSON.stringify(文));
    assert.equal(await 试('主题', 文), 参照题合法(文), '主题 ' + JSON.stringify(文));
    assert.equal(await 试('正文', 文), 参照文合法(文), '正文 ' + JSON.stringify(文));
  });
  // 带合法骨架的地址：本地部分与域部分各随机，命中“恰一个 @”的分支
  const 骨架 = Array.from({length: 800}, () => 造(6) + (随() < 0.85 ? '@' : '') + 造(6) + (随() < 0.3 ? '@' : ''));
  await 分块并发(骨架, async 文 => assert.equal(await 试('收件', 文), 参照址合法(文), '骨架 ' + JSON.stringify(文)));
});

test('校验顺序：发件、收件、主题、正文，先失败者先返回', async () => {
  await 应败({发件: 'bad', 收件: 'bad', 主题: '\n', 正文: '\u0000'}, '发件地址无效');
  await 应败({收件: 'bad', 主题: '\n', 正文: '\u0000'}, '收件地址无效');
  await 应败({主题: '\n', 正文: '\u0000'}, '邮件主题无效');
  await 应败({正文: '\u0000'}, '邮件正文无效');
});

test('平台同步抛出与异步拒绝：返回（阴，固定原因），不含地址、令牌与平台消息', async () => {
  const 消息 = `recipient ${收件正常} is not allowed; token=${令牌}`;
  const 误们 = [
    [new TypeError(消息), '邮件发送失败：TypeError'],
    [Object.assign(new Error(消息), {code: 'E_RECIPIENT_NOT_ALLOWED'}), '邮件发送失败：Error'],
    [new RangeError(消息), '邮件发送失败：RangeError'],
    [Object.assign(new Error(消息), {name: 'E_DELIVERY_FAILED'}), '邮件发送失败：E_DELIVERY_FAILED'],
    [Object.assign(new Error(消息), {name: 'a.b$c-d_9'}), '邮件发送失败：a.b$c-d_9'],
    [Object.assign(new Error(消息), {name: 'n'.repeat(64)}), '邮件发送失败：' + 'n'.repeat(64)],
    [Object.assign(new Error(消息), {name: 'n'.repeat(65)}), '邮件发送失败'],
    [Object.assign(new Error(消息), {name: 'has space'}), '邮件发送失败'],
    [Object.assign(new Error(消息), {name: 'quote"inside'}), '邮件发送失败'],
    [Object.assign(new Error(消息), {name: 'back\\slash'}), '邮件发送失败'],
    [Object.assign(new Error(消息), {name: '中文名'}), '邮件发送失败'],
    [Object.assign(new Error(消息), {name: ''}), '邮件发送失败'],
    [Object.assign(new Error(消息), {name: 'ctl\u0001'}), '邮件发送失败'],
    [消息, '邮件发送失败：Error'],
    [null, '邮件发送失败：Error'],
    [undefined, '邮件发送失败：Error'],
    [{message: 消息}, '邮件发送失败：Error'],
  ];
  for (const [误, 期] of 误们) {
    for (const 类 of [同步抛出邮件, 异步拒绝邮件]) {
      const 邮件 = new 类(误);
      const r = await 发({环境: {EMAIL: 邮件}, 收件: 收件正常, 正文: '令牌 ' + 令牌});
      assert.equal(r.文, '失败：' + 期, `${类.name} ${String(误?.name ?? 误)}`);
      assert.ok(!r.文.includes(收件正常) && !r.文.includes(令牌) && !r.文.includes('recipient'), '原因不得含地址、令牌与平台消息');
      assert.equal(邮件.已发.length, 1, '失败的 send 也恰被调用一次');
    }
  }
});

test('绑定缺失：已授权名称在环境里不存在，或绑定没有 send，返回（阴，邮件服务未配置）', async () => {
  for (const 环境 of [{}, {EMAIL: null}, {EMAIL: undefined}, {EMAIL: new (class 没有发送 {})()}, {EMAIL: {notSend: 1}}]) {
    const r = await 发({环境});
    assert.equal(r.状态, 200);
    assert.equal(r.文, '失败：邮件服务未配置', JSON.stringify(Object.keys(环境)));
  }
});

test('未授权的绑定名是部署错误，宿主中止请求', async () => {
  await assert.rejects(发({绑定: 'OTHER', 环境: {EMAIL: new 模拟邮件(), OTHER: new 模拟邮件()}}), /未授权的EMAIL绑定：OTHER/);
  await assert.rejects(发({绑定: '', 环境: {EMAIL: new 模拟邮件()}}), /未授权的EMAIL绑定/);
});

test('并发事件互不干扰，每个事件只发自己的邮件', async () => {
  const 邮件们 = Array.from({length: 24}, () => new 模拟邮件());
  await Promise.all(邮件们.map((邮件, 序) => 发({收件: `u${序}@example.com`, 主题: `第 ${序} 封`, 正文: `内容 ${序}\n` + '行\n'.repeat(序), 环境: {EMAIL: 邮件}})));
  邮件们.forEach((邮件, 序) => {
    assert.equal(邮件.已发.length, 1);
    assert.deepEqual(邮件.已发[0], {from: 发件正常, to: `u${序}@example.com`, subject: `第 ${序} 封`, text: `内容 ${序}\n` + '行\n'.repeat(序)});
  });
});
