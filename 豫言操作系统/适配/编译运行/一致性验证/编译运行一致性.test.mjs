// 文言：以真豫言 Wasm、真编器、真宿主之运行器验编译运行之约，先于 Node 之内，复于本地 workerd 之中。
// 汉语：编译运行接口的云工适配一致性验证。同一组场景在两种环境里各跑一遍：（一）Node 进程内：真实豫言 Wasm（试验应用）+ 真实宿主运行器（宿主/云工/编译运行.mjs）+ 真实编译器 Wasm 与标准库资料；
// （二）本地 workerd（Miniflare）：把试验应用的构建产物与运行器一起装载，标准库资料经服务绑定替身供给。另有只在 Node 内可做的运行器级测试（等待者互斥、状态 1 超时、并发上限、无残留）。
// 用法（在私有暂存目录里执行，其中有 dist/）：
//   1. ./yy双宿主构建 --自动 云工 <本目录>/应用 dist/编译运行一致性
//   2. node --test <本目录>/编译运行一致性.test.mjs
// 环境变量：YY_DIST_ROOT 产物根目录（默认 ./dist）；YY_CLOUD_ROOT 云仓根（默认相邻的 ../yuyan-cloud，取其中 工具/云端编译服务 的编译器 Wasm、值桥、Binaryen 胶水与标准库资料）；
// YY_BINARYEN Node 用的 binaryen 入口（默认云仓 应用/豫言体验/node_modules/binaryen）；E2E_MINIFLARE miniflare 入口（默认云仓 工具/包管理服务/node_modules）。缺任何一项则报错并说明。
import {after, before, describe, test} from 'node:test';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cpSync, existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, realpathSync, rmSync, writeFileSync} from 'node:fs';
import {readFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {gunzipSync} from 'node:zlib';
import {fileURLToPath, pathToFileURL} from 'node:url';

const 本目录 = path.dirname(fileURLToPath(import.meta.url));
const 语言仓根 = path.resolve(本目录, '../../../..');
const 云仓根 = path.resolve(process.env.YY_CLOUD_ROOT ?? path.resolve(语言仓根, '../yuyan-cloud'));
const 服务目录 = path.join(云仓根, '工具/云端编译服务');
const 资源目录 = path.join(服务目录, '资源');
const 产物 = path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '编译运行一致性');
const binaryen路径 = process.env.YY_BINARYEN ?? path.join(云仓根, '应用/豫言体验/node_modules/binaryen/index.js');
const miniflare路径 = process.env.E2E_MINIFLARE ?? path.join(云仓根, '工具/包管理服务/node_modules/miniflare/dist/src/index.js');
for (const [说明, 路径] of [['试验应用产物（先构建，输出名 编译运行一致性）', path.join(产物, '入口.mjs')], ['编译器 Wasm', path.join(资源目录, '编译器.wasm')],
  ['值桥 Wasm', path.join(资源目录, '值桥接.wasm')], ['标准库资料', path.join(资源目录, '标准库.json.gz')], ['Binaryen 胶水', path.join(服务目录, '组装器.mjs')],
  ['binaryen（Node 版，环境变量 YY_BINARYEN）', binaryen路径], ['miniflare（环境变量 E2E_MINIFLARE）', miniflare路径]]) {
  if (!existsSync(路径)) throw new Error('缺少' + 说明 + '：' + 路径);
}

const 摘要 = 字节 => createHash('sha256').update(字节).digest('hex');
const 睡 = 毫秒 => new Promise(完成 => setTimeout(完成, 毫秒));
const 你好 = {
  '入口。豫': '寻观「标准库」之书。寻观「云项目」之「问候」之书。「打印行」于「问候」。',
  '问候。豫': '「问候」者『你好，多文件』也。',
  '云项目。包。豫': '「名称」者『云项目』也。「所有者」者『访客』也。「依赖」者「列」【『标准库』】也。'
};
const 请求文 = (files, 额外 = {}) => JSON.stringify({files, ...额外});

// ---------- 共用：真实编译资源 ----------
const 编译模块 = await WebAssembly.compile(readFileSync(path.join(资源目录, '编译器.wasm')));
const 桥模块 = await WebAssembly.compile(readFileSync(path.join(资源目录, '值桥接.wasm')));
const 标准资料 = JSON.parse(gunzipSync(readFileSync(path.join(资源目录, '标准库.json.gz'))));
const binaryen = (await import(pathToFileURL(binaryen路径).href)).default;
const {创建编译运行器工厂} = await import(pathToFileURL(path.join(语言仓根, '豫言操作系统/宿主/云工/编译运行.mjs')).href);

const 应用模块 = await import(pathToFileURL(path.join(产物, '宿主.mjs')).href);
const 程序模块 = await WebAssembly.compile(readFileSync(path.join(产物, '程序.wasm')));
const 值桥模块 = await WebAssembly.compile(readFileSync(path.join(产物, '值桥.wasm')));
const 许可 = JSON.parse(readFileSync(path.join(产物, '许可.json'), 'utf8'));
const 执行配置 = (await import(pathToFileURL(path.join(产物, '动态资源.mjs')).href)).执行配置 ?? null;
const {Miniflare, convertV4MiniflareOptions} = await import(pathToFileURL(miniflare路径).href);

// ---------- 场景共用：脚本调用器 ----------
// 文言：一事一脚本：Worker 不许一事之运行续于他事，故每场景之诸步同处一 HTTP 请求。汉语：试验应用在同一个事件里依脚本执行接口调用；跑(步骤们) 返回每步的结果数组。
const 造调用 = 底层 => async 步们 => {
  const 回 = await 底层({steps: 步们});
  const 文 = await 回.text();
  assert.equal(回.status, 200, 文.slice(0, 300));
  const {results} = JSON.parse(文);
  assert.equal(results.length, 步们.length);
  return results;
};
const 启 = (文件们, 额外 = {}, 名 = {}) => ({op: '启动', request: 请求文(文件们, 额外), ...名});
const 事件们 = 读尽结果 => 读尽结果.events.map(文 => JSON.parse(文));
const 终止 = 诸事件 => 诸事件.at(-1);

// 文言：套件之体，两境共用。汉语：两种环境共用的场景；参数 取跑 返回“跑(步骤们)”函数。
function 套件(取跑) {
  test('全流程：事件序列、终止事件、产物字节与摘要、Base64、输出拼接、取尽后状态 2、结束后失效', async () => {
    const [启动, 读尽, 产物, 产物文, 取尽后, 结束, 再读, 再取产物, 再取产物文, 再结束, 号] = await 取跑()([
      启(你好), {op: '读尽', run: 0}, {op: '产物', run: 0}, {op: '产物文', run: 0}, {op: '事件', run: 0, wait: 100},
      {op: '结束', run: 0}, {op: '事件', run: 0, wait: 100}, {op: '产物', run: 0}, {op: '产物文', run: 0}, {op: '结束', run: 0}, {op: '产物', run: 0}
    ]);
    assert.equal(启动.run, 0); assert.ok(启动.id.length >= 40);
    const 诸事 = 事件们(读尽);
    assert.deepEqual(诸事[0], {type: 'stage', label: '正在加载标准库与项目文件'});
    assert.deepEqual(诸事[1], {type: 'stage', label: '正在执行豫言编译器'});
    const 终 = 终止(诸事);
    assert.equal(终.type, 'finished', JSON.stringify(终).slice(0, 300));
    assert.equal(诸事.filter(事 => 事.type === 'finished' || 事.type === 'failed').length, 1, '恰有一个终止事件');
    assert.equal(终.result.ok, true); assert.equal(终.result.exitCode, 0);
    assert.equal(终.artifact.format, 'wasmgc'); assert.match(终.artifact.sha256, /^[0-9a-f]{64}$/);
    const 输出们 = 诸事.filter(事 => 事.type === 'output');
    assert.ok(输出们.length > 5, '编译器输出应逐条转发');
    for (const 事 of 输出们) { assert.equal(事.phase, 'compile'); assert.ok(['stdout', 'stderr'].includes(事.stream)); assert.equal(typeof 事.text, 'string'); }
    for (const 流 of ['stdout', 'stderr']) assert.equal(输出们.filter(事 => 事.stream === 流).map(事 => 事.text).join(''), 终.result[流], '输出逐字拼接应等于终止事件的 ' + 流);
    assert.ok(诸事.some(事 => 事.type === 'stage' && /^正在组装 Wasm（\d+ 字符）$/.test(事.label)), '应有组装阶段');
    assert.equal(产物.bytes, 终.artifact.bytes); assert.equal(产物.magic, '0,97,115,109'); assert.equal(产物.sha256, 终.artifact.sha256);
    const 字节 = Buffer.from(产物文.base64, 'base64');
    assert.equal(字节.length, 终.artifact.bytes); assert.equal(摘要(字节), 终.artifact.sha256); assert.equal(产物文.base64, 字节.toString('base64'));
    assert.ok(WebAssembly.validate(字节));
    assert.deepEqual(取尽后, {state: 2, event: ''}, '取尽后再读仍为状态 2');
    assert.deepEqual(结束, {ok: true});
    for (const 果 of [再读, 再取产物, 再取产物文, 号]) assert.match(果.error, /不存在或已结束/);
    assert.deepEqual(再结束, {ok: true}, '本事件内重复结束不报错');
  });

  test('产物可运行：隔离执行输出匹配', async () => {
    const [, , 产物文] = await 取跑()([启(你好), {op: '读尽', run: 0}, {op: '产物文', run: 0}, {op: '结束', run: 0}]);
    const {内存文件系统, 执行模块} = await import(pathToFileURL(path.join(语言仓根, '豫言操作系统/宿主/浏览器/编译器/宿主.mjs')).href);
    const 运行 = await 执行模块(new WebAssembly.Module(Buffer.from(产物文.base64, 'base64')), 桥模块, new 内存文件系统());
    assert.equal(运行.ok, true); assert.equal(运行.stdout, '你好，多文件\n');
  });

  test('编译失败是返回值：finished 且 ok 为假，无 artifact，读取产物抛异常', async () => {
    const [, 读尽, 产物, 产物文] = await 取跑()([启({'入口。豫': '此语不成立。'}), {op: '读尽', run: 0}, {op: '产物', run: 0}, {op: '产物文', run: 0}, {op: '结束', run: 0}]);
    const 诸事 = 事件们(读尽), 终 = 终止(诸事);
    assert.equal(终.type, 'finished'); assert.equal(终.result.ok, false); assert.equal(终.artifact, undefined);
    assert.notEqual(终.result.exitCode, 0);
    assert.match(终.result.error ?? 终.result.stderr, /入口。豫/);
    assert.ok(诸事.some(事 => 事.type === 'diagnostic'), '失败时应有 diagnostic 事件');
    assert.match(产物.error, /编译产物不可用/); assert.match(产物文.error, /编译产物不可用/);
  });

  test('请求违规：启动抛异常，宿主里不生运行', async () => {
    const 大 = 'a'.repeat(262145);
    const 例 = [
      ['路径逃逸', 请求文({'../逃逸。豫': ''}), /项目路径或内容无效/], ['空 files', 请求文({}), /项目须含 1 至 256 个文件/],
      ['内容非串', 请求文({'入口。豫': 1}), /项目路径或内容无效/], ['内容超额', 请求文({'入口。豫': 大}), /项目超过 256 KiB/],
      ['路径反斜线', 请求文({'a\\b。豫': ''}), /项目路径或内容无效/], ['路径绝对', 请求文({'/入口。豫': ''}), /项目路径或内容无效/],
      ['入口不在', 请求文({'甲。豫': ''}), /入口文件不存在/], ['入口不合规', 请求文({'入口。豫': ''}, {entry: '../入口。豫'}), /入口文件不存在/],
      ['入口非串', 请求文({'入口。豫': ''}, {entry: 5}), /入口文件不存在/], ['不是 JSON', '{', /不是有效的 JSON/], ['不是对象', '[]', /须为 JSON 对象/],
      ['files 是数组', '{"files":[]}', /须提供 files 文件映射/], ['257 个文件', 请求文(Object.fromEntries(Array.from({length: 257}, (_, i) => [i === 0 ? '入口。豫' : 'f' + i, '']))), /项目须含 1 至 256 个文件/],
      ['路径 257 宽', 请求文({['入口。豫' + 'a'.repeat(254)]: ''}), /项目路径或内容无效/]
    ];
    const 果们 = await 取跑()([...例.map(([, 文]) => ({op: '启动', request: 文})), {op: '启动', request: 请求文(你好), padding: 2097153}, {op: '启动', request: ''},
      {op: '启动', name: '', request: 请求文(你好)}, {op: '启动', name: 'a\u0001b', request: 请求文(你好)}, {op: '启动', name: 'x'.repeat(129), request: 请求文(你好)}]);
    例.forEach(([说明, , 期望], 序) => { assert.match(果们[序].error ?? '', 期望, 说明 + '：' + JSON.stringify(果们[序]).slice(0, 200)); assert.equal(果们[序].run, undefined, 说明); });
    assert.match(果们[例.length].error, /超过 2 MiB/);
    assert.match(果们[例.length + 1].error, /不是有效的 JSON/, '空请求文');
    for (const 序 of [2, 3, 4]) assert.match(果们[例.length + 序].error, /编译器名无效/);
  });

  test('运行之间不遗留用户文件，产物与事件互不串扰', async () => {
    const 果们 = await 取跑()([
      启(你好), {op: '读尽', run: 0},
      // 第二个项目同名入口，但不含 问候。豫：若第一个项目的文件遗留，它就会编过。
      启({'入口。豫': 你好['入口。豫'], '云项目。包。豫': 你好['云项目。包。豫']}), {op: '读尽', run: 1},
      启({...你好, '问候。豫': '「问候」者『另一句』也。'}), {op: '读尽', run: 2},
      {op: '结束', run: 0}, {op: '结束', run: 1}, {op: '结束', run: 2}
    ]);
    const 甲 = 终止(事件们(果们[1])), 乙 = 终止(事件们(果们[3])), 丙 = 终止(事件们(果们[5]));
    assert.equal(甲.result.ok, true);
    assert.equal(乙.result.ok, false, '不应看到前一次运行写入的 问候。豫'); assert.equal(乙.artifact, undefined);
    assert.equal(丙.result.ok, true); assert.notEqual(丙.artifact.sha256, 甲.artifact.sha256, '不同源码应得不同产物');
  });

  test('同一项目两次编译的产物逐字节相同（确定性）', async () => {
    const 果们 = await 取跑()([启(你好), {op: '读尽', run: 0}, 启(你好), {op: '读尽', run: 1}, {op: '产物文', run: 0}, {op: '产物文', run: 1}, {op: '结束', run: 0}, {op: '结束', run: 1}]);
    assert.equal(终止(事件们(果们[1])).artifact.sha256, 终止(事件们(果们[3])).artifact.sha256);
    assert.equal(果们[4].base64, 果们[5].base64);
  });

  test('指定入口与默认入口；entry 为 null 取默认', async () => {
    const 文件们 = {...你好, '乙。豫': '寻观「标准库」之书。「打印行」于『乙』。'};
    const 果们 = await 取跑()([启(文件们, {entry: '乙。豫'}), {op: '读尽', run: 0}, 启(文件们, {entry: null}), {op: '读尽', run: 1}, 启(文件们), {op: '读尽', run: 2},
      {op: '结束', run: 0}, {op: '结束', run: 1}, {op: '结束', run: 2}]);
    for (const 序 of [1, 3, 5]) assert.equal(终止(事件们(果们[序])).result.ok, true, '第 ' + 序 + ' 步');
  });

  test('接近单文件限额的源码可编译', async () => {
    const 果们 = await 取跑()([启({'入口。豫': '寻观「标准库」之书。「长文」者『' + '甲'.repeat(10000) + '』也。「打印行」于「长文」。'}), {op: '读尽', run: 0}, {op: '结束', run: 0}]);
    const 终 = 终止(事件们(果们[1]));
    assert.equal(终.result.ok, true, JSON.stringify(终).slice(0, 300)); assert.ok(终.artifact.bytes > 0);
  });

  test('两次运行交错读取，各自终止，产物不同', async () => {
    const 果们 = await 取跑()([启({...你好, '问候。豫': '「问候」者『甲』也。'}), 启({...你好, '问候。豫': '「问候」者『乙』也。'}), {op: '交错', run: 0, run2: 1}, {op: '结束', run: 0}, {op: '结束', run: 1}]);
    assert.notEqual(果们[0].id, 果们[1].id);
    const [甲, 乙] = 果们[2].terminals.map(文 => JSON.parse(文));
    assert.equal(甲.type, 'finished'); assert.equal(乙.type, 'finished');
    assert.equal(甲.result.ok, true); assert.equal(乙.result.ok, true);
    assert.notEqual(甲.artifact.sha256, 乙.artifact.sha256);
  });

  test('等待毫秒与运行号的校验', async () => {
    const 果们 = await 取跑()([
      启({'入口。豫': '寻观「标准库」之书。'}), {op: '读尽', run: 0},
      {op: '事件', run: 0, wait: 0}, {op: '事件', run: 0, wait: -1}, {op: '事件', run: 0, wait: 60001}, {op: '事件', run: 0, wait: 1000000},
      {op: '事件', run: 0, wait: 60000}, {op: '事件', run: 0, wait: 1},
      {op: '事件', run: ''}, {op: '事件', run: '不存在的号'}, {op: '结束', run: '不存在的号'}, {op: '产物', run: ''}, {op: '产物文', run: 'x:y'}, {op: '结束', run: 0}
    ]);
    for (const 序 of [2, 3, 4, 5]) assert.match(果们[序].error, /等待毫秒须在 1 至 60000/, String(序));
    assert.equal(果们[6].state, 2, '上界 60000 合法'); assert.equal(果们[7].state, 2, '下界 1 合法');
    assert.match(果们[8].error, /运行号无效/);
    for (const 序 of [9, 10, 12]) assert.match(果们[序].error, /运行号不属于当前事件/, String(序));
    assert.match(果们[11].error, /运行号无效/);
    assert.deepEqual(果们[13], {ok: true});
  });

  test('多个事件并发各编各的（互斥的编译阶段跨事件协作），产物互不相同', async () => {
    const 跑 = 取跑();
    const 诸 = ['甲', '乙', '丙', '丁', '戊'];
    const 果们 = await Promise.all(诸.map(名 => 跑([启({...你好, '问候。豫': '「问候」者『' + 名 + '』也。'}), {op: '读尽', run: 0}, {op: '产物文', run: 0}, {op: '结束', run: 0}])));
    const 摘们 = 果们.map((果, 序) => {
      const 终 = 终止(事件们(果[1]));
      assert.equal(终.type, 'finished', 诸[序]); assert.equal(终.result.ok, true, 诸[序]);
      assert.equal(摘要(Buffer.from(果[2].base64, 'base64')), 终.artifact.sha256, 诸[序]);
      return 终.artifact.sha256;
    });
    assert.equal(new Set(摘们).size, 诸.length, '各事件的产物应各不相同');
  });

  test('运行号只在启动它的事件内有效：别的事件用它一律抛异常', async () => {
    const [启动, , 结束] = await 取跑()([启(你好), {op: '读尽', run: 0}, {op: '结束', run: 0}]);
    const 果们 = await 取跑()([{op: '事件', run: 启动.id, wait: 100}, {op: '产物', run: 启动.id}, {op: '产物文', run: 启动.id}, {op: '结束', run: 启动.id}]);
    for (const 果 of 果们) assert.match(果.error, /运行号不属于当前事件/, JSON.stringify(果));
    assert.deepEqual(结束, {ok: true});
  });
}

// ---------- （一）Node 进程内：真 Wasm + 真运行器 ----------
describe('Node 进程内：真实豫言 Wasm 与真实运行器', () => {
  const 造运行器 = 创建编译运行器工厂({binaryen, 编译模块, 桥模块, 取标准库资料: async () => 标准资料});
  const 运行器 = 造运行器();
  const 宿主 = 应用模块.创建云工宿主({程序模块, 值桥模块, 许可, 执行配置});
  const 底层 = 体 => 宿主.fetch(new Request('https://编译运行.test/脚本', {method: 'POST', body: JSON.stringify(体)}), {COMPILER_RUNTIME: 运行器}, {waitUntil() {}});
  套件(() => 造调用(底层));

  test('未授权的绑定名使本事件失败（宿主错误，不能被应用捕获）', async () => {
    const 请 = 体 => new Request('https://编译运行.test/脚本', {method: 'POST', body: JSON.stringify({steps: [体]})});
    await assert.rejects(() => 宿主.fetch(请({op: '启动', name: '未授权名', request: 请求文(你好)}), {COMPILER_RUNTIME: 运行器, 未授权名: 运行器}, {waitUntil() {}}));
    await assert.rejects(() => 宿主.fetch(请({op: '启动', request: 请求文(你好)}), {}, {waitUntil() {}}), '绑定不存在');
  });

  test('运行器：全部结束后无残留运行', () => { assert.equal(运行器.运行数(), 0); });
});

// ---------- （二）运行器级：等待者互斥、状态 1、并发上限、回收（只在 Node 内做） ----------
describe('运行器级行为（Node 内）', () => {
  const 慢运行器 = (毫秒) => 创建编译运行器工厂({binaryen, 编译模块, 桥模块, 取标准库资料: async () => { await 睡(毫秒); return 标准资料; }})();
  const 读尽 = async (器, 号) => { const 们 = []; for (;;) { const 答 = await 器.读事件(号, 5000); if (答.状态 === 2) return 们; if (答.状态 === 0) 们.push(JSON.parse(答.事件)); } };

  test('同一运行同一时刻至多一个等待者；结束使等待者得状态 2', async () => {
    const 器 = 慢运行器(300);
    const 号 = 器.启动(请求文(你好));
    assert.equal((await 器.读事件(号, 2000)).状态, 0, '首个事件（加载阶段）');
    const 等 = 器.读事件(号, 5000);
    assert.throws(() => 器.读事件(号, 5000), /已有等待者/);
    器.结束(号);
    assert.deepEqual(await 等, {状态: 2, 事件: ''});
    assert.equal(器.运行数(), 0);
  });

  test('等待期满而无新事件得状态 1，随后仍能取到后续事件', async () => {
    const 器 = 慢运行器(400);
    const 号 = 器.启动(请求文(你好));
    assert.equal((await 器.读事件(号, 2000)).状态, 0);
    const 一 = await 器.读事件(号, 1);
    assert.deepEqual(一, {状态: 1, 事件: ''});
    const 事们 = await 读尽(器, 号);
    assert.equal(事们.at(-1).type, 'finished');
    器.结束(号);
  });

  test('并发运行上限 8：第 9 个启动抛异常，结束后可再启动', async () => {
    const 器 = 慢运行器(500);
    const 号们 = Array.from({length: 8}, () => 器.启动(请求文(你好)));
    assert.throws(() => 器.启动(请求文(你好)), /编译运行过多/);
    assert.equal(器.运行数(), 8);
    器.结束(号们[0]);
    const 再 = 器.启动(请求文(你好));
    assert.equal(器.运行数(), 8);
    for (const 号 of [...号们.slice(1), 再]) 器.结束(号);
    assert.equal(器.运行数(), 0);
  });

  test('尚未开始编译的运行被结束后不再编译；运行号不可猜测且互异', async () => {
    let 调用数 = 0;
    const 器 = 创建编译运行器工厂({binaryen, 编译模块, 桥模块, 取标准库资料: async () => { 调用数++; return 标准资料; }})();
    const 号们 = new Set(Array.from({length: 5}, () => 器.启动(请求文(你好))));
    assert.equal(号们.size, 5); for (const 号 of 号们) assert.match(号, /^[0-9a-f-]{36}$/);
    for (const 号 of 号们) 器.结束(号);
    await 睡(300);
    assert.equal(器.运行数(), 0);
    assert.equal(调用数, 0, '已结束而未开始的运行不应再加载标准库或编译');
  });

  test('已终止而未结束的运行不占并发额度，且至多保留 32 个（丢弃最旧的）', async () => {
    const 器 = 创建编译运行器工厂({binaryen, 编译模块, 桥模块, 取标准库资料: async () => { throw Error('标准库资源不可用'); }})();
    const 号们 = [];
    for (let 序 = 0; 序 < 40; 序++) {
      const 号 = 器.启动(请求文(你好));
      号们.push(号);
      assert.equal((await 读尽(器, 号)).at(-1).type, 'failed');
    }
    assert.equal(器.运行数(), 32);
    await assert.rejects(async () => 器.读事件(号们[0], 10), /不存在或已结束/);
    assert.equal((await 器.读事件(号们.at(-1), 10)).状态, 2, '最新的运行仍在');
    for (const 号 of 号们) 器.结束(号);
    assert.equal(器.运行数(), 0);
  });

  test('宿主故障是 failed 事件而不是异常（标准库资料不可用）', async () => {
    const 器 = 创建编译运行器工厂({binaryen, 编译模块, 桥模块, 取标准库资料: async () => { throw Error('标准库资源不可用'); }})();
    const 号 = 器.启动(请求文(你好));
    const 事们 = await 读尽(器, 号);
    assert.deepEqual(事们.at(-1), {type: 'failed', error: '标准库资源不可用'});
    assert.equal(事们.filter(事 => 事.type === 'finished').length, 0);
    assert.throws(() => 器.取产物(号), /编译产物不可用/);
    器.结束(号);
  });
});

// ---------- （三）本地 workerd：真实 Worker 运行时（Miniflare） ----------
describe('本地 workerd（Miniflare）：同一场景', () => {
  let mf, 临时;
  // 文言：外壳与云仓之编译外壳同构：静态导入器、桥、组装与运行器，放运行器于 env。汉语：装配一个与云端编译服务外壳同构的 rig，把试验应用产物、编译运行宿主模块与编译资源放进临时目录树（保持相对位置）。
  const 装配 = () => {
    临时 = realpathSync(mkdtempSync(path.join(process.env.TMPDIR ?? tmpdir(), 'yy-编译运行-')));
    const 拷 = (来源, 目标) => { mkdirSync(path.dirname(path.join(临时, 目标)), {recursive: true}); cpSync(来源, path.join(临时, 目标)); };
    for (const 名 of readdirSync(产物)) if (!名.endsWith('.wat')) 拷(path.join(产物, 名), '产物/' + 名);
    拷(path.join(语言仓根, '豫言操作系统/宿主/云工/编译运行.mjs'), '豫言操作系统/宿主/云工/编译运行.mjs');
    拷(path.join(语言仓根, '豫言操作系统/宿主/浏览器/编译器/宿主.mjs'), '豫言操作系统/宿主/浏览器/编译器/宿主.mjs');
    拷(path.join(语言仓根, '豫言操作系统/宿主/浏览器/编译器/组装.mjs'), '豫言操作系统/宿主/浏览器/编译器/组装.mjs');
    拷(path.join(服务目录, '组装器.mjs'), '组装器.mjs');
    for (const 名 of ['编译器.wasm', '值桥接.wasm', '组装器.wasm']) 拷(path.join(资源目录, 名), '资源/' + 名);
    writeFileSync(path.join(临时, 'rig.mjs'), [
      "import 应用 from './产物/入口.mjs';", "import binaryen from './组装器.mjs';", "import 编译模块 from './资源/编译器.wasm';", "import 桥模块 from './资源/值桥接.wasm';",
      "import {创建编译运行器工厂} from './豫言操作系统/宿主/云工/编译运行.mjs';",
      'const 造运行器 = 创建编译运行器工厂({binaryen, 编译模块, 桥模块});',
      'export default {fetch: (请求, 环境, 上下文) => 应用.fetch(请求, {...环境, COMPILER_RUNTIME: 造运行器(环境.ASSETS)}, 上下文)};', ''].join('\n'));
    return path.join(临时, 'rig.mjs');
  };
  const 装载模块 = 入口路径 => {
    const 模块们 = [], 已见 = new Set();
    const 载 = 路径 => {
      if (已见.has(路径)) return;
      已见.add(路径);
      const 是wasm = 路径.endsWith('.wasm'), 是json = 路径.endsWith('.json');
      const 原文 = readFileSync(路径, 是wasm ? undefined : 'utf8');
      模块们.push({type: 是wasm ? 'CompiledWasm' : 'ESModule', path: 路径, contents: 是json ? 'export default ' + 原文 : 原文});
      // 文言：只循行首之静态 import/export…from，免读动态资源字串内嵌之源。汉语：不解析 动态资源.mjs 字符串里内嵌的模块源码。
      if (!是wasm && !是json) for (const 项 of 原文.matchAll(/^\s*(?:import|export)\b[^;\n]*?\bfrom\s+['"]([^'"]+)['"]/gm)) if (项[1].startsWith('.')) 载(path.resolve(path.dirname(路径), 项[1]));
    };
    载(入口路径);
    return 模块们;
  };
  before(async () => {
    const 入口 = 装配();
    const gz = readFileSync(path.join(资源目录, '标准库.json.gz'));
    mf = new Miniflare(convertV4MiniflareOptions({
      modules: 装载模块(入口), modulesRoot: 临时, compatibilityDate: '2026-09-09',
      serviceBindings: {ASSETS: async 请求 => decodeURIComponent(new URL(请求.url).pathname) === '/标准库.json.gz' ? new Response(gz) : new Response('无此资源', {status: 404})}
    }));
    await mf.ready;
  });
  after(async () => { await mf?.dispose(); if (临时) rmSync(临时, {recursive: true, force: true}); });
  套件(() => 造调用(体 => mf.dispatchFetch('https://编译运行.test/脚本', {method: 'POST', body: JSON.stringify(体)})));
});
