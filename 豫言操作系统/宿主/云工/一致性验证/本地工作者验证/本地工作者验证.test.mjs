// 文言：以真 workerd 验宿主壳：独占区、事务、告警、先应、值之限、时限配置，并与模拟作差分比对。
// 汉语：可选的本地 workerd 验证（Wrangler 4.129.0，兼容日期 2026-09-05，SQLite 存储）。前置：
//   1. 把待验产物（dist/持久对象壳一致性应用）的下列文件复制到本目录 `产物/`：值桥.mjs 入口.mjs 动态资源.mjs 句柄.mjs
//      宿主.mjs 宿主提供组.json 接口核对.mjs 接口要求组.json 程序.wasm 值桥.wasm 许可.json；并把 产物/许可.json 改为 {}（本地上游是 http）。
//   2. 在本目录用云仓已装的 wrangler 起服务：
//        wrangler dev --config wrangler.jsonc --port 8791 --inspector-port 9331 --persist-to .state --local
//   3. 运行：env "持久对象壳产物=<dist/持久对象壳一致性应用>" node --test 本地工作者验证.test.mjs
//      环境变量 本地工作者地址（默认 http://127.0.0.1:8791）；本地工作者产物目录（wrangler 所用的 产物/ 目录，默认本目录 ./产物，
//      “事件时限”用例会临时改写其中 动态资源.mjs 的 执行配置 并在结束后还原）；含慢速验证=1 时另验平台 30 秒重置线（各约 35 秒）。
//   建议把本目录复制到临时位置再运行，避免在语言仓里留下 产物/ 与 .state。
// 服务不可达时全部用例跳过。用例之间用不同对象名隔离，互不相染。
import {test, after} from 'node:test';
import assert from 'node:assert/strict';
import http from 'node:http';
import {readFile, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {pathToFileURL} from 'node:url';
import {创建模拟持久对象} from '../模拟持久对象.mjs';
import {创建模拟慢网络} from '../模拟慢网络.mjs';

const 基址 = process.env.本地工作者地址 ?? 'http://127.0.0.1:8791';
const 上游端口 = 8792;
const 上游址 = 'http://127.0.0.1:' + 上游端口;
const 产物 = process.env.持久对象壳产物;
const 含慢速 = process.env.含慢速验证 === '1';
const 睡 = 毫秒 => new Promise(完成 => setTimeout(完成, 毫秒));
const 可用 = await fetch(基址 + '/__instance?do=探活', {signal: AbortSignal.timeout(3000)}).then(回 => 回.ok, () => false);
const 选项 = {skip: 可用 ? false : '本地 workerd 服务不可达（见文件头说明）'};

const 上游日志 = [];
const 上游 = 可用 ? await new Promise(完成 => {
  const 服务 = http.createServer(async (请求, 回应) => {
    const 址 = new URL(请求.url, 'http://x');
    const 编号 = 址.searchParams.get('id') ?? '', 毫秒 = Number(址.searchParams.get('ms') ?? 0);
    上游日志.push({事件: '开始', 编号, 时: Date.now()});
    await 睡(毫秒);
    上游日志.push({事件: '结束', 编号, 时: Date.now()});
    回应.end('ok');
  });
  服务.listen(上游端口, '127.0.0.1', () => 完成(服务));
}) : null;
after(() => { 上游?.close(); 上游?.closeAllConnections?.(); });
const 区间 = 编号 => { const 出 = []; let 始 = null; for (const 项 of 上游日志) { if (项.编号 !== 编号) continue; if (项.事件 === '开始') 始 = 项.时; else if (始 !== null) { 出.push([始, 项.时]); 始 = null; } } return 出; };
const 相交 = (甲, 乙) => 区间(甲).some(([a, b]) => 区间(乙).some(([c, d]) => a < d && c < b));

let 计数 = 0;
const 新名 = 前缀 => 前缀 + '-' + Date.now() + '-' + (计数++);
const 发 = async (对象名, 体, 附加 = {}) => {
  const 起 = Date.now();
  try {
    const 回 = await fetch(基址 + '/?do=' + encodeURIComponent(对象名), {method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify(体), ...附加});
    return {状态: 回.status, 文: await 回.text(), 耗: Date.now() - 起};
  } catch (错) { return {状态: 0, 文: '抛出 ' + String(错), 耗: Date.now() - 起}; }
};
const 探针 = async (对象名, 路径) => (await fetch(基址 + 路径 + (路径.includes('?') ? '&' : '?') + 'do=' + encodeURIComponent(对象名))).json();
const 读 = async (对象名, 键) => (await 发(对象名, {op: 'get', k: 键})).文;
const 设上游 = 对象名 => 发(对象名, {op: 'put', k: 'upstream', text: JSON.stringify(上游址)});

test('独占区：非独占的读—等—写丢失更新，独占区内不丢且区间不交错', 选项, async () => {
  const 名 = 新名('e1');
  await 设上游(名);
  const 直行 = await Promise.all([1, 2, 3, 4, 5].map(序 => 发(名, {op: 'counter-plain', ms: 100, id: '直' + 序})));
  assert.equal(直行.every(项 => 项.文 === 'C|1'), true, '五个并发读到同一旧值：' + 直行.map(项 => 项.文));
  assert.equal(相交('直1', '直2'), true);
  await 发(名, {op: 'del', k: 'n'});
  const 占 = await Promise.all([1, 2, 3, 4, 5].map(序 => 发(名, {op: 'block', name: 'counter', args: JSON.stringify({ms: 100, id: '占' + 序})})));
  assert.deepEqual(占.map(项 => JSON.parse(项.文.slice(2)).n).sort(), [1, 2, 3, 4, 5]);
  assert.equal(await 读(名, 'n'), 'E|5');
  for (let 甲 = 1; 甲 <= 5; 甲++) for (let 乙 = 甲 + 1; 乙 <= 5; 乙++) assert.equal(相交('占' + 甲, '占' + 乙), false, `独占区 ${甲}、${乙} 交错`);
});

test('独占区：区行之际到达的普通事件排队至区毕', 选项, async () => {
  const 名 = 新名('e2');
  await 设上游(名);
  const 占 = 发(名, {op: 'block', name: 'wait', args: JSON.stringify({ms: 500, id: '甲'})});
  await 睡(100);
  const 普通 = await 发(名, {op: 'kind'});
  await 占;
  assert.equal(普通.文, 'K|durable-fetch');
  assert.equal(普通.耗 > 300, true, `普通事件应被挡到区毕（约 400 毫秒），实际 ${普通.耗}`);
});

test('独占区：区内失败不重置对象；平台原生回调抛错则重置', 选项, async () => {
  const 名 = 新名('e3');
  const 前 = await 探针(名, '/__instance');
  assert.match((await 发(名, {op: 'block', name: 'fail', args: '{}'})).文, /^X\|持久独占失败：/);
  assert.equal((await 发(名, {op: 'block', name: 'no-result', args: '{}'})).文, 'X|持久独占失败：豫言独占区未供结果');
  assert.equal((await 探针(名, '/__instance')).实例号, 前.实例号, '豫言独占区失败不重置对象');
  const 名2 = 新名('e3raw');
  const 前2 = await 探针(名2, '/__instance');
  await fetch(基址 + '/__raw-block-throw?do=' + 名2).then(回 => 回.text());
  const 后2 = await 探针(名2, '/__instance');
  assert.notEqual(后2.实例号, 前2.实例号, '原生 blockConcurrencyWhile 回调抛错使对象重置');
});

test('事务：回调运行期间他事排队；事务内等待外部输入输出可行', 选项, async () => {
  const 名 = 新名('e4');
  const 事务 = 发(名, {op: 'tx', args: JSON.stringify({name: 'wait', base: 上游址, id: '务', ms: 500})});
  await 睡(100);
  const 普通 = await 发(名, {op: 'kind'});
  assert.equal((await 事务).文, 'T|"waited"');
  assert.equal(普通.文, 'K|durable-fetch');
  assert.equal(普通.耗 > 300, true, `普通事件应排队（约 400 毫秒），实际 ${普通.耗}`);
});

test('告警：处理中视为未设、失败后重试且重试数递增、告警内可重设', 选项, async () => {
  const 甲 = 新名('e5a');
  await 发(甲, {op: 'put', k: 'alarm-mode', text: '"record"'});
  await 发(甲, {op: 'alarm-set', t: Date.now() + 300});
  await 睡(1500);
  assert.equal(await 读(甲, 'alarm-log'), 'E|"0:0;"', '首次触发：重试数 0，处理开始时告警视为已清除');
  assert.equal((await 发(甲, {op: 'alarm-get'})).文, 'A|0|0');
  const 乙 = 新名('e5b');
  await 发(乙, {op: 'put', k: 'alarm-mode', text: '"fail"'});
  await 发(乙, {op: 'alarm-set', t: Date.now() + 200});
  await 睡(1200);
  assert.equal((await 发(乙, {op: 'alarm-get'})).文.startsWith('A|1|'), true, '失败后等待重试期间告警视为已设');
  await 睡(4500);
  assert.equal(await 读(乙, 'alarm-log'), 'E|"0:0;1:0;"', '平台退避重试，重试数递增');
  await 发(乙, {op: 'put', k: 'alarm-mode', text: '"record"'});
  const 丙 = 新名('e5c');
  const 下次 = Date.now() + 60_000_000;
  await 发(丙, {op: 'put', k: 'alarm-mode', text: '"rearm"'});
  await 发(丙, {op: 'put', k: 'alarm-next', text: String(下次)});
  await 发(丙, {op: 'alarm-set', t: Date.now() + 200});
  await 睡(1200);
  assert.equal((await 发(丙, {op: 'alarm-get'})).文, 'A|1|' + 下次, '告警内重设');
});

test('告警：事务内的告警变更与本事务同命运；原生探针证实 storage 直写亦随回滚', 选项, async () => {
  const 名 = 新名('e6');
  const 未来 = Date.now() + 3_600_000;
  assert.equal((await 发(名, {op: 'tx', args: JSON.stringify({name: 'alarm', t: 未来})})).文, 'T|"rolled"');
  assert.equal((await 发(名, {op: 'alarm-get'})).文, 'A|0|0', '回滚撤销事务内设置的告警');
  assert.equal((await 发(名, {op: 'tx', args: JSON.stringify({name: 'alarm-commit', t: 未来})})).文, 'T|"committed"');
  assert.equal((await 发(名, {op: 'alarm-get'})).文, 'A|1|' + 未来);
  assert.equal((await 发(名, {op: 'tx', args: '{"name":"alarm-del-rollback"}'})).文, 'T|"rolled"');
  assert.equal((await 发(名, {op: 'alarm-get'})).文, 'A|1|' + 未来, '回滚恢复事务内删除的告警');
  const 原 = await 探针(新名('e6raw'), '/__raw-tx-alarm2?case=direct-put');
  assert.equal(原.事后direct, null, '事务回调内经 storage 直接 put 的值随回滚撤销');
  assert.equal(原.事后告警, null);
});

test('清空持久仓连告警一并清除（兼容日期 2026-09-05，SQLite）', 选项, async () => {
  const 名 = 新名('e7');
  await 发(名, {op: 'put', k: 'a', text: '1'});
  await 发(名, {op: 'alarm-set', t: Date.now() + 3_600_000});
  assert.equal((await 发(名, {op: 'clear'})).文, 'OK');
  assert.equal((await 发(名, {op: 'alarm-get'})).文, 'A|0|0');
  assert.equal((await 发(名, {op: 'list', opts: '{}'})).文, 'L|[]');
});

test('响应先返：Wasm 继续运行；原生代码无论是否 waitUntil 均存活；客户端断开不取消运行', 选项, async () => {
  const 名 = 新名('e8');
  await 设上游(名);
  const 回 = await 发(名, {op: 'early', ms: 800});
  assert.equal(回.文, 'early');
  assert.equal(回.耗 < 500, true, '响应先于后续工作返回');
  assert.equal(await 读(名, 'after'), 'M|', '返回时后续写入尚未发生');
  await 睡(1800);
  assert.equal(await 读(名, 'after'), 'E|"done"');
  for (const 模式 of ['none', 'waituntil']) {
    const 原名 = 新名('e8raw-' + 模式);
    await fetch(基址 + '/__raw-bg?mode=' + 模式 + '&ms=800&do=' + 原名).then(回 => 回.text());
    await 睡(2000);
    assert.notEqual((await 探针(原名, '/__raw-get?k=bg-' + 模式)).值, null, `原生 ${模式}：响应返回后 JS 仍完成`);
  }
  const 断 = 新名('e8abort');
  await 设上游(断);
  const 起 = 上游日志.length;
  const 控 = new AbortController();
  const 请 = 发(断, {op: 'sleep', n: 6, ms: 300}, {signal: 控.signal});
  await 睡(500);
  控.abort();
  await 请;
  await 睡(2500);
  const 次数 = 上游日志.slice(起).filter(项 => 项.事件 === '开始').length;
  assert.equal(次数, 6, `客户端断开后豫言运行仍执行完毕（上游被调用 ${次数} 次）`);
});

test('值大小：SQLite 对象上约 2 MiB 的值、2048 字节的键、4 MiB 独占区参数与结果可用', 选项, async () => {
  const 名 = 新名('e9');
  assert.equal((await 发(名, {op: 'put-big', k: '大', exp: 20})).文, 'OK');
  assert.equal((await 发(名, {op: 'getlen', k: '大'})).文, 'N|' + (2 ** 20 + 2));
  const 近二兆 = 'q'.repeat(2 * 1024 * 1024 - 200);
  assert.equal((await 发(名, {op: 'put', k: '近', text: JSON.stringify(近二兆)})).文, 'OK');
  assert.equal((await 发(名, {op: 'put', k: 'k'.repeat(2048), text: '1'})).文, 'OK');
  assert.equal((await 发(名, {op: 'put', k: 'k'.repeat(2049), text: '1'})).文, 'X|持久键须为 1 至 2048 字节');
  assert.equal((await 发(名, {op: 'block-big', name: 'echo', exp: 22})).文, 'B|4194335');
  assert.equal((await 发(名, {op: 'block-len', name: 'big-result', args: '{"exp":22}'})).文, 'B|' + (2 ** 22 + 2));
});

test('事件时限：本地 workerd 中 durable-alarm 时限 250 毫秒失败、5000 毫秒成功（改写产物 动态资源.mjs 的执行配置后等待热重载）', 选项, async () => {
  const 路径 = join(process.env.本地工作者产物目录 ?? new URL('./产物/', import.meta.url).pathname, '动态资源.mjs');
  const 原文 = await readFile(路径, 'utf8');
  const 设配置 = async 配置 => {
    const 文 = (await readFile(路径, 'utf8')).replace(/export const 执行配置 = .*;\n?$/s, 'export const 执行配置 = ' + JSON.stringify(配置) + ';\n');
    await writeFile(路径, 文);
    await 睡(5000);
  };
  try {
    for (const [限, 成功] of [[250, false], [5000, true]]) {
      await 设配置({事件时限毫秒: {'durable-alarm': 限}});
      const 名 = 新名('e10-' + 限);
      await 设上游(名);
      await 发(名, {op: 'put', k: 'alarm-mode', text: '"long"'});
      await 发(名, {op: 'put', k: 'alarm-n', text: '5'});
      await 发(名, {op: 'put', k: 'alarm-ms', text: '100'});
      await 发(名, {op: 'alarm-set', t: Date.now() + 200});
      await 睡(2500);
      assert.equal(await 读(名, 'alarm-done'), 成功 ? 'E|1' : 'M|', `时限 ${限}`);
    }
  } finally { await writeFile(路径, 原文); await 睡(4000); }
});

test('差分：模拟持久对象与本地 workerd 对同一组操作输出完全一致', {skip: 选项.skip || (产物 ? false : '需要环境变量 持久对象壳产物')}, async () => {
  const {创建云工宿主} = await import(pathToFileURL(join(产物, '宿主.mjs')));
  const 程序模块 = await WebAssembly.compile(await readFile(join(产物, '程序.wasm')));
  const 值桥模块 = await WebAssembly.compile(await readFile(join(产物, '值桥.wasm')));
  const 许可 = JSON.parse(await readFile(join(产物, '许可.json'), 'utf8'));
  const 网络 = 创建模拟慢网络();
  const 对象 = 创建模拟持久对象({宿主: 创建云工宿主({程序模块, 值桥模块, 许可, 网络: 网络.fetch})});
  const 模拟发 = async 体 => { try { const 回 = await 对象.fetch(new Request('https://do.test/', {method: 'POST', headers: {'content-type': 'application/json'}, body: JSON.stringify(体)})); return 回.status + ' ' + await 回.text(); } catch (错) { return '抛出 ' + String(错).slice(0, 120); } };
  const 真名 = 新名('diff');
  const 真发 = async 体 => { const 果 = await 发(真名, 体); return 果.状态 + ' ' + 果.文.slice(0, 5000); };
  const 序 = [
    {op: 'put', k: 'p:a', text: '1'}, {op: 'put', k: 'p:b', text: '2'}, {op: 'put', k: 'p:c', text: '3'},
    {op: 'put', k: 'p:汉', text: '"汉"'}, {op: 'put', k: 'p:😀', text: '"表情"'}, {op: 'put', k: 'p:\u{ff5e}', text: '"全角"'}, {op: 'put', k: 'q:z', text: '{"深":{"套":[1,2,{"三":null}]}}'},
    {op: 'list', opts: '{}'}, {op: 'list', opts: '{"prefix":"p:"}'}, {op: 'list', opts: '{"prefix":"p:","reverse":true}'}, {op: 'list', opts: '{"prefix":"p:","limit":2}'},
    {op: 'list', opts: '{"start":"p:b","end":"p:汉"}'}, {op: 'list', opts: '{"startAfter":"p:b","limit":3}'},
    {op: 'list', opts: '{"prefix":"","start":"","startAfter":"","end":"","limit":100,"reverse":false}'},
    {op: 'get', k: 'q:z'}, {op: 'get', k: 'nokey'}, {op: 'put', k: 'nul', text: 'null'}, {op: 'get', k: 'nul'},
    {op: 'put', k: 'sp', text: '{"a":1,"b":[]}'}, {op: 'get', k: 'sp'}, {op: 'put', k: 'sp2', text: '  {"a" : 1 }  '}, {op: 'get', k: 'sp2'},
    {op: 'put', k: 'num', text: '[1.5,-0,1e3,12345678901234567890]'}, {op: 'get', k: 'num'},
    {op: 'put', k: 'str', text: '"引\\"号\\\\反斜\\n换行\\t制表"'}, {op: 'get', k: 'str'},
    {op: 'del', k: 'p:a'}, {op: 'del', k: 'p:a'}, {op: 'list', opts: '{"prefix":"p:"}'},
    {op: 'tx', args: '{"name":"basic"}'}, {op: 'list', opts: '{"prefix":"p:"}'},
    {op: 'tx', args: '{"name":"rollback"}'}, {op: 'list', opts: '{"prefix":"p:"}'},
    {op: 'tx', args: '{"name":"throw"}'}, {op: 'get', k: 'x'},
    {op: 'tx', args: '{"name":"after-rollback"}'}, {op: 'get', k: 'x'}, {op: 'get', k: 'y'},
    {op: 'tx', args: JSON.stringify({name: 'alarm-commit', t: 1900000000456})}, {op: 'alarm-get'}, {op: 'tx', args: '{"name":"alarm-del-rollback"}'}, {op: 'alarm-get'}, {op: 'alarm-del'},
    {op: 'tx', args: '{"name":"misuse"}'}, {op: 'tx', args: '{"name":"nest-block"}'}, {op: 'tx', args: '{"name":"no-result"}'},
    {op: 'tx', args: JSON.stringify({name: 'list', opts: '{"prefix":"p:","limit":1}'})}, {op: 'tx', args: JSON.stringify({name: 'list', opts: '{"prefix":1}'})},
    {op: 'tx-outside', which: 'put'}, {op: 'tx-outside', which: 'finish'},
    {op: 'alarm-get'}, {op: 'alarm-set', t: 1900000000000}, {op: 'alarm-get'}, {op: 'alarm-del'}, {op: 'alarm-get'}, {op: 'alarm-set', t: -1}, {op: 'alarm-set', t: 8640000000000001}, {op: 'retry'},
    {op: 'tx', args: JSON.stringify({name: 'alarm', t: 1900000000123})}, {op: 'alarm-get'}, {op: 'alarm-del'},
    {op: 'block', name: 'echo', args: '{"a": [1, 2],"中":"文"}'}, {op: 'block', name: 'kind', args: '{}'}, {op: 'block', name: 'store', args: '{}'}, {op: 'block', name: 'tx-inside', args: '{}'},
    {op: 'block', name: 'fail', args: '{}'}, {op: 'block', name: 'bad-result', args: '{}'}, {op: 'block', name: 'double-result', args: '{}'}, {op: 'get', k: 'double-msg'},
    {op: 'block', name: 'no-result', args: '{}'}, {op: 'block', name: 'nested', args: '{}'}, {op: 'block', name: 'echo', args: 'notjson'}, {op: 'block', name: '', args: '{}'},
    {op: 'put', k: 'k', text: '{'}, {op: 'put', k: 'k', text: '{"$句柄":"1"}'}, {op: 'put', k: '', text: '1'}, {op: 'put-big', k: 'k', exp: 21}, {op: 'put-big', k: '大', exp: 20}, {op: 'getlen', k: '大'},
    {op: 'clear'}, {op: 'list', opts: '{}'}, {op: 'alarm-get'}
  ];
  await 真发({op: 'clear'});
  const 差异 = [];
  for (const 体 of 序) {
    const [甲, 乙] = [await 模拟发(体), await 真发(体)];
    if (甲 !== 乙) 差异.push(JSON.stringify(体).slice(0, 100) + '\n  模拟：' + 甲.slice(0, 200) + '\n  真器：' + 乙.slice(0, 200));
  }
  assert.deepEqual(差异, [], `共 ${序.length} 项，差异 ${差异.length} 项`);
});

test('慢速：原生 blockConcurrencyWhile 与事务回调超过 30 秒，平台重置对象', {skip: 选项.skip || (含慢速 ? false : '设置 含慢速验证=1 才运行（各约 35 秒）')}, async () => {
  for (const [路径, 名前缀] of [['/__raw-block-long?ms=35000', 'e11a'], ['/__raw-tx-hold?ms=36000', 'e11b']]) {
    const 名 = 新名(名前缀);
    const 前 = await 探针(名, '/__instance');
    const 起 = Date.now();
    const 文 = await fetch(基址 + 路径 + '&do=' + 名).then(回 => 回.text()).catch(错 => '抛出 ' + 错);
    const 耗 = Date.now() - 起;
    assert.equal(耗 >= 29000 && 耗 < 34000, true, `平台约在 30 秒中止回调，实际 ${耗} 毫秒：${文.slice(0, 80)}`);
    assert.notEqual((await 探针(名, '/__instance')).实例号, 前.实例号, '对象被重置');
  }
});
