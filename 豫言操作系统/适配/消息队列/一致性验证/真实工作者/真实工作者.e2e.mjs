// 文言：以真本地 workerd（wrangler dev）综验消息队列、定时事件、邮件发送、文字规整、日志五约，验平台之真义，补 Node 模拟之不及。
// 汉语：把已构建的“平台综合一致性”产物复制到临时目录，配上队列、定时、邮件、KV 绑定，用 wrangler dev --local 启动，再逐场景发请求、读 workerd 日志断言。约需 1 分钟。
// 用法：在私有暂存根目录执行 `node <本文件>`；环境变量：
//   YY_DIST_ROOT     产物根目录，默认 ./dist（其下应有 平台综合一致性/）
//   E2E_WRANGLER     wrangler.js 的路径，默认取相邻云仓 工具/包管理服务/node_modules 里的 wrangler
//   E2E_PATCH_HOST=1 产物 宿主.mjs 缺少原语 豫言_云工_授权绑定存在 时（旧宿主），在临时副本里补上（仅验证用；正式部署须用含该原语的宿主）
//   E2E_FILTER       只运行名称含此文字的场景
//   E2E_SAVE_LOG     把 wrangler 输出保存到此路径
//   E2E_KEEP=1       结束后保留临时 Worker 目录以便排查
import {spawn} from 'node:child_process';
import {cpSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync} from 'node:fs';
import {createServer} from 'node:net';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import assert from 'node:assert/strict';

const 本目录 = path.dirname(fileURLToPath(import.meta.url));
const 产物 = path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '平台综合一致性');
const 语言仓根 = path.resolve(本目录, '../../../../..');
const wrangler = process.env.E2E_WRANGLER ?? path.resolve(语言仓根, '../yuyan-cloud/工具/包管理服务/node_modules/wrangler/bin/wrangler.js');
if (!existsSync(path.join(产物, '入口.mjs'))) throw new Error('缺少构建产物：' + 产物 + '（先构建 真实工作者/应用，输出名 平台综合一致性）');
if (!existsSync(wrangler)) throw new Error('找不到 wrangler：' + wrangler + '（用环境变量 E2E_WRANGLER 指定）');

const 去色 = 串 => 串.replace(/\x1b\[[0-9;]*[A-Za-z]/g, '');
const 睡 = 毫秒 => new Promise(成 => setTimeout(成, 毫秒));
const 取空闲端口 = () => new Promise((成, 败) => { const 服 = createServer(); 服.unref(); 服.on('error', 败); 服.listen(0, '127.0.0.1', () => { const {port} = 服.address(); 服.close(() => 成(port)); }); });

// ---------- 准备临时 Worker ----------
const 工作 = mkdtempSync(path.join(process.env.TMPDIR ?? tmpdir(), 'yy-真实工作者-'));
cpSync(产物, 工作, {recursive: true});
const 宿主路径 = path.join(工作, '宿主.mjs');
let 宿主源码 = readFileSync(宿主路径, 'utf8');
if (!宿主源码.includes('豫言_云工_授权绑定存在')) {
  if (process.env.E2E_PATCH_HOST !== '1') throw new Error('产物 宿主.mjs 缺少原语 豫言_云工_授权绑定存在（见 适配/邮件发送/说明.汉语.md；请用含该原语的宿主）；仅验证旧宿主时可设 E2E_PATCH_HOST=1 在临时副本里补上');
  const 锚 = '        豫言_云工_绑定句柄: (种类, 名) => 句柄.登记(绑定(环境, 许可, 名, 文字(种类))),\n';
  assert.ok(宿主源码.includes(锚), '找不到补丁锚点');
  宿主源码 = 宿主源码.replace(锚, 锚 + `        豫言_云工_授权绑定存在: (种类, 名) => {
          const 类别 = 文字(种类), 名称 = 文字(名);
          if (!许可[类别]?.includes(名称)) throw Error('未授权的' + 类别 + '绑定：' + 名称);
          return Object.hasOwn(环境, 名称) && 环境[名称] != null;
        },\n`);
  writeFileSync(宿主路径, 宿主源码);
}
writeFileSync(path.join(工作, 'wrangler.jsonc'), JSON.stringify({
  name: 'yuyan-e2e', main: '入口.mjs', compatibility_date: '2026-09-10',
  queues: {producers: [{binding: 'E2E_QUEUE', queue: 'yuyan-e2e-q'}], consumers: [{queue: 'yuyan-e2e-q', max_batch_size: 5, max_batch_timeout: 1, max_retries: 3}]},
  triggers: {crons: ['*/10 * * * *']},
  send_email: [{name: 'EMAIL', allowed_sender_addresses: ['noreply@yuyan-lang.org']}],
  kv_namespaces: [{binding: 'KV', id: 'e2e-kv'}],
}, null, 2));

// ---------- 启停 wrangler dev ----------
const 端口 = await 取空闲端口(), 检查口 = await 取空闲端口();
const 状态 = path.join(工作, '状态');
mkdirSync(状态, {recursive: true});
const 日志行们 = [];
const 子 = spawn(process.execPath, [wrangler, 'dev', '--local', '-c', 'wrangler.jsonc', '--test-scheduled', '--port', String(端口), '--inspector-port', String(检查口), '--persist-to', 状态, '--ip', '127.0.0.1'], {
  cwd: 工作, detached: true, stdio: ['ignore', 'pipe', 'pipe'],
  env: {...process.env, WRANGLER_SEND_METRICS: 'false', WRANGLER_HIDE_BANNER: 'true', CI: '1', NO_COLOR: '1', FORCE_COLOR: '0', WRANGLER_LOG_PATH: path.join(状态, '日志'), WRANGLER_REGISTRY_PATH: path.join(状态, '注册表')},
});
// 文言：wrangler 派生孙进程，必以进程组尽杀之。汉语：wrangler 会派生 workerd 等子进程，退出时按进程组清理。
const 杀 = () => { try { process.kill(-子.pid, 'SIGKILL'); } catch { /* 已亡 */ } };
process.on('exit', () => { 杀(); if (process.env.E2E_KEEP !== '1') { try { rmSync(工作, {recursive: true, force: true}); } catch { /* 忽略 */ } } });
for (const 信号 of ['SIGINT', 'SIGTERM']) process.on(信号, () => { 杀(); process.exit(1); });
let 缓 = '';
const 收 = 数据 => { 缓 += 数据.toString('utf8'); let 位; while ((位 = 缓.indexOf('\n')) >= 0) { 日志行们.push(去色(缓.slice(0, 位)).replace(/\r$/, '')); 缓 = 缓.slice(位 + 1); } };
子.stdout.on('data', 收); 子.stderr.on('data', 收);
const 基址 = `http://127.0.0.1:${端口}`;
{
  const 截止 = Date.now() + 90000;
  while (!日志行们.some(行 => /Ready on/.test(行))) {
    if (Date.now() > 截止) { 杀(); throw new Error('wrangler dev 未就绪：\n' + 日志行们.slice(-30).join('\n')); }
    await 睡(200);
  }
  await 睡(1500);
}

const 请 = async (径, {方法 = 'POST', 体 = ''} = {}) => {
  const 回 = await fetch(基址 + 径, {method: 方法, body: 方法 === 'GET' ? undefined : 体});
  return {状态: 回.status, 文: await 回.text()};
};
const 候日志 = async (谓词, 毫秒 = 15000, 起 = 0) => { const 截止 = Date.now() + 毫秒; while (Date.now() < 截止) { const 命中 = 日志行们.slice(起).filter(谓词); if (命中.length) return 命中; await 睡(150); } return []; };
const 消息行 = (起, 标) => 日志行们.slice(起).filter(行 => 行.includes('E2E消息') && 行.includes(标));
const 结果 = [];
const 场景 = async (名, 函数) => {
  if (process.env.E2E_FILTER && !名.includes(process.env.E2E_FILTER)) return;
  const 始 = Date.now();
  try { await 函数(); 结果.push([名, '通过']); console.log('✔', 名, `(${Date.now() - 始}ms)`); }
  catch (错) { 结果.push([名, '失败']); console.log('✖', 名, '\n   ', String(错?.stack ?? 错).split('\n').slice(0, 6).join('\n    ')); }
};

// ---------- 场景 ----------
await 场景('事件种类：fetch', async () => { assert.deepEqual(await 请('/kind', {方法: 'GET'}), {状态: 200, 文: 'fetch'}); });

await 场景('文字规整：workerd 里的 JS 语义与规范样例一致', async () => {
  for (const [径, 入, 期] of [['/norm/trim', '\u3000 a b\u3000', 'a b'], ['/norm/lower', 'ABC İ ΑΣ', 'abc i̇ ας'], ['/norm/len', 'a😀豫', '4'], ['/norm/trim', '\uFEFFx\uFEFF', 'x'], ['/norm/lower', 'ÀÉÎ', 'àéî']]) {
    const r = await 请(径, {体: 入});
    assert.equal(r.状态, 200, 径);
    assert.equal(r.文, 期, JSON.stringify(入));
  }
});

await 场景('日志：三级分别落到 error、warn、info，超长文字截断并带标记', async () => {
  const 起 = 日志行们.length;
  for (const 级 of ['error', 'warn', 'info']) assert.equal((await 请('/log/' + 级, {体: `E2E日志-${级}-你好`})).状态, 200);
  assert.equal((await 请('/log/info', {体: 'E2E长日志' + 'x'.repeat(20000)})).状态, 200);
  await 睡(1500);
  const 新 = 日志行们.slice(起);
  assert.ok(新.some(行 => /\[ERROR\]/.test(行) && 行.includes('E2E日志-error-你好')), 'error 级应显示为 [ERROR]');
  assert.ok(新.some(行 => /\[WARNING\]/.test(行) && 行.includes('E2E日志-warn-你好')), 'warn 级应显示为 [WARNING]');
  assert.ok(新.some(行 => !/\[(ERROR|WARNING)\]/.test(行) && 行.includes('E2E日志-info-你好')), 'info 级应是普通日志');
  const 长行 = 新.filter(行 => 行.includes('E2E长日志'));
  assert.equal(长行.length, 1);
  assert.ok(长行[0].includes('…（已截断）'));
  assert.ok(Buffer.byteLength(长行[0].slice(长行[0].indexOf('E2E长日志'))) <= 8192);
});

await 场景('队列：单条投递、消费、确认；批次 JSON 形状、attempts、毫秒 timestamp、body 原样', async () => {
  const 起 = 日志行们.length;
  assert.deepEqual(await 请('/send?d=0', {体: JSON.stringify({op: 'ack', 标: 'single-1', 文: '中文"引号"'})}), {状态: 200, 文: '成功'});
  const 批行 = await 候日志(行 => 行.startsWith('E2E批次 ') && 行.includes('single-1'), 20000, 起);
  assert.ok(批行.length >= 1, '应收到消费日志：' + 日志行们.slice(起).slice(-15).join('\n'));
  const 批 = JSON.parse(批行[0].slice('E2E批次 '.length));
  assert.equal(批.queue, 'yuyan-e2e-q');
  assert.equal(批.messages.length, 1);
  assert.deepEqual(Object.keys(批.messages[0]), ['id', 'attempts', 'timestamp', 'body']);
  assert.equal(批.messages[0].attempts, 1);
  assert.deepEqual(批.messages[0].body, {op: 'ack', 标: 'single-1', 文: '中文"引号"'});
  assert.ok(Number.isSafeInteger(批.messages[0].timestamp) && Math.abs(批.messages[0].timestamp - Date.now()) < 120000, '时间戳应是当前毫秒');
  await 睡(3000);
  assert.equal(消息行(起, 'single-1').length, 1, '已确认的消息不应重投');
});

await 场景('队列：延迟重试，attempts 递增，超过 max_retries 后不再投递', async () => {
  const 起 = 日志行们.length;
  assert.equal((await 请('/send?d=0', {体: JSON.stringify({op: 'retry', delay: 1, 标: 'retry-1'})})).文, '成功');
  await 睡(9000);
  const 行 = 消息行(起, 'retry-1');
  assert.deepEqual(行.map(项 => Number(项.match(/attempts=(\d+)/)[1])), [1, 2, 3, 4]);
});

await 场景('队列：未捕获异常使本批重投，attempts 递增', async () => {
  const 起 = 日志行们.length;
  assert.equal((await 请('/send?d=0', {体: JSON.stringify({op: 'boom', 标: 'boom-1'})})).文, '成功');
  await 睡(8000);
  assert.deepEqual(消息行(起, 'boom-1').map(项 => Number(项.match(/attempts=(\d+)/)[1])), [1, 2, 3, 4]);
});

await 场景('队列：同批两条，先确认后抛异常——记录本地 Miniflare 的重投行为', async () => {
  const 起 = 日志行们.length;
  assert.equal((await 请('/batch', {体: JSON.stringify([{body: {op: 'ack', 标: 'mix-acked'}}, {body: {op: 'boom', 标: 'mix-boom'}}])})).文, '成功');
  await 睡(9000);
  const 已确 = 消息行(起, 'mix-acked').length, 炸 = 消息行(起, 'mix-boom').length;
  console.log(`    投递次数：已确认者 ${已确} 次，抛异常者 ${炸} 次（本地 Miniflare 处理失败时整批重投，含已确认者；生产平台文档承诺已确认者不重投）`);
  assert.ok(炸 >= 2, '抛异常的消息应被重投');
  assert.ok(已确 >= 1);
});

await 场景('队列：无显式确认或重试而正常结束，视为成功，不重投', async () => {
  const 起 = 日志行们.length;
  assert.equal((await 请('/send?d=0', {体: JSON.stringify({op: 'skip', 标: 'skip-1'})})).文, '成功');
  await 睡(6000);
  assert.equal(消息行(起, 'skip-1').length, 1);
});

await 场景('队列：消息体大小的平台边界（记录本地 Miniflare 的限额，适配按 128000 / 256000 字节校验）', async () => {
  const 体 = 字节 => '"' + 'x'.repeat(字节 - 2) + '"';
  const 单结果 = {};
  for (const 字节 of [100000, 127998, 128000, 128001, 131072]) 单结果[字节] = (await 请('/send?d=0', {体: 体(字节)})).文;
  console.log('    单条：' + Object.entries(单结果).map(([k, v]) => `${k}→${v.slice(0, 80)}`).join('\n          '));
  assert.equal(单结果[100000], '成功');
  assert.equal(单结果[128000], '成功', '本地平台接受恰 128000 字节的单条');
  assert.equal(单结果[128001], '失败：消息体超过 128000 字节', '超过 128000 字节由适配先拒绝');
  const 批结果 = {};
  for (const [标签, 诸体] of [['1×127990', [127990]], ['1×128000', [128000]], ['2×127000', [127000, 127000]], ['2×128000', [128000, 128000]], ['3×85000', [85000, 85000, 85000]], ['3×90000', [90000, 90000, 90000]]]) 批结果[标签] = (await 请('/batch', {体: JSON.stringify(诸体.map(字节 => ({body: 'x'.repeat(字节 - 2)})))})).文;
  console.log('    批量：' + Object.entries(批结果).map(([k, v]) => `${k}→${v.slice(0, 80)}`).join('\n          '));
  for (const 标签 of ['1×127990', '1×128000', '2×127000', '2×128000', '3×85000']) assert.equal(批结果[标签], '成功', 标签 + '：本地平台每条以 128000 字节为限，合计 256000 字节可投');
  assert.equal(批结果['3×90000'], '失败：批量消息体合计超过 256000 字节');
});

await 场景('队列：批量投递（含各条延迟）', async () => {
  const 起 = 日志行们.length;
  const 始 = Date.now();
  assert.deepEqual(await 请('/batch', {体: JSON.stringify([{body: {op: 'ack', 标: 'batch-a'}}, {body: {op: 'ack', 标: 'batch-b'}}, {body: {op: 'ack', 标: 'batch-delay'}, delaySeconds: 4}])}), {状态: 200, 文: '成功'});
  assert.ok((await 候日志(行 => 行.includes('E2E消息') && 行.includes('batch-a'), 10000, 起)).length);
  assert.ok((await 候日志(行 => 行.includes('E2E消息') && 行.includes('batch-b'), 10000, 起)).length);
  assert.ok((await 候日志(行 => 行.includes('E2E消息') && 行.includes('batch-delay'), 12000, 起)).length, '延迟消息应最终送达');
  assert.ok(Date.now() - 始 >= 3500, '延迟消息不得早于约 4 秒送达');
});

await 场景('定时：cron 与计划时刻（毫秒整数），事件内异步写入在调用结束前完成', async () => {
  const 起 = 日志行们.length;
  const 回 = await fetch(`${基址}/cdn-cgi/handler/scheduled?cron=*%2F10+*+*+*+*&time=1758801600000`);
  assert.equal(回.status, 200);
  const 行 = await 候日志(行 => 行.startsWith('E2E定时 '), 15000, 起);
  assert.ok(行.length >= 1, '应有定时日志：' + 日志行们.slice(起).slice(-15).join('\n'));
  assert.deepEqual(JSON.parse(行[0].slice('E2E定时 '.length)), {cron: '*/10 * * * *', scheduledTime: 1758801600000});
  assert.equal((await 请('/kv', {体: '定时'})).文, 行[0].slice('E2E定时 '.length));
});

await 场景('定时：不带 time 参数时 scheduledTime 为当前毫秒', async () => {
  const 起 = 日志行们.length;
  await fetch(`${基址}/cdn-cgi/handler/scheduled?cron=0+*+*+*+*`);
  const 行 = await 候日志(行 => 行.startsWith('E2E定时 '), 10000, 起);
  const 事件 = JSON.parse(行[0].slice('E2E定时 '.length));
  assert.equal(事件.cron, '0 * * * *');
  assert.ok(Math.abs(事件.scheduledTime - Date.now()) < 60000, String(事件.scheduledTime));
});

await 场景('非队列、非定时事件里调用对应函数是可捕获异常', async () => {
  assert.deepEqual(await 请('/sched', {方法: 'GET'}), {状态: 400, 文: '失败：当前事件不是定时事件'});
  assert.deepEqual(await 请('/consume', {方法: 'GET'}), {状态: 400, 文: '失败：当前事件不是队列事件'});
});

await 场景('邮件：本地 send_email 收到的 From/To/Subject/Text 与输入逐字相同；平台拒绝时原因不含地址与令牌；注入不到达平台', async () => {
  const 起 = 日志行们.length;
  const 令牌 = 'tok-e2e-0123456789abcdef';
  const 正文 = `打开链接：https://x.example/个人#verify=${令牌}\n\n链接 60 分钟内有效。\r\n"引号" \\反斜线\\ 😀\n` + '行\n'.repeat(50);
  const 主体 = `?from=${encodeURIComponent('noreply@yuyan-lang.org')}&to=${encodeURIComponent('user@example.com')}&subject=${encodeURIComponent('豫言：验证邮箱')}`;
  assert.deepEqual(await 请('/mail' + 主体, {体: 正文}), {状态: 200, 文: '成功'});
  const 块 = await 候日志(行 => 行.includes('send_email binding called'), 8000, 起);
  assert.equal(块.length, 1, '本地平台应恰收到一封邮件');
  const 序 = 日志行们.indexOf(块[0], 起);
  const 头 = Object.fromEntries(日志行们.slice(序 + 1, 序 + 6).filter(行 => /^(From|To|Subject|Text): /.test(行)).map(行 => [行.slice(0, 行.indexOf(':')), 行.slice(行.indexOf(': ') + 2)]));
  assert.equal(头.From, 'noreply@yuyan-lang.org');
  assert.equal(头.To, 'user@example.com');
  assert.equal(头.Subject, '豫言：验证邮箱');
  assert.equal(readFileSync(头.Text, 'utf8'), 正文, '正文文件应与输入逐字相同');
  const 坏 = await 请(`/mail?from=${encodeURIComponent('intruder@evil.example')}&to=${encodeURIComponent('user@example.com')}&subject=x`, {体: `令牌 ${令牌}`});
  assert.equal(坏.状态, 200);
  assert.match(坏.文, /^失败：邮件发送失败/);
  assert.ok(!坏.文.includes('evil') && !坏.文.includes('user@') && !坏.文.includes(令牌), '原因不得含地址与令牌：' + 坏.文);
  const 之前 = 日志行们.filter(行 => 行.includes('send_email binding called')).length;
  assert.equal((await 请('/mail?from=noreply%40yuyan-lang.org&to=' + encodeURIComponent('a@b.c\r\nBcc: x@y.z') + '&subject=x', {体: 'x'})).文, '失败：收件地址无效');
  await 睡(1500);
  assert.equal(日志行们.filter(行 => 行.includes('send_email binding called')).length, 之前, '注入的请求不得到达平台');
});

// ---------- 收尾 ----------
杀();
await 睡(500);
if (process.env.E2E_SAVE_LOG) writeFileSync(process.env.E2E_SAVE_LOG, 日志行们.join('\n'));
const 失败们 = 结果.filter(项 => 项[1] === '失败');
console.log(`\n场景 ${结果.length} 个，通过 ${结果.length - 失败们.length}，失败 ${失败们.length}`);
process.exit(失败们.length ? 1 : 0);
