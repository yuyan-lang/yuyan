// 文言：以真本地 workerd（Miniflare 之 Worker Loader）与真豫言 Wasm 验隔离运行之约；另以假加载器验隔离体之标识、模块与限额。
// 汉语：隔离运行接口的云工适配一致性验证。共两部分：（一）真实 workerd：Miniflare 装载试验应用的构建产物，Worker Loader 绑定 LOADER 真实加载子 Worker，
// 用一个由豫言编译的文件式探针程序（./探针）逐项验证规范里的返回、限额、失败情形、句柄不泄漏；（二）Node 假加载器：核对同（范围，摘要）取得同一标识、
// 异范围异标识、子 Worker 的模块集合、CPU 与子请求限额、请求体逐字节等于输入。
// 用法（在私有暂存目录里执行，其中有 yy3_bs、库/、dist/）：
//   1. 建应用.sh <暂存目录> 云工 <本目录>/应用 隔离运行一致性
//   2. node --test <本目录>/隔离运行.test.mjs
// 环境变量：YY_DIST_ROOT 产物根目录（默认 ./dist）；YY_PROBE_WASM 已编译好的探针 Wasm（缺省则用暂存里的 yy3_bs 现编）；E2E_MINIFLARE miniflare 入口路径
// （缺省取相邻云仓 工具/包管理服务/node_modules）。注意：本地 workerd 不执行 CPU 限额，故不运行死循环程序。
import {after, before, test} from 'node:test';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {execFileSync} from 'node:child_process';
import {cpSync, existsSync, mkdtempSync, readFileSync, realpathSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';

const 本目录 = path.dirname(fileURLToPath(import.meta.url));
const 语言仓根 = path.resolve(本目录, '../../../..');
const 产物 = path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '隔离运行一致性');
const miniflare路径 = process.env.E2E_MINIFLARE ?? path.resolve(语言仓根, '../yuyan-cloud/工具/包管理服务/node_modules/miniflare/dist/src/index.js');
const 核心程序路径 = path.resolve(语言仓根, '../yuyan-cloud/工具/云端项目核心/产物/yy项目核心.wasm');
if (!existsSync(path.join(产物, '入口.mjs'))) throw new Error('缺少构建产物：' + 产物 + '（先构建 应用/，输出名 隔离运行一致性，见本目录说明）');
if (!existsSync(miniflare路径)) throw new Error('找不到 miniflare：' + miniflare路径 + '（用环境变量 E2E_MINIFLARE 指定）');

const 摘要 = 字节 => createHash('sha256').update(字节).digest('hex');
// 文言：探针程序由豫言编器现编。汉语：在暂存目录里用 yy3_bs 把 ./探针 编译为 WasmGC 文件式程序（包描述以 .txt 存放，复制到临时目录改回原名）。
function 取探针字节() {
  if (process.env.YY_PROBE_WASM) return readFileSync(process.env.YY_PROBE_WASM);
  const 编译器 = path.resolve('yy3_bs');
  if (!existsSync(编译器)) throw new Error('需在私有暂存目录运行（其中有 yy3_bs），或用 YY_PROBE_WASM 指定已编译的探针');
  const 临时 = realpathSync(mkdtempSync(path.join(process.env.TMPDIR ?? tmpdir(), 'yy-探针-')));
  const 库根 = realpathSync(path.resolve('库'));
  try {
    cpSync(path.join(本目录, '探针'), 临时, {recursive: true});
    const 包文件 = path.join(临时, '隔离运行探针。包。豫');
    cpSync(包文件 + '.txt', 包文件);
    rmSync(包文件 + '.txt');
    const 上下文 = path.join(临时, '包上下文');
    writeFileSync(上下文, ['豫构包上下文二', `豫言\t标准库\t${库根}/标准库/标准库。包。豫`, `豫言\t隔离运行探针\t${包文件}\t豫言\t标准库`].join('\n') + '\n');
    const 输出 = path.join(临时, '探针.wasm');
    execFileSync(编译器, [path.join(临时, '入口。豫'), '--package-context', 上下文, '--target=wasmgc', '-o', 输出], {stdio: ['ignore', 'pipe', 'pipe']});
    return readFileSync(输出);
  } finally { rmSync(临时, {recursive: true, force: true}); }
}
const 探针字节 = 取探针字节();
const 探针摘要 = 摘要(探针字节);
const 探针文 = 探针字节.toString('base64');

// ---------- 真实 workerd ----------
const {Miniflare, convertV4MiniflareOptions} = await import(pathToFileURL(miniflare路径).href);
function 装载模块(入口路径) {
  const 模块们 = [], 已见 = new Set();
  const 载 = 路径 => {
    if (已见.has(路径)) return;
    已见.add(路径);
    const 是wasm = 路径.endsWith('.wasm'), 是json = 路径.endsWith('.json');
    const 原文 = readFileSync(路径, 是wasm ? undefined : 'utf8');
    模块们.push({type: 是wasm ? 'CompiledWasm' : 'ESModule', path: 路径, contents: 是json ? 'export default ' + 原文 : 原文});
    // 文言：只循行首之 import/export…from，免读入动态资源字串里内嵌之源码。汉语：只解析行首的静态导入，不解析 动态资源.mjs 字符串里内嵌的模块源码。
    if (!是wasm && !是json) for (const 项 of 原文.matchAll(/^\s*(?:import|export)\b[^;\n]*?\bfrom\s+['"]([^'"]+)['"]/gm)) if (项[1].startsWith('.')) 载(path.resolve(path.dirname(路径), 项[1]));
  };
  载(入口路径);
  return 模块们;
}
let mf;
before(async () => {
  mf = new Miniflare(convertV4MiniflareOptions({
    modules: 装载模块(path.join(产物, '入口.mjs')), modulesRoot: 产物, compatibilityDate: '2026-09-10',
    workerLoaders: {LOADER: {}},
  }));
  await mf.ready;
});
after(async () => { await mf?.dispose(); });

// 文言：试验应用之体乃 JSON；答亦 JSON。汉语：向试验应用提交 {runner,scope,digest,program,input,repeat,tolerant}，返回其 JSON 答复。
const 提交 = async 体 => {
  const 回 = await mf.dispatchFetch('https://隔离运行.test/', {method: 'POST', body: JSON.stringify(体)});
  const 文 = await 回.text();
  assert.equal(回.status, 200, 文.slice(0, 500));
  return JSON.parse(文);
};
const 探针输入 = (命令, files = {}) => JSON.stringify({args: [命令], files});
// 文言：运毕成功，返其内层结果。汉语：要求接口调用没有抛异常，返回内层结果对象。
const 运行 = async (命令, {files = {}, 范围 = '范围甲', ...额外} = {}) => {
  const 答 = await 提交({scope: 范围, digest: 探针摘要, program: 探针文, input: 探针输入(命令, files), ...额外});
  assert.equal(答.ok, true, JSON.stringify(答).slice(0, 500));
  return JSON.parse(答.result);
};
// 文言：运而必抛，返其消息。汉语：要求接口调用抛出可捕获异常，返回异常消息。
const 应抛 = async (命令, 额外 = {}) => {
  const 答 = await 提交({scope: '范围甲', digest: 探针摘要, program: 探针文, input: 探针输入(命令), ...额外});
  assert.equal(答.ok, false, '应当抛出异常：' + JSON.stringify(答).slice(0, 300));
  return 答.error;
};

test('回显：文件回传只含数据与响应目录，输入的数据文件原样保留', async () => {
  const 果 = await 运行('echo', {files: {'/请求/路径.txt': '/x', '/数据/旧.txt': '旧'}});
  assert.equal(果.ok, true);
  assert.equal(果.exitCode, 0);
  assert.equal(果.stdout, '你好，隔离\n');
  assert.equal(果.stderr, '');
  assert.deepEqual(Object.keys(果.files).sort(), ['/响应/正文.txt', '/响应/状态.txt', '/数据/旧.txt', '/数据/记录.txt']);
  assert.equal(果.files['/数据/旧.txt'], '旧');
  assert.equal(果.files['/数据/记录.txt'], '见：/x');
  assert.equal(果.files['/响应/正文.txt'], '回显：/x');
  assert.equal(Object.hasOwn(果.files, '/其他/密.txt'), false);
  assert.equal(Object.keys(果.files).some(名 => 名.startsWith('/请求/')), false);
  assert.deepEqual(Object.keys(果), ['ok', 'stdout', 'stderr', 'exitCode', 'files']);
});

test('输入路径依斜线规整（补首斜线、折叠点段），文字按严格 UTF-8 往返', async () => {
  const 果 = await 运行('echo', {files: {'请求/./甲/../路径.txt': '路径：😀é汉', '/数据//新.txt': '换\n行'}});
  assert.equal(果.files['/数据/记录.txt'], '见：路径：😀é汉');
  assert.equal(果.files['/数据/新.txt'], '换\n行');
});

test('程序自身的失败是返回值：退出码、陷阱、标准错误、输出超限', async () => {
  const 败 = await 运行('fail');
  assert.equal(败.ok, false);
  assert.equal(败.exitCode, 3);
  assert.equal(Object.hasOwn(败, 'error'), false);
  const 陷 = await 运行('trap');
  assert.equal(陷.ok, false);
  assert.equal(陷.exitCode, 1);
  assert.equal(typeof 陷.error, 'string');
  assert.ok(陷.error.length > 0);
  const 误 = await 运行('stderr');
  assert.equal(误.ok, true);
  assert.equal(误.stdout, '照常\n');
  assert.equal(误.stderr, '警告：探针\n');
  const 大 = await 运行('bigout');
  assert.equal(大.ok, false);
  assert.equal(大.exitCode, 1);
  assert.equal(大.error, '输出超过 64 KB');
  const 未知 = await 运行('没有这个命令');
  assert.equal(未知.ok, true);
  assert.equal(未知.stdout, '未知命令：没有这个命令\n');
});

test('输出文件的个数、字节数与 UTF-8 限额：合规全部返回，越限整体失败为异常', async () => {
  const 满 = await 运行('many:256');
  assert.equal(Object.keys(满.files).length, 256);
  assert.match(await 应抛('many:257'), /^隔离运行失败：500：运行数据超过上限$/);
  const 大 = await 运行('big:1048576');
  assert.equal(大.files['/数据/大.txt'].length, 1048576);
  assert.match(await 应抛('big:1048577'), /^隔离运行失败：500：运行数据超过上限$/);
  assert.match(await 应抛('badutf8'), /^隔离运行失败：500：输出文件不是有效 UTF-8：\/数据\/坏\.txt$/);
});

test('参数违规：摘要、Base64、范围、运行器、程序长度', async () => {
  assert.equal(await 应抛('echo', {digest: '0'.repeat(64)}), '运行产物摘要不匹配');
  assert.equal(await 应抛('echo', {digest: 探针摘要.toUpperCase()}), '产物摘要无效');
  assert.equal(await 应抛('echo', {digest: 探针摘要 + '0'}), '产物摘要无效');
  assert.equal(await 应抛('echo', {digest: ''}), '产物摘要无效');
  assert.equal(await 应抛('echo', {program: '@@@@'}), '运行产物无效：不是有效的 Base64');
  assert.equal(await 应抛('echo', {program: 'A'}), '运行产物无效：不是有效的 Base64');
  assert.equal(await 应抛('echo', {program: ''}), '运行产物无效');
  assert.equal(await 应抛('echo', {program: 'A'.repeat(4194305)}), '运行产物无效');
  assert.equal(await 应抛('echo', {scope: ''}), '隔离范围无效');
  assert.equal(await 应抛('echo', {scope: '甲\u0001乙'}), '隔离范围无效');
  assert.equal(await 应抛('echo', {scope: '\u007f'}), '隔离范围无效');
  assert.equal(await 应抛('echo', {scope: 'x'.repeat(129)}), '隔离范围无效');
  assert.equal(await 应抛('echo', {scope: '甲'.repeat(43)}), '隔离范围无效', '43 个汉字是 129 字节');
  assert.equal(await 应抛('echo', {runner: ''}), '运行器名无效');
  assert.equal(await 应抛('echo', {runner: 'x'.repeat(129)}), '运行器名无效');
  for (const 范围 of ['x'.repeat(128), '甲'.repeat(42), '站点:甲/乙 丙']) assert.equal((await 运行('echo', {范围})).ok, true, 范围);
});

test('运行器：未授权与不存在的绑定得可捕获异常，不中止事件', async () => {
  assert.equal(await 应抛('echo', {runner: 'NOT_ALLOWED'}), '隔离运行失败：未授权的LOADER绑定：NOT_ALLOWED');
  assert.equal(await 应抛('echo', {runner: 'MISSING_LOADER'}), '隔离运行失败：绑定不存在：MISSING_LOADER');
  assert.equal((await 运行('echo')).ok, true, '异常之后同一应用照常服务');
});

test('输入违规：非 JSON、args、files、超限；非 Wasm 字节', async () => {
  assert.equal(await 应抛('echo', {input: '不是JSON'}), '隔离运行输入无效：输入不是有效 JSON');
  assert.equal(await 应抛('echo', {input: JSON.stringify({args: [1]})}), '隔离运行输入无效：args 须为字符串数组');
  assert.equal(await 应抛('echo', {input: JSON.stringify({args: 'echo'})}), '隔离运行输入无效：args 须为字符串数组');
  assert.equal(await 应抛('echo', {input: JSON.stringify({files: {'/a': 1}})}), '隔离运行输入无效：files 须为路径到文字的对象');
  assert.equal(await 应抛('echo', {input: JSON.stringify({files: []})}), '隔离运行输入无效：files 须为路径到文字的对象');
  assert.equal(await 应抛('echo', {input: JSON.stringify({args: ['x'.repeat(1572864)]})}), '运行输入超过上限');
  const 缺省 = JSON.parse((await 提交({scope: '范围甲', digest: 探针摘要, program: 探针文, input: '{}'})).result);
  assert.equal(缺省.ok, true, 'args 与 files 缺省为空');
  assert.equal(缺省.stdout, '未知命令：\n');
  const 非wasm = Buffer.from('这不是 Wasm');
  const 答 = await 提交({scope: '范围乙', digest: 摘要(非wasm), program: 非wasm.toString('base64'), input: '{}'});
  assert.equal(答.ok, false);
  assert.equal(答.error, '隔离运行失败：动态 Worker 程序不是有效 Wasm');
});

test('Base64 宽容 ASCII 空白与缺省填充（同 atob）', async () => {
  const 折行 = 探针文.replace(/=+$/, '').replace(/(.{76})/g, '$1\r\n ') + '\n';
  const 答 = await 提交({scope: '范围甲', digest: 探针摘要, program: 折行, input: 探针输入('echo')});
  assert.equal(答.ok, true, JSON.stringify(答).slice(0, 300));
  assert.equal(JSON.parse(答.result).stdout, '你好，隔离\n');
});

test('同一事件里连续运行一千次（成功与各种失败）不耗尽宿主句柄', async () => {
  const 成 = await 提交({scope: '范围甲', digest: 探针摘要, program: 探针文, input: 探针输入('echo'), repeat: 1000});
  assert.equal(成.ok, true, JSON.stringify(成).slice(0, 300));
  assert.equal(成.failures, 0);
  assert.equal(JSON.parse(成.result).stdout, '你好，隔离\n');
  const 输入败 = await 提交({scope: '范围甲', digest: 探针摘要, program: 探针文, input: '不是JSON', repeat: 1000, tolerant: true});
  assert.equal(输入败.failures, 1000);
  assert.equal(输入败.lastError, '隔离运行输入无效：输入不是有效 JSON');
  const 摘要败 = await 提交({scope: '范围甲', digest: '0'.repeat(64), program: 探针文, input: 探针输入('echo'), repeat: 1000, tolerant: true});
  assert.equal(摘要败.failures, 1000);
  const 超限败 = await 提交({scope: '范围甲', digest: 探针摘要, program: 探针文, input: 探针输入('many:257'), repeat: 300, tolerant: true});
  assert.equal(超限败.failures, 300);
  assert.equal(超限败.lastError, '隔离运行失败：500：运行数据超过上限');
  const 非wasm = Buffer.from('还不是 Wasm');
  const 载入败 = await 提交({scope: '范围乙', digest: 摘要(非wasm), program: 非wasm.toString('base64'), input: '{}', repeat: 300, tolerant: true});
  assert.equal(载入败.failures, 300);
  assert.equal(载入败.lastError, '隔离运行失败：动态 Worker 程序不是有效 Wasm');
  const 后 = await 运行('echo');
  assert.equal(后.ok, true, '一千次失败之后仍可运行');
});

test('每次运行都是全新实例与全新文件系统；不同范围互不影响', async () => {
  const 甲 = await 运行('echo', {范围: '范围甲', files: {'/请求/路径.txt': '/a'}});
  const 乙 = await 运行('echo', {范围: '范围乙', files: {'/请求/路径.txt': '/b'}});
  const 甲又 = await 运行('echo', {范围: '范围甲'});
  assert.equal(甲.files['/数据/记录.txt'], '见：/a');
  assert.equal(乙.files['/数据/记录.txt'], '见：/b');
  assert.equal(甲又.files['/数据/记录.txt'], '见：', '上一次运行写下的文件不会留到下一次');
  const 并发 = await Promise.all(Array.from({length: 12}, (_, 序) => 运行('echo', {范围: '并发' + (序 % 3), files: {'/请求/路径.txt': '/' + 序}})));
  并发.forEach((果, 序) => assert.equal(果.files['/数据/记录.txt'], '见：/' + 序));
});

test('第二个真实程序：云端项目核心 Wasm 读命令行参数、写标准输出', {skip: !existsSync(核心程序路径)}, async () => {
  const 字节 = readFileSync(核心程序路径);
  const 答 = await 提交({scope: '核心', digest: 摘要(字节), program: 字节.toString('base64'), input: JSON.stringify({args: [JSON.stringify({操作: '校验路径', path: '合法。豫'})]})});
  assert.equal(答.ok, true, JSON.stringify(答).slice(0, 300));
  const 果 = JSON.parse(答.result);
  assert.equal(果.ok, true);
  assert.equal(JSON.parse(果.stdout).ok, true);
  const 越 = JSON.parse(JSON.parse((await 提交({scope: '核心', digest: 摘要(字节), program: 字节.toString('base64'), input: JSON.stringify({args: [JSON.stringify({操作: '校验路径', path: '../越界'})]})})).result).stdout);
  assert.equal(越.ok, false);
});

// ---------- 假加载器（Node） ----------
test('假加载器：同（范围，摘要）同标识，异范围异标识；子 Worker 模块、限额与请求体', async () => {
  const 资源 = await import(pathToFileURL(path.join(产物, '动态资源.mjs')).href);
  const {创建云工宿主} = await import(pathToFileURL(path.join(产物, '宿主.mjs')).href);
  const 程序模块 = await WebAssembly.compile(readFileSync(path.join(产物, '程序.wasm')));
  const 值桥模块 = await WebAssembly.compile(readFileSync(path.join(产物, '值桥.wasm')));
  const 宿主 = 创建云工宿主({程序模块, 值桥模块, 许可: {LOADER: ['LOADER']}, 动态资源: {模块源码: 资源.模块源码, 值桥字节: 资源.值桥字节}});
  const 记录 = [];
  const 假加载器 = {
    get(标识, 取码) {
      const 项 = {标识, 取码};
      记录.push(项);
      return {getEntrypoint: () => ({fetch: async 请求 => {
        项.方法 = 请求.method; 项.网址 = 请求.url; 项.体 = await 请求.text();
        项.代码 = await 取码();
        return Response.json({ok: true, stdout: '', stderr: '', exitCode: 0, files: {'/数据/甲.txt': '乙'}});
      }})};
    },
  };
  const 输入文 = JSON.stringify({args: ['é😀\n'], files: {'/请求/正文.txt': 'a\u0000b 汉'}});
  const 调 = 范围 => 宿主.fetch(new Request('https://t/', {method: 'POST', body: JSON.stringify({scope: 范围, digest: 探针摘要, program: 探针文, input: 输入文})}), {LOADER: 假加载器}, {waitUntil() {}}).then(回 => 回.json());
  assert.equal((await 调('范围甲')).ok, true);
  assert.equal((await 调('范围甲')).ok, true);
  assert.equal((await 调('范围乙')).ok, true);
  assert.deepEqual(记录.map(项 => 项.标识), [`范围甲:${探针摘要}`, `范围甲:${探针摘要}`, `范围乙:${探针摘要}`]);
  assert.notEqual(记录[0].标识, 记录[2].标识);
  for (const 项 of 记录) {
    assert.equal(项.方法, 'POST');
    assert.equal(项.网址, 'https://isolated/run');
    assert.equal(项.体, 输入文, '请求体是输入的原字节，不经二次 JSON 转义');
    const 码 = 项.代码;
    assert.equal(码.mainModule, '隔离运行客.mjs');
    assert.equal(码.compatibilityDate, '2026-09-10');
    assert.deepEqual(Object.keys(码.modules).sort(), ['程序.wasm', '值桥.wasm', '隔离运行客.mjs', '编译宿主.mjs'].sort());
    assert.equal(typeof 码.modules['隔离运行客.mjs'].js, 'string');
    assert.equal(码.modules['隔离运行客.mjs'].js, 资源.模块源码['隔离运行客.mjs']);
    assert.equal(码.modules['编译宿主.mjs'].js, 资源.模块源码['编译宿主.mjs']);
    assert.equal(摘要(new Uint8Array(码.modules['程序.wasm'].wasm)), 探针摘要, '子 Worker 载入的正是传入的程序字节');
    assert.equal(码.globalOutbound, null);
    assert.deepEqual(码.env, {});
    assert.deepEqual(码.limits, {cpuMs: 1000, subRequests: 0});
  }
  // 子 Worker 回 200 与回 400、500 的映射（豫言异常的消息）
  for (const [状态, 期望] of [[400, /^隔离运行输入无效：坏输入$/], [500, /^隔离运行失败：500：坏运行$/], [502, /^隔离运行失败：502：网关$/]]) {
    const 桩 = {get: () => ({getEntrypoint: () => ({fetch: async () => new Response(状态 === 400 ? '坏输入' : 状态 === 500 ? '坏运行' : '网关', {status: 状态})})})};
    const 回 = await 宿主.fetch(new Request('https://t/', {method: 'POST', body: JSON.stringify({scope: '范围甲', digest: 探针摘要, program: 探针文, input: 输入文})}), {LOADER: 桩}, {waitUntil() {}}).then(回 => 回.json());
    assert.equal(回.ok, false);
    assert.match(回.error, 期望);
  }
  // 子 Worker 的 fetch 自己抛异常（如 CPU 限额触发）：转成可捕获的「隔离运行失败」
  const 崩 = {get: () => ({getEntrypoint: () => ({fetch: async () => { throw new Error('Worker exceeded CPU time limit'); }})})};
  const 崩回 = await 宿主.fetch(new Request('https://t/', {method: 'POST', body: JSON.stringify({scope: '范围甲', digest: 探针摘要, program: 探针文, input: 输入文})}), {LOADER: 崩}, {waitUntil() {}}).then(回 => 回.json());
  assert.equal(崩回.ok, false);
  assert.match(崩回.error, /^隔离运行失败：.*Worker exceeded CPU time limit/);
});
