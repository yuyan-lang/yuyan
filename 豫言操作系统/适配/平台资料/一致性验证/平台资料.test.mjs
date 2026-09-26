// 文言：以真 Wasm 与假之资料服务验平台资料适配之诸一致性样例：列键、读文、未知集、未授权、败与复原、参数越限、缓存。
// 汉语：加载已构建的“平台资料一致性”产物；BROWSER_COMPILER 用假服务返回 gzip 压缩的 JSON（形状同真实的 标准库.json.gz：路径 → 文字 或 {内容}）。
// 用法见同目录说明：在私有暂存根目录执行 `node --test <本文件>`，产物根目录由环境变量 YY_DIST_ROOT 指定（默认 ./dist）；产物目录需含 平台资料.mjs。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {gzipSync} from 'node:zlib';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const 产物 = pathToFileURL(path.resolve(process.env.YY_DIST_ROOT ?? 'dist', '平台资料一致性') + '/');
const {创建云工宿主} = await import(new URL('宿主.mjs', 产物));
const 程序模块 = await WebAssembly.compile(await readFile(new URL('程序.wasm', 产物)));
const 值桥模块 = await WebAssembly.compile(await readFile(new URL('值桥.wasm', 产物)));

const {核对接口装载} = await import(new URL('接口核对.mjs', 产物));
const 应用要求 = JSON.parse(await readFile(new URL('接口要求组.json', 产物), 'utf8'));
const 宿主提供 = JSON.parse(await readFile(new URL('宿主提供组.json', 产物), 'utf8'));
test('装载前的接口核对通过，且应用要求含本接口', () => {
  核对接口装载({程序模块, 应用要求, 宿主提供, 宿主: '云工'});
  assert.ok(应用要求.some(项 => 项.接口名称 === '豫言操作系统平台资料' && 项.接口版本 === '0.1.0'), '应用要求里应有本接口 0.1.0');
  assert.ok(宿主提供.some(项 => 项.接口名称 === '豫言操作系统平台资料' && 项.接口版本 === '0.1.0'), '宿主支持清单里应有本接口 0.1.0');
});

const 资料 = {
  '/编译器.wasm': 'AAAA',
  '/库/标准库/总集。豫': {内容: '首行\n次行\n末行'},
  '/库/标准库/数据结构/整数操作。豫': '整数正文',
  '/库/其他/旁支。豫': '不可列',
  '/库/标准库/二进制条目': 12,
  '/库/标准库/对象无内容': {别的: 'x'}
};
const 压缩 = gzipSync(JSON.stringify(资料));
// 文言：假服务记其取数，可令其败。汉语：假的 BROWSER_COMPILER：记录请求网址与次数，mode 为 ok/500/坏gzip/坏json。
const 造环境 = () => {
  const 状态 = {次数: 0, 网址: [], 模式: 'ok'};
  const 服务 = {async fetch(请求) {
    状态.次数++; 状态.网址.push(请求.url);
    if (状态.模式 === '500') return new Response('炸', {status: 500});
    if (状态.模式 === '坏gzip') return new Response(Buffer.from('不是gzip'));
    if (状态.模式 === '坏json') return new Response(gzipSync('{oops'));
    return new Response(压缩);
  }};
  return {状态, env: {BROWSER_COMPILER: 服务}};
};
const 造宿主 = (许可 = {SERVICE: ['BROWSER_COMPILER']}) => 创建云工宿主({程序模块, 值桥模块, 许可});
const 调 = async (宿主, env, 查询) => {
  const 回 = await 宿主.fetch(new Request('https://x.test/?' + new URLSearchParams(查询).toString()), env);
  assert.equal(回.status, 200);
  return 回.json();
};
const 列 = async (宿主, env, 集, 前缀 = '') => { const r = await 调(宿主, env, {op: 'list', set: 集, prefix: 前缀}); return r.ok ? {ok: true, 键们: JSON.parse(r.text)} : r; };
const 读 = (宿主, env, 集, 键) => 调(宿主, env, {op: 'read', set: 集, key: 键});

test('列出平台资料键：只含 /库/标准库/ 起首的字符串条目，按 UTF-8 字节序，前缀过滤', async () => {
  const {env, 状态} = 造环境(), 宿主 = 造宿主();
  assert.deepEqual(await 列(宿主, env, '标准库'), {ok: true, 键们: ['/库/标准库/总集。豫', '/库/标准库/数据结构/整数操作。豫']});
  assert.deepEqual(await 列(宿主, env, '标准库', '/库/标准库/数据结构/'), {ok: true, 键们: ['/库/标准库/数据结构/整数操作。豫']});
  assert.deepEqual(await 列(宿主, env, '标准库', '/无此前缀'), {ok: true, 键们: []});
  assert.deepEqual(状态.网址, [new URL('https://playground.yuyan-lang.org/编译/资源/标准库.json.gz').href]);
});

test('读取平台资料文字：存在、不存在、旁支不可见、带内容字段的对象取其内容', async () => {
  const {env} = 造环境(), 宿主 = 造宿主();
  assert.deepEqual(await 读(宿主, env, '标准库', '/库/标准库/总集。豫'), {ok: true, found: true, text: '首行\n次行\n末行'});
  assert.deepEqual(await 读(宿主, env, '标准库', '/库/标准库/数据结构/整数操作。豫'), {ok: true, found: true, text: '整数正文'});
  assert.deepEqual(await 读(宿主, env, '标准库', '/库/标准库/无此文。豫'), {ok: true, found: false, text: ''});
  for (const 键 of ['/库/其他/旁支。豫', '/编译器.wasm', '/库/标准库/二进制条目', '/库/标准库/对象无内容']) assert.deepEqual(await 读(宿主, env, '标准库', 键), {ok: true, found: false, text: ''}, 键);
});

test('未知集、未授权与服务问题：可捕获的豫言异常，消息与规范一致；恢复后重取', async () => {
  const {env, 状态} = 造环境(), 宿主 = 造宿主();
  assert.deepEqual(await 列(宿主, env, '不存在'), {ok: false, error: '未知的平台资料集：不存在'});
  assert.deepEqual(await 读(宿主, env, '不存在', '/x'), {ok: false, error: '未知的平台资料集：不存在'});
  const 无授权 = 造宿主({});
  assert.deepEqual(await 列(无授权, env, '标准库'), {ok: false, error: '未授权的平台资料集：标准库'});
  assert.equal(状态.次数, 0, '未授权时不得访问服务');
  assert.deepEqual(await 列(宿主, {}, '标准库'), {ok: false, error: '标准库资料不可用'});
  状态.模式 = '500';
  assert.deepEqual(await 列(宿主, env, '标准库'), {ok: false, error: '标准库资料不可用'});
  状态.模式 = '坏gzip';
  assert.deepEqual(await 读(宿主, env, '标准库', '/库/标准库/总集。豫'), {ok: false, error: '标准库资料不可用'});
  状态.模式 = '坏json';
  assert.deepEqual(await 列(宿主, env, '标准库'), {ok: false, error: '标准库资料不可用'});
  状态.模式 = 'ok';
  assert.equal((await 列(宿主, env, '标准库')).键们.length, 2, '资料恢复后再调用应成功');
});

test('参数越限：集名、前缀、键的长度与字符', async () => {
  const {env} = 造环境(), 宿主 = 造宿主(), 无效 = {ok: false, error: '平台资料参数无效'};
  assert.deepEqual(await 列(宿主, env, ''), 无效);
  assert.deepEqual(await 列(宿主, env, 'x'.repeat(65)), 无效);
  assert.deepEqual(await 列(宿主, env, '标\n准'), 无效);
  assert.deepEqual(await 列(宿主, env, '标准库', 'p'.repeat(513)), 无效);
  assert.equal((await 列(宿主, env, '标准库', 'p'.repeat(512))).ok, true);
  assert.deepEqual(await 读(宿主, env, '标准库', ''), 无效);
  assert.deepEqual(await 读(宿主, env, '标准库', 'k'.repeat(513)), 无效);
  assert.deepEqual(await 读(宿主, env, '标准库', 'k'.repeat(512)), {ok: true, found: false, text: ''});
  assert.deepEqual(await 读(宿主, env, 'x'.repeat(65), '/x'), 无效);
  assert.equal((await 列(宿主, env, 's'.repeat(64))).error, '未知的平台资料集：' + 's'.repeat(64), '恰 64 字节的集名合规（只是未知）');
});

test('缓存：同一宿主实例连续调用一百次，只向服务取一次资料', async () => {
  const {env, 状态} = 造环境(), 宿主 = 造宿主();
  for (let i = 0; i < 50; i++) { assert.equal((await 列(宿主, env, '标准库')).键们.length, 2); assert.equal((await 读(宿主, env, '标准库', '/库/标准库/总集。豫')).found, true); }
  assert.equal(状态.次数, 1);
  const 另一宿主 = 造宿主();
  await 列(另一宿主, env, '标准库');
  assert.equal(状态.次数, 2, '缓存按宿主实例区分');
});
