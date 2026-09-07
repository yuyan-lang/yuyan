import { test } from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { readFileSync } from 'node:fs';
import { 即时发布入口, 用户内容入口, 安全文件路径 } from '../源码/即时发布桥.js';
import { 摘要 } from '../源码/账户.js';

// 古曰：验缺物可观，越权不可传，客页不得借主权。今释：真实 SQLite 加模拟原子 R2，验证简化上传和安全边界。
const id = 'a'.repeat(32), origin = 'https://packages.yuyan-lang.org';
function 环境() {
  const sql = new DatabaseSync(':memory:'); sql.exec('PRAGMA foreign_keys=ON');
  for (const file of ['0001_建立包管理.sql', '0002_邮箱注册.sql', '0003_发布者验证.sql', '0004_邮箱验证与密码重置.sql', '0005_即时发布.sql'])
    sql.exec('BEGIN;' + readFileSync(new URL('../迁移/' + file, import.meta.url), 'utf8') + 'COMMIT;');
  sql.prepare('INSERT INTO "即时版本" ("编号","所有者编号","名称","版本","类型","简介") VALUES (?,1,?,?,?,?)')
    .run(id, '例包', '0.1.0', '可执行文件', '<script>不应执行</script>');
  const db = { prepare(q) { return { args: [], bind(...args) { this.args = args; return this; },
    first() { return sql.prepare(q).get(...this.args) || null; },
    all() { return { results: sql.prepare(q).all(...this.args) }; },
  }; } };
  const objects = new Map(), calls = [];
  const env = { DB: db, PACKAGES: {
    async head(key) { const v = objects.get(key); return v ? { ...v, size: v.bytes.length } : null; },
    async get(key) { const v = objects.get(key); return v ? { ...v, size: v.bytes.length, body: v.bytes, httpEtag: '"etag"' } : null; },
    async list({ prefix }) { return { truncated: false, objects: [...objects].filter(([k]) => k.startsWith(prefix)).map(([key, v]) => ({ key, size: v.bytes.length })) }; },
    async put(key, body, options) {
      assert.equal(options.onlyIf.get('If-None-Match'), '*');
      const bytes = new Uint8Array(await new Response(body).arrayBuffer());
      const hash = Buffer.from(await crypto.subtle.digest('SHA-256', bytes)).toString('hex');
      if (hash !== options.sha256) throw Error('bad digest');
      if (objects.has(key)) return null;
      objects.set(key, { bytes, customMetadata: options.customMetadata }); return { key };
    },
  }, PACKAGE_CONTAINER: { getByName() { return { async fetch(req) {
    calls.push(req); if (env.deny) return Response.json({ error: '无权上传' }, { status: 403 });
    return Response.json({ id });
  } }; } } };
  return { env, sql, objects, calls };
}
function 请求(path, options = {}) { return new Request(origin + '/api/releases' + path, options); }
async function 上传(env, body = 'hello', headers = {}, path = '/files/docs/index.html') {
  return 即时发布入口(请求('/' + id + path, { method: 'PUT', body, headers: {
    Authorization: 'Bearer test-token', 'X-Package-Size': String(new TextEncoder().encode(body).length),
    'X-Package-SHA256': await 摘要(body), ...headers,
  } }), env);
}
test('元数据存在但无文件时版本立即公开，所有者命名空间允许同名', async () => {
  const { env, sql } = 环境();
  const r = await 即时发布入口(请求('/' + id), env), data = await r.json();
  assert.equal(r.status, 200); assert.deepEqual(data.files, []); assert.equal(data.review, 'unreviewed');
  sql.exec('INSERT INTO "用户" ("编号","名称") VALUES (2,\'别人\')');
  sql.prepare('INSERT INTO "即时版本" ("编号","所有者编号","名称","版本","类型","简介") VALUES (?,2,?,?,?,?)').run('b'.repeat(32), '例包', '0.1.0', '库', '');
  assert.equal((await (await 即时发布入口(请求(''), env)).json()).releases.length, 2);
});
test('每个文件上传后立即可见，同摘要重试成功，不同内容和并发覆盖被拒绝', async () => {
  const { env, calls } = 环境();
  assert.equal((await 上传(env)).status, 201);
  assert.equal((await 上传(env)).status, 200);
  assert.equal((await 上传(env, 'changed')).status, 409);
  assert.ok(calls.every(r => r.url.endsWith('/__direct/authorize')));
  const r = await 即时发布入口(请求('/' + id + '/files/docs/index.html'), env);
  assert.equal(await r.text(), 'hello'); assert.match(r.headers.get('Content-Disposition'), /^attachment/);
  const pair = await Promise.all([上传(env, 'a', {}, '/files/source/a.zip'), 上传(env, 'b', {}, '/files/source/a.zip')]);
  assert.deepEqual(pair.map(x => x.status).sort(), [201, 409]);
});
test('摘要或实际长度错误不产生文件，缺失文件不缓存', async () => {
  const { env, objects } = 环境();
  assert.equal((await 上传(env, 'hello', { 'X-Package-SHA256': '0'.repeat(64) })).status, 400);
  assert.equal((await 上传(env, 'hello', { 'X-Package-Size': '1' })).status, 400);
  assert.equal((await 上传(env, 'hello', { 'X-Package-Size': '8' })).status, 400);
  assert.equal(objects.size, 0);
  const r = await 即时发布入口(请求('/' + id + '/files/docs/index.html'), env);
  assert.equal(r.status, 404); assert.equal(r.headers.get('Cache-Control'), 'no-store');
  assert.equal((await 上传(env)).status, 201);
});
test('越权、跨站、无会话和伪造发布者不能上传，关闭发布开关有效', async () => {
  const { env, objects, calls } = 环境();
  env.deny = true; assert.equal((await 上传(env)).status, 403); assert.equal(objects.size, 0);
  env.deny = false;
  assert.equal((await 上传(env, 'hello', { Origin: 'https://usercontent.yuyan-lang.org' })).status, 403);
  await 上传(env, 'hello', { 'X-Yuyan-Publisher': '999', 'X-Yuyan-Path': '/__direct/create' });
  assert.equal(calls.at(-1).headers.get('X-Yuyan-Publisher'), null);
  assert.equal(calls.at(-1).headers.get('X-Yuyan-Path'), null);
  env.REGISTRATION_ENABLED = 'false'; assert.equal((await 上传(env)).status, 503);
});
test('创建只需一个请求，Cookie 会话身份由桥写入，不生成或撤销 CLI 令牌', async () => {
  const { env, sql, calls } = 环境(), token = '1'.repeat(64);
  sql.prepare('INSERT INTO "邮箱账户" VALUES (1,?,?,?,?,?,?,1)').run('test@example.test', 'hash', 'salt', 100000, '2026-09-07', 0);
  sql.prepare('INSERT INTO "登录会话" VALUES (?,1,?)').run(await 摘要(token), Math.floor(Date.now() / 1000) + 60);
  const r = await 即时发布入口(请求('', { method: 'POST', headers: {
    'Content-Type': 'application/json', Cookie: '__Host-yy_session=' + token, Origin: origin, 'X-Yuyan-Publisher': '999',
  }, body: JSON.stringify({ name: '例包', version: '0.1.0', type: '库', description: '说明' }) }), env);
  assert.equal(r.status, 201); assert.equal((await r.json()).url, '/release/' + id);
  assert.equal(calls[0].headers.get('X-Yuyan-Publisher'), '1');
  assert.equal(calls[0].headers.get('Cookie'), null);
  assert.equal(sql.prepare('SELECT COUNT(*) AS n FROM "访问令牌"').get().n, 1);
  sql.exec('UPDATE "邮箱账户" SET "邮箱已验证"=0');
  const denied = await 即时发布入口(请求('', { method: 'POST', headers: {
    'Content-Type': 'application/json', Cookie: '__Host-yy_session=' + token, Origin: origin,
  }, body: '{}' }), env);
  assert.equal(denied.status, 403);
  assert.equal(calls.length, 1);
});
test('不解压归档，路径异常、过大和无摘要上传均拒绝', async () => {
  const { env, objects } = 环境();
  for (const p of ['../a', 'a//b', 'a%2f..%2fb', 'a%5cb', 'a%00b', '%ZZ', '/abs']) assert.throws(() => 安全文件路径(p));
  assert.equal((await 上传(env, 'hello', { 'X-Package-Size': '104857601' })).status, 400);
  assert.equal((await 上传(env, 'hello', { 'X-Package-SHA256': '' })).status, 400);
  assert.equal(objects.size, 0);
});
test('用户文档无账户接口，沙箱即便直接打开仍生效；所有响应无凭据且禁止缓存缺失', async () => {
  const { env } = 环境();
  await 上传(env, '<script>window.example=1</script>');
  const r = await 用户内容入口(new Request('https://usercontent.yuyan-lang.org/' + id + '/index.html', { headers: { Cookie: 'secret', Authorization: 'Bearer secret' } }), env);
  assert.equal(r.status, 200);
  const csp = r.headers.get('Content-Security-Policy');
  assert.match(csp, /sandbox allow-scripts;/); assert.doesNotMatch(csp, /allow-same-origin/);
  assert.match(csp, /frame-ancestors https:\/\/packages.yuyan-lang.org/);
  assert.equal(r.headers.get('Set-Cookie'), null); assert.equal(r.headers.get('Access-Control-Allow-Credentials'), null);
  for (const p of ['/api/account/session', '/' + id + '/missing.html']) {
    const missing = await 用户内容入口(new Request('https://usercontent.yuyan-lang.org' + p), env);
    assert.equal(missing.status, 404); assert.equal(missing.headers.get('Cache-Control'), 'no-store');
  }
  assert.equal((await 用户内容入口(new Request('https://usercontent.yuyan-lang.org/' + id + '/index.html', { method: 'POST' }), env)).status, 405);
});
