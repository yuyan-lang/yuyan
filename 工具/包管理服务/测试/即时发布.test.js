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
  for (const file of ['0001_建立包管理.sql', '0002_邮箱注册.sql', '0003_发布者验证.sql', '0004_邮箱验证与密码重置.sql', '0005_即时发布.sql', '0006_所有者绑定.sql'])
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





test('用户文档无账户接口，沙箱即便直接打开仍生效；所有响应无凭据且禁止缓存缺失', async () => {
  const { env, objects } = 环境();
  objects.set('releases/' + id + '/docs/index.html', {bytes:new TextEncoder().encode('<script>window.example=1</script>')});
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

test('旧元数据和逐文件写入均关闭，缺失下载仍为 no-store',async()=>{
 const {env}=环境();
 assert.equal((await 即时发布入口(请求('',{method:'POST',body:'{}'}),env)).status,410);
 assert.equal((await 上传(env)).status,410);
 const r=await 即时发布入口(请求('/'+id+'/files/docs/index.html'),env);
 assert.equal(r.status,404);assert.equal(r.headers.get('Cache-Control'),'no-store');
 for(const p of ['../a','a//b','a%2f..%2fb','a%5cb','a%00b','%ZZ','/abs'])assert.throws(()=>安全文件路径(p));
});
