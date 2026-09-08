import { test } from 'node:test';
import assert from 'node:assert/strict';
import { DatabaseSync } from 'node:sqlite';
import { readFileSync } from 'node:fs';
import { 即时发布入口, 用户内容入口, 安全文件路径 } from '../源码/即时发布桥.js';
import { 摘要 } from '../源码/账户.js';
import { 阅读页面入口 } from '../源码/阅读转发.js';

// 古曰：所指有误，不以他篇代之。今释：桥只传材料；错误、语言及 HTTP 状态不因缓存或 HEAD 丢失。
test('原生阅读桥保留错误路径与语言，默认总集且不将 cookie 传入容器',async()=>{
 const {env,objects,sql}=环境();
 objects.set('releases/'+id+'/source/总集。豫',{bytes:new TextEncoder().encode('总集源码')});
 let received;
 env.PACKAGE_CONTAINER={getByName(){return{async fetch(req){assert.equal(req.headers.get('cookie'),null);received=await req.json();return new Response('<html>由豫言成页</html>');}};}};
 let response=await 阅读页面入口(new Request(origin+'/release/'+id+'/files?path=source/不存在。豫',{headers:{Cookie:'yuyan_lang=wen'}}),env);
 assert.equal(response.status,404);assert.equal(received.lang,'wen');assert.equal(received.path,'source/不存在。豫');assert.ok(received.error);assert.deepEqual(received.file,{});
 response=await 阅读页面入口(new Request(origin+'/release/'+id+'/files?lang=han&historyOffset=bad',{method:'HEAD',headers:{Cookie:'yuyan_lang=wen'}}),env);
 assert.equal(response.status,200);assert.equal(received.lang,'han');assert.equal(received.path,'source/总集。豫');assert.equal(await response.text(),'');assert.match(response.headers.get('set-cookie'),/yuyan_lang=han/);sql.close();
});
test('不存在的修订保留404并提供不依赖容器的静态返回入口',async()=>{
 const {env,sql}=环境();let staticReads=0;
 env.ASSETS={async fetch(req){staticReads++;assert.equal(decodeURIComponent(new URL(req.url).pathname),'/错误.html');return new Response('<html><a href="/">返回全部包</a></html>');}};
 const response=await 阅读页面入口(new Request(origin+'/release/'+'b'.repeat(32)),env);
 assert.equal(response.status,404);assert.equal(staticReads,1);assert.match(await response.text(),/返回全部包/);assert.match(response.headers.get('content-security-policy'),/default-src 'none'/);sql.close();
});

// 古曰：验缺物可观，越权不可传，客页不得借主权。今释：真实 SQLite 加模拟原子 R2，验证简化上传和安全边界。
const id = 'a'.repeat(32), origin = 'https://packages.yuyan-lang.org';
function 环境(修订=true) {
  const sql = new DatabaseSync(':memory:'); sql.exec('PRAGMA foreign_keys=ON');
  for (const file of ['0001_建立包管理.sql', '0002_邮箱注册.sql', '0003_发布者验证.sql', '0004_邮箱验证与密码重置.sql', '0005_即时发布.sql', '0006_所有者绑定.sql', '0007_上传修订.sql'].filter(f=>修订||!f.startsWith('0007')))
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
test('主站文件接口返回结构化文档与纯文本源码，不返回上传 HTML',async()=>{
 const {env,objects,sql}=环境(),put=(path,text)=>objects.set('releases/'+id+'/'+path,{bytes:new TextEncoder().encode(text)});
 put('source/例。豫','<script>source</script>');put('docs/接口/模块映射.json',JSON.stringify([{source:'例。豫',document:'模块-1.html'}]));
 put('docs/接口/模块-1.json',JSON.stringify({names:[{name:'<script>x</script>',type:'字符串',description:'说明'}]}));
 let data=await(await 即时发布入口(请求('/'+id+'/file?path='+encodeURIComponent('source/例。豫')),env)).json();assert.equal(data.source,'<script>source</script>');assert.equal(data.documentation.names[0].name,'<script>x</script>');assert.equal(data.html,undefined);
 objects.delete('releases/'+id+'/docs/接口/模块-1.json');put('docs/接口/模块-1.html','<article class="symbol-card"><h3>甲&lt;乙</h3><pre class="type-signature"><code>字符串</code></pre><p class="symbol-description">说明</p></article>');
 data=await(await 即时发布入口(请求('/'+id+'/file?path='+encodeURIComponent('source/例。豫')),env)).json();assert.equal(data.documentation.names[0].name,'甲<乙');
 assert.equal((await 即时发布入口(请求('/'+id+'/file?path=../bad'),env)).status,400);sql.close();
});
test('文件阅读默认映射文档，源码复用浏览器，普通文本转义，路径与来源隔离',async()=>{
  const {env,objects,sql}=环境(),put=(p,text)=>objects.set('releases/'+id+'/'+p,{bytes:new TextEncoder().encode(text)}),source='目录/例。豫';
  put('source/'+source,'源码');put('docs/接口/模块映射.json',JSON.stringify([{source,document:'模块-1.html'},{source:'../越界',document:'模块-2.html'}]));
  put('docs/接口/模块-1.html','<html><body><p class="source-path">目录/例。豫</p>接口</body></html>');
  put('docs/源码浏览/index.html','<body><article id="源码-1"><div class="breadcrumb">目录/例。豫</div></article></body>');
  const read=(file,view='docs')=>用户内容入口(new Request('https://usercontent.yuyan-lang.org/'+id+'/阅读?'+new URLSearchParams({file,view})),env);
  const mapping=await(await 即时发布入口(请求('/'+id+'/reading'),env)).json();assert.equal(mapping.modules.length,1);
  let r=await read(source);assert.equal(r.status,302);assert.match(decodeURIComponent(r.headers.get('location')),/接口\/模块-1.html/);
  r=await 用户内容入口(new Request('https://usercontent.yuyan-lang.org'+r.headers.get('location')),env);const html=await r.text();assert.match(html,/包阅读导航/);assert.match(html,/源代码/);assert.match(r.headers.get('content-security-policy'),/sandbox allow-scripts/);assert.doesNotMatch(r.headers.get('content-security-policy'),/allow-same-origin/);
  r=await read(source,'source');assert.equal(r.status,302);assert.match(decodeURIComponent(r.headers.get('location')),/源码浏览\/index.html/);
  r=await 用户内容入口(new Request('https://usercontent.yuyan-lang.org'+r.headers.get('location')),env);assert.match(await r.text(),/location.hash=e.id/);
  put('source/绑定.c','<script>不执行</script>');r=await read('绑定.c','source');assert.match(await r.text(),/&lt;script&gt;/);
  r=await read('绑定.c');assert.match(await r.text(),/暂无生成文档/);
  assert.equal((await read('../越界')).status,400);assert.equal((await read('不存在。豫')).status,404);assert.equal((await read(source,'evil')).status,400);sql.close();
});
test('旧修订可从生成页面恢复源码映射，受限路径不采纳',async()=>{
  const {env,objects,sql}=环境();objects.set('releases/'+id+'/docs/接口/模块-1.html',{bytes:new TextEncoder().encode('<p class="source-path">甲&amp;乙。豫</p>')});
  const data=await(await 即时发布入口(请求('/'+id+'/reading'),env)).json();assert.deepEqual(data.modules,[{source:'甲&乙。豫',document:'接口/模块-1.html'}]);sql.close();
});
test('修订迁移保留已有发布 ID、时间和归档摘要，设置序数 1',()=>{
  const {sql}=环境(false);sql.prepare('UPDATE 即时版本 SET 归档摘要=? WHERE 编号=?').run('legacyhash',id);
  const before=sql.prepare('SELECT * FROM 即时版本').get();sql.exec(readFileSync(new URL('../迁移/0007_上传修订.sql',import.meta.url),'utf8'));
  const after=sql.prepare('SELECT * FROM 即时版本').get();assert.deepEqual({...after},{...before,上传序数:1});assert.deepEqual(sql.prepare('PRAGMA foreign_key_check').all(),[]);sql.close();
});
test('同版本列表只示最新修订，历史分页可下载旧 ZIP，旧详情保留原文件',async()=>{
  const {env,sql,objects}=环境(),newId='0'.repeat(32);
  sql.prepare('INSERT INTO 即时版本 (编号,所有者编号,名称,版本,类型,简介,创建时间,上传序数,归档摘要) SELECT ?,所有者编号,名称,版本,类型,简介,创建时间,2,? FROM 即时版本 WHERE 编号=?').run(newId,'newhash',id);
  objects.set('releases/'+id+'/archive/发布.zip',{bytes:new TextEncoder().encode('old zip')});
  for(const query of ['', '?catalog=1']){const page=await(await 即时发布入口(请求(query),env)).json();assert.deepEqual(page.releases.map(x=>x.id),[newId]);assert.equal(page.releases[0].revision,2);}
  const old=await(await 即时发布入口(请求('/'+id),env)).json();assert.equal(old.revision,1);assert.equal(old.latestId,newId);
  const history=await(await 即时发布入口(请求('/'+newId+'/history'),env)).json();assert.deepEqual(history.revisions.map(x=>x.revision),[2,1]);assert.equal(history.nextOffset,null);
  const download=await 即时发布入口(new Request(origin+history.revisions[1].downloadUrl),env);assert.equal(await download.text(),'old zip');assert.match(download.headers.get('cache-control'),/immutable/);
  const page=await(await 即时发布入口(请求('/'+id+'/history?offset=1'),env)).json();assert.equal(page.revisions[0].id,id);
  assert.equal((await 即时发布入口(请求('/'+id+'/history?offset=-1'),env)).status,400);sql.close();
});
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

test('包市场按所有者与包名聚合、支持搜索，我的包只取当前账户',async()=>{
 const {env,sql}=环境();sql.exec(`INSERT INTO 用户 (编号,名称) VALUES (2,'另一位');`);
 const add=sql.prepare('INSERT INTO 即时版本 (编号,所有者编号,名称,版本,类型,简介,创建时间) VALUES (?,?,?,?,?,?,?)');
 add.run('b'.repeat(32),1,'例包','0.2.0','库','新版描述','2099-01-01');add.run('c'.repeat(32),2,'例包','0.1.0','库','独立命名空间','2099-01-02');
 const catalog=await (await 即时发布入口(请求('?catalog=1'),env)).json();assert.equal(catalog.releases.length,2);assert.ok(!catalog.releases.some(x=>x.id===id));
 const search=await (await 即时发布入口(请求('?catalog=1&q='+encodeURIComponent('独立')),env)).json();assert.equal(search.releases.length,1);assert.equal(search.releases[0].owner,'另一位');
 assert.equal((await 即时发布入口(请求('?mine=1'),env)).status,401);
 sql.exec(`INSERT INTO 邮箱账户 (用户编号,邮箱,密码摘要,盐,迭代数,条款版本,接受时间) VALUES (2,'test@example.invalid','h','s',1,'test',0);`);
 const token='d'.repeat(64);sql.prepare('INSERT INTO 登录会话 VALUES (?,?,?)').run(await 摘要(token),2,Math.floor(Date.now()/1000)+60);
 const mine=await (await 即时发布入口(请求('?mine=1',{headers:{Cookie:'__Host-yy_session='+token}}),env)).json();assert.equal(mine.releases.length,1);assert.equal(mine.releases[0].owner,'另一位');sql.close();
});
