import {test} from 'node:test';
import assert from 'node:assert/strict';
import {DatabaseSync} from 'node:sqlite';
import {readFileSync,readdirSync} from 'node:fs';
import {归档上传入口,存归档材料} from '../源码/归档桥.js';
import {摘要} from '../源码/账户.js';
// 文言：客不得自定主，内桥不得覆成物。汉语：验证身份传递、上传限制、摘要和原子重试。
const origin='https://packages.yuyan-lang.org',id='a'.repeat(32),token='1'.repeat(64);
async function env(){
  const sql=new DatabaseSync(':memory:');
  for(const f of readdirSync(new URL('../迁移/',import.meta.url)).filter(f=>f.endsWith('.sql')).sort())
    sql.exec('BEGIN;'+readFileSync(new URL('../迁移/'+f,import.meta.url),'utf8')+'COMMIT;');
  sql.exec(`INSERT INTO 邮箱账户 VALUES (1,'a@example.test','h','s',100000,'v',0,1)`);
  sql.prepare('INSERT INTO 登录会话 VALUES (?,1,?)').run(await 摘要(token),Math.floor(Date.now()/1000)+60);
  const calls=[],objects=new Map();
  const e={DB:{prepare(q){return{args:[],bind(...a){this.args=a;return this;},first(){return sql.prepare(q).get(...this.args);}}}},
    PACKAGE_CONTAINER:{getByName(){return{async fetch(req){calls.push(req);return Response.json({id,url:'/release/'+id});}}}},
    PACKAGES:{
      async head(k){return objects.get(k);},
      async put(k,b,o){assert.equal(o.onlyIf.get('If-None-Match'),'*');if(objects.has(k))return null;objects.set(k,{bytes:b,...o});return{k};}
    }};
  return{e,sql,calls,objects};
}
const req=(path,body,headers={})=>new Request(origin+path,{method:'POST',headers:{Origin:origin,Cookie:'__Host-yy_session='+token,...headers},body});
test('ZIP 一次上传自行计算摘要，身份标头不可伪造，旧 JSON 不被接受',async()=>{
  const {e,calls,sql}=await env(),r=await 归档上传入口(req('/api/releases/zip','hello',{'Content-Type':'application/zip','X-Yuyan-Publisher':'999','X-Package-SHA256':'bad'}),e);
  assert.equal(r.status,200);assert.equal(calls.length,2);
  assert.equal(calls[0].url,'http://container.internal/__direct/zip-auth');
  assert.equal(calls[1].headers.get('X-Yuyan-Publisher'),'1');assert.equal(calls[1].headers.get('Cookie'),null);
  const [hash,body]=(await calls[1].text()).split('\n');assert.equal(hash,await 摘要('hello'));assert.equal(atob(body),'hello');
  assert.equal((await 归档上传入口(req('/api/releases/zip','{}',{'Content-Type':'application/json'}),e)).status,400);
  e.REGISTRATION_ENABLED='false';assert.equal((await 归档上传入口(req('/api/releases/zip','x',{'Content-Type':'application/zip'}),e)).status,503);
  sql.close();
});
test('未验证、未绑定、跨站请求及令牌绑定名称被阻止',async()=>{
  const {e,sql,calls}=await env();
  assert.equal((await 归档上传入口(req('/api/account/owner','{"name":"甲"}',{'Content-Type':'application/json',Authorization:'Bearer abc'}),e)).status,403);
  assert.equal((await 归档上传入口(req('/api/account/owner','{"name":"甲"}',{'Content-Type':'application/json',Origin:'https://evil.test'}),e)).status,403);
  sql.exec('UPDATE 邮箱账户 SET 邮箱已验证=0');
  assert.equal((await 归档上传入口(req('/api/releases/zip','x',{'Content-Type':'application/zip'}),e)).status,403);
  sql.exec("INSERT INTO 用户 (编号,名称) VALUES (2,'用户-新'); UPDATE 邮箱账户 SET 用户编号=2,邮箱已验证=1; UPDATE 登录会话 SET 用户编号=2");
  assert.equal((await 归档上传入口(req('/api/releases/zip','x',{'Content-Type':'application/zip'}),e)).status,403);
  assert.equal(calls.length,0);
  assert.equal((await 归档上传入口(req('/api/account/owner','{"name":"甲"}',{'Content-Type':'application/json'}),e)).status,200);
  assert.equal(calls[0].headers.get('X-Yuyan-Publisher'),'2');sql.close();
});
test('材料桥支持二进制、原始 ZIP 摘要核对、同内容重试及并发不可覆盖',async()=>{
  const {e,sql,objects}=await env();
  sql.prepare('INSERT INTO 即时版本 (编号,所有者编号,名称,版本,类型,简介,归档摘要) VALUES (?,1,?,?,?,?,?)').run(id,'包','1.0.0','库','说明',await 摘要('zip'));
  const put=(path,data)=>存归档材料(new Request('http://release.internal/',{method:'POST',headers:{'X-Release-Id':id},body:JSON.stringify({path,data:Buffer.from(data).toString('base64')})}),e);
  assert.equal((await put('archive/发布.zip','bad')).status,409);
  assert.equal((await put('archive/发布.zip','zip')).status,204);
  assert.equal((await put('docs/index.html','hello')).status,204);
  assert.equal((await put('docs/index.html','hello')).status,204);
  assert.equal((await put('docs/index.html','different')).status,409);
  assert.equal((await put('source/../bad','bad')).status,503);
  const pair=await Promise.all([put('build/包',Buffer.from([0,255])),put('build/包',Buffer.from([1]))]);
  assert.deepEqual(pair.map(r=>r.status).sort(),[204,409]);
  assert.deepEqual([...objects.get('releases/'+id+'/build/包').bytes],pair[0].status===204?[0,255]:[1]);sql.close();
});
