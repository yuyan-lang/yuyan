// 古曰：旧籍不毁，非主不越。今释：使用真实 SQLite 验证迁移、注册、会话、令牌隔离和事务所有权约束。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {DatabaseSync} from 'node:sqlite';
import {readFileSync} from 'node:fs';
import {账户入口,条款版本,摘要} from '../源码/账户.js';
import {公开包入口} from '../源码/公开包.js';
function 数据库(){const sql=new DatabaseSync(':memory:');sql.exec('PRAGMA foreign_keys=ON');sql.exec(readFileSync(new URL('../迁移/0001_建立包管理.sql',import.meta.url),'utf8'));sql.exec(`INSERT INTO "包"("名称","所有者编号") VALUES ('旧包',1); INSERT INTO "上传" VALUES ('旧包','1','旧包-1.zip','abc',1,'pending/abc',1,CURRENT_TIMESTAMP); INSERT INTO "上传" VALUES ('待发布旧包','1','待发布旧包-1.zip','def',1,'pending/def',1,CURRENT_TIMESTAMP);`);sql.exec('BEGIN;'+readFileSync(new URL('../迁移/0002_邮箱注册.sql',import.meta.url),'utf8')+'COMMIT;');sql.exec(readFileSync(new URL('../迁移/0003_发布者验证.sql',import.meta.url),'utf8'));const db={prepare(query){return{args:[],bind(...args){this.args=args;return this;},first(){return sql.prepare(query).get(...this.args)??null;},run(){return sql.prepare(query).run(...this.args);}}},async batch(stmts){sql.exec('BEGIN');try{const out=stmts.map(s=>s.run());sql.exec('COMMIT');return out;}catch(e){sql.exec('ROLLBACK');throw e;}}};return {sql,db};}
const 基址='https://packages.example',密码='一段足够长而且不容易猜到的测试密码！';
async function 请求(db,action,data,cookie='',ip='192.0.2.1',origin=基址){const headers={'CF-Connecting-IP':ip};if(cookie)headers.Cookie=cookie;if(data!==undefined){headers.Origin=origin;headers['Content-Type']='application/json';}return 账户入口(new Request(基址+'/api/account/'+action,{method:data===undefined?'GET':'POST',headers,body:data===undefined?undefined:JSON.stringify(data)}),{DB:db});}
const 注册=(db,email,ip)=>请求(db,'register',{email,password:密码,acceptTerms:true,termsVersion:条款版本},'',ip);
test('迁移保留旧用户令牌及包，并拒绝跨用户写入',()=>{const {sql}=数据库();assert.equal(sql.prepare('SELECT COUNT(*) AS n FROM "访问令牌"').get().n,1);assert.equal(sql.prepare('SELECT "所有者编号" AS n FROM "包"').get().n,1);assert.equal(sql.prepare('SELECT COUNT(*) AS n FROM "上传"').get().n,2);sql.exec(`INSERT INTO "用户" ("编号","名称") VALUES (2,'新用户')`);assert.throws(()=>sql.exec(`INSERT INTO "上传" VALUES ('旧包','2','旧包-2.zip','abc',1,'pending/abc',2,CURRENT_TIMESTAMP)`),/包不属于/);assert.throws(()=>sql.exec(`INSERT INTO "包版本" VALUES ('旧包','2','旧包-2.zip','sha256/abc','abc',1,1,1,2,CURRENT_TIMESTAMP)`),/包不属于/);assert.deepEqual(sql.prepare('PRAGMA foreign_key_check').all(),[]);sql.close();});
test('无需邮箱验证即可注册登录，密码和会话不以明文存储',async()=>{const {sql,db}=数据库();const r=await 注册(db,' Person@EXAMPLE.com ');assert.equal(r.status,201);const c=r.headers.get('set-cookie');assert.match(c,/HttpOnly/);assert.match(c,/Secure/);assert.match(c,/SameSite=Strict/);const row=sql.prepare('SELECT * FROM "邮箱账户"').get();assert.equal(row.邮箱,'person@example.com');assert.notEqual(row.密码摘要,密码);assert.equal(row.条款版本,条款版本);const session=await (await 请求(db,'session',undefined,c)).json();assert.equal(session.user.email,'person@example.com');assert.ok(!session.user.name.includes('@'));assert.equal((await 请求(db,'login',{email:'person@example.com',password:'错误密码'})).status,401);const login=await 请求(db,'login',{email:'person@example.com',password:密码});assert.equal(login.status,200);assert.equal((await (await 请求(db,'session',undefined,c)).json()).user,null);await 请求(db,'logout',{},login.headers.get('set-cookie'));assert.equal((await (await 请求(db,'session',undefined,login.headers.get('set-cookie'))).json()).user,null);sql.close();});
test('重复邮箱、未接受协议、短密码、跨站与过期会话被拒绝',async()=>{const {sql,db}=数据库();const r=await 注册(db,'same@example.com');assert.equal((await 注册(db,'SAME@example.com')).status,409);assert.equal(sql.prepare('SELECT COUNT(*) AS n FROM "用户"').get().n,2);assert.equal((await 请求(db,'register',{email:'other@example.com',password:密码})).status,400);assert.equal((await 请求(db,'register',{email:'other@example.com',password:'short',acceptTerms:true,termsVersion:条款版本})).status,400);assert.equal((await 请求(db,'token',{},r.headers.get('set-cookie'),'192.0.2.1','https://evil.example')).status,403);sql.exec('UPDATE "登录会话" SET "到期"=0');assert.equal((await 请求(db,'token',{},r.headers.get('set-cookie'))).status,401);sql.close();});
test('令牌仅授权本人，重置和撤销不影响其他用户',async()=>{const {sql,db}=数据库();const a=await 注册(db,'a@example.com','192.0.2.2'),b=await 注册(db,'b@example.com','192.0.2.3');const ca=a.headers.get('set-cookie'),cb=b.headers.get('set-cookie');const first=await (await 请求(db,'token',{},ca)).json();const second=await (await 请求(db,'token',{},cb)).json();const uid=sql.prepare('SELECT "用户编号" AS id FROM "访问令牌" WHERE "SHA256"=?').get(await 摘要(first.token)).id;const other=sql.prepare('SELECT "用户编号" AS id FROM "访问令牌" WHERE "SHA256"=?').get(await 摘要(second.token)).id;assert.notEqual(uid,other);await 请求(db,'token',{},ca);assert.equal(sql.prepare('SELECT "已启用" AS n FROM "访问令牌" WHERE "SHA256"=?').get(await 摘要(first.token)).n,0);await 请求(db,'revoke',{},ca);assert.equal(sql.prepare('SELECT "已启用" AS n FROM "访问令牌" WHERE "SHA256"=?').get(await 摘要(second.token)).n,1);sql.close();});
test('注册限流及请求体上限生效',async()=>{const {sql,db}=数据库();for(let i=0;i<5;i++)assert.equal((await 注册(db,`x${i}@example.com`)).status,201);assert.equal((await 注册(db,'last@example.com')).status,429);assert.equal((await 请求(db,'login',{email:'x@example.com',password:'x'.repeat(5000)})).status,413);sql.close();});


test('官方 verified，新用户与其包 unverified，不能通过注册字段自授验证',async()=>{
 const {sql,db}=数据库();
 assert.equal(sql.prepare('SELECT "已验证" AS v FROM "用户" WHERE "编号"=1').get().v,1);
 const r=await 请求(db,'register',{email:'badge@example.com',password:密码,acceptTerms:true,termsVersion:条款版本,verified:true,verification:'verified'});
 assert.equal(r.status,201);
 const {user}=await (await 请求(db,'session',undefined,r.headers.get('set-cookie'))).json();assert.equal(user.verification,'unverified');
 sql.prepare('INSERT INTO "包"("名称","所有者编号") VALUES (?,?)').run('第三方包',user.id);
 for(const [name,id] of [['旧包',1],['第三方包',user.id]])sql.prepare('INSERT INTO "包版本" ("包名","版本","文件名","对象键","SHA256","压缩字节数","文件数","解压字节数","发布者编号") VALUES (?,1,?,123,456,1,1,1,?)').run(name,name+'-1.zip',id);
 const query=async name=>公开包入口(new Request('https://packages.example/api/packages/'+encodeURIComponent(name+'-1.zip')),{DB:db});
 const official=await (await query('旧包')).json();assert.equal(official.verification,'verified');assert.equal(official.publisher.name,'豫言');
 const result=await query('第三方包');assert.equal(result.headers.get('cache-control'),'no-store');const pkg=await result.json();assert.equal(pkg.verification,'unverified');assert.equal(pkg.publisher.verification,'unverified');assert.ok(!JSON.stringify(pkg).includes('badge@example.com'));
 sql.prepare('UPDATE "用户" SET "已验证"=1 WHERE "编号"=?').run(user.id);assert.equal((await (await query('第三方包')).json()).verification,'verified');
 assert.equal((await query('不存在')).status,404);sql.close();
});


test('上传 Access Token 不能充当登录会话或操作账户',async()=>{
 const {sql,db}=数据库();const register=await 注册(db,'upload-only@example.com');
 const cookie=register.headers.get('set-cookie');const tokenReply=await 请求(db,'token',{},cookie);const {token,scope}=await tokenReply.json();assert.equal(scope,'packages:upload');
 assert.equal((await 请求(db,'token',{})).status,401);
 for(const action of ['token','revoke','logout']){
  const r=await 账户入口(new Request(基址+'/api/account/'+action,{method:'POST',headers:{Origin:基址,'Content-Type':'application/json','CF-Connecting-IP':'192.0.2.20',Authorization:'Bearer '+token},body:'{}'}),{DB:db});assert.equal(r.status,401,action);
 }
 const r=await 账户入口(new Request(基址+'/api/account/session',{headers:{Authorization:'Bearer '+token}}),{DB:db});assert.equal((await r.json()).user,null);
 assert.equal((await (await 请求(db,'session',undefined,'__Host-yy_session='+token)).json()).user,null);
 const row=sql.prepare('SELECT "可上传" AS upload,"SHA256" AS hash FROM "访问令牌" WHERE "SHA256"=?').get(await 摘要(token));assert.equal(row.upload,1);assert.notEqual(row.hash,token);sql.close();
});
