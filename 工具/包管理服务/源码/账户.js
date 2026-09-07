// 古曰：密码加盐藏其验，会话持符不露于页。今释：邮箱不验证，密码仅保存派生值，会话使用 HttpOnly Cookie。
export const 条款版本 = '2026-09-07';
const 编码 = new TextEncoder(), 迭代数 = 100000, 会话秒数 = 604800;
const 十六进制 = b => Array.from(new Uint8Array(b), x => x.toString(16).padStart(2, '0')).join('');
export const 摘要 = async s => 十六进制(await crypto.subtle.digest('SHA-256', 编码.encode(s)));
const 随机 = () => 十六进制(crypto.getRandomValues(new Uint8Array(32)));
export async function 派生密码(密码, 盐, 次数 = 迭代数) {
  const key = await crypto.subtle.importKey('raw', 编码.encode(密码), 'PBKDF2', false, ['deriveBits']);
  return 十六进制(await crypto.subtle.deriveBits({name:'PBKDF2', hash:'SHA-256', salt:编码.encode(盐), iterations:次数}, key, 256));
}
const 相等 = (a,b) => {if(a.length!==b.length)return false;let d=0;for(let i=0;i<a.length;i++)d|=a.charCodeAt(i)^b.charCodeAt(i);return d===0;};
const 回 = (data,status=200,headers={}) => Response.json(data,{status,headers:{'Cache-Control':'no-store','X-Content-Type-Options':'nosniff',...headers}});
const 拒 = (message,status=400) => {throw Object.assign(new Error(message),{status});};
function 会话值(req){return req.headers.get('Cookie')?.match(/(?:^|;\s*)__Host-yy_session=([a-f0-9]{64})(?:;|$)/)?.[1];}
async function 读正文(req){if(!req.headers.get('Content-Type')?.startsWith('application/json'))拒('需要 JSON 请求');let size=0,parts=[];if(!req.body)拒('缺少请求正文');const reader=req.body.getReader();while(true){const {done,value}=await reader.read();if(done)break;size+=value.length;if(size>4096){await reader.cancel();拒('请求过大',413);}parts.push(value);}const bytes=new Uint8Array(size);let offset=0;for(const p of parts){bytes.set(p,offset);offset+=p.length;}try{const data=JSON.parse(new TextDecoder().decode(bytes));if(!data||typeof data!=='object'||Array.isArray(data))拒('请求格式错误');return data;}catch{拒('请求格式错误');}}
async function 限流(db,key,limit,now){const until=Math.floor(now/600)+1;const row=await db.prepare('INSERT INTO "账户限流" VALUES (?,1,?) ON CONFLICT("键") DO UPDATE SET "次数"=CASE WHEN "到期"<=? THEN 1 ELSE "次数"+1 END,"到期"=excluded."到期" RETURNING "次数"').bind(key,until*600,now).first();if(row.次数>limit)拒('请求过于频繁，请稍后重试',429);}
async function 查会话(req,db,now){const token=会话值(req);if(!token)return null;return db.prepare('SELECT u."编号" AS id,u."名称" AS name,CASE WHEN u."已验证"=1 THEN \'verified\' ELSE \'unverified\' END AS verification,a."邮箱" AS email FROM "登录会话" s JOIN "用户" u ON u."编号"=s."用户编号" JOIN "邮箱账户" a ON a."用户编号"=u."编号" WHERE s."摘要"=? AND s."到期">?').bind(await 摘要(token),now).first();}
function cookie(token,age=会话秒数){return `__Host-yy_session=${token}; Path=/; Secure; HttpOnly; SameSite=Strict; Max-Age=${age}`;}
export async function 账户入口(req,env) {
  const url=new URL(req.url);if(!url.pathname.startsWith('/api/account/'))return null;
  try {
    const action=url.pathname.slice('/api/account/'.length),db=env.DB,now=Math.floor(Date.now()/1000);
    if(req.method==='GET'&&action==='session'){return 回({user:await 查会话(req,db,now)});}
    if(req.method!=='POST')return 回({error:'不支持的请求'},405);
    if(req.headers.get('Origin')!==url.origin)拒('仅接受本站请求',403);
    if(env.REGISTRATION_ENABLED==='false'&&['register','token'].includes(action))拒('注册与令牌生成暂未开放，请稍后重试',503);
    const data=await 读正文(req);
    const ip=req.headers.get('CF-Connecting-IP');if(!ip)拒('无法识别请求来源',403);
    await 限流(db,'ip:'+await 摘要(ip),60,now);
    await db.batch([db.prepare('DELETE FROM "登录会话" WHERE "到期"<=?').bind(now),db.prepare('DELETE FROM "账户限流" WHERE "到期"<=?').bind(now)]);
    if(action==='register'||action==='login'){
      const email=typeof data.email==='string'?data.email.trim().toLowerCase():'';
      const password=data.password;
      if(!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)||email.length>254||typeof password!=='string'||password.length>128||password.length<1)拒('邮箱或密码格式不正确');
      await 限流(db,'account:'+await 摘要(email),15,now);
      let user;
      if(action==='register'){
        await 限流(db,'register:'+await 摘要(ip),5,now);
        if(password.length<15)拒('密码至少 15 个字符，最多 128 个字符');
        if(data.termsVersion!==条款版本||data.acceptTerms!==true)拒('请阅读并接受当前内测条款和隐私说明');
        const salt=随机(),hash=await 派生密码(password,salt),name='用户-'+crypto.randomUUID();
        try{await db.batch([db.prepare('INSERT INTO "用户" ("名称") VALUES (?)').bind(name),db.prepare('INSERT INTO "邮箱账户" VALUES ((SELECT "编号" FROM "用户" WHERE "名称"=?),?,?,?,?,?,?)').bind(name,email,hash,salt,迭代数,条款版本,now)]);}catch(e){if(/UNIQUE|constraint/i.test(String(e)))拒('无法注册此邮箱，请尝试登录',409);throw e;}
        user=await db.prepare('SELECT "用户编号" AS id FROM "邮箱账户" WHERE "邮箱"=?').bind(email).first();
      }else{
        const row=await db.prepare('SELECT * FROM "邮箱账户" WHERE "邮箱"=?').bind(email).first();
        const hash=await 派生密码(password,row?.盐??'0'.repeat(64),row?.迭代数??迭代数);
        if(!row||!相等(hash,row.密码摘要))拒('邮箱或密码错误',401);user={id:row.用户编号};
      }
      const token=随机();await db.batch([db.prepare('DELETE FROM "登录会话" WHERE "用户编号"=?').bind(user.id),db.prepare('INSERT INTO "登录会话" VALUES (?,?,?)').bind(await 摘要(token),user.id,now+会话秒数)]);
      return 回({ok:true},action==='register'?201:200,{'Set-Cookie':cookie(token)});
    }
    const user=await 查会话(req,db,now);if(!user)拒('请先登录',401);
    if(action==='logout'){await db.prepare('DELETE FROM "登录会话" WHERE "摘要"=?').bind(await 摘要(会话值(req))).run();return 回({ok:true},200,{'Set-Cookie':cookie('',0)});}
    if(action==='token'){
      // 古曰：新符既出，旧符尽废。今释：每次生成令牌撤销本账户所有旧上传令牌，不影响其他用户。
      const token=随机();await db.batch([db.prepare('UPDATE "访问令牌" SET "已启用"=0 WHERE "用户编号"=?').bind(user.id),db.prepare('INSERT INTO "访问令牌" ("用户编号","名称","SHA256","可上传") VALUES (?,?,?,1)').bind(user.id,'自行生成',await 摘要(token))]);return 回({token,scope:'packages:upload'});
    }
    if(action==='revoke'){await db.prepare('UPDATE "访问令牌" SET "已启用"=0 WHERE "用户编号"=?').bind(user.id).run();return 回({ok:true});}
    return 回({error:'未知账户操作'},404);
  }catch(e){if(e.status)return 回({error:e.message},e.status);return 回({error:'账户服务暂不可用'},503);}
}
