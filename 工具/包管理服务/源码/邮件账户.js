// 古曰：信持一用之符，过时则废，不载于日志。今释：仅存凭据摘要；链接用 fragment，避免进入请求 URL 和 Referer。
const 编码=new TextEncoder();
export const 随机凭据=()=>Array.from(crypto.getRandomValues(new Uint8Array(32)),x=>x.toString(16).padStart(2,'0')).join('');
export const 邮件摘要=async s=>Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256',编码.encode(s))),x=>x.toString(16).padStart(2,'0')).join('');
const 拒=(message,status=400)=>{throw Object.assign(new Error(message),{status});};
export async function 发送账户邮件(env,user,purpose,now){
  if(!env.EMAIL?.send)拒('邮件服务暂不可用，请稍后重试',503);
  const token=随机凭据(),hash=await 邮件摘要(token),minutes=purpose==='verify'?60:30;
  // 古曰：址定于内，不从客辞。今释：不使用请求 Host 或用户提供的跳转地址生成邮件链接。
  const link='https://xn--uiry18e4gf.yuyan-lang.org/个人#'+purpose+'='+token;
  await env.DB.prepare('INSERT INTO "邮件凭据" VALUES (?,?,?,?)').bind(hash,user.id,purpose,now+minutes*60).run();
  try{
    await env.EMAIL.send({from:'noreply@yuyan-lang.org',to:user.email,subject:purpose==='verify'?'豫言：验证邮箱':'豫言：重置密码',text:purpose==='verify'
      ?`打开以下链接即可自动完成邮箱验证，无需登录或再次确认：\n${link}\n\n链接 ${minutes} 分钟内有效，只可使用一次。邮箱验证仅开放包上传权限，不授予 verified 身份认证标识。非本人申请请忽略。\n\n古曰：循址即验邮箱，无须登入；一用即废，逾时亦废。验邮箱非验身份。非己所请，勿应之。`
      :`打开链接并设置新密码：\n${link}\n\n链接 ${minutes} 分钟内有效，只可使用一次。重置后旧登录会话及上传令牌全部撤销，请重新登录。非本人申请请忽略，本次申请本身不会更改密码。\n\n古曰：循址更密，一用即废，逾时亦废。既更其密，旧会话与上传之符皆废。非己所请，勿应之。`});
  }catch{
    await env.DB.prepare('DELETE FROM "邮件凭据" WHERE "摘要"=?').bind(hash).run();
    拒('邮件服务暂不可用，请稍后重试',503);
  }
}
// 古曰：同券不可再用，其改与其废同成。今释：D1 batch 中检查有效凭据、更新账户、撤销旧凭据；整个事务串行提交。
export async function 使用邮件凭据(db,purpose,token,now,userId,passwordData){
  if(typeof token!=='string'||!/^[a-f0-9]{64}$/.test(token))拒('链接无效或已过期，请重新申请');
  const hash=await 邮件摘要(token);
  const where='SELECT "用户编号" FROM "邮件凭据" WHERE "摘要"=? AND "用途"=? AND "到期">?';
  const args=[hash,purpose,now];
  const query=(sql,...prefix)=>db.prepare(sql).bind(...prefix,...args);
  const statements=[];
  if(purpose==='reset'){
    statements.push(query(`UPDATE "邮箱账户" SET "密码摘要"=?,"盐"=?,"迭代数"=?,"邮箱已验证"=1 WHERE "用户编号" IN (${where})`,...passwordData));
    statements.push(query(`DELETE FROM "登录会话" WHERE "用户编号" IN (${where})`));
    statements.push(query(`UPDATE "访问令牌" SET "已启用"=0 WHERE "用户编号" IN (${where})`));
  }else statements.push(query(`UPDATE "邮箱账户" SET "邮箱已验证"=1 WHERE "用户编号" IN (${where})`));
  statements.push(query(`DELETE FROM "邮件凭据" WHERE "用户编号" IN (${where})`));
  const result=await db.batch(statements);
  if(Number(result[0].meta?.changes??result[0].changes)!==1)拒('链接无效或已过期，请重新申请');
}
