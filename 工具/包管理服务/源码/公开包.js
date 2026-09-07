// 古曰：验其发布者，非验其程序。今释：包标识取当前发布者的服务端验证状态，不公开邮箱或鉴权资料。
export function 发布者资料(row){return {id:row.publisherId,name:row.publisherName,verification:row.publisherVerified===1?'verified':'unverified'};}
export async function 公开包入口(req,env){
  const url=new URL(req.url);if(!url.pathname.startsWith('/api/packages/'))return null;
  const headers={'Cache-Control':'no-store','X-Content-Type-Options':'nosniff'};
  const reply=(data,status=200)=>Response.json(data,{status,headers});
  if(req.method!=='GET')return reply({error:'仅支持查询'},405);
  let name;try{name=decodeURIComponent(url.pathname.slice('/api/packages/'.length));}catch{return reply({error:'包文件名编码无效'},400);}
  if(name.length>256||!name.endsWith('.zip')||/[\\/\x00-\x1f]/.test(name)||name.includes('..'))return reply({error:'包文件名无效'},400);
  const row=await env.DB.prepare('SELECT v."文件名" AS fileName,v."包名" AS name,v."版本" AS version,u."编号" AS publisherId,u."名称" AS publisherName,u."已验证" AS publisherVerified FROM "包版本" v JOIN "用户" u ON u."编号"=v."发布者编号" WHERE v."文件名"=?').bind(name).first();
  if(!row)return reply({error:'未找到已发布的包'},404);
  const publisher=发布者资料(row);
  return reply({fileName:row.fileName,name:row.name,version:row.version,verification:publisher.verification,publisher,downloadUrl:'/'+encodeURIComponent(row.fileName)});
}
