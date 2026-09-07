import { 请求内服, 安全文件路径 } from './即时发布桥.js';
const 上限 = 16 * 1024 * 1024;
const 回 = (error, status) => Response.json({ error }, { status, headers: { 'Cache-Control': 'no-store' } });
async function 有限字节(req, max) {
  if (!req.body) throw Error('缺少上传正文');
  const r=req.body.getReader(), parts=[];let n=0;
  while(true){const {done,value}=await r.read();if(done)break;n+=value.length;if(n>max){await r.cancel();throw Error('上传超过限制');}parts.push(value);}
  const bytes=new Uint8Array(n);let i=0;for(const p of parts){bytes.set(p,i);i+=p.length;}return bytes;
}
function 六四(bytes){let s='';for(let i=0;i<bytes.length;i+=32768)s+=String.fromCharCode(...bytes.subarray(i,i+32768));return btoa(s);}
const 印 = async bytes => Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256',bytes)),n=>n.toString(16).padStart(2,'0')).join('');
// 文言：外桥惟传档，包籍由内服析之。汉语：不接受客户端声明元数据或归档摘要；服务端自行计算摘要。
export async function 归档上传入口(req,env){
  const path=new URL(req.url).pathname;
  if(!['/api/releases/zip','/api/account/owner'].includes(path))return null;
  if(req.method!=='POST')return 回('仅接受 POST',405);
  if(req.headers.has('Content-Encoding'))return 回('不接受压缩的 HTTP 正文',400);
  try{
    if(path==='/api/account/owner'){
      if(!req.headers.get('Content-Type')?.startsWith('application/json'))return 回('需要 JSON',400);
      return await 请求内服(req,env,'owner',new TextDecoder('utf-8',{fatal:true}).decode(await 有限字节(req,4096)));
    }
    if(req.headers.get('Content-Type')!=='application/zip')return 回('需要 application/zip',400);
    // 文言：先验身份，后受大物。汉语：用内部权限查询拒绝未绑定或无权账户，避免其触发归档解析。
    const auth=await 请求内服(req,env,'zip-auth','{}');if(!auth.ok)return auth;
    const bytes=await 有限字节(req,上限);
    return await 请求内服(req,env,'zip',(await 印(bytes))+'\n'+六四(bytes));
  }catch{return 回('上传未完成、格式错误或超过 16 MiB，可以重试',400);}
}
// 文言：内桥纳受限字节，成物不覆。汉语：仅容器可调用；按文件原子写入，同内容重试成功。
export async function 存归档材料(req,env){
  try{
    const id=req.headers.get('X-Release-Id');
    const text=new TextDecoder('utf-8',{fatal:true}).decode(await 有限字节(req,Math.ceil(上限/3)*4+8192));
    const newline=text.indexOf('\n');
    // 文言：首行记径，余文载物。汉语：兼容部署过渡期的旧 JSON 格式，新协议只解析很小的路径前缀。
    const payload=newline<0?JSON.parse(text):null;
    const raw=newline<0?payload.path:JSON.parse(text.slice(0,newline));
    if(req.method!=='POST'||! /^[a-f0-9]{32}$/.test(id||'')||typeof raw!=='string')return 回('内部参数错误',400);
    const path=安全文件路径(raw.split('/').map(encodeURIComponent).join('/'));
    if(!/^(source|docs|build|runtime|archive)\//.test(path))return 回('分类错误',400);
    const row=await env.DB.prepare('SELECT "归档摘要" AS hash FROM "即时版本" WHERE "编号"=?').bind(id).first();
    if(!row?.hash)return 回('版本不存在',404);
    const encoded=newline<0?payload.data:text.slice(newline+1);
    if(typeof encoded!=='string'||encoded.length>Math.ceil(上限/3)*4)return 回('材料编码过大',413);
    const binary=atob(encoded);if(binary.length>上限)return 回('材料过大',413);
    const bytes=Uint8Array.from(binary,c=>c.charCodeAt(0)),hash=await 印(bytes),key='releases/'+id+'/'+path;
    if(path.startsWith('archive/')&&(path!=='archive/发布.zip'||hash!==row.hash))return 回('归档摘要不一致',409);
    const existing=await env.PACKAGES.head(key);
    if(existing)return existing.customMetadata?.sha256===hash?new Response(null,{status:204}):回('文件内容冲突',409);
    const result=await env.PACKAGES.put(key,bytes,{onlyIf:new Headers({'If-None-Match':'*'}),sha256:hash,
      httpMetadata:{contentType:'application/octet-stream'},customMetadata:{sha256:hash}});
    if(!result){const other=await env.PACKAGES.head(key);if(other?.customMetadata?.sha256!==hash)return 回('并发内容冲突',409);}
    return new Response(null,{status:204});
  }catch{return 回('材料暂未写入，可以重传同一 ZIP',503);}
}
