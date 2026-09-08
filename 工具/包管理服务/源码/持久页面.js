import { 即时发布入口 } from './即时发布桥.js';
async function 公开数据(env,origin,path) { const r=await 即时发布入口(new Request(origin+path),env);if(!r?.ok)throw Error('发布材料不可用');return r.json(); }

// 文言：豫言定篇，云桥存取；成页不失，败事可续。汉语：此模块只桥接 R2、Queue 与内部豫言生成器。
export const 页面版本 = 'persistent-reader-3';
const 前缀 = 'rendered/' + 页面版本 + '/';
const 来源 = env => env.PORTAL_ORIGIN || 'https://packages.yuyan-lang.org';
const 合号 = id => /^[a-f0-9]{32}$/.test(id || '');
export async function 页面键(url) {
  const u = new URL(url), parts = u.pathname.split('/'), tab = parts[3] ? 'files' : 'overview';
  if (!合号(parts[2])) throw Error('无效修订');
  const values = [tab,u.searchParams.get('lang') === 'wen' ? 'wen' : 'han',tab === 'files' ? u.searchParams.get('path') || '' : '',tab === 'files' && u.searchParams.get('view') === 'source' ? 'source' : 'docs',tab === 'overview' ? Number(u.searchParams.get('historyOffset') || 0) : 0];
  const hash = [...new Uint8Array(await crypto.subtle.digest('SHA-256',new TextEncoder().encode(JSON.stringify(values))))].map(x=>x.toString(16).padStart(2,'0')).join('');
  return 前缀 + parts[2] + '/' + tab + '/' + hash + '.html';
}
export const 完成键 = id => 'publication-ready/' + id;
export async function 提交页面(env,url) {
  const key = await 页面键(url), lock = key + '.pending', old = await env.PACKAGES.get(lock);
  if (old && Date.now() - Number(old.customMetadata?.created || 0) < 900000) return;
  const claim = await env.PACKAGES.put(lock,'',{onlyIf:new Headers(old ? {'If-Match':old.httpEtag} : {'If-None-Match':'*'}),customMetadata:{created:String(Date.now())}});
  if (!claim) return;
  try { await env.PAGE_QUEUE.send({kind:'page',url:new URL(url).pathname + new URL(url).search,version:页面版本,refresh:!new URL(url).pathname.endsWith('/files')}); }
  catch (error) { await env.PACKAGES.delete(lock); throw error; }
}
export async function 提交修订(env,id) {
  if (!合号(id)) throw Error('无效修订');
  await env.PAGE_QUEUE.send({kind:'release',id,version:页面版本});
}
export async function 读取发布(env,id) {
  const data = await 公开数据(env,来源(env),'/api/releases/' + id);
  while(data.cursor) { const next = await 公开数据(env,来源(env),'/api/releases/' + id + '?cursor=' + encodeURIComponent(data.cursor)); data.files.push(...next.files);data.cursor=next.cursor;if(data.files.length>4096)throw Error('材料过多'); }
  return data;
}
const 清单缓存 = new Map();
async function 任务发布(env,id) {
  const old = 清单缓存.get(id); if(old && old.until>Date.now())return old.data;
  const object = await env.PACKAGES.get(前缀+id+'/release.json');
  if(!object)return null;
  const data = await new Response(object.body).json();
  if(清单缓存.size>16)清单缓存.clear();清单缓存.set(id,{data,until:Date.now()+60000});return data;
}
export async function 消费页面(batch,env) {
  for(const message of batch.messages) {
    try {
      const task=message.body;if(task.version!==页面版本){message.ack();continue;}
      console.log('持久页面开始',task.kind,task.id||task.url);
      if(task.kind==='release' && 合号(task.id)) {
        const release=await 读取发布(env,task.id);
        if(!await env.PACKAGES.head(完成键(task.id))) {
          if(!task.legacy || !release.files.some(f=>f.path==='archive/发布.zip') || !release.files.some(f=>f.path==='docs/index.html') || !release.files.some(f=>/^source\/[^/]+。包。豫$/.test(f.path)))throw Error('上传未完成');
          await env.PACKAGES.put(完成键(task.id),'{}',{customMetadata:{legacy:'true'}});
        }
        const input=JSON.stringify({release});
        const response=await env.PACKAGE_CONTAINER.getByName('豫言包管理').fetch(new Request('http://container.internal/__direct/render-plan',{method:'POST',signal:AbortSignal.timeout(60000),headers:{'Content-Type':'application/json','Content-Length':String(new TextEncoder().encode(input).byteLength)},body:input}));
        if(!response.ok)throw Error('生成计划失败');const urls=await response.json();
        if(!Array.isArray(urls)||urls.length>20000)throw Error('生成计划无效');
        await env.PACKAGES.put(前缀+task.id+'/release.json',JSON.stringify(release));
        const related=await env.DB.prepare('SELECT "编号" AS id FROM "即时版本" WHERE "所有者编号"=(SELECT "所有者编号" FROM "即时版本" WHERE "编号"=?) AND "名称"=? AND "版本"=?').bind(task.id,release.name,release.version).all();
        for(const row of related.results || [])if(row.id!==task.id)for(const lang of ['han','wen'])urls.push('/release/'+row.id+'?lang='+lang);
        for(let i=0;i<urls.length;i+=20)await env.PAGE_QUEUE.sendBatch(urls.slice(i,i+20).map(url=>({body:{kind:'page',url,version:页面版本,refresh:!url.includes('/files?')}})));
        const own=[...new Set(urls.filter(url=>url.startsWith('/release/'+task.id)))];
        await env.PACKAGES.put(前缀+task.id+'/plan.json',JSON.stringify({urls:own,keys:await Promise.all(own.map(url=>页面键(new URL(url,来源(env))))),created:Date.now()}));
      } else if(task.kind==='page') {
        const url=new URL(task.url,来源(env));if(url.origin!==new URL(来源(env)).origin||!/^\/release\/[a-f0-9]{32}(?:\/files)?$/.test(url.pathname))throw Error('生成地址无效');
        const key=await 页面键(url), id=url.pathname.split('/')[2];
        if(!task.refresh && await env.PACKAGES.head(key)){await env.PACKAGES.delete(key+'.pending');message.ack();continue;}
        if(!await env.PACKAGES.head(完成键(id)))throw Error('上传未完成');
        const release=url.pathname.endsWith('/files')?await 任务发布(env,id):null;
        const { 阅读页面入口 } = await import('./阅读转发.js');
        const response=await 阅读页面入口(new Request(url),env,{生成:true,发布:release});
        if(response.status!==200)throw Error('页面生成失败：'+response.status);
        await response.arrayBuffer();await env.PACKAGES.delete(key+'.pending');
      } else throw Error('未知任务');
      console.log('持久页面完成',task.kind,task.id||task.url);message.ack();
    } catch(error) { console.error('持久页面生成失败',error.message); message.retry({delaySeconds:Math.min(300,15*2**Math.min(message.attempts||1,4))}); }
  }
}
// 文言：颁后若失入列，定时补之。汉语：完成标记持久存在；定时扫描弥补上传响应前后的队列故障。
export async function 补发页面(env) {
  const checkpoint=await env.PACKAGES.get(前缀+'repair-cursor');
  const cursor=checkpoint?await new Response(checkpoint.body).text():'';
  const rows=await env.DB.prepare('SELECT "编号" AS id FROM "即时版本" WHERE "编号">? ORDER BY "编号" LIMIT 100').bind(cursor).all();
  for(const row of rows.results || [])if(await env.PACKAGES.head(完成键(row.id)) && !await env.PACKAGES.head(前缀+row.id+'/plan.json'))await 提交修订(env,row.id);
  await env.PACKAGES.put(前缀+'repair-cursor',rows.results?.length===100?rows.results.at(-1).id:'');
}
export async function 页面状态(req,env) {
  const match=new URL(req.url).pathname.match(/^\/api\/releases\/([a-f0-9]{32})\/pages$/);if(!match||req.method!=='GET')return null;
  const object=await env.PACKAGES.get(前缀+match[1]+'/plan.json');
  if(!object){const row=await env.DB.prepare('SELECT "编号" FROM "即时版本" WHERE "编号"=?').bind(match[1]).first();return Response.json({status:row?'pending':'not_found',version:页面版本,expected:0,completed:0},{status:row?200:404,headers:{'Cache-Control':'no-store'}});}
  const plan=await new Response(object.body).json(),keys=new Set();let cursor;
  do{const page=await env.PACKAGES.list({prefix:前缀+match[1]+'/',limit:1000,cursor});for(const o of page.objects)keys.add(o.key);cursor=page.truncated?page.cursor:undefined;}while(cursor);
  const expected=plan.keys.length,completed=plan.keys.filter(key=>keys.has(key)).length;
  return Response.json({status:completed===expected?'ready':'generating',version:页面版本,expected,completed},{headers:{'Cache-Control':'no-store'}});
}
