// 文言：客文自成页，主权不相通。汉语：全页阅读只在内容域运行，映射、HTML 与源码均视为不可信输入。
const escape=s=>String(s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const encode=p=>p.split('/').map(encodeURIComponent).join('/');
const safe=p=>typeof p==='string'&&p.length>0&&p.length<=1024&&!/[\\\x00-\x1f\x7f]/u.test(p)&&p.split('/').every(x=>x&&x!=='.'&&x!=='..'&&!x.includes(':'));
const decode=s=>s.replace(/&(amp|lt|gt|quot|apos|#39|#\d+|#x[0-9a-f]+);/gi,(m,x)=>{if(x[0]==='#'){const n=x[1].toLowerCase()==='x'?parseInt(x.slice(2),16):Number(x.slice(1));return n>0&&n<=0x10ffff?String.fromCodePoint(n):m;}return{amp:'&',lt:'<',gt:'>',quot:'"',apos:"'"}[x.toLowerCase()]||m;});
async function textObject(env,key,max=1048576){const o=await env.PACKAGES.get(key);if(!o)return null;if(o.size>max)throw Error('阅读材料过大');return new Response(o.body).text();}
// 古曰：旧籍既索，毋使来者复劳。今释：仅缓存旧材料适配结果；同修订的并发读取共享一次 R2 扫描，失败不入缓存。
const 映射缓存 = new WeakMap();
export async function 读取文档映射(env,id){
  let entries=映射缓存.get(env);if(!entries){entries=new Map();映射缓存.set(env,entries);}
  const old=entries.get(id);if(old&&old.expires>Date.now())return old.promise;
  if(entries.size>=32)entries.delete(entries.keys().next().value);
  const entry={expires:Date.now()+300000,promise:null};
  entry.promise=(async()=>{
    const cache=globalThis.caches?.default,key=new Request(new URL('/__reader_maps/'+id,env.PORTAL_ORIGIN||'https://packages.yuyan-lang.org'));
    try{const hit=await cache?.match(key);if(hit)return hit.json();}catch{}
    const rows=await 读取原文档映射(env,id);
    if(rows.length){try{await cache?.put(key,Response.json(rows,{headers:{'Cache-Control':'public, max-age=300'}}));}catch{}}
    else if(entries.get(id)===entry)entries.delete(id);
    return rows;
  })().catch(error=>{if(entries.get(id)===entry)entries.delete(id);throw error;});
  entries.set(id,entry);return entry.promise;
}
async function 读取原文档映射(env,id){
  const prefix='releases/'+id+'/docs/',raw=await textObject(env,prefix+'接口/模块映射.json');
  let rows=[];
  if(raw!==null){const parsed=JSON.parse(raw);if(!Array.isArray(parsed)||parsed.length>2048)throw Error('文档映射无效');rows=parsed.map(x=>({source:x.source,document:'接口/'+x.document}));}
  else{
    // 文言：旧篇无图，循其所载之源。汉语：兼容已发布修订，仅解析受限生成页的路径标签，不执行 HTML。
    const objects=await env.PACKAGES.list({prefix:prefix+'接口/模块-',limit:1000});
    if(objects.truncated||objects.objects.length>256)throw Error('旧文档过多，请重新生成映射');
    for(const o of objects.objects){const document=o.key.slice(prefix.length);if(!/^接口\/模块-\d+\.html$/.test(document))continue;const html=await textObject(env,o.key,16777216),match=html?.match(/<p class="source-path">([^<]*)<\/p>/);if(match)rows.push({source:decode(match[1]),document});}
  }
  const seen=new Set();return rows.filter(x=>safe(x.source)&&safe(x.document)&&/^接口\/模块-\d+\.html$/.test(x.document)&&!seen.has(x.source)&&seen.add(x.source));
}
const reading=(id,file,view='docs')=>'/'+id+'/阅读?'+new URLSearchParams({...(file?{file}:{}),view});
// 文言：惟取数据，不纳客之标记。汉语：账户站点只接收文本字段，不返回可插入 DOM 的上传 HTML。
export async function 文件阅读数据(env,id,path,view='docs'){
  if(!safe(path)||! /^(source|docs|build|runtime|archive)\//.test(path))throw Object.assign(Error('文件路径无效'),{status:400});
  const prefix='releases/'+id+'/',object=await env.PACKAGES.get(prefix+path);if(!object)throw Object.assign(Error('文件暂不可用'),{status:404});
  let source=null;if(object.size<=1048576){try{source=new TextDecoder('utf-8',{fatal:true}).decode(await new Response(object.body).arrayBuffer());if(source.includes('\0'))source=null;}catch{}}
  const result={path,size:object.size,source,documentation:null};
  if(source!==null&&path.startsWith('source/')){try{const raw=await textObject(env,prefix+'docs/源码浏览/语义标记/'+path.slice(7)+'.json');if(raw){const marks=JSON.parse(raw).标记;if(Array.isArray(marks)&&marks.length<=50000)result.tokens=marks;}}catch{}}
  if(view!=='source'&&path.startsWith('source/')&&!path.endsWith('。包。豫')){
    let row;try{row=(await 读取文档映射(env,id)).find(x=>x.source===path.slice(7));}catch{return {...result,documentationError:'文档映射暂不可用'};}
    if(row){const data=await textObject(env,prefix+'docs/'+row.document.replace(/\.html$/,'.json'));if(data!==null){const doc=JSON.parse(data);if(!Array.isArray(doc.names)||doc.names.length>10000)throw Error('文档数据无效');result.documentation={names:doc.names.map(x=>({name:String(x.name??''),type:String(x.type??''),description:String(x.description??''),source:String(x.source??'')}))};}
      else{const html=await textObject(env,prefix+'docs/'+row.document,16777216);if(html!==null){const text=s=>decode(s.replace(/<[^>]*>/g,''));const names=[];for(const m of html.matchAll(/<article class="symbol-card">([\s\S]*?)<\/article>/g)){const name=m[1].match(/<h3>([\s\S]*?)<\/h3>/),type=m[1].match(/<pre class="type-signature"><code>([\s\S]*?)<\/code><\/pre>/),description=m[1].match(/<p class="symbol-description[^\"]*">([\s\S]*?)<\/p>/);if(name&&type)names.push({name:text(name[1]),type:text(type[1]),description:text(description?.[1]||'')});}result.documentation={names};}else result.documentationError='文档材料暂不可用';}
    }
  }
  return result;
}
export function 阅读导航(env,id,file){
  const portal=new URL('/release/'+id,env.PORTAL_ORIGIN||'https://packages.yuyan-lang.org').href;
  const mode=view=>reading(id,file,view);
  return '<nav aria-label="包阅读导航" style="position:fixed;bottom:1rem;right:1rem;z-index:2147483647;background:#fff;color:#222;border:1px solid #888;padding:.7rem;max-width:90vw;writing-mode:horizontal-tb"><a href="'+escape(portal)+'">包首页</a> · <a href="'+escape(portal+'/docs')+'">文档目录</a> · <a href="'+escape(portal+'/files')+'">所有文件</a> · <a href="'+escape(mode('docs'))+'">文档</a> · <a href="'+escape(mode('source'))+'">源代码</a></nav>';
}
export function 补全阅读页(html,env,id,file){
  if(!file){const m=html.match(/<p class="source-path">([^<]*)<\/p>/);if(m&&safe(decode(m[1])))file=decode(m[1]);}
  const script='<script>try{const p=new URLSearchParams(location.search).get("file");if(p){const e=[...document.querySelectorAll(".breadcrumb")].find(x=>x.textContent===p)?.closest("article[id]");if(e)location.hash=e.id;}}catch{}</script>';
  const addition=阅读导航(env,id,file)+script;
  return html.includes('</body>')?html.replace('</body>',addition+'</body>'):html+addition;
}
export async function 阅读入口(req,env,id,headers){
  const url=new URL(req.url),file=url.searchParams.get('file')||'',view=url.searchParams.get('view')||'docs';
  const page=(body,status=200)=>new Response(req.method==='HEAD'?null:'<!doctype html><html lang="zh-CN"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>包文件阅读</title><style>body{margin:2rem;font:17px/1.7 system-ui;padding-bottom:5rem}pre{white-space:pre-wrap;overflow-wrap:anywhere}.line{display:block}.line:target{background:#fff3b0}.num{display:inline-block;min-width:4em;color:#777}</style><body>'+body+阅读导航(env,id,file)+'</body></html>',{status,headers:{...headers,'Content-Type':'text/html; charset=utf-8','Cache-Control':'no-store'}});
  if((file&&!safe(file))||!['docs','source'].includes(view))return page('文件参数无效',400);
  const prefix='releases/'+id+'/';
  const redirect=path=>new Response(null,{status:302,headers:{...headers,Location:'/'+id+'/'+encode(path)+'?'+new URLSearchParams({reader:'1',...(file?{file}:{})}),'Cache-Control':'no-store'}});
  if(!file){const target=view==='source'?'源码浏览/index.html':await env.PACKAGES.head(prefix+'docs/接口/index.html')?'接口/index.html':'index.html';return redirect(target);}
  const source=await env.PACKAGES.get(prefix+'source/'+file);if(!source)return page('该修订没有此文件，或上传尚未完成。',404);
  if(view==='docs'){
    let rows;try{rows=await 读取文档映射(env,id);}catch{return page('<h1>'+escape(file)+'</h1><p>文档映射暂不可用，可切换源码或查看文档目录。</p>',503);}
    const row=rows.find(x=>x.source===file);if(row)return redirect(row.document);
    return page('<h1>'+escape(file)+'</h1><p>此文件暂无生成文档。可切换源代码，或查看文档目录。</p>');
  }
  if(/(?:。豫|\.yuyan)$/.test(file)&&await env.PACKAGES.head(prefix+'docs/源码浏览/index.html'))return redirect('源码浏览/index.html');
  const download=new URL('/api/releases/'+id+'/files/source/'+encode(file),env.PORTAL_ORIGIN||'https://packages.yuyan-lang.org').href;
  const title='<h1>'+escape(file)+'</h1><p><a href="'+escape(download)+'">下载原文件</a></p>';
  if(source.size>1048576)return page(title+'<p>文件过大，请下载查看。</p>');
  let text;try{text=new TextDecoder('utf-8',{fatal:true}).decode(await new Response(source.body).arrayBuffer());if(text.includes('\0'))throw Error('二进制');}catch{return page(title+'<p>此文件不适合文本预览，请下载查看。</p>');}
  return page(title+'<pre>'+text.split('\n').map((line,i)=>'<span class="line" id="L'+(i+1)+'"><a class="num" href="#L'+(i+1)+'">'+(i+1)+'</a>'+escape(line)+'</span>').join('')+'</pre>');
}
