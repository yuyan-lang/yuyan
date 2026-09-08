// 古曰：市示众包，私事归己。今释：市场只读公开接口；旧邮件链接携带原 fragment 转至个人页。
const 元素=id=>document.getElementById(id);
const 文=(el,han,wen=han)=>{el.dataset.han=han;el.dataset.wen=wen;el.textContent=window.豫言界面?.语言==='wen'?wen:han;};
// 古曰：所寻载于址，往返不失。今释：此处仅桥接浏览器 URL 状态，包数据仍来自公开接口。
let 下页=0,查询=new URLSearchParams(location.search).get('q')?.trim().slice(0,100)||'',序=0,忙=false;
async function 求(url){const r=await fetch(url,{cache:'no-store'});const data=await r.json();if(!r.ok)throw Error(data.error||'读取失败');return data;}
function 链(text,url){const a=document.createElement('a');a.textContent=text;const u=new URL(url,location.href);if(u.origin===location.origin&&u.pathname.startsWith('/release/'))u.searchParams.set('lang',window.豫言界面?.语言||new URLSearchParams(location.search).get('lang')||'han');a.href=u.href;return a;}
async function 列表(reset=false){
  if(忙&&!reset)return;const 当前=reset?++序:序;忙=true;元素('市场更多').disabled=true;
  if(reset){下页=0;元素('市场列表').replaceChildren();元素('市场更多').hidden=true;}
  文(元素('市场状态'),'正在加载…','正载入…');
  try{const data=await 求('/api/releases?catalog=1&offset='+下页+'&q='+encodeURIComponent(查询));if(当前!==序)return;
    for(const v of data.releases){const li=document.createElement('li');li.className='包卡片';const top=document.createElement('p');top.className='包卡片主';top.textContent=v.owner;const badge=document.createElement('span');badge.className='验证标识';badge.textContent=v.identityVerified===1?'verified':'unverified';badge.dataset.verified=String(v.identityVerified===1);badge.title='发布者真实身份认证状态，与邮箱验证无关';top.append(' ',badge);const h=document.createElement('h2');h.append(链(v.name,'/release/'+v.id));const desc=document.createElement('p');desc.textContent=v.description||'暂无简介';const meta=document.createElement('p');meta.className='注';meta.textContent=v.type+' · '+v.version;li.append(top,h,desc,meta);元素('市场列表').append(li);}
    下页=data.nextOffset;元素('市场更多').hidden=下页===null;const count=元素('市场列表').children.length;文(元素('市场状态'),count?'已展示 '+count+' 个包，每个包展示最近发布的版本。':查询?'没有找到匹配的包。':'尚无公开包。可前往个人管理发布第一个包。',count?'已陈 '+count+' 包，各示近颁之版。':'未得其包。');
  }catch(e){if(当前===序)文(元素('市场状态'),e.message);}finally{if(当前===序){忙=false;元素('市场更多').disabled=false;}}
}
// 文言：诸次皆可追，旧档不失。汉语：历史按页读取；链接来自已认证服务器字段，文字仅用 textContent。
async function 展示上传历史(data,容器){
  容器.querySelector('[data-upload-history]')?.remove();
  if(!data.historyUrl)return;
  const section=document.createElement('section');section.dataset.uploadHistory='';
  const title=document.createElement('h3');title.textContent='同版本上传历史 / 同版诸次';section.append(title);
  if(data.latestId&&data.latestId!==data.id){const a=document.createElement('a');a.textContent='查看最新修订';a.href='/release/'+data.latestId;section.append(a);}
  const list=document.createElement('ul'),more=document.createElement('button');more.type='button';more.textContent='更多历史';section.append(list,more);容器.append(section);let offset=0;
  async function load(){more.disabled=true;try{const page=await 求(data.historyUrl+'?offset='+offset);for(const r of page.revisions){const li=document.createElement('li'),a=document.createElement('a'),zip=document.createElement('a');a.textContent='第 '+r.revision+' 次上传 · '+r.created;a.href=r.url;zip.textContent='下载 ZIP';zip.href=r.downloadUrl;li.append(a,' · ',zip);list.append(li);}offset=page.nextOffset;more.hidden=offset===null;}catch(e){more.textContent='重试读取历史：'+e.message;}finally{more.disabled=false;}}
  more.addEventListener('click',load);await load();
}
function 阅读地址(data,file='',view='docs'){return '/release/'+data.id+'/files?'+new URLSearchParams({...(file?{path:'source/'+file}:{}),view});}
function 文件树(files,data,container,select){
  const root=new Map();for(const f of files){let map=root;const parts=f.path.split('/');for(const part of parts.slice(0,-1)){if(!map.has(part))map.set(part,new Map());const next=map.get(part);if(!(next instanceof Map))break;map=next;}map.set(parts.at(-1),f);}
  function render(map){const ul=document.createElement('ul');ul.className='tree-list';for(const[name,value]of [...map].sort(([a],[b])=>a.localeCompare(b,'zh'))){const li=document.createElement('li');li.className='tree-item';if(value instanceof Map){const details=document.createElement('details'),summary=document.createElement('summary');details.open=true;summary.textContent=name;details.append(summary,render(value));li.append(details);}else{const a=链(name,'/release/'+data.id+'/files?'+new URLSearchParams({path:value.path,view:'docs'}));a.className='tree-link';a.addEventListener('click',e=>{e.preventDefault();select(value.path,'docs',true);});li.append(a);}ul.append(li);}return ul;}container.append(render(root));
}
function 着色源码(el,source,tokens){
  const chars=Array.from(source),classes={'结构操作符':'structure','类型操作符':'type','控制操作符':'control','普通操作符':'operator','内建类型':'builtin-type','内建函数':'builtin-function','内建常量':'builtin-constant','绑定标识符':'binder','引用标识符':'identifier','数值':'number','字符串':'string','注释':'comment','结构终止符':'structure-terminator'};
  if(!Array.isArray(tokens)||tokens.length>50000){el.textContent=source;return;}let end=0;for(const t of tokens){if(!t||!Number.isSafeInteger(t.开始)||!Number.isSafeInteger(t.结束)||t.开始<end||t.结束<t.开始||t.结束>chars.length||!Object.hasOwn(classes,t.种类)){el.textContent=source;return;}end=t.结束;}
  end=0;for(const t of tokens){el.append(document.createTextNode(chars.slice(end,t.开始).join('')));const span=document.createElement('span');span.className='tok-'+classes[t.种类];span.textContent=chars.slice(t.开始,t.结束).join('');el.append(span);end=t.结束;}el.append(document.createTextNode(chars.slice(end).join('')));
}
function 统一浏览器(files,data,container){
  // 文言：惟陈豫源，余材仍存。汉语：文件浏览仅展示豫言源码和包定义，不改变发布材料与下载。
  files=files.filter(f=>f.path.startsWith('source/')&&f.path.endsWith('。豫'));
  // 文言：一器览文与源，右侧易之。汉语：复用原浏览器的布局和样式，所有上传内容只作为文本节点渲染。
  document.body.classList.add('浏览模式');const style=document.createElement('link');style.rel='stylesheet';style.href='/共用/代码浏览器.css';document.head.append(style);
  container.innerHTML='<div class="workspace 统一浏览器"><aside class="sidebar"><div class="sidebar-head">全部文件</div><nav class="tree" id="浏览文件树"></nav></aside><section class="reader"><header class="统一阅读头"><strong id="当前文件名">选择文件</strong><div role="group" aria-label="阅读模式"><button type="button" id="看文档">文档</button><button type="button" id="看源码">源代码</button></div></header><div id="统一阅读内容"></div></section></div>';
  let selected='',mode='docs',request=0;const body=元素('统一阅读内容'),cache=new Map();
  function show(d){body.replaceChildren();const f=files.find(x=>x.path===selected);if(f)body.append(链('下载原文件',f.url));if(mode==='docs'){if(!d.documentation){const p=document.createElement('p');p.textContent=d.documentationError||'此文件暂无生成文档，可用右侧开关查看源代码。';body.append(p);return;}for(const item of d.documentation.names){const article=document.createElement('article'),h=document.createElement('h3'),type=document.createElement('pre'),desc=document.createElement('p');article.className='symbol-card';h.textContent=item.name;type.textContent=item.type;desc.textContent=item.description;article.append(h,type,desc);body.append(article);}if(!d.documentation.names.length)body.append(document.createTextNode('该模块没有公开声明。'));}
    else if(d.source===null)body.append(document.createTextNode('二进制或大文件，请下载查看。'));else{const layout=document.createElement('div'),numbers=document.createElement('pre'),code=document.createElement('pre');layout.className='code-layout';numbers.className='line-numbers';numbers.textContent=d.source.split('\n').map((_,i)=>i+1).join('\n');code.className='source-code';着色源码(code,d.source,d.tokens);layout.append(numbers,code);body.append(layout);}}
  async function select(path,view,push=false){const seq=++request;selected=path;mode=view==='source'?'source':'docs';元素('当前文件名').textContent=path;元素('看文档').setAttribute('aria-pressed',String(mode==='docs'));元素('看源码').setAttribute('aria-pressed',String(mode==='source'));if(push)history.pushState(null,'','/release/'+data.id+'/files?'+new URLSearchParams({path,view:mode}));const cached=cache.get(path+'|'+mode)||(mode==='source'&&cache.get(path+'|docs'));if(cached){show(cached);return;}body.textContent='正在读取…';const requestedMode=mode;try{const d=await 求('/api/releases/'+data.id+'/file?'+new URLSearchParams({path,view:requestedMode}));cache.set(path+'|'+requestedMode,d);if(seq===request)show(d);}catch(e){if(seq===request)body.textContent=e.message;}}
  文件树(files,data,元素('浏览文件树'),select);for(const[id,view]of [['看文档','docs'],['看源码','source']])元素(id).addEventListener('click',()=>{if(selected)select(selected,view,true);});
  const restore=()=>{const q=new URLSearchParams(location.search),path=files.find(f=>f.path===q.get('path'))?.path||files[0]?.path;if(path)select(path,q.get('view'));else{body.textContent='此包暂无豫言源文件。';元素('看文档').disabled=true;元素('看源码').disabled=true;}};window.addEventListener('popstate',restore);restore();
}
async function 详情(id,tab=''){
  if(tab==='docs')tab='files';
  for(const key of ['市场搜索','市场列表','市场更多'])元素(key).hidden=true;
  document.querySelector('.市场标题')?.setAttribute('hidden','');文(元素('市场状态'),'正在读取包详情…','正读包之详…');
  try{let data=await 求('/api/releases/'+id),files=[...data.files];while(data.cursor){const page=await 求('/api/releases/'+id+'?cursor='+encodeURIComponent(data.cursor));files.push(...page.files);data.cursor=page.cursor;}
    元素('市场详情').hidden=false;元素('详情标题').textContent=data.owner+' / '+data.name+' · '+data.version+' · 上传 #'+(data.revision||1);元素('详情简介').textContent=data.description;
    const nav=元素('包导航'),content=元素('包内容');nav.replaceChildren();content.replaceChildren();
    for(const[key,label]of [['','概览'],['files','文档与源码']]){const a=链(label,'/release/'+id+(key?'/'+key:''));if(key===tab)a.setAttribute('aria-current','page');nav.append(a,' ');}
    if(tab==='files'){统一浏览器(files,data,content);
    }else{
      const summary=document.createElement('p');summary.textContent='类型：'+data.type+' · 上传时间：'+data.created;content.append(summary);
      const downloads=document.createElement('section'),h=document.createElement('h3');h.textContent='下载';downloads.append(h);
      for(const f of files.filter(x=>/^(archive|build|runtime)\//.test(x.path)))downloads.append(链(f.path,f.url),document.createElement('br'));content.append(downloads);
      let info=null;const metadata=files.find(x=>x.path==='docs/包信息.json');if(metadata&&metadata.size<=65536){try{info=await 求(metadata.url);}catch{}}
      const deps=document.createElement('p');deps.textContent=Array.isArray(info?.dependencies)?'依赖：'+(info.dependencies.slice(0,256).map(x=>String(x)).join('、')||'无'):'此修订未提供依赖摘要，可查看包配置。';content.append(deps);
      for(const f of files.filter(x=>x.path.startsWith('source/')&&x.path.endsWith('。包。豫')&&x.path.split('/').length===2))content.append(链('包配置',阅读地址(data,f.path.slice(7),'source')));
      const readmes=files.filter(x=>Array.isArray(info?.readmes)?info.readmes.some(p=>x.path==='source/'+p):/^source\/[^/]+\.(汉语|文言)\.md$/.test(x.path));
      for(const f of readmes){const section=document.createElement('section'),h=document.createElement('h3'),pre=document.createElement('pre');h.textContent=f.path.slice(7);pre.className='包说明原文';section.append(h,pre);content.append(section);if(f.size>262144){pre.textContent='说明过长，请下载查看。';section.append(链('下载说明',f.url));continue;}try{const r=await fetch(f.url);if(!r.ok)throw Error();pre.textContent=await r.text();}catch{pre.textContent='说明暂不可用。';}}
      await 展示上传历史(data,content);
    }
    文(元素('市场状态'),'');
  }catch(e){文(元素('市场状态'),e.message);}
}
function 邮件跳转(){const p=new URLSearchParams(location.hash.slice(1));if(!p.has('verify')&&!p.has('reset'))return false;location.replace('/个人'+location.search+location.hash);return true;}
window.addEventListener('hashchange',邮件跳转);
元素('搜索词').value=查询;
元素('市场搜索').addEventListener('submit',e=>{e.preventDefault();查询=元素('搜索词').value.trim();const u=new URL(location.href);if(查询)u.searchParams.set('q',查询);else u.searchParams.delete('q');history.pushState(null,'',u);列表(true);});
window.addEventListener('popstate',()=>{if(location.pathname!=='/')return;查询=new URLSearchParams(location.search).get('q')?.trim().slice(0,100)||'';元素('搜索词').value=查询;列表(true);});
元素('市场更多').addEventListener('click',()=>列表());
if(!邮件跳转()){const match=location.pathname.match(/^\/release\/([a-f0-9]{32})(?:\/(docs|files))?$/);if(match)详情(match[1],match[2]||'');else 列表(true);}
