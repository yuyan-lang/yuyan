// 古曰：市示众包，私事归己。今释：市场只读公开接口；旧邮件链接携带原 fragment 转至个人页。
const 元素=id=>document.getElementById(id);
const 文=(el,han,wen=han)=>{el.dataset.han=han;el.dataset.wen=wen;el.textContent=window.豫言界面?.语言==='wen'?wen:han;};
let 下页=0,查询='',序=0,忙=false;
async function 求(url){const r=await fetch(url,{cache:'no-store'});const data=await r.json();if(!r.ok)throw Error(data.error||'读取失败');return data;}
function 链(text,url){const a=document.createElement('a');a.textContent=text;a.href=url;return a;}
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
async function 详情(id){
  元素('市场搜索').hidden=true;元素('市场列表').hidden=true;元素('市场更多').hidden=true;文(元素('市场状态'),'正在读取包详情…','正读包之详…');
  try{let data=await 求('/api/releases/'+id),files=[...data.files];while(data.cursor){const page=await 求('/api/releases/'+id+'?cursor='+encodeURIComponent(data.cursor));files.push(...page.files);data.cursor=page.cursor;}
    元素('市场详情').hidden=false;元素('详情标题').textContent=data.owner+' / '+data.name+' · '+data.version+' · 上传 #'+(data.revision||1);元素('详情简介').textContent=data.description;
    文(元素('详情材料'),'已提供 '+files.length+' 个文件。缺失材料可由发布者补传。','已供 '+files.length+' 档，缺者发布者可补。');
    for(const f of files){const li=document.createElement('li');li.append(链(f.path+' · '+f.size+' 字节',f.url));元素('详情文件').append(li);}
    const url=new URL(data.docsUrl);if(url.protocol!=='https:'||url.hostname!=='usercontent.yuyan-lang.org')throw Error('文档来源配置错误');元素('详情文档').src=url.href;await 展示上传历史(data,元素('市场详情'));文(元素('市场状态'),'');
  }catch(e){文(元素('市场状态'),e.message);}
}
function 邮件跳转(){const p=new URLSearchParams(location.hash.slice(1));if(!p.has('verify')&&!p.has('reset'))return false;location.replace('/个人'+location.search+location.hash);return true;}
window.addEventListener('hashchange',邮件跳转);
元素('市场搜索').addEventListener('submit',e=>{e.preventDefault();查询=元素('搜索词').value.trim();列表(true);});
元素('市场更多').addEventListener('click',()=>列表());
if(!邮件跳转()){const id=location.pathname.match(/^\/release\/([a-f0-9]{32})$/)?.[1];if(id)详情(id);else 列表(true);}
