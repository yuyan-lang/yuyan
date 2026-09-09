/* 古曰：汉语为始，文言可易；客书原录，不妄改之。今释：文档内选择语言，仅翻译界面，保留代码和日志。 */
(()=>{
const 站=document.querySelector('[data-yuyan-site]');if(!站)return;
const 名称={docs:['接口文档','接口之文'],source:['源码浏览','源码观览']};
const 类型=站.dataset.yuyanSite;document.documentElement.dataset.yuyan=类型;
const 参数=new URL(location.href).searchParams.get('lang');
// 古曰：客文在沙箱，禁存亦可易辞。今释：无同源权限时 Cookie 与历史记录可能抛异常，语言切换仍在内存中生效。
function 读取偏好(){try{return document.cookie.match(/(?:^|; )yuyan_lang=(han|wen)(?:;|$)/)?.[1];}catch{return undefined;}}
let 语言=['han','wen'].includes(参数)?参数:读取偏好()||'han';
const 词={"模块目录":"模块之目","接口参考":"接口之录","公开名称":"公开之名","公开":"公开","暂无说明。":"尚无说明。","此模块没有公开名称。":"此模块无公开之名。","选择模块，查看其公开名称、中文类型与源码注释。":"择模块以观公开之名、中文类型与源码之注。","入口":"始于","模块":"模块","名称":"名","公开接口":"公开之名","选择文件查看源码":"择书以观其文","源码树":"源码目录","阅读设置":"观览之制","请阅读汉语版本。":"请阅汉语本。","暂无文言版本":"文言本尚缺","复制代码":"复制此文","菜单":"目录","跳到正文":"直至正文"};
function 译(s){return 语言==='han'?s:(词[s]||s);}
// 文言：本地之文不系云府。汉语：语言偏好仅用于当前文档，不设置跨站 Cookie。
function 存(){}
const 导航=document.createElement('header');导航.className='豫导航';导航.innerHTML='<a class="豫跳转" href="#正文">跳到正文</a><a class="豫站名" href="index.html"></a><button class="豫菜单" type="button" aria-expanded="false" aria-controls="豫导航内">菜单</button><nav class="豫导航内" id="豫导航内"><div class="豫语言" role="group" aria-label="界面语言"><button type="button" data-lang="han">汉语</button><button type="button" data-lang="wen">文言</button></div></nav>';
document.body.prepend(导航);const 正文=document.querySelector('main');if(正文&&!document.getElementById('正文'))正文.id='正文';
if(['docs','source'].includes(类型)){const 提示=document.createElement('p');提示.className='豫原文说明';提示.dataset.han='源码、类型签名和源码注释按原文显示。';提示.dataset.wen='源码、类型签名与原注皆存其本，无独立文言译本者不妄改之。';提示.textContent=提示.dataset.han;导航.after(提示);}
const 原字=new WeakMap();
function 刷(){if(!document?.body)return;document.documentElement.lang=语言==='wen'?'lzh':'zh-CN';document.documentElement.dataset.lang=语言;const 站名=(名称[类型]||名称.docs)[语言==='wen'?1:0];if(导航.querySelector('.豫站名').textContent!==站名)导航.querySelector('.豫站名').textContent=站名;导航.querySelectorAll('[data-lang]').forEach(b=>b.setAttribute('aria-pressed',String(b.dataset.lang===语言)));
 document.querySelectorAll('[data-han][data-wen]').forEach(e=>{const t=语言==='wen'?e.dataset.wen:e.dataset.han;if(e.textContent!==t)e.textContent=t;});
 document.querySelectorAll('[data-language-version]').forEach(e=>{e.hidden=e.dataset.languageVersion!==语言;});
 const walker=document.createTreeWalker(document.body,NodeFilter.SHOW_ELEMENT|NodeFilter.SHOW_TEXT,{acceptNode:n=>n.nodeType===1?(n.matches('script,style,code,pre,textarea,[role=log],[data-原文],[data-han],[data-language-version],.tree-name,.module-link-name,.module-title h2,.symbol-heading h3,.source-path,.breadcrumb,.symbol-description:not(.is-empty)')?NodeFilter.FILTER_REJECT:NodeFilter.FILTER_SKIP):NodeFilter.FILTER_ACCEPT});let n;while(n=walker.nextNode()){const p=n.parentElement;if(!p||p.closest('script,style,code,pre,textarea,[role=log],[data-原文],[data-han],[data-language-version],.tree-name,.module-link-name,.module-title h2,.symbol-heading h3,.source-path,.breadcrumb,.symbol-description:not(.is-empty)'))continue;const old=原字.get(n);const source=old&&n.nodeValue===old.out?old.source:n.nodeValue;const trimmed=source.trim();const out=source.replace(trimmed,译(trimmed));原字.set(n,{source,out});if(n.nodeValue!==out)n.nodeValue=out;}
 document.querySelectorAll('[data-placeholder-han]').forEach(e=>e.placeholder=语言==='wen'?e.dataset.placeholderWen:e.dataset.placeholderHan);
 document.querySelectorAll('a[data-language-peer]').forEach(a=>{a.href=语言==='wen'?a.dataset.wenHref:a.dataset.hanHref;});
}
function 设置(v){if(!['han','wen'].includes(v))return;语言=v;存();const u=new URL(location.href);u.searchParams.set('lang',v);try{history.replaceState(null,'',u);}catch{}刷();document.dispatchEvent(new CustomEvent('豫言语言改变',{detail:v}));const peer=站.dataset[语言==='wen'?'wenPage':'hanPage'];if(peer&&new URL(peer,location.href).pathname!==location.pathname){const x=new URL(peer,location.href);x.searchParams.set('lang',v);x.hash=location.hash;location.assign(x);}}
导航.querySelectorAll('[data-lang]').forEach(b=>b.addEventListener('click',()=>设置(b.dataset.lang)));导航.querySelector('.豫菜单').addEventListener('click',e=>{const open=导航.dataset.open!=='true';导航.dataset.open=String(open);e.currentTarget.setAttribute('aria-expanded',String(open));});
window.豫言界面={译,设置语言:设置,get 语言(){return 语言}};if(参数)存();刷();let 排队=false;new MutationObserver(()=>{if(排队)return;排队=true;queueMicrotask(()=>{排队=false;刷();});}).observe(document.body,{childList:true,subtree:true,characterData:true});
if(站.dataset.hanPage||站.dataset.wenPage){const peer=站.dataset[语言==='wen'?'wenPage':'hanPage'];if(peer&&new URL(peer,location.href).pathname!==location.pathname){const u=new URL(peer,location.href);u.searchParams.set('lang',语言);u.hash=location.hash;location.replace(u);}}
})();
