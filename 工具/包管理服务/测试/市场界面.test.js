import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {JSDOM} from 'jsdom';
// 文言：隐其章而存其籍。汉语：认证标识只在呈现层隐藏，不改变认证状态。
test('市场与账户的认证标识均不展示',()=>{
 const dom=new JSDOM('<style>'+readFileSync(new URL('../网页/界面.css',import.meta.url),'utf8')+'</style><span class="验证标识" data-verified="true">verified</span><span class="验证标识" data-verified="false">unverified</span>');
 for(const badge of dom.window.document.querySelectorAll('.验证标识'))assert.equal(dom.window.getComputedStyle(badge).display,'none');
 dom.window.close();
});
// 文言：豫源可览，杂材不陈。汉语：仅过滤浏览界面，并验证旧链接不会打开被隐藏的文件。
test('所有文件仅展示豫言源码和包定义，空包不请求其他材料',async()=>{
 for(const sources of [['source/目录/例。豫','source/例。包。豫'],[]]){
  const id='a'.repeat(32),dom=new JSDOM(readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),{url:'https://packages.yuyan-lang.org/release/'+id+'/files?path=docs/index.html',runScripts:'outside-only'}),w=dom.window,reads=[];
  const paths=[...sources,'source/绑定.c','source/绑定.h','source/说明.汉语.md','docs/index.html','docs/副本。豫','build/程序','archive/发布.zip'];
  w.fetch=async url=>{if(url.includes('/file?')){reads.push(new URL(url,w.location.origin).searchParams.get('path'));return Response.json({source:'源码',documentation:null});}return Response.json({id,owner:'甲',name:'包',version:'1',files:paths.map(path=>({path,url:'/下载'}))});};
  w.eval(readFileSync(new URL('../网页/市场.mjs',import.meta.url),'utf8'));await new Promise(r=>setTimeout(r,30));
  assert.deepEqual([...w.document.querySelectorAll('#浏览文件树 a')].map(a=>new URL(a.href).searchParams.get('path')).sort(),[...sources].sort());
  assert.deepEqual(reads,sources.length?[sources[0]]:[]);
  if(!sources.length){assert.match(w.document.getElementById('统一阅读内容').textContent,/暂无豫言源文件/);assert.equal(w.document.getElementById('看源码').disabled,true);}
  dom.window.close();
 }
});
// 古曰：客辞作字，不作令。今释：市场卡片使用文本渲染，搜索请求保持只读且不访问账户。
test('统一浏览器默认文档，右侧切换源码且仅渲染文本，保留同一文件',async()=>{
 const id='a'.repeat(32),dom=new JSDOM(readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),{url:'https://packages.yuyan-lang.org/release/'+id+'/files?path=source/例。豫',runScripts:'outside-only'}),w=dom.window;let reads=0;
 w.fetch=async url=>{if(url.includes('/file?')){reads++;return Response.json({path:'source/例。豫',source:'<script>源码</script>',documentation:{names:[{name:'<img src=x>',type:'字符串',description:'说明'}]}});}return Response.json({id,owner:'甲',name:'包',version:'1',type:'库',files:[{path:'source/例。豫',size:20,url:'/原文件'}],docsUrl:'https://usercontent.yuyan-lang.org/'+id+'/index.html'});};
 w.eval(readFileSync(new URL('../网页/市场.mjs',import.meta.url),'utf8'));await new Promise(r=>setTimeout(r,40));
 const body=w.document.getElementById('统一阅读内容');assert.match(body.textContent,/<img src=x>/);assert.equal(body.querySelector('img'),null);assert.ok(w.document.querySelector('.workspace .sidebar .tree-list'));assert.ok(w.document.querySelector('link[href="/共用/代码浏览器.css"]'));
 w.document.getElementById('看源码').click();await new Promise(r=>setTimeout(r,20));assert.match(body.textContent,/<script>源码/);assert.equal(body.querySelector('script'),null);assert.equal(reads,1);assert.equal(new URL(w.location.href).searchParams.get('path'),'source/例。豫');assert.equal(w.document.getElementById('看源码').getAttribute('aria-pressed'),'true');dom.window.close();
});
test('市场展示真实字段、转义上传者文字并支持搜索',async()=>{
 const dom=new JSDOM(readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),{url:'https://packages.yuyan-lang.org/',runScripts:'outside-only'}),w=dom.window,calls=[];
 w.fetch=async url=>{calls.push(url);return Response.json({releases:[{id:'a'.repeat(32),owner:'甲',name:'<img src=x onerror=alert(1)>',description:'<script>bad()</script>',version:'1.0',type:'库',identityVerified:0}],nextOffset:null});};
 w.eval(readFileSync(new URL('../网页/市场.mjs',import.meta.url),'utf8'));await new Promise(r=>setTimeout(r,20));
 assert.equal(w.document.querySelectorAll('.包卡片').length,1);assert.equal(w.document.querySelectorAll('.包卡片 img,.包卡片 script').length,0);assert.match(w.document.querySelector('.包卡片').textContent,/unverified/);
 w.document.getElementById('搜索词').value='解析器';w.document.getElementById('市场搜索').dispatchEvent(new w.Event('submit',{cancelable:true}));await new Promise(r=>setTimeout(r,20));
 assert.ok(calls.at(-1).includes(encodeURIComponent('解析器')));assert.equal(new URL(w.location.href).searchParams.get('q'),'解析器');assert.ok(calls.every(x=>x.startsWith('/api/releases?catalog=1')));assert.equal(w.document.getElementById('账户表单'),null);dom.window.close();
});
test('市场恢复链接中的查询与语言，浏览器返回恢复搜索',async()=>{
 const dom=new JSDOM(readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),{url:'https://packages.yuyan-lang.org/?q=标准库&lang=wen',runScripts:'outside-only'}),w=dom.window,calls=[];
 w.fetch=async url=>{calls.push(url);return Response.json({releases:[{id:'a'.repeat(32),owner:'豫言',name:'标准库',version:'1',type:'库'}],nextOffset:null});};
 w.eval(readFileSync(new URL('../网页/市场.mjs',import.meta.url),'utf8'));await new Promise(r=>setTimeout(r,20));
 assert.equal(w.document.getElementById('搜索词').value,'标准库');assert.ok(calls[0].includes(encodeURIComponent('标准库')));assert.equal(new URL(w.document.querySelector('.包卡片 h2 a').href).searchParams.get('lang'),'wen');
 w.history.pushState(null,'','/?q=解析&lang=wen');w.dispatchEvent(new w.PopStateEvent('popstate'));await new Promise(r=>setTimeout(r,20));assert.equal(w.document.getElementById('搜索词').value,'解析');assert.ok(calls.at(-1).includes(encodeURIComponent('解析')));dom.window.close();
});
test('包首页、文档目录与文件树分路由，文件默认全页阅读，不嵌入上传 HTML',async()=>{
 const id='a'.repeat(32),base='https://packages.yuyan-lang.org/release/'+id;
 for(const tab of ['', '/docs','/files']){
  const dom=new JSDOM(readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),{url:base+tab,runScripts:'outside-only'}),w=dom.window,calls=[];
  w.fetch=async url=>{calls.push(url);return Response.json(url.endsWith('/reading')?{modules:[{source:'目录/例。豫',document:'接口/模块-1.html'}]}:{id,owner:'甲',name:'例',version:'1',revision:2,type:'库',created:'今天',description:'介绍',files:[{path:'source/目录/例。豫',size:10,url:'/下载'}],docsUrl:'https://usercontent.yuyan-lang.org/'+id+'/index.html'});};
  w.eval(readFileSync(new URL('../网页/市场.mjs',import.meta.url),'utf8'));await new Promise(r=>setTimeout(r,30));
  assert.equal(w.document.querySelector('iframe'),null);assert.equal(w.document.querySelectorAll('#包导航 a').length,2);
  const content=w.document.getElementById('包内容');if(!tab){assert.doesNotMatch(content.textContent,/目录\/例。豫/);assert.ok(calls.every(x=>!x.endsWith('/reading')));}else{const a=[...content.querySelectorAll('a')].find(x=>x.textContent.includes('例。豫'));assert.ok(a);assert.equal(new URL(a.href).hostname,'packages.yuyan-lang.org');assert.equal(new URL(a.href).searchParams.get('view'),'docs');if(tab==='/files')assert.ok(content.querySelector('details'));}dom.window.close();
 }
});
test('旧修订详情显示序数、最新入口及分页历史 ZIP 下载',async()=>{
 const id='a'.repeat(32),latest='b'.repeat(32),dom=new JSDOM(readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),{url:'https://packages.yuyan-lang.org/release/'+id,runScripts:'outside-only'}),w=dom.window;
 w.fetch=async url=>Response.json(url.includes('/history')?{revisions:[{revision:1,created:'今天',url:'/release/'+id,downloadUrl:'/api/releases/'+id+'/files/archive/发布.zip'}],nextOffset:null}:{id,owner:'甲',name:'包',version:'1',revision:1,latestId:latest,historyUrl:'/api/releases/'+id+'/history',description:'说明',files:[],cursor:null,docsUrl:'https://usercontent.yuyan-lang.org/'+id+'/index.html'});
 w.eval(readFileSync(new URL('../网页/市场.mjs',import.meta.url),'utf8'));await new Promise(r=>setTimeout(r,40));
 assert.match(w.document.getElementById('详情标题').textContent,/上传 #1/);const history=w.document.querySelector('[data-upload-history]');assert.ok(history);assert.equal(history.querySelector('a').getAttribute('href'),'/release/'+latest);assert.match(history.textContent,/下载 ZIP/);assert.equal(history.querySelector('button').hidden,true);dom.window.close();
});
