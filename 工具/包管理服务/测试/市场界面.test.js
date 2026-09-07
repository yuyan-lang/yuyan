import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {JSDOM} from 'jsdom';
// 古曰：客辞作字，不作令。今释：市场卡片使用文本渲染，搜索请求保持只读且不访问账户。
test('市场展示真实字段、转义上传者文字并支持搜索',async()=>{
 const dom=new JSDOM(readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),{url:'https://packages.yuyan-lang.org/',runScripts:'outside-only'}),w=dom.window,calls=[];
 w.fetch=async url=>{calls.push(url);return Response.json({releases:[{id:'a'.repeat(32),owner:'甲',name:'<img src=x onerror=alert(1)>',description:'<script>bad()</script>',version:'1.0',type:'库',identityVerified:0}],nextOffset:null});};
 w.eval(readFileSync(new URL('../网页/市场.mjs',import.meta.url),'utf8'));await new Promise(r=>setTimeout(r,20));
 assert.equal(w.document.querySelectorAll('.包卡片').length,1);assert.equal(w.document.querySelectorAll('.包卡片 img,.包卡片 script').length,0);assert.match(w.document.querySelector('.包卡片').textContent,/unverified/);
 w.document.getElementById('搜索词').value='解析器';w.document.getElementById('市场搜索').dispatchEvent(new w.Event('submit',{cancelable:true}));await new Promise(r=>setTimeout(r,20));
 assert.ok(calls.at(-1).includes(encodeURIComponent('解析器')));assert.ok(calls.every(x=>x.startsWith('/api/releases?catalog=1')));assert.equal(w.document.getElementById('账户表单'),null);dom.window.close();
});
test('旧修订详情显示序数、最新入口及分页历史 ZIP 下载',async()=>{
 const id='a'.repeat(32),latest='b'.repeat(32),dom=new JSDOM(readFileSync(new URL('../网页/首页.html',import.meta.url),'utf8'),{url:'https://packages.yuyan-lang.org/release/'+id,runScripts:'outside-only'}),w=dom.window;
 w.fetch=async url=>Response.json(url.includes('/history')?{revisions:[{revision:1,created:'今天',url:'/release/'+id,downloadUrl:'/api/releases/'+id+'/files/archive/发布.zip'}],nextOffset:null}:{id,owner:'甲',name:'包',version:'1',revision:1,latestId:latest,historyUrl:'/api/releases/'+id+'/history',description:'说明',files:[],cursor:null,docsUrl:'https://usercontent.yuyan-lang.org/'+id+'/index.html'});
 w.eval(readFileSync(new URL('../网页/市场.mjs',import.meta.url),'utf8'));await new Promise(r=>setTimeout(r,40));
 assert.match(w.document.getElementById('详情标题').textContent,/上传 #1/);const history=w.document.querySelector('[data-upload-history]');assert.ok(history);assert.equal(history.querySelector('a').getAttribute('href'),'/release/'+latest);assert.match(history.textContent,/下载 ZIP/);assert.equal(history.querySelector('button').hidden,true);dom.window.close();
});
