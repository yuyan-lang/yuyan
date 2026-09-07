import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {JSDOM} from 'jsdom';
// 文言：一举传档，未定主则先问之。汉语：DOM 验证所有者对话框、单 ZIP 发布与文档隔离。
const id='a'.repeat(32),origin='https://packages.yuyan-lang.org';
const code=readFileSync(new URL('../网页/发布.mjs',import.meta.url),'utf8');
const tick=()=>new Promise(r=>setTimeout(r,20));
test('未绑定先弹窗，绑定后只上传 ZIP，失败补传仍用同一归档，上传文本不进入 HTML',async()=>{
  const dom=new JSDOM('<main id="正文"></main>',{url:origin,runScripts:'outside-only'}),w=dom.window,calls=[];
  let bound=false,fail=true;
  w.HTMLDialogElement.prototype.showModal=function(){this.open=true;};
  w.HTMLDialogElement.prototype.close=function(){this.open=false;};
  w.fetch=async(url,options={})=>{
    calls.push({url,options});
    if(url==='/api/account/session')return Response.json({user:{name:'甲',ownerBound:bound,emailVerified:1}});
    if(url==='/api/account/owner'){assert.deepEqual(JSON.parse(options.body),{name:'甲'});bound=true;return Response.json({owner:'甲'});}
    if(url==='/api/releases/zip')return Response.json({id,url:'/release/'+id,incomplete:fail});
    if(url.startsWith('/api/releases?'))return Response.json({releases:[],nextOffset:null});
    return Response.json({id,owner:'甲',name:'<img src=x onerror=alert(1)>',description:'<script>bad()</script>',
      version:'1.0.0',type:'库',files:[],docsUrl:'https://usercontent.yuyan-lang.org/'+id+'/index.html'});
  };
  w.eval(code);await tick();
  const form=w.document.getElementById('即时表单'),dialog=w.document.getElementById('所有者对话框');
  const archive=new w.File(['zip'],'发布.zip',{type:'application/zip'});
  Object.defineProperty(form.elements.archive,'files',{value:[archive]});
  form.dispatchEvent(new w.Event('submit',{cancelable:true}));await tick();
  assert.equal(dialog.open,true);assert.equal(calls.some(c=>c.url==='/api/releases/zip'),false);
  const owner=w.document.getElementById('所有者表单');owner.elements.owner.value='甲';
  owner.dispatchEvent(new w.Event('submit',{cancelable:true}));await tick();
  assert.equal(dialog.open,false);assert.match(w.document.getElementById('所有者状态').textContent,/甲/);
  form.dispatchEvent(new w.Event('submit',{cancelable:true}));await tick();await tick();
  assert.equal(decodeURIComponent(w.location.pathname),'/个人');
  assert.match(w.document.getElementById('即时提示').textContent,/部分材料/);
  assert.equal(w.document.querySelector('#版本标题 img'),null);
  const iframe=w.document.getElementById('包文档');
  assert.equal(iframe.getAttribute('sandbox'),'allow-scripts');assert.equal(iframe.hasAttribute('srcdoc'),false);
  fail=false;form.dispatchEvent(new w.Event('submit',{cancelable:true}));await tick();await tick();
  assert.match(w.document.getElementById('即时提示').textContent,/发布完成/);
  const uploads=calls.filter(c=>c.url==='/api/releases/zip');assert.equal(uploads.length,2);
  assert.ok(uploads.every(c=>c.options.body===archive&&c.options.headers['Content-Type']==='application/zip'));
  assert.equal(calls.some(c=>c.options.method==='PUT'||/prepare|validate|complete/.test(c.url)),false);
  dom.window.close();
});
