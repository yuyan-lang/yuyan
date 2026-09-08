import {test} from 'node:test';
import assert from 'node:assert/strict';
import {页面键,页面版本,提交页面,消费页面,完成键,页面状态} from '../源码/持久页面.js';
import {阅读页面入口} from '../源码/阅读转发.js';
const id='a'.repeat(32),origin='https://packages.yuyan-lang.org',url=origin+'/release/'+id+'/files?path=source/例。豫&lang=han';
function setup(){
 const objects=new Map(),sent=[];let sequence=0;
 const env={PERSISTENT_PAGES:'true',PAGE_QUEUE:{async send(x){sent.push(x);},async sendBatch(xs){sent.push(...xs.map(x=>x.body));}},PACKAGES:{
  async get(k){const o=objects.get(k);return o?{...o,body:o.bytes.slice(),size:o.bytes.length,text:async()=>new TextDecoder().decode(o.bytes)}:null;},
  async head(k){return this.get(k);},
  async put(k,value,options={}){const old=objects.get(k);if(options.onlyIf?.get('If-None-Match')==='*'&&old)return null;if(options.onlyIf?.get('If-Match')&&options.onlyIf.get('If-Match')!==old?.httpEtag)return null;const bytes=new Uint8Array(await new Response(value).arrayBuffer());objects.set(k,{bytes,customMetadata:options.customMetadata,httpEtag:String(++sequence)});return{key:k};},
  async delete(k){objects.delete(k);}
 },DB:{prepare(){return{bind(){return this;},async first(){return{id};}}}},ASSETS:{async fetch(){return new Response('<html>正在生成</html>');}},PACKAGE_CONTAINER:{getByName(){throw Error('不应访问容器');}}};
 return{env,objects,sent};
}
test('持久键隔离修订语言模式并统一旧地址',async()=>{
 assert.equal(await 页面键(url),await 页面键(url.replace('/files?','/docs?')));
 assert.notEqual(await 页面键(url),await 页面键(url+'&view=source'));
 assert.notEqual(await 页面键(url),await 页面键(url.replace('lang=han','lang=wen')));
});
test('R2命中无需数据库与容器，响应凭据不进入存储',async()=>{
 const{env}=setup();env.DB={prepare(){throw Error('不应查数据库');}};const key=await 页面键(url);await env.PACKAGES.put(key,'<html>已生成</html>');
 const r=await 阅读页面入口(new Request(url,{headers:{Cookie:'private=do-not-forward'}}),env);
 assert.equal(r.status,200);assert.match(await r.text(),/已生成/);assert.equal(r.headers.get('x-yuyan-page-store'),'r2');assert.ok(r.headers.get('set-cookie'));assert.equal((await env.PACKAGES.get(key)).customMetadata,undefined);
 const h=await 阅读页面入口(new Request(url,{method:'HEAD'}),env);assert.equal(await h.text(),'');
});
test('未生成只入队一次，202临时提示不保存为成页',async()=>{
 const{env,sent}=setup();await env.PACKAGES.put(完成键(id),'{}');await env.PACKAGES.put('releases/'+id+'/source/例。豫','源码');
 for(let i=0;i<2;i++){const r=await 阅读页面入口(new Request(url),env);assert.equal(r.status,202);assert.equal(r.headers.get('cache-control'),'no-store');}
 assert.equal(sent.length,1);assert.equal(await env.PACKAGES.get(await 页面键(url)),null);
});
test('未完成上传不入队，不存在文件返回404',async()=>{
 const{env,sent}=setup();await env.PACKAGES.put('releases/'+id+'/source/例。豫','源码');assert.equal((await 阅读页面入口(new Request(url),env)).status,202);assert.equal(sent.length,0);
 assert.equal((await 阅读页面入口(new Request(url.replace('例。豫','不存在。豫')),env)).status,404);
});
test('并发提交合并任务，入队失败可重试',async()=>{
 const{env,sent}=setup();await Promise.all([提交页面(env,url),提交页面(env,url)]);assert.equal(sent.length,1);
 const other=url+'&view=source';env.PAGE_QUEUE.send=async()=>{throw Error('队列暂不可用');};await assert.rejects(提交页面(env,other));assert.equal(await env.PACKAGES.get((await 页面键(other))+'.pending'),null);
});
test('成功成页的重复任务跳过，失败任务重试且不写HTML',async()=>{
 const{env}=setup(),key=await 页面键(url);let ack=0,retry=0;const message={body:{kind:'page',url,version:页面版本},ack(){ack++;},retry(){retry++;}};
 await env.PACKAGES.put(key,'成页');await 消费页面({messages:[message]},env);assert.equal(ack,1);await env.PACKAGES.delete(key);await 消费页面({messages:[message]},env);assert.equal(retry,1);assert.equal(await env.PACKAGES.get(key),null);
});
test('过期概览先返回R2旧页，再异步刷新',async()=>{
 const{env,sent}=setup(),u=origin+'/release/'+id+'?lang=han';await env.PACKAGES.put(await 页面键(u),'旧概览',{customMetadata:{created:'0'}});
 const r=await 阅读页面入口(new Request(u),env);assert.equal(r.status,200);assert.equal(await r.text(),'旧概览');assert.equal(sent[0].refresh,true);
});
test('后台成功后才原子保存HTML，失败刷新不覆盖旧页面',async()=>{
 const{env}=setup(),u=url+'&view=source',key=await 页面键(u),release={id,name:'例',owner:'豫言',version:'1',type:'库',files:[{path:'source/例。豫',url:'/原文'}]};
 await env.PACKAGES.put(完成键(id),'{}');await env.PACKAGES.put('rendered/'+页面版本+'/'+id+'/release.json',JSON.stringify(release));await env.PACKAGES.put('releases/'+id+'/source/例。豫','源码');
 let ack=0,retry=0;env.PACKAGE_CONTAINER={getByName(name){assert.match(name,/^豫言文档-[012]$/);return{async fetch(r){assert.equal(r.headers.get('cookie'),null);return new Response('<html>生成成功</html>');}};}};
 const message={body:{kind:'page',url:u,version:页面版本},ack(){ack++;},retry(){retry++;}};
 await 消费页面({messages:[message]},env);assert.equal(ack,1);assert.equal(retry,0);assert.match(await(await env.PACKAGES.get(key)).text(),/生成成功/);
 env.PACKAGE_CONTAINER={getByName(){return{async fetch(){return new Response('错误',{status:500});}};}};message.body.refresh=true;
 await 消费页面({messages:[message]},env);assert.equal(retry,1);assert.match(await(await env.PACKAGES.get(key)).text(),/生成成功/);
});
test('无效历史分页和越界路径不会放大生成队列',async()=>{
 const{env,sent}=setup();await env.PACKAGES.put(完成键(id),'{}');
 const root=origin+'/release/'+id;
 assert.equal((await 阅读页面入口(new Request(root+'?historyOffset=1'),env)).status,400);
 assert.equal((await 阅读页面入口(new Request(root+'?historyOffset=50'),env)).status,404);
 assert.equal((await 阅读页面入口(new Request(root+'/files?path=source/../例。豫'),env)).status,404);
 assert.equal(sent.length,0);
});
test('刷新验收只统计计划HTML，不把占位锁或额外对象算成完成',async()=>{
 const{env,objects}=setup(),key=await 页面键(url),other=await 页面键(url+'&view=source');
 const prefix='rendered/'+页面版本+'/'+id+'/';
 await env.PACKAGES.put(prefix+'plan.json',JSON.stringify({keys:[key,other]}));
 await env.PACKAGES.put(key,'成页');await env.PACKAGES.put(other+'.pending','');
 env.PACKAGES.list=async()=>({objects:[...objects.keys()].map(key=>({key})),truncated:false});
 const req=new Request(origin+'/api/releases/'+id+'/pages');
 assert.deepEqual(await(await 页面状态(req,env)).json(),{status:'generating',version:页面版本,expected:2,completed:1});
 await env.PACKAGES.put(other,'成页');
 assert.equal((await(await 页面状态(req,env)).json()).status,'ready');
});
test('旧修订迁移时缺页沿用即时渲染，成功后持久保存',async()=>{
 const{env}=setup(),release={id,name:'例',owner:'豫言',version:'1',files:[]};
 await env.PACKAGES.put(完成键(id),'{}',{customMetadata:{legacy:'true'}});
 env.PACKAGES.list=async()=>({objects:[],truncated:false});
 env.DB.prepare=()=>({bind(){return this;},async first(){return release;}});
 env.PACKAGE_CONTAINER={getByName(){return{async fetch(){return new Response('<html>旧文档继续可读</html>');}};}};
 const u=origin+'/release/'+id+'/files?lang=han';
 const r=await 阅读页面入口(new Request(u),env);assert.equal(r.status,200);assert.match(await r.text(),/旧文档继续可读/);
 assert.ok(await env.PACKAGES.get(await 页面键(u)));
});
