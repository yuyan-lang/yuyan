// 文言：额度应用以旧接口（持久事务一版之术）构建，新壳与新适配下仍须通过云仓原验收之诸断言。
// 汉语：回归测试：断言逐字取自云仓 应用/豫言体验/验收/额度豫言验收.mjs，仅把产物路径改为环境变量 额度应用产物
// （用私有暂存构建云仓 应用/豫言体验/额度应用 所得的 dist 目录，见同目录说明）。
import {test} from 'node:test';
import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {join} from 'node:path';
import {pathToFileURL} from 'node:url';

// 文言：真器承旧双键，异客并求亦不得越总额。汉语：用真实豫言 Wasm 与事务存储模拟器验证旧数据格式和并发扣减。
const 产物=process.env.额度应用产物;
if(!产物)throw new Error('请设置环境变量 额度应用产物 为构建产物目录（dist/额度应用）');
const {创建云工宿主}=await import(pathToFileURL(join(产物,'宿主.mjs')));
const 程序模块=await WebAssembly.compile(await readFile(join(产物,'程序.wasm')));
const 值桥模块=await WebAssembly.compile(await readFile(join(产物,'值桥.wasm')));
const 宿主=创建云工宿主({程序模块,值桥模块,许可:{ENV:['DAILY_AI_LIMIT','DAILY_RUN_LIMIT']}});
const 新存储=(初始={})=>{
 const 数据=new Map(Object.entries(structuredClone(初始)));let 前=Promise.resolve();
 const storage={
  async transaction(执行){
   let 放行;const 后=new Promise(完成=>{放行=完成;});const 旧前=前;前=后;await 旧前;
   const 草稿=new Map([...数据].map(([键,值])=>[键,structuredClone(值)]));let 回滚=false;
   const 事务={async get(键){return structuredClone(草稿.get(键));},async put(键,值){草稿.set(键,structuredClone(值));},rollback(){回滚=true;}};
   try{const 结果=await 执行(事务);if(!回滚){数据.clear();for(const [键,值] of 草稿)数据.set(键,值);}return 结果;}finally{放行();}
  }
 };
 return {数据,storage};
};
const 标识=数=>数.toString(16).padStart(64,'0');
const 请求=(ip,action='assist')=>new Request('https://quota.internal/',{method:'POST',body:JSON.stringify({ip,action})});
const 扣=(存储,环境,ip,action)=>宿主.durableFetch(请求(ip,action),环境,{storage:存储});

test('旧总账与访客桶保持原结构并在事务中扣减',async()=>{
 const 日=new Date().toISOString().slice(0,10),分=Math.floor(Date.now()/60000),ip=标识(1);
 const {数据,storage}=新存储({'总账':{day:日,assist:2,run:1},'客簿-00':{day:日,visitors:{[ip]:{minute:分-1,count:6,assist:2,run:1}}}});
 const 环境={DAILY_AI_LIMIT:'4',DAILY_RUN_LIMIT:'10'};
 let 回=await 扣(storage,环境,ip,'assist');assert.equal(回.status,200);assert.deepEqual(await 回.json(),{ok:true});
 assert.deepEqual(数据.get('总账'),{day:日,assist:3,run:1});
 assert.deepEqual(数据.get('客簿-00').visitors[ip],{minute:分,count:1,assist:3,run:1});
 回=await 扣(storage,环境,标识(2),'assist');assert.equal(回.status,200);
 回=await 扣(storage,环境,标识(3),'assist');assert.equal(回.status,429);
 assert.equal(回.headers.get('Cache-Control'),'no-store');
 assert.equal(数据.get('总账').assist,4);
});

test('并发访客不能超过全站限额，非法请求和限额不写存储',async()=>{
 const {数据,storage}=新存储(),环境={DAILY_AI_LIMIT:'2',DAILY_RUN_LIMIT:'10'};
 const 回=await Promise.all(Array.from({length:8},(_,序)=>扣(storage,环境,标识(序+10),'assist')));
 assert.equal(回.filter(项=>项.status===200).length,2);
 assert.equal(回.filter(项=>项.status===429).length,6);
 assert.equal(数据.get('总账').assist,2);
 const 旧=JSON.stringify([...数据]);
 assert.equal((await 扣(storage,环境,'bad','assist')).status,400);
 assert.equal((await 扣(storage,{...环境,DAILY_AI_LIMIT:'0'},标识(20),'assist')).status,503);
 assert.equal(JSON.stringify([...数据]),旧);
});

test('日切换清空旧访客，单访客分钟与每日动作限额生效',async()=>{
 const ip=标识(30),旧客=标识(31),日=new Date().toISOString().slice(0,10),分=Math.floor(Date.now()/60000);
 const {数据,storage}=新存储({'总账':{day:'2000-01-01',assist:99,run:99},'客簿-00':{day:'2000-01-01',visitors:{[旧客]:{minute:分,count:6,assist:10,run:50}}}});
 const 环境={DAILY_AI_LIMIT:'100',DAILY_RUN_LIMIT:'100'};
 assert.equal((await 扣(storage,环境,ip,'run')).status,200);
 assert.deepEqual(数据.get('总账'),{day:日,assist:0,run:1});
 assert.equal(Object.keys(数据.get('客簿-00').visitors).length,1);
 assert.equal(数据.get('客簿-00').visitors[旧客],undefined);
 for(let 次=1;次<6;次++)assert.equal((await 扣(storage,环境,ip,'run')).status,200);
 assert.equal((await 扣(storage,环境,ip,'run')).status,429);
 assert.equal(数据.get('总账').run,6);
 const 老客=数据.get('客簿-00');老客.visitors[ip]={minute:分-1,count:6,assist:10,run:2};数据.set('客簿-00',老客);
 assert.equal((await 扣(storage,环境,ip,'assist')).status,429);
 assert.equal((await 扣(storage,环境,ip,'run')).status,200);
 assert.equal(数据.get('客簿-00').visitors[ip].count,1);
});
