// 文言：工虽再用，客态不得相遗，二出不得相杂。汉语：直接测试 Node 宿主适配层的实例隔离、模块更新、输出完整性、异常恢复与取消；不执行编译算法。
'use strict';
const 文件=require('node:fs'),路径=require('node:path'),系统=require('node:os'),{spawnSync}=require('node:child_process'),断言=require('node:assert/strict');
const 根=路径.resolve(__dirname,'../..'),宿主=路径.join(根,'工具/节点网页汇编宿主/宿主.cjs');
const 临时=文件.mkdtempSync(路径.join(系统.tmpdir(),'yy线程复用验-')),输入=路径.join(临时,'yy探针.wasm');
const {建立编译线程}=require('../../工具/节点网页汇编宿主/编译线程.cjs');
process.env.YY_NODE_COMPILER_REUSE='1';
const 启动=建立编译线程(宿主,输入,[]),记录=[];
function 造(体){const 文=路径.join(临时,'yy探针.wat');文件.writeFileSync(文,体);const 果=spawnSync(路径.join(根,'yy网页汇编宿主'),['--组装',文,输入],{cwd:根,encoding:'utf8'});断言.equal(果.status,0,果.stderr);}
const 隔离='(module (global $seen (mut i32) (i32.const 0)) (func (export "_start") global.get $seen if unreachable end i32.const 1 global.set $seen))';
function 行(标记,启动者=启动){
 const 桥=启动者(process.execPath,[宿主,输入,'--mode=worker',标记],true);断言.ok(桥);
 let 出=[],错=[],始=0;
 const 毕=new Promise((成,拒)=>{
  const 限=setTimeout(()=>{桥.kill();拒(Error('测试任务超时'));},15000);
  桥.on('spawn',()=>始++);桥.stdout.on('data',块=>出.push(块));桥.stderr.on('data',块=>错.push(块));桥.on('error',拒);
  桥.once('close',码=>{clearTimeout(限);成({码,出:Buffer.concat(出),错:Buffer.concat(错),始,桥});});
 });
 return {桥,毕};
}
function 线程(果){const 行=果.错.toString().trim().split('\n').find(文=>文.startsWith('{"轮次":'));return JSON.parse(行).线程;}
function 记(名){记录.push(名);process.stdout.write(名+'通过\n');}
function 字(名,文){const 值=Buffer.from(文);return {段:`(data $${名} "${[...值].map(字=>'\\'+字.toString(16).padStart(2,'0')).join('')}")`,取:`i32.const 0 i32.const ${值.length} array.new_data $bytes $${名}`};}
async function 主(){try{
 断言.equal(启动('/bin/true',[],false),null);
 断言.equal(启动(process.execPath,[宿主,输入,'普通任务'],true),null);记('普通任务不入池');
 造(隔离);
 for(const 复用 of [true,false]){
  if(复用)delete process.env.YY_NODE_COMPILER_REUSE;else process.env.YY_NODE_COMPILER_REUSE='0';
  const 选择池=建立编译线程(宿主,输入,[]);process.env.YY_NODE_COMPILER_REUSE='1';
  try {const 甲=await 行('选甲',选择池).毕,乙=await 行('选乙',选择池).毕;断言.equal(甲.码,0);断言.equal(乙.码,0);断言.equal(线程(甲)===线程(乙),复用);}
  finally{选择池.清理();}
 }记('默认复用与显式关闭');
 const 甲=await 行('甲').毕,乙事=行('乙');甲.桥.kill();
 const 乙=await 乙事.毕,丙=await 行('丙').毕;
 for(const 果 of [甲,乙,丙]){断言.equal(果.码,0);断言.equal(果.始,1);断言.equal(果.出.length,0);}
 断言.equal(线程(甲),线程(乙));断言.equal(线程(乙),线程(丙));记('同线程全新实例');
 const 错池=建立编译线程(宿主,输入,[]);
 try {
  const 先=await 行('错前',错池).毕;
  造('(module (func (export "_start") unreachable))');
  const 错=await 行('陷阱',错池).毕;断言.equal(错.码,1);断言.match(错.错.toString(),/unreachable/);
  造(隔离);const 复=await 行('恢复',错池).毕;断言.equal(复.码,0);断言.equal(线程(先),线程(复));记('模块更新与异常恢复');
 } finally {错池.清理();}
 const 正文=Buffer.from('首😀\u0000尾'.repeat(60000)),错文=Buffer.from('错😀\u0000末'.repeat(30000));
 const 出名=字('outname','豫言_打印字符串'),错名=字('errname','豫言_标准错误打印行'),参名=字('argname','豫言_获取命令行参数'),出段=字('out',正文),错段=字('err',错文);
 造(`(module (type $bytes (array (mut i8))) (type $tuple (array (mut (ref null eq))))
  (import "yuyan:gc-host/v1" "call" (func $call (param (ref null eq) (ref null eq)) (result (ref null eq))))
  ${[出名,错名,参名,出段,错段].map(段=>段.段).join('\n')}
  (func (export "_start") ${出名.取} ${出段.取} array.new_fixed $tuple 1 call $call drop
   ${错名.取} ${错段.取} array.new_fixed $tuple 1 call $call drop
   ${出名.取} ${参名.取} array.new_fixed $tuple 0 call $call
   ref.cast (ref $tuple) i32.const 0 array.get $tuple
   ref.cast (ref $tuple) i32.const 1 array.get $tuple
   array.new_fixed $tuple 1 call $call drop))`);
 const 多=await Promise.all(Array.from({length:4},(_,序)=>行('输出'+序).毕));
 for(const [序,果] of 多.entries()){断言.equal(果.码,0);断言.deepEqual(果.出,Buffer.concat([正文,Buffer.from('输出'+序)]));断言.deepEqual(果.错.subarray(0,错文.length+1),Buffer.concat([错文,Buffer.from('\n')]));JSON.parse(果.错.subarray(错文.length+1).toString());断言.equal(果.始,1);}
 记('并发二出完整与参数隔离');
 const 取消池=建立编译线程(宿主,输入,[]);
 try {
  const 被止=行('输出取消',取消池);被止.桥.stdout.once('data',()=>被止.桥.kill());await 被止.毕;
  const 后验=await 行('输出恢复',取消池).毕;断言.equal(后验.码,0);断言.deepEqual(后验.出,Buffer.concat([正文,Buffer.from('输出恢复')]));记('输出与完成竞态取消');
 } finally {取消池.清理();}
 造('(module (func (export "_start") (loop $forever br $forever)))');
 const 死=行('取消');setTimeout(()=>死.桥.kill(),60);const 止=await 死.毕;断言.notEqual(止.码,0);
 造(隔离);const 新=行('取消后');丙.桥.kill();const 成=await 新.毕;断言.equal(成.码,0);记('取消与旧句柄隔离');
 const 限池=建立编译线程(宿主,输入,[]);
 try {
  const 诸号=[];for(let 序=0;序<5;序++){const 果=await 行('轮'+序,限池).毕;断言.equal(果.码,0);诸号.push(线程(果));}
  断言.equal(new Set(诸号.slice(0,4)).size,1);断言.notEqual(诸号[4],诸号[0]);记('四任务后回收');
  await new Promise(成=>setTimeout(成,350));const 后=await 行('闲后',限池).毕;断言.equal(后.码,0);断言.notEqual(线程(后),诸号[4]);记('空闲超时回收');
 } finally {限池.清理();}
 启动.清理();断言.throws(()=>行('关闭后'),/已经关闭/);记('池关闭拒绝新任务');
 文件.writeFileSync(路径.join(根,'性能研究/自动内存分析/线程复用宿主验证.json'),JSON.stringify({passed:true,checks:记录,node:process.version},null,2)+'\n');
}finally{启动.清理();文件.rmSync(临时,{recursive:true,force:true});}}
主().catch(错=>{console.error(错);process.exitCode=1;});
