// 文言：WASI 之对照：以执行器与 Node 自带之 WASI 各行同一 Wasm，较其标准出与退出码。
// 汉语：WASI 对照测试（Node）。用法：node WASI对照.cjs 执行器.wasm 程序.wasm [--实参 甲]... [--输入文件 文件] [--程序名 路径]
//   1. 把程序放进执行器内存 0x10000、命令行参数写进 0xC000（个数，其后每项 [长度][字节…] 对齐到 4）、程序名写进 0xD000，
//      调用导出 启动（缺省行 _start）；平台函数里 写字节 收集输出、读字节 供给输入文件的字节（供尽而程序仍要读则以 201 退出）、
//      时钟滴答 每次加 3；
//   2. 另起 Node 子进程，以 node:wasi（preview1，标准输入取输入文件，标准输出与标准错误接同一文件以保持写入之序）行同一程序；
//   3. 较二者之输出与退出码：同则退出 0，不同则打印差异并退出 1。
//   若 Node 一侧以陷阱（RuntimeError）终止，则只要求执行器也以非零退出码终止（陷阱之码与信息各有不同，输出之尾不作比较）。
//   Node 之 WASI 属实验之功能，会向标准错误打一行警告，已由 NODE_NO_WARNINGS 关掉。
//   标准输入：执行器像终端一样回显读入的行，Node 不回显，故含标准输入之程序（如 回声）二者之输出必有此差异，另由壳测试 WASI 核对。
const fs=require('fs'), os=require('os'), path=require('path'), {spawnSync}=require('child_process');

if(process.argv[2]==='--参考'){
  // 子进程：node WASI对照.cjs --参考 程序.wasm 输出文件 [--程序名 名] [--实参 x]...
  const {WASI}=require('node:wasi');
  const 程序=process.argv[3]; const 出文件=process.argv[4]; const 余=process.argv.slice(5);
  let 名='/程序.wasm'; const 实参=[];
  for(let i=0;i<余.length;i++){ if(余[i]==='--实参') 实参.push(余[++i]); else if(余[i]==='--程序名') 名=余[++i]; }
  const 出=fs.openSync(出文件,'a');
  const wasi=new WASI({version:'preview1',args:[名,...实参],env:{},returnOnExit:true,stdin:0,stdout:出,stderr:出});
  const 实例=new WebAssembly.Instance(new WebAssembly.Module(fs.readFileSync(程序)),wasi.getImportObject());
  let 码;
  try{ 码=wasi.start(实例); }
  catch(e){ fs.writeSync(出,String(e&&e.stack||e)+'\n'); fs.closeSync(出); process.exit(1); }
  fs.closeSync(出); process.exit(码|0);
}

const [,, 执行器文件, 程序文件, ...选项]=process.argv;
if(!执行器文件||!程序文件){ console.error('用法：node WASI对照.cjs 执行器.wasm 程序.wasm [--实参 甲]... [--输入文件 文件] [--程序名 路径]'); process.exit(2); }
let 实参=[], 程序名='/程序.wasm', 输入=Buffer.alloc(0), 输入路径=null;
for(let i=0;i<选项.length;i++){
  if(选项[i]==='--实参') 实参.push(选项[++i]);
  else if(选项[i]==='--程序名') 程序名=选项[++i];
  else if(选项[i]==='--输入文件'){ 输入路径=选项[++i]; 输入=fs.readFileSync(输入路径); }
}

function 补内存导出(buf){
  const b=Array.from(buf); let p=8;
  const rd=()=>{let r=0,s=0;for(;;){const x=b[p++];r|=(x&127)<<s;s+=7;if(!(x&128))return r>>>0;}};
  const enc=n=>{const o=[];do{let x=n&127;n>>>=7;if(n)x|=128;o.push(x);}while(n);return o;};
  const out=b.slice(0,8);
  while(p<b.length){
    const id=b[p++]; const sz=rd(); const body=b.slice(p,p+sz); p+=sz;
    if(id===7){
      let q=0; const rd2=()=>{let r=0,s=0;for(;;){const x=body[q++];r|=(x&127)<<s;s+=7;if(!(x&128))return r>>>0;}};
      const n=rd2(); const rest=body.slice(q);
      const nm=Array.from(Buffer.from('memory'));
      const nb=[...enc(n+1),...rest,...enc(nm.length),...nm,2,0];
      out.push(7,...enc(nb.length),...nb);
    } else out.push(id,...enc(sz),...body);
  }
  return Buffer.from(out);
}

async function 执行器运行(){
  const 输出字节=[]; let 输入位=0, 空轮询=0, 滴答=0;
  const platform={
    写字节:x=>{输出字节.push(x&255);},
    退出任务:c=>{throw {退出:c}},
    块容量:()=>0,块读:()=>-1,块写:()=>-1,
    读字节:()=>{ if(输入位<输入.length){空轮询=0;return 输入[输入位++];} if(++空轮询>2000) throw {退出:201}; return -1; },
    时钟滴答:()=>{滴答+=3;return 滴答;},
    复制任务:()=>-1,等待任务:()=>-1,回收任务:()=>-1,发送消息:()=>-1,接收消息:()=>0,消息来源:()=>-1,等待消息:()=>0,任务状态:()=>-1,任务退出码:()=>-1,
    设备配置读:()=>-1,设备寄存器读:()=>-1,设备寄存器写:()=>-1,搬入静态:()=>-1,搬出静态:()=>-1};
  const {instance}=await WebAssembly.instantiate(补内存导出(fs.readFileSync(执行器文件)),{平台:platform});
  const 客体=fs.readFileSync(程序文件);
  const mem=instance.exports.memory;
  const 需=Math.ceil((0x10000+客体.length)/65536);
  if(mem.buffer.byteLength/65536<需) mem.grow(需-mem.buffer.byteLength/65536);
  const u8=new Uint8Array(mem.buffer), dv=new DataView(mem.buffer);
  u8.set(客体,0x10000); dv.setUint32(0x408,客体.length,true);
  { let p=0xC000; dv.setUint32(p,实参.length,true); p+=4;
    for(const a of 实参){ const nb=Buffer.from(a,'utf8'); dv.setUint32(p,nb.length,true); u8.set(nb,p+4); p+=4+((nb.length+3)&~3); } }
  { const nb=Buffer.from(程序名,'utf8'); dv.setUint32(0xD000,nb.length,true); u8.set(nb,0xD004); }
  u8[0x2400]=0; dv.setUint32(0x2440,0,true);
  let 码=0;
  try{ instance.exports.启动(); }
  catch(e){ if(e&&e.退出!==undefined) 码=e.退出; else throw e; }
  return {码,输出:Buffer.from(输出字节)};
}

function 参考运行(){
  const 出文件=path.join(os.tmpdir(),'WASI对照-'+process.pid+'.out');
  fs.writeFileSync(出文件,'');
  const 参数=[__filename,'--参考',程序文件,出文件,'--程序名',程序名];
  for(const a of 实参) 参数.push('--实参',a);
  const r=spawnSync(process.execPath,参数,{input:输入,env:{...process.env,NODE_NO_WARNINGS:'1'},stdio:['pipe','ignore','ignore']});
  const 输出=fs.readFileSync(出文件); fs.unlinkSync(出文件);
  return {码:r.status,输出};
}

(async()=>{
  const 得=await 执行器运行(); const 参=参考运行();
  const 参陷阱=参.输出.toString('utf8').includes('RuntimeError');
  let 同;
  if(参陷阱) 同=得.码!==0;
  else 同=得.码===参.码 && Buffer.compare(得.输出,参.输出)===0;
  if(同){ console.log(`一致：${path.basename(程序文件)} 退出码 ${得.码}${参陷阱?'（陷阱）':''}，输出 ${得.输出.length} 字节`); process.exit(0); }
  console.log(`不同：${path.basename(程序文件)} 执行器退出 ${得.码}，Node 退出 ${参.码}`);
  const 甲=得.输出.toString('utf8').split('\n'), 乙=参.输出.toString('utf8').split('\n');
  let 数=0; for(let i=0;i<Math.max(甲.length,乙.length)&&数<10;i++) if(甲[i]!==乙[i]){ console.log(`第 ${i+1} 行：执行器 ${JSON.stringify(甲[i])}；Node ${JSON.stringify(乙[i])}`); 数++; }
  process.exit(1);
})().catch(e=>{ console.error('对照脚本出错',e); process.exit(2); });
