// 文言：浮点指令之对照：直召执行器所导出之 浮点运算（运算号 256 加 Wasm 操作码，或 512 加 0xFC 子操作码），与 V8 之 WebAssembly 逐位相校；陷阱亦须相合。
// 汉语：Wasm 浮点指令对照测试（Node）。用法：node 浮点指令对照.cjs 执行器.wasm [每指令轮数] [种子]
//   执行器.wasm 由 ./yy豫构 构建 客体二号 --输出 x.wasm 得到。本脚本对 f32/f64 的全部比较、算术、取整、开方、转换与饱和截断指令，
//   各造一个只含该指令的小模块在 V8 里运行作为期望，再用执行器内的 执行浮点／浮饱和截断 处理同一批随机与边界输入（f32、f64 位型，整数）比较：
//   数值结果逐位相同（两边都是非数视为相同，abs/neg/copysign 因规范规定按位操作，非数也逐位比较），陷阱（V8 抛 RuntimeError、执行器以退出码 125 结束）也要两边一致。
//   全部相符退出 0，否则打印前若干个不符并退出 1。环境变量 FP_DEBUG=1 时每条指令打印前两组输入与两边的结果。
// 文言：诸运算之栈置于 0x2030 起之测试栈；结果自 0x2590 读之（浮槽_果）；格式格在 0xE30。
// 汉语：参数为（运算号，甲低，甲高，乙低，乙高）；结果读 浮槽_果（9616）；每次陷阱后要把 浮格_格式（3632）清零，否则下一次会沿用单精度格式。
const fs=require('fs');
const {T,表,leb,节,是非数64,是非数32,造随机}=require('./浮点指令表.cjs');
const exeFile=process.argv[2]; const 轮数=parseInt(process.argv[3]||'20000'); const 种子=parseInt(process.argv[4]||'424242')>>>0;
const {生成}=造随机(种子);
const exe=fs.readFileSync(exeFile);
function withMemoryExport(buf){
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
const platform={写字节:()=>{},退出任务:c=>{throw {退出:c}},块容量:()=>0,块读:()=>-1,块写:()=>-1,读字节:()=>-1,时钟滴答:()=>0,复制任务:()=>-1,等待任务:()=>-1,回收任务:()=>-1,发送消息:()=>-1,接收消息:()=>0,消息来源:()=>-1,等待消息:()=>0,任务状态:()=>-1,任务退出码:()=>-1,设备配置读:()=>-1,设备寄存器读:()=>-1,设备寄存器写:()=>-1,搬入静态:()=>-1,搬出静态:()=>-1};

// ---- V8 参考模块：参数一律用 i32/i64 位型，内部再按位重解释，避免 JS 边界改动非数载荷
function 造参考(操作码,入,出){
  const 参型=入.map(t=> t==='f32'?T.i32: t==='f64'?T.i64: T[t]);
  const 果型= 出==='f32'?T.i32: 出==='f64'?T.i64: T[出];
  const 码=[];
  入.forEach((t,i)=>{ 码.push(0x20,i); if(t==='f32')码.push(0xBE); if(t==='f64')码.push(0xBF); });
  if(操作码>=0xFC00) 码.push(0xFC,...leb(操作码&0xFF)); else 码.push(操作码);
  if(出==='f32')码.push(0xBC); if(出==='f64')码.push(0xBD);
  码.push(0x0B);
  const 体=[0,...码];
  const 字节=[0,0x61,0x73,0x6d,1,0,0,0,
    ...节(1,[1,0x60,参型.length,...参型,1,果型]),
    ...节(3,[1,0]),
    ...节(7,[1,1,0x66,0,0]),
    ...节(10,[1,...leb(体.length),...体])];
  return new WebAssembly.Instance(new WebAssembly.Module(new Uint8Array(字节))).exports.f;
}
function 规范(出,值,按位){
  if(出==='f32'){ if(!按位&&是非数32(值)) return 'NaN'; return 值.toString(16); }
  if(出==='f64'){ if(!按位&&是非数64(值)) return 'NaN'; return 值.toString(16); }
  return 值.toString(16);
}
WebAssembly.instantiate(withMemoryExport(exe),{平台:platform}).then(({instance})=>{
  const mem=instance.exports.memory; const 运算=instance.exports.浮点运算;
  const 内存=()=>new DataView(mem.buffer);
  const 甲低=v=>Number(v&0xFFFFFFFFn)|0, 甲高=v=>Number((v>>32n)&0xFFFFFFFFn)|0;
  function 调执行器(号,值们){
    const a=值们[0]??0n, b=值们[1]??0n;
    内存().setUint32(3632,0,true);
    try{ 运算(号,甲低(a),甲高(a),甲低(b),甲高(b)); }
    catch(e){ 内存().setUint32(3632,0,true); if(e&&e.退出!==undefined) return {陷阱:e.退出}; throw e; }
    return {值:内存().getBigUint64(9616,true)};
  }
  let 错=0, 总=0; const 各错={};
  for(const [操作码,名,入,出,按位] of 表){
    const 参考=造参考(操作码,入,出); const 号=(操作码>=0xFC00)?512+(操作码&0xFF):256+操作码;
    let 本错=0;
    for(let i=0;i<轮数;i++){
      const 值们=入.map(t=>生成[t]());
      let 期;
      try{ const 参=值们.map((v,j)=> (入[j]==='i32'||入[j]==='f32')?Number(BigInt.asIntN(32,v)):BigInt.asIntN(64,v));
           const r=参考(...参); 期={值:(出==='i64'||出==='f64')?BigInt.asUintN(64,BigInt(r)):BigInt.asUintN(32,BigInt(r))}; }
      catch(e){ if(e instanceof WebAssembly.RuntimeError) 期={陷阱:true}; else throw e; }
      const 得=调执行器(号,值们); 总++;
      if(process.env.FP_DEBUG&&i<2) console.log(名,值们.map(v=>'0x'+v.toString(16)).join(','),'得',得.陷阱!==undefined?'陷阱'+得.陷阱:'0x'+得.值.toString(16),'期',期.陷阱?'陷阱':'0x'+期.值.toString(16));
      let 同;
      if(期.陷阱) 同=(得.陷阱===125);
      else if(得.陷阱!==undefined) 同=false;
      else {
        const 宽=(出==='i64'||出==='f64'); const 得值=宽?得.值:(得.值&0xFFFFFFFFn);
        同=(规范(出,得值,按位)===规范(出,期.值,按位));
      }
      if(!同){ 本错++; 错++; if(本错<=3) console.log('不符',名,'入',值们.map(v=>'0x'+v.toString(16)).join(','),'得',得.陷阱!==undefined?'陷阱'+得.陷阱:'0x'+得.值.toString(16),'期',期.陷阱?'陷阱':'0x'+期.值.toString(16)); }
    }
    各错[名]=本错;
  }
  const 有错=Object.entries(各错).filter(([,n])=>n>0);
  console.log(`指令 ${表.length} 条，共 ${总} 次比较，不符 ${错} 次`+(有错.length?'；不符指令：'+有错.map(([k,n])=>k+'×'+n).join(' '):''));
  process.exit(错?1:0);
}).catch(e=>{ console.error('对照脚本出错',e); process.exit(2); });
