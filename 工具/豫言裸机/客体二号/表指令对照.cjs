// 文言：表指令之对照：随机造小模块，其中行若干条表指令（table.init、elem.drop、table.copy、table.grow、table.fill、table.get/set/size），于 V8 与执行器各行之，较其果或陷阱。
// 汉语：Wasm 表指令对照测试（Node）。用法：node 表指令对照.cjs 执行器.wasm [场景数] [种子]
//   每个场景是一个随机生成的核心 Wasm 模块：两张 funcref 表、八个返回 100+k 的函数、五个元素段（主动、被动函数号、声明式、被动常量式、主动入表一），
//   导出 main：顺序执行 3–9 条随机表指令（下标与个数偶尔越界以触发陷阱），再遍历两张表用 call_indirect 累积校验和并返回。
//   同一模块先在 V8 中运行得到期望（返回值或陷阱），再放进执行器（0x10000 起，长度 0x408，导出名 0x2400）运行，比较：
//   有返回值则执行器打印的数必须相同，V8 陷阱则执行器必须以非零退出码结束。全部相符退出 0，否则打印前若干个不符并退出 1。
//   node 表指令对照.cjs --造探针 输出目录 期望文件 [种子]：改为生成 12 个不越界的场景模块（表0.wasm…表11.wasm，各 14–21 条表指令）与期望文件（每行 “序号 返回值”，取自 V8），
//   供 普通探针/盘/表-一。豫 在裸机上加载运行（实例总数至多 16，故只有 12 个）。
const fs=require('fs');
const {leb,sleb,节,造随机}=require('./浮点指令表.cjs');
const 造探针=process.argv[2]==='--造探针';
const exeFile=造探针?null:process.argv[2]; const 场景数=parseInt(process.argv[3]||'300'); const 种子=parseInt((造探针?process.argv[5]:process.argv[4])||'777')>>>0;
const {随机}=造随机(种子);
const exe=造探针?Buffer.alloc(0):fs.readFileSync(exeFile);
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
const 执行器模块=造探针?null:withMemoryExport(exe);
let 输出字节=[]; let 盘=null;
const platform={写字节:x=>{输出字节.push(x&255);},退出任务:c=>{throw {退出:c}},块容量:()=>0,块读:()=>-1,块写:()=>-1,读字节:()=>-1,时钟滴答:()=>0,复制任务:()=>-1,等待任务:()=>-1,回收任务:()=>-1,发送消息:()=>-1,接收消息:()=>0,消息来源:()=>-1,等待消息:()=>0,任务状态:()=>-1,任务退出码:()=>-1,设备配置读:()=>-1,设备寄存器读:()=>-1,设备寄存器写:()=>-1,搬入静态:()=>-1,搬出静态:()=>-1};
const 编译执行器=造探针?null:WebAssembly.compile(执行器模块);

// ---- 指令编码
const i32c=v=>[0x41,...sleb(v)];
const 取=n=>[0x20,n], 置=n=>[0x21,n];
const 引用=k=> k<0 ? [0xD0,0x70] : [0xD2,...leb(k)];      // ref.null func / ref.func k
const 表初始化=(段,表)=>[0xFC,12,...leb(段),...leb(表)];
const 丢弃段=段=>[0xFC,13,...leb(段)];
const 表复制=(目,源)=>[0xFC,14,...leb(目),...leb(源)];
const 表增长=表=>[0xFC,15,...leb(表)];
const 表填充=表=>[0xFC,17,...leb(表)];
const 表大小=表=>[0xFC,16,...leb(表)];
const 表取=表=>[0x25,...leb(表)];
const 表置=表=>[0x26,...leb(表)];

function 造场景(选项){
  const 险=选项?选项.险:随机()%5===0;                                          // 五分之一的场景允许越界参数（应陷阱）
  const 界=t=> t===0?3:2;                                          // 表 0 初始大小 6、表 1 初始大小 3；起点与个数都取小值，保证一般不越界
  const 数=(上,坏)=>{ if(险&&随机()%4===0) return [-1,上+8,100][随机()%3]; return 随机()%(上+1); };
  const 操作=[];
  const 个数=选项?选项.个数:3+随机()%7;
  for(let i=0;i<个数;i++){
    const 种=随机()%9, t=随机()%2, u=随机()%2;
    if(种===0){ const 段=(随机()%5===0)?随机()%5:(随机()%2?1:3); 操作.push([...i32c(数(界(t))),...i32c(数(1)),...i32c(数(2)),...表初始化(段,t)]); }
    else if(种===1) 操作.push(丢弃段(随机()%5===0?随机()%5:1+2*(随机()%2)));
    else if(种===2) 操作.push([...i32c(数(界(t))),...i32c(数(界(u))),...i32c(数(2)),...表复制(t,u)]);
    else if(种===3) 操作.push([...引用((随机()%3===0)?-1:随机()%8),...i32c(随机()%4),...表增长(t),...取(0),0x6A,...置(0)]);
    else if(种===4) 操作.push([...i32c(数(界(t))),...引用((随机()%3===0)?-1:随机()%8),...i32c(数(2)),...表填充(t)]);
    else if(种===5) 操作.push([...i32c(数(界(t))),...引用((随机()%3===0)?-1:随机()%8),...表置(t)]);
    else if(种===6) 操作.push([...i32c(数(界(t))),...表取(t),0xD1,...取(0),0x6A,...置(0)]);
    else if(种===7) 操作.push([...表大小(t),...取(0),0x6A,...置(0)]);
    else 操作.push([...i32c(数(界(t))),...表取(t),0x1A]);
  }
  return 操作;
}
// ---- 模块：类型 0 = ()->i32；函数 0..7 返回 100+k；函数 8 = chk；函数 9 = main
function 造模块(操作){
  const 型节=节(1,[1,0x60,0,1,0x7F]);
  const 函数节=节(3,[...leb(10),...Array(10).fill(0)]);
  const 表节=节(4,[2,0x70,1,6,12,0x70,0,3]);              // T0 最小 6 最大 12；T1 最小 3 无最大
  const 导出节=节(7,[1,4,...Buffer.from('main'),0,9]);
  const 段=(标志,内)=>[标志,...内];
  const 元素节=节(9,[5,
    // E0 主动入表 0，偏移 1：[F0,F1]
    0,...i32c(1),0x0B,2,0,1,
    // E1 被动函数号：[F2..F5]
    1,0,4,2,3,4,5,
    // E2 声明式：[F6]
    3,0,1,6,
    // E3 被动常量式：[ref.func F7, ref.null, ref.func F0]
    5,0x70,3,0xD2,7,0x0B,0xD0,0x70,0x0B,0xD2,0,0x0B,
    // E4 主动入表 1（显式表号）偏移 0：[F1]
    2,1,...i32c(0),0x0B,0,1,1]);
  const 体们=[];
  for(let k=0;k<8;k++) 体们.push([0,...i32c(100+k),0x0B]);
  // 表取 已含 table.get；call_indirect 需要索引在栈上：0x11 typeidx tableidx，栈顶为索引
  const 遍历表=表=>[
    ...i32c(0),...置(1),
    0x02,0x40,0x03,0x40,
      ...取(1),...表大小(表),0x4F,0x0D,1,
      ...取(1),...表取(表),0xD1,0x04,0x40,
        ...取(0),...i32c(1),0x6A,...置(0),
      0x05,
        ...取(1),0x11,0,...leb(表),...取(0),0x6A,...置(0),
      0x0B,
      ...取(1),...i32c(1),0x6A,...置(1),0x0C,0,
    0x0B,0x0B];
  // 局部声明：2 个 i32（累加器 acc = 局部 0，下标 i = 局部 1）；chk 返回 acc + 表0大小 + 表1大小×1000
  体们.push([1,2,0x7F,...遍历表(0),...遍历表(1),...表大小(0),...取(0),0x6A,...表大小(1),...i32c(1000),0x6C,0x6A,0x0B]);
  const 主体=[1,2,0x7F,...操作.flat(),0x10,8,...取(0),0x6A,0x0B];
  体们.push(主体);
  const 代码节=节(10,[...leb(体们.length),...体们.flatMap(b=>[...leb(b.length),...b])]);
  return Buffer.from([0,0x61,0x73,0x6d,1,0,0,0,...型节,...函数节,...表节,...导出节,...元素节,...代码节]);
}
function 期望V8(模块){
  try{ const 实例=new WebAssembly.Instance(new WebAssembly.Module(模块)); return {值:实例.exports.main()|0}; }
  catch(e){ if(e instanceof WebAssembly.RuntimeError||e instanceof WebAssembly.CompileError) return {陷阱:String(e.message)}; throw e; }
}
async function 执行器运行(模块){
  const instance=await WebAssembly.instantiate(await 编译执行器,{平台:platform});
  const mem=instance.exports.memory; 输出字节=[];
  const need=Math.ceil((0x10000+模块.length)/65536);
  if(mem.buffer.byteLength/65536<need) mem.grow(need-mem.buffer.byteLength/65536);
  new Uint8Array(mem.buffer).set(模块,0x10000);
  const dv=new DataView(mem.buffer); dv.setUint32(0x408,模块.length,true);
  const 名=Buffer.from('main'); new Uint8Array(mem.buffer).set(名,0x2400); new Uint8Array(mem.buffer)[0x2400+名.length]=0; dv.setUint32(0x2440,0,true);
  let 码=null;
  try{ instance.exports.启动(); }catch(e){ if(e&&e.退出!==undefined) 码=e.退出; else throw e; }
  return {退出码:码,输出:Buffer.from(输出字节).toString('utf8')};
}
(async()=>{
  if(process.argv[2]==='--造探针'){
    const 目录=process.argv[3], 期文件=process.argv[4]; const 行们=[];
    for(let i=0;i<12;i++){
      let 模块,期;
      do{ 模块=造模块(造场景({险:false,个数:14+随机()%8})); 期=期望V8(模块); }while(期.陷阱!==undefined);
      fs.writeFileSync(目录+'/表'+i+'.wasm',模块); 行们.push(i+' '+期.值);
    }
    fs.writeFileSync(期文件,行们.join('\n')+'\n'); console.error('已生成 12 个场景模块'); return;
  }
  let 错=0, 陷阱数=0, 值数=0;
  for(let i=0;i<场景数;i++){
    const 操作=造场景(); const 模块=造模块(操作);
    const 期=期望V8(模块); if(期.陷阱===undefined) 值数++; else 陷阱数++;
    const 得=await 执行器运行(模块);
    let 同;
    if(期.陷阱!==undefined) 同=(得.退出码!==null&&得.退出码!==0);
    else { const 行=得.输出.trim().split('\n').filter(s=>/^-?\d+$/.test(s.trim())).pop(); 同=(得.退出码===null||得.退出码===0)&&行!==undefined&&(parseInt(行)|0)===期.值; }
    if(!同){ 错++; if(错<=5){ console.log('不符 场景',i,'期',JSON.stringify(期),'得',JSON.stringify(得)); fs.writeFileSync('/tmp/表指令不符'+i+'.wasm',模块); } }
  }
  console.log(`场景 ${场景数} 个（返回值 ${值数}、陷阱 ${陷阱数}），不符 ${错} 个`);
  process.exit(错?1:0);
})().catch(e=>{ console.error('对照脚本出错',e); process.exit(2); });
