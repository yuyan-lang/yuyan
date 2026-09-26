// 文言：造浮点之客模块：每条浮点指令一导出函数 op0…，遍历四张输入表（f64、i64、f32、i32 之位型各一千二十四项），以 FNV 式累积各果之位；并以 V8 运行之，得期望之校验和。
// 汉语：生成浮点指令的端到端测试模块（Node）。用法：node 造浮点模块.cjs 输出.wasm 期望.txt
//   模块导出 op0…op65（次序同 浮点指令表.cjs 的表），签名 (n:i32)->i64：对输入表前 n 项逐项把指令作用于表里的位型（第二操作数取表中另一项），
//   把结果位型（非数的载荷除 abs/neg/copysign 外一律规范化）累积进 acc = (acc ^ 位) × 0x100000001B3，返回 acc。
//   会陷阱的截断指令（非饱和）先把输入清理成范围内的值（非数→0，再夹在最小最大之间）再截断，这样不会陷阱、又用到 min/max/select/ne。
//   期望文件每行 “序号 校验和”（校验和取有符号十进制，n = 1024），由本脚本在 V8 中运行同一模块得到；普通探针 浮点-一 在裸机上加载同一模块运行，逐行比对。
//   用 node 造浮点模块.cjs --名单 可列出序号与指令名。
const fs=require('fs');
const {T,表,leb,sleb,节,f64位,f32位,造随机}=require('./浮点指令表.cjs');
if(process.argv[2]==='--名单'){ 表.forEach((项,k)=>console.log(k,项[1])); process.exit(0); }
const 输出=process.argv[2], 期望=process.argv[3];
const N=1024;
// ---- 输入表
const {生成}=造随机(20260926);
const 表基={f64:0,i64:8192,f32:16384,i32:20480};
const 数据=Buffer.alloc(24576);
for(let i=0;i<N;i++){
  数据.writeBigUInt64LE(BigInt.asUintN(64,生成.f64()),表基.f64+i*8);
  数据.writeBigUInt64LE(BigInt.asUintN(64,生成.i64()),表基.i64+i*8);
  数据.writeUInt32LE(Number(BigInt.asUintN(32,生成.f32())),表基.f32+i*4);
  数据.writeUInt32LE(Number(BigInt.asUintN(32,生成.i32())),表基.i32+i*4);
}
// ---- 码
const i32c=v=>[0x41,...sleb(v)], i64c=v=>[0x42,...sleb(v)];
const f32c=v=>{ const b=Buffer.alloc(4); b.writeFloatLE(v); return [0x43,...b]; };
const f64c=v=>{ const b=Buffer.alloc(8); b.writeDoubleLE(v); return [0x44,...b]; };
const 取=n=>[0x20,n], 置=n=>[0x21,n], 存=n=>[0x22,n];
function 装入(型,号){
  const 索引= 号===0 ? 取(1) : [...取(1),...i32c(5),0x6C,...i32c(3),0x6A,...i32c(1023),0x71];
  const 宽=(型==='f64'||型==='i64');
  const 码=[...索引,...i32c(宽?3:2),0x74];
  if(宽) 码.push(0x29,0x03,...leb(表基[型])); else 码.push(0x28,0x02,...leb(表基[型]));
  if(型==='f64') 码.push(0xBF); if(型==='f32') 码.push(0xBE);
  return 码;
}
// 会陷阱之截断：范围（含端点）取该指令可无陷阱截断之最大、最小浮点值
const 范围={
  0xAA:[-2147483648,2147483647],0xAB:[0,4294967295],0xB0:[-9223372036854775808,9223372036854774784],0xB1:[0,18446744073709549568],
  0xA8:[-2147483648,2147483520],0xA9:[0,4294967040],0xAE:[-9223372036854775808,9223371487098961920],0xAF:[0,18446742974197923840]};
const 会陷阱=码=>码 in 范围;
function 清理(操作码,入型){
  const [低,高]=范围[操作码];
  if(入型==='f64') return [...置(4),...f64c(0),...取(4),...取(4),...取(4),0x62,0x1B,...f64c(低),0xA5,...f64c(高),0xA4];
  return [...置(5),...f32c(0),...取(5),...取(5),...取(5),0x5C,0x1B,...f32c(低),0x97,...f32c(高),0x96];
}
function 造函数(项){
  const [操作码,名,入,出,按位]=项;
  const 算=[];
  入.forEach((t,j)=>{ 算.push(...装入(t,j)); if(会陷阱(操作码)) 算.push(...清理(操作码,t)); });
  if(操作码>=0xFC00) 算.push(0xFC,...leb(操作码&0xFF)); else 算.push(操作码);
  if(出==='f64') 算.push(0xBD);
  if(出==='f32') 算.push(0xBC,0xAD);
  if(出==='i32') 算.push(0xAD);
  const 规范化=[];
  if((出==='f64'||出==='f32')&&!按位){
    规范化.push(...存(3));
    if(出==='f64') 规范化.push(...i64c(0x7FFFFFFFFFFFFFFFn),0x83,...i64c(0x7FF0000000000000n),0x56,0x04,0x7E,...i64c(0x7FF8000000000000n),0x05,...取(3),0x0B);
    else 规范化.push(...i64c(0x7FFFFFFFn),0x83,...i64c(0x7F800000n),0x56,0x04,0x7E,...i64c(0x7FC00000n),0x05,...取(3),0x0B);
  }
  const 体=[
    ...i64c(BigInt.asIntN(64,0xCBF29CE484222325n)),...置(2),
    0x02,0x40,0x03,0x40,
      ...取(1),...取(0),0x4F,0x0D,1,
      ...算,...规范化,...置(3),
      ...取(2),...取(3),0x85,...i64c(0x100000001B3n),0x7E,...置(2),
      ...取(1),...i32c(1),0x6A,...置(1),
      0x0C,0,
    0x0B,0x0B,
    ...取(2),0x0B];
  const 局部=[1,T.i32,2,T.i64,1,T.f64,1,T.f32];
  return [...leb(局部.length/2),...局部,...体];
}
const 函数体们=表.map(造函数);
const 导出=表.map((_,k)=>{ const 名=Buffer.from('op'+k); return [名.length,...名,0,k]; });
const 字节=[0,0x61,0x73,0x6d,1,0,0,0,
  ...节(1,[1,0x60,1,T.i32,1,T.i64]),
  ...节(3,[...leb(表.length),...表.map(()=>0)]),
  ...节(5,[1,0,1]),
  ...节(7,[...leb(表.length),...导出.flat()]),
  ...节(10,[...leb(表.length),...函数体们.flatMap(b=>[...leb(b.length),...b])]),
  ...节(11,[1,0,0x41,0,0x0B,...leb(数据.length),...数据])];
const 模块=Buffer.from(字节);
// ---- 在 V8 中运行，取期望
const 实例=new WebAssembly.Instance(new WebAssembly.Module(模块));
const 行=表.map((项,k)=>k+' '+BigInt.asIntN(64,实例.exports['op'+k](N)).toString());
if(输出) fs.writeFileSync(输出,模块);
if(期望) fs.writeFileSync(期望,行.join('\n')+'\n');
if(!输出) console.log(行.join('\n'));
console.error(`模块 ${模块.length} 字节，${表.length} 个导出`);
