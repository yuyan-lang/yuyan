// 文言：软浮点之对照：直召执行器所导出之 浮点运算，与 V8 之硬件双精度随机相校，兼校已知之舍入难例。
// 汉语：软浮点对照测试（Node）。用法：node 软浮点对照.cjs 执行器.wasm [轮数] [种子]
//   执行器.wasm 由 ./yy豫构 构建 客体二号 --输出 x.wasm 得到；本脚本直接调用它导出的 浮点运算（不经客体模块），
//   对每轮随机双精度做加、减、乘、除、整数转浮点、浮点转整数、定点六位串、字符串转小数，与 V8 结果逐位比较（非数视为相同），
//   再校一批已知的十进制转二进制难例（2.2250738585072011e-308、9007199254740993、1e309 等）。全部相符退出 0，否则打印前若干个不符并退出 1。
// 文言：诸运算于内存暂存区（0x2580 起）中间行之；结果自 0x2590 读之，定点串自 0x2A20 读之；字符串置于 0x20000。
// 汉语：浮点运算导出的参数为（运算号，甲低，甲高，乙低，乙高），运算号 0 加、1 减、2 乘、3 除、4 整数转浮点、5 浮点转整数、6 定点串（长度在结果低字）、7 字符串转小数（甲低为串址、乙低为长度）。
const fs=require('fs');
const exeFile=process.argv[2]; const 轮数=parseInt(process.argv[3]||'200000'); let 种=parseInt(process.argv[4]||'12345')>>>0;
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
const platform={写字节:()=>{},退出任务:c=>{throw {退出:c}},块容量:()=>0,块读:()=>-1,块写:()=>-1,读字节:()=>-1,时钟滴答:()=>0,复制任务:()=>-1,等待任务:()=>-1,回收任务:()=>-1,设备配置读:()=>-1,设备寄存器读:()=>-1,设备寄存器写:()=>-1,搬入静态:()=>-1,搬出静态:()=>-1};
function 随机(){ 种=(Math.imul(种,1664525)+1013904223)>>>0; return 种; }
function 随机64(){ return (BigInt(随机())<<32n)|BigInt(随机()); }
const buf8=new ArrayBuffer(8), dv=new DataView(buf8);
function 位到数(x){ dv.setBigUint64(0,x,true); return dv.getFloat64(0,true); }
function 数到位(d){ dv.setFloat64(0,d,true); return dv.getBigUint64(0,true); }
function 生成(){
  const 种类=随机()%10;
  if(种类===0) return 随机64();                               // 任意位型（含非数、无穷、次正规）
  if(种类===1) return 数到位((随机()%2001-1000)/((随机()%1000)+1));  // 小有理数
  if(种类===2) return 数到位(Number((随机64()>>11n))*(2**(随机()%200-100)));  // 大小混合
  if(种类===3) return 数到位([0,-0,1,-1,0.5,2,1e308,-1e308,5e-324,-5e-324,2.2250738585072014e-308,Infinity,-Infinity,NaN,1.7976931348623157e308,4.9e-324,3,0.1,0.2,0.3][随机()%20]);
  if(种类===4) { const e=BigInt(1000+随机()%50)<<52n; return e|(随机64()&0xFFFFFFFFFFFFFn); }  // 近上限
  if(种类===5) { const e=BigInt(随机()%50)<<52n; return e|(随机64()&0xFFFFFFFFFFFFFn); }         // 次正规与近下限
  if(种类===6) { const e=BigInt(1023+随机()%80-40)<<52n; return e|(随机64()&0xFFFFFFFFFFFFFn)|((随机()%2)?(1n<<63n):0n); } // 近 1
  if(种类===7) return 数到位((随机()%100000-50000)/Math.pow(2,随机()%40));   // 二进制小数，多平局
  if(种类===8) return 数到位(Number(随机64()>>BigInt(11+随机()%40))/Math.pow(2,随机()%60));
  return 数到位((随机()/4294967296-0.5)*Math.pow(10,随机()%20-5));
}
WebAssembly.instantiate(withMemoryExport(exe),{平台:platform}).then(({instance})=>{
  const mem=instance.exports.memory; const 运算=instance.exports.浮点运算;
  const 内存=()=>new DataView(mem.buffer);
  function 调(op,a,b){ 运算(op,Number(a&0xFFFFFFFFn)|0,Number(a>>32n)|0,Number(b&0xFFFFFFFFn)|0,Number(b>>32n)|0); return 内存().getBigUint64(9616,true); }
  function 同(得,期){ // 位相同，非数视为同
    const dg=位到数(得), dq=位到数(期);
    if(Number.isNaN(dg)&&Number.isNaN(dq)) return true;
    return 得===期;
  }
  const 名=['加','减','乘','除'];
  let 错=0; const 计=[0,0,0,0,0,0,0,0]; let 慢差=0;
  for(let i=0;i<轮数;i++){
    const a=生成(), b=生成(); const da=位到数(a), db=位到数(b);
    const 期=[da+db,da-db,da*db,da/db];
    for(let op=0;op<4;op++){
      const 得=调(op,a,b); const 期位=数到位(期[op]); 计[op]++;
      if(!同(得,期位)){ if(错++<20) console.log('不符',名[op],da,db,'得',位到数(得),'期',期[op],a.toString(16),b.toString(16),得.toString(16),期位.toString(16)); }
    }
    // 整数转浮点
    const 整=BigInt.asIntN(64, (随机()%3===0)?BigInt.asIntN(64,随机64())>>BigInt(随机()%64):随机64()>>BigInt(随机()%64));
    { const 得=调(4,BigInt.asUintN(64,整),0n); const 期=数到位(Number(整)); 计[4]++;
      if(!同(得,期)){ if(错++<20) console.log('不符 整转浮',整,'得',位到数(得),'期',Number(整)); } }
    // 定点六位
    { 调(6,a,0n); const 长=Number(内存().getBigUint64(9616,true)&0xFFFFFFFFn); const 串=Buffer.from(new Uint8Array(mem.buffer,10784,长)).toString(); let 期; const x=da; if(Number.isNaN(x)) 期="NaN"; else if(x===Infinity) 期="Infinity"; else if(x===-Infinity) 期="-Infinity"; else if(Math.abs(x)>=1e21) 期=null; else 期=x.toFixed(6); 计[6]++; if(期!==null&&串!==期){ if(错++<20) console.log("不符 定点",x,"得",串,"期",期); } }
    // 字符串转小数
    { const 位数=1+随机()%25; let 数字=""; for(let j=0;j<位数;j++) 数字+=String(随机()%10); if(随机()%4===0) 数字="0".repeat(随机()%5)+数字; const 点=随机()%3===0?随机()%(数字.length+1):-1; let 文=(随机()%2?"-":"")+(点>=0?数字.slice(0,点)+"."+数字.slice(点):数字); const 指选=随机()%5; if(指选===0) 文+="e"+String(随机()%61-30); else if(指选===1) 文+="E"+String(随机()%700-350); else if(指选===2) 文+="e+"+String(随机()%40); if(随机()%6===0) 文="  "+文; const 字节=Buffer.from(文); new Uint8Array(mem.buffer).set(字节,0x20000); 运算(7,0x20000,0,字节.length,0); const 得=内存().getBigUint64(9616,true); const 期=数到位(Number(文.trim())); 计[7]++; if(!同(得,期)){ 慢差++; if(慢差<=10) console.log("析文不符",JSON.stringify(文),"得",位到数(得),"期",Number(文.trim()),得.toString(16),期.toString(16)); } }
    // 浮点转整数
    { const 得=BigInt.asIntN(64,调(5,a,0n)); let 期; const t=Math.trunc(da);
      if(Number.isNaN(da)) 期=0n; else if(!Number.isFinite(t)||t>=9223372036854775808) 期=t>0?9223372036854775807n:-9223372036854775808n; else if(t<=-9223372036854775808) 期=-9223372036854775808n; else 期=BigInt(t);
      计[5]++;
      if(得!==期){ if(错++<20) console.log('不符 浮转整',da,'得',得,'期',期); } }
  }
  // 文言：已知之舍入难例与边界写法。汉语：已知的十进制转二进制难例与各种边界写法（期望值取 strtod 风格的最长合法前缀）。
  const 例=['2.2250738585072011e-308','2.2250738585072012e-308','2.2250738585072014e-308','4.9406564584124654e-324','2.4703282292062327e-324','2.4703282292062328e-324','1.7976931348623157e308','1.7976931348623158e308','1.7976931348623159e308','1e309','9007199254740993','9007199254740992.5','9007199254740995','0.1','0.3','123456789012345678901234567890','1e-400','1e-323','5e-324','3.14159265358979323846264338327950288','0.'+'0'.repeat(300)+'1','1'+'0'.repeat(300),'1e22','1e23','8.41e21','0.000001','1.0000000000000002','1.00000000000000011102230246251565404236316680908203125','1.00000000000000011102230246251565404236316680908203126','.5','5.','-0','+7','1e','1e+','1ex','abc','  12abc','0x10','inf','-Infinity','nan','1'+'0'.repeat(600)+'e-600','123.456e-2','1234567890123456789.0123456789','0.30000000000000004'];
  for(const 文 of 例){
    const 字节=Buffer.from(文); new Uint8Array(mem.buffer).set(字节,0x20000); 运算(7,0x20000,0,字节.length,0);
    const 得=内存().getBigUint64(9616,true);
    let 期; const m=文.trimStart().match(/^[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?/);
    if(m) 期=Number(m[0]); else { const t=文.trimStart().toLowerCase(); const sm=t.match(/^([+-]?)(inf|nan)/); if(sm) 期=sm[2]==='inf'?(sm[1]==='-'?-Infinity:Infinity):NaN; else 期=0; }
    if(!同(得,数到位(期))){ 慢差++; console.log('定例不符',JSON.stringify(文.length>60?文.slice(0,60)+'…':文),'得',位到数(得),'期',期); }
  }
  console.log("轮数",轮数,"各项次数",计.join(","),"错误",错,"析文不符",慢差);
  process.exit((错||慢差)?1:0);
});
