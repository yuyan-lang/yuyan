// 文言：浮点指令之表：Wasm 之 f32、f64 诸指令，其操作码、名、入型、出型及诸性；对照与造模块二器共用之。
// 汉语：Wasm 浮点指令表（Node 测试工具共用）。每项 [操作码, 名, 入型们, 出型, 按位比较非数]：
//   操作码 0xFC00+n 表示 0xFC 前缀的子操作码 n（饱和截断）；入型/出型取 f32 f64 i32 i64；
//   最后一项为真者（abs、neg、copysign）规范规定只动符号位，非数载荷也须逐位相同，其余指令的非数结果只要求“都是非数”。
const T={f32:0x7D,f64:0x7C,i32:0x7F,i64:0x7E};
const 表=[];
const 比较名=['eq','ne','lt','gt','le','ge'];
for(let i=0;i<6;i++) 表.push([0x5B+i,'f32.'+比较名[i],['f32','f32'],'i32',false]);
for(let i=0;i<6;i++) 表.push([0x61+i,'f64.'+比较名[i],['f64','f64'],'i32',false]);
const 一元名=['abs','neg','ceil','floor','trunc','nearest','sqrt'], 二元名=['add','sub','mul','div','min','max','copysign'];
for(const [基,型] of [[0x8B,'f32'],[0x99,'f64']]){
  for(let i=0;i<7;i++) 表.push([基+i,型+'.'+一元名[i],[型],型,i<2]);
  for(let i=0;i<7;i++) 表.push([基+7+i,型+'.'+二元名[i],[型,型],型,i===6]);
}
表.push([0xA8,'i32.trunc_f32_s',['f32'],'i32',false],[0xA9,'i32.trunc_f32_u',['f32'],'i32',false],[0xAA,'i32.trunc_f64_s',['f64'],'i32',false],[0xAB,'i32.trunc_f64_u',['f64'],'i32',false]);
表.push([0xAE,'i64.trunc_f32_s',['f32'],'i64',false],[0xAF,'i64.trunc_f32_u',['f32'],'i64',false],[0xB0,'i64.trunc_f64_s',['f64'],'i64',false],[0xB1,'i64.trunc_f64_u',['f64'],'i64',false]);
表.push([0xB2,'f32.convert_i32_s',['i32'],'f32',false],[0xB3,'f32.convert_i32_u',['i32'],'f32',false],[0xB4,'f32.convert_i64_s',['i64'],'f32',false],[0xB5,'f32.convert_i64_u',['i64'],'f32',false],[0xB6,'f32.demote_f64',['f64'],'f32',false]);
表.push([0xB7,'f64.convert_i32_s',['i32'],'f64',false],[0xB8,'f64.convert_i32_u',['i32'],'f64',false],[0xB9,'f64.convert_i64_s',['i64'],'f64',false],[0xBA,'f64.convert_i64_u',['i64'],'f64',false],[0xBB,'f64.promote_f32',['f32'],'f64',false]);
const 饱和名=['i32.trunc_sat_f32_s','i32.trunc_sat_f32_u','i32.trunc_sat_f64_s','i32.trunc_sat_f64_u','i64.trunc_sat_f32_s','i64.trunc_sat_f32_u','i64.trunc_sat_f64_s','i64.trunc_sat_f64_u'];
for(let i=0;i<8;i++) 表.push([0xFC00+i,饱和名[i],[(i&2)?'f64':'f32'],(i&4)?'i64':'i32',false]);

function leb(n){ const o=[]; do{ let x=n&127; n>>>=7; if(n)x|=128; o.push(x);}while(n); return o; }
function sleb(v){ v=BigInt(v); const o=[]; for(;;){ let x=Number(v&0x7Fn); v>>=7n; if((v===0n&&!(x&0x40))||(v===-1n&&(x&0x40))){ o.push(x); return o; } o.push(x|0x80); } }
const 节=(id,内)=>[id,...leb(内.length),...内];

const buf8=new ArrayBuffer(8), dv=new DataView(buf8);
const f64位=d=>{ dv.setFloat64(0,d,true); return dv.getBigUint64(0,true); };
const 位f64=x=>{ dv.setBigUint64(0,BigInt.asUintN(64,x),true); return dv.getFloat64(0,true); };
const f32位=d=>{ dv.setFloat32(0,d,true); return BigInt(dv.getUint32(0,true)); };
const 位f32=x=>{ dv.setUint32(0,Number(BigInt.asUintN(32,x)),true); return dv.getFloat32(0,true); };
const 是非数64=x=>{ const e=(x>>52n)&0x7FFn; return e===0x7FFn && (x&0xFFFFFFFFFFFFFn)!==0n; };
const 是非数32=x=>{ const e=(x>>23n)&0xFFn; return e===0xFFn && (x&0x7FFFFFn)!==0n; };

// 文言：随机数与输入之池；对照与造模块共用。
// 汉语：xorshift32 随机数、边界值池与各类型的输入生成器（f32、f64 位型，i32、i64）。
function 造随机(种子){
  let 种=种子>>>0;
  const 随机=()=>{ 种^=种<<13; 种>>>=0; 种^=种>>>17; 种^=种<<5; 种>>>=0; return 种; };
  const 随机64=()=>(BigInt(随机())<<32n)|BigInt(随机());
  const 池64=[0,-0,1,-1,0.5,-0.5,1.5,-1.5,2.5,-2.5,3.5,0.49999999999999994,0.5000000000000001,4503599627370495.5,4503599627370496,4503599627370497,9007199254740993,
    2147483647,2147483648,2147483647.5,-2147483648,-2147483648.5,-2147483649,4294967295,4294967295.5,4294967296,-4294967296,9223372036854775807,9223372036854775808,-9223372036854775808,-9223372036854777856,
    1.8446744073709552e19,1.8446744073709550e19,1e300,-1e300,5e-324,-5e-324,2.2250738585072014e-308,1.7976931348623157e308,Infinity,-Infinity,NaN,0.1,0.2,0.3,100.5,-100.5,1e10,1e-10,
    3.4028234663852886e38,3.4028235677973366e38,1.401298464324817e-45,7.006492321624085e-46,1.1754943508222875e-38,16777216,16777217,8388608.5,255.5,65535.5];
  function 生成64(){
    const k=随机()%12;
    if(k<2) return f64位(池64[随机()%池64.length]);
    if(k===2) return 随机64();
    if(k===3){ const e=BigInt(随机()%2047)<<52n; return e|(随机64()&0xFFFFFFFFFFFFFn)|((随机()&1)?(1n<<63n):0n); }
    if(k===4){ const e=BigInt(1023+(随机()%140)-20)<<52n; return e|(随机64()&0xFFFFFFFFFFFFFn)|((随机()&1)?(1n<<63n):0n); }
    if(k===5){ const 整=Number(随机64()>>BigInt(随机()%60+4)); return f64位((随机()&1?-1:1)*(整+[0,0.5,0.25,0.75,0.125][随机()%5])); }
    if(k===6){ const 整=随机()%2000-1000; return f64位(整+[0,0.5,-0.5,0.25,0.75][随机()%5]); }
    if(k===7){ const 底=[2147483648,4294967296,9223372036854775808,1.8446744073709552e19,2147483648.5,4294967295.5][随机()%6]; const 位=f64位((随机()&1?-1:1)*底); return 位+BigInt((随机()%5)-2); }
    if(k===8) return f64位(Number(BigInt.asIntN(64,随机64())>>BigInt(随机()%64)));
    if(k===9){ const e=BigInt(随机()%60)<<52n; return e|(随机64()&0xFFFFFFFFFFFFFn); }
    if(k===10) return f64位((随机()/4294967296-0.5)*Math.pow(2,随机()%80-20));
    return f64位(位f32(BigInt(随机())));
  }
  function 生成32(){
    const k=随机()%10;
    if(k<2){ return f32位(池64[随机()%池64.length]); }
    if(k===2) return BigInt(随机());
    if(k===3){ const e=BigInt(随机()%255)<<23n; return e|BigInt(随机()&0x7FFFFF)|((随机()&1)?(1n<<31n):0n); }
    if(k===4){ const e=BigInt(127+(随机()%60)-10)<<23n; return e|BigInt(随机()&0x7FFFFF)|((随机()&1)?(1n<<31n):0n); }
    if(k===5){ const 整=Number(随机64()>>BigInt(随机()%40+24)); return f32位((随机()&1?-1:1)*(整+[0,0.5,0.25,0.75][随机()%4])); }
    if(k===6){ const 整=随机()%2000-1000; return f32位(整+[0,0.5,-0.5,0.25,0.75][随机()%4]); }
    if(k===7){ const 底=[2147483648,4294967296,9223372036854775808,1.8446744073709552e19,16777216,8388608][随机()%6]; return f32位((随机()&1?-1:1)*底)+BigInt((随机()%5)-2); }
    if(k===8){ const e=BigInt(随机()%30)<<23n; return e|BigInt(随机()&0x7FFFFF); }
    return f32位((随机()/4294967296-0.5)*Math.pow(2,随机()%60-20));
  }
  function 生成i32(){ const k=随机()%6; if(k===0) return BigInt(随机()); if(k===1) return BigInt([0,1,-1,2147483647,-2147483648,16777216,16777217,16777219,33554431,33554433,0x7FFFFFFF,0x80000000,0xFFFFFFFF,0x01000001][随机()%14]>>>0);
    if(k===2) return BigInt((随机()>>>(随机()%32))>>>0); if(k===3) return BigInt(((随机()&0xFF)<<(随机()%25))>>>0); return BigInt((随机()|0)>>(随机()%32))&0xFFFFFFFFn; }
  function 生成i64(){ const k=随机()%7; if(k===0) return 随机64(); if(k===1) return BigInt.asUintN(64,[0n,1n,-1n,9223372036854775807n,-9223372036854775808n,9007199254740993n,9007199254740992n,72057594037927937n,0x7FFFFFFFFFFFFFFFn,0x8000000000000001n,0xFFFFFFFFFFFFFFFFn,1n<<53n|1n,(1n<<24n)+1n,(1n<<62n)+(1n<<38n)][随机()%14]);
    if(k===2) return 随机64()>>BigInt(随机()%64); if(k===3) return BigInt.asUintN(64,BigInt.asIntN(64,随机64())>>BigInt(随机()%64));
    if(k===4){ const 高=随机(); return BigInt(高)<<32n|BigInt(随机()&(随机()&1?0xFFFFFFFF:0)); }
    if(k===5){ const 位=BigInt(随机()%64); return BigInt.asUintN(64,(1n<<位)+BigInt((随机()%9)-4)); } return 随机64()&(随机64()|随机64()); }
  return {随机,随机64,生成:{f32:生成32,f64:生成64,i32:生成i32,i64:生成i64}};
}
module.exports={T,表,leb,sleb,节,f64位,位f64,f32位,位f32,是非数64,是非数32,造随机};
