// 文言：豫言为入口，此层唯接 Node 与客值。汉语：Node 引擎及系统接口适配，不执行编译器算法。
'use strict';
const 文件 = require('node:fs'), 路径 = require('node:path'), 系统 = require('node:os');
const 子进程 = require('node:child_process');
const {Worker, isMainThread, workerData} = require('node:worker_threads');
if (isMainThread) {
  const 工 = new Worker(__filename, {workerData: process.argv.slice(2), resourceLimits:{stackSizeMb:128}});
  工.on('error', 错 => {console.error(错); process.exitCode=1;});
  工.on('exit', 码 => {process.exitCode=码;});
} else {
  try {
    let 缓存=null;
    const 轮数=Number(process.env.YY_NODE_REPEAT??1);
    if(!Number.isInteger(轮数)||轮数<1||轮数>5)throw Error('重复次数须为一至五');
    for(let 轮=1;轮<=轮数;轮++){
      const 参数=[...workerData];
      if(轮>1){const 当前=new Date();文件.utimesSync(参数[0],当前,当前);const 位=参数.indexOf('-o');if(位<0)throw Error('重复验证须指定 -o');参数[位+1]=参数[位+1]+'.第'+轮+'.wasm';}
      缓存=执行(参数,缓存,轮);
    }
  } catch (错) { console.error(错); process.exitCode=1; }
}
function 执行(参数, 缓存, 轮) {
  const 开始=performance.now();
  const 模块路径=路径.resolve(参数[0]), 客参数=参数.slice(1);
  const 桥=new WebAssembly.Instance(new WebAssembly.Module(文件.readFileSync('yy节点值桥接.wasm'))).exports;
  const 模块=缓存??new WebAssembly.Module(文件.readFileSync(模块路径));
  const 编译毕=performance.now();
  function 留字节(数) { const 差=数-桥.memory.buffer.byteLength; if(差>0) 桥.memory.grow(Math.ceil(差/65536)); }
  function 解(值) {
    switch(桥.kind(值)) {
      case 0:return null;
      case 1:case 4:return 桥.int(值);
      case 5:return {小数:桥.float(值)};
      case 2:{const 长=桥.bytes_len(值);留字节(长);桥.bytes_out(值);return Buffer.from(new Uint8Array(桥.memory.buffer,0,长));}
      case 3:return Array.from({length:桥.tuple_len(值)},(_,序)=>解(桥.tuple_get(值,序)));
      default:throw Error('未知客值');
    }
  }
  function 编(值) {
    if(值==null)return null;
    if(typeof 值==='boolean')return 桥.new_int(值?1n:0n);
    if(typeof 值==='bigint'||typeof 值==='number')return 桥.new_int(BigInt(值));
    if(typeof 值==='string')值=Buffer.from(值);
    if(Buffer.isBuffer(值)){留字节(值.length);new Uint8Array(桥.memory.buffer,0,值.length).set(值);return 桥.bytes_in(值.length);}
    if(Array.isArray(值)){const 组=桥.new_tuple(值.length);值.forEach((项,序)=>桥.tuple_set(组,序,编(项)));return 组;}
    if(Object.hasOwn(值,'小数'))return 桥.new_float(值.小数);
    throw Error('未知宿主值');
  }
  const 文=值=>Buffer.isBuffer(值)?值.toString('utf8'):String(值);
  const 数=值=>Number(值?.小数??值);
  const 列=诸值=>[诸值,诸值.length];
  const 可执行=名=>{try{文件.accessSync(名,文件.constants.X_OK);return 文件.statSync(名).isFile();}catch{return false;}};
  function 精确小数(值){const 数字=数(值);if(Object.is(数字,-0))return '-0';if(!Number.isFinite(数字))return String(数字).toLowerCase().replace('infinity','inf');const [尾,指数]=数字.toExponential(16).split('e');const 幂=Number(指数);if(幂 < -4 || 幂 >= 17)return 尾.replace(/\.?0+$/,'')+'e'+(幂>=0?'+':'-')+String(Math.abs(幂)).padStart(2,'0');return 数字.toFixed(Math.max(0,16-幂)).replace(/(\.\d*?)0+$/,'$1').replace(/\.$/,'');}
  function 运行子进程(名,参){let 程序=文(名),参数组=参[0].map(文);if(路径.resolve(程序)===模块路径 || 程序.endsWith('.wasm')){参数组=[__filename,路径.resolve(程序),...参数组];程序=process.execPath;}const 果=子进程.spawnSync(程序,参数组,{maxBuffer:256*1024*1024});if(果.error)throw 果.error;const 结果=[果.status===0,果.stdout??Buffer.alloc(0),果.stderr??Buffer.alloc(0)];结果.状态=果.status??1;return 结果;}
  const 原语={
    豫言_存放包上下文:内容=>{for(const 名 of 文件.readdirSync(".yybuild/豫构上下文")){if(!名.endsWith(".上下文"))continue;const 径=路径.resolve(".yybuild/豫构上下文",名);if(文件.readFileSync(径).equals(内容))return 径;}throw Error("请先用豫构准备相同包上下文");},
    豫言_获取命令行程序名:()=>模块路径,
    豫言_获取命令行参数:()=>列(客参数),
    豫言_获取当前工作目录:()=>process.cwd(),
    豫言_获取文件修改时间:名=>BigInt(Math.floor(文件.statSync(文(名)).mtimeMs/1000)),
    豫言_获取环境变量:名=>[Object.hasOwn(process.env,文(名)),process.env[文(名)]??''],
    豫言_获取当前纳秒时间:()=>({小数:Number(process.hrtime.bigint())}),
    豫言_获取当前本地日期时间字符串:()=>{const 时=new Date();return `${时.getFullYear()}-${String(时.getMonth()+1).padStart(2,'0')}-${String(时.getDate()).padStart(2,'0')} ${String(时.getHours()).padStart(2,'0')}:${String(时.getMinutes()).padStart(2,'0')}:${String(时.getSeconds()).padStart(2,'0')}`;},
    豫言_格式化当前本地日期时间:格式=>{const 时=new Date(),补=数=>String(数).padStart(2,'0'),表={'%Y':String(时.getFullYear()),'%m':补(时.getMonth()+1),'%d':补(时.getDate()),'%H':补(时.getHours()),'%M':补(时.getMinutes()),'%S':补(时.getSeconds()),'%%':'%'};return 文(格式).replace(/%./g,项=>{if(!(项 in 表))throw Error('未支持日期格式 '+项);return 表[项];});},
    豫言_同步读取文件:名=>文件.readFileSync(文(名)),
    豫言_同步读取文件字节串:名=>文件.readFileSync(文(名)),
    豫言_同步写入文件:(名,内容)=>{文件.mkdirSync(路径.dirname(文(名)),{recursive:true});文件.writeFileSync(文(名),内容);},
    豫言_同步写入文件字节串:(名,内容)=>{文件.mkdirSync(路径.dirname(文(名)),{recursive:true});文件.writeFileSync(文(名),内容);},
    豫言_同步删除文件:名=>文件.unlinkSync(文(名)),
    豫言_同步列出文件夹:名=>列(['.','..',...文件.readdirSync(文(名))]),
    豫言_路径存在:名=>文件.existsSync(文(名)),
    豫言_路径是文件夹:名=>文件.statSync(文(名)).isDirectory(),
    豫言_路径是普通文件:名=>文件.statSync(文(名)).isFile(),
    豫言_路径为符号链接:名=>文件.lstatSync(文(名)).isSymbolicLink(),
    豫言_取得真实路径:名=>文件.realpathSync(文(名)),
    豫言_路径可执行:名=>可执行(文(名)),
    豫言_查找可执行程序:名=>{const 候选=文(名).includes('/')?[文(名)]:(process.env.PATH??'').split(':').map(径=>路径.join(径,文(名)));const 找到=候选.find(可执行);return [!!找到,找到?路径.resolve(找到):''];},
    豫言_在线处理器数量:()=>系统.availableParallelism(),
    豫言_运行于Windows:()=>process.platform==='win32',
    豫言_运行于MacOS:()=>process.platform==='darwin',
    豫言_运行于Linux:()=>process.platform==='linux',
    豫言_可绘监视面板:()=>false,
    豫言_标准输出是终端:()=>false,
    豫言_标准输入是终端:()=>false,
    豫言_打印行:值=>{文件.writeSync(1,Buffer.concat([值,Buffer.from('\n')]));},
    豫言_标准错误打印行:值=>{文件.writeSync(2,Buffer.concat([值,Buffer.from('\n')]));},
    豫言_打印字符串:值=>{文件.writeSync(1,值);},
    豫言_字节转字符串:值=>Buffer.from([数(值)]),
    豫言_整数转小数:值=>({小数:数(值)}),
    豫言_小数转整数:值=>BigInt(Math.trunc(数(值))),
    豫言_整数加:(甲,乙)=>BigInt.asIntN(64,甲+乙),
    豫言_整数乘:(甲,乙)=>BigInt.asIntN(64,甲*乙),
    豫言_整数除:(甲,乙)=>甲/乙,
    豫言_小数加:(甲,乙)=>({小数:数(甲)+数(乙)}),
    豫言_小数减:(甲,乙)=>({小数:数(甲)-数(乙)}),
    豫言_小数乘:(甲,乙)=>({小数:数(甲)*数(乙)}),
    豫言_小数除:(甲,乙)=>({小数:数(甲)/数(乙)}),
    豫言_字符串按字节在前:(甲,乙)=>Buffer.compare(甲,乙)<0,
    豫言_整数转字符串:值=>String(值),
    豫言_字符串转整数:值=>BigInt(文(值)),
    豫言_小数转字符串:值=>数(值).toFixed(6),
    豫言_小数精确表示:值=>精确小数(值),
    豫言_字符串转小数:值=>({小数:Number(文(值))}),
    豫言_源码数字名:值=>/^[0-9-]+$/.test(文(值)),
    豫言_源码可用名:值=>!/^[0-9-]+$/.test(文(值))&&!文(值).startsWith('《《')&&!文(值).startsWith('：')&&!文(值).includes('」'),
    豫言_源码字符串表示:值=>'『'+文(值).replace(/「：|』/g,字=>字==='』'?'「：』：」':'「：「：：」')+'』',
    豫言_同步运行子进程并获取输出:运行子进程,
    豫言_同步运行子进程:(名,参)=>运行子进程(名,参).状态,
    豫言_同步运行子进程并传递输出:(名,参)=>{const 果=运行子进程(名,参);文件.writeSync(1,果[1]);文件.writeSync(2,果[2]);return 果.状态;},
    豫言_同步运行子进程并继承标准流:(名,参)=>{const 果=运行子进程(名,参);文件.writeSync(1,果[1]);文件.writeSync(2,果[2]);return 果.状态;},
    豫言_退出进程:码=>{const 错=Error('客体退出');错.退出码=数(码);throw 错;}
  };
  let 调用数=0;
  const 实例=new WebAssembly.Instance(模块,{'yuyan:gc-host/v1':{call:(名,参)=>{
    const 名称=文(解(名)),参数组=解(参);调用数++;
    if(!原语[名称])throw Error('未实现 Node 宿主原语：'+名称);
    try{return 编(原语[名称](...参数组));}catch(错){错.message=名称+': '+错.message;throw 错;}
  }}});
  try{实例.exports._start();}catch(错){if(错.退出码!==0)throw 错;}
  console.error(JSON.stringify({轮次:轮,复用模块:!!缓存,引擎:process.versions.v8,模块准备毫秒:编译毕-开始,执行毫秒:performance.now()-编译毕,宿主调用数:调用数}));
  return 模块;
}
