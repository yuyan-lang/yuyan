// 文言：豫言为入口，此层唯接 Node 与客值。汉语：Node 引擎及系统接口适配，不执行编译器算法。
'use strict';
const 文件 = require('node:fs'), 路径 = require('node:path'), 系统 = require('node:os');
const 子进程 = require('node:child_process');
const {Worker, MessageChannel, isMainThread, workerData, threadId, parentPort} = require('node:worker_threads');
const {接管进程, 客体请求} = require('./进程桥接.cjs');
const {建立编译线程} = require('./编译线程.cjs');
const 选引擎参数=参数=>参数.filter(参=>/^--(?:no-)?(?:wasm-|liftoff)/.test(参)||/^--(?:v8-pool-size|initial-heap-size|initial-old-space-size|min-semi-space-size|max-semi-space-size|max-old-space-size)=/.test(参));
if (isMainThread) {
  // 文言：客执行虽塞，主仍候诸工。汉语：Wasm 同步执行留在工作线程，主线程持续收集真实子进程输出。
  const {port1, port2} = new MessageChannel(), 信号 = new SharedArrayBuffer(4);
  const 模式=process.env.YY_NODE_COMPILER_WORKERS??'threads';
  if(!['process','threads'].includes(模式))throw Error('YY_NODE_COMPILER_WORKERS 须为 process 或 threads');
  const 启动编译 = 模式==='threads'?建立编译线程(__filename,process.argv[2],选引擎参数(process.execArgv)):null;
  const 清桥 = 接管进程(port1, 信号, 启动编译);
  const 清理 = () => {清桥(); 启动编译?.清理?.();};
  let 终止码 = null;
  const 工 = new Worker(__filename, {workerData: {参数:process.argv.slice(2), 端口:port2, 信号}, transferList:[port2], resourceLimits:{stackSizeMb:128}});
  工.on('error', 错 => {console.error(错); process.exitCode=1;});
  工.on('exit', 码 => {清理(); process.exitCode=终止码??码;});
  for (const 信 of ['SIGINT', 'SIGTERM']) process.once(信, () => {终止码=信==='SIGINT'?130:143; 清理(); 工.terminate();});
} else if (workerData.复用线程) {
  // 文言：工可复用，客实例与桥每事新立；二出先递，毕报在后。汉语：只有内部编译任务使用线程池；每项任务调用执行创建全新实例，通过同一消息通道保证输出先于完成，异常不污染下一任务退出码。
  parentPort.on('message', 任务 => {
    let 码 = 0;
    try {执行(任务.参数, null, 1, 任务);}
    catch (错) {码 = 1; parentPort.postMessage({种: '出', 号: 2, 值: Buffer.from(String(错.stack ?? 错)+'\n')});}
    finally {任务.端口.close(); parentPort.postMessage({种: '毕', 码});}
  });
} else {
  try {
    let 缓存=null;
    const 轮数=workerData.编译线程?1:Number(process.env.YY_NODE_REPEAT??1);
    if(!Number.isInteger(轮数)||轮数<1||轮数>5)throw Error('重复次数须为一至五');
    for(let 轮=1;轮<=轮数;轮++){
      const 参数=[...workerData.参数];
      if(轮>1){const 当前=new Date();文件.utimesSync(参数[0],当前,当前);const 位=参数.indexOf('-o');if(位<0)throw Error('重复验证须指定 -o');参数[位+1]=参数[位+1]+'.第'+轮+'.wasm';}
      缓存=执行(参数,缓存,轮);
    }
  } catch (错) { console.error(错); process.exitCode=1; }
  finally {workerData.端口.close();}
}
function 执行(参数, 缓存, 轮, 本工 = workerData) {
  const 开始=performance.now();
  const 模块路径=路径.resolve(参数[0]), 客参数=参数.slice(1);
  const 桥=new WebAssembly.Instance(本工.桥模块??new WebAssembly.Module(文件.readFileSync('yy节点值桥接.wasm'))).exports;
  const 模块=缓存??本工.模块??new WebAssembly.Module(文件.readFileSync(模块路径));
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
  // 文言：线程之出各归其管。汉语：内部编译线程通过 Worker 标准流汇集输出，不能直接写共享进程的文件描述符。
  const 写输出=(号,值)=>本工.复用线程?parentPort.postMessage({种:'出',号,值}):本工.编译线程?(号===1?process.stdout:process.stderr).write(值):文件.writeSync(号,值);
  function 精确小数(值){const 数字=数(值);if(Object.is(数字,-0))return '-0';if(!Number.isFinite(数字))return String(数字).toLowerCase().replace('infinity','inf');const [尾,指数]=数字.toExponential(16).split('e');const 幂=Number(指数);if(幂 < -4 || 幂 >= 17)return 尾.replace(/\.?0+$/,'')+'e'+(幂>=0?'+':'-')+String(Math.abs(幂)).padStart(2,'0');return 数字.toFixed(Math.max(0,16-幂)).replace(/(\.\d*?)0+$/,'$1').replace(/\.$/,'');}
  // 文言：诸客同用引擎之制，不令调参独及调度者。汉语：传播显式 Wasm 与 V8 线程池选项；不传播调试端口或 CPU 剖析输出选项。
  const 引擎参数=本工.引擎参数??选引擎参数(process.execArgv);
  function 子进程参数(名,参){let 程序=文(名),参数组=参[0].map(文);const 客体=路径.resolve(程序)===模块路径 || 程序.endsWith('.wasm');if(客体){参数组=[...引擎参数,__filename,路径.resolve(程序),...参数组];程序=process.execPath;}return [程序,参数组,客体];}
  function 运行子进程(名,参){const [程序,参数组,客体]=子进程参数(名,参);const 果=子进程.spawnSync(程序,参数组,{maxBuffer:256*1024*1024,env:客体?{...process.env,YY_NODE_REPEAT:'1'}:process.env});if(果.error)throw 果.error;const 结果=[果.status===0,果.stdout??Buffer.alloc(0),果.stderr??Buffer.alloc(0)];结果.状态=果.status??1;return 结果;}
  const 请求进程=客体请求(本工.端口,本工.信号);
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
    豫言_启动异步子进程:(名,参)=>请求进程('启动',...子进程参数(名,参)),
    豫言_尝试收取异步子进程:号=>请求进程('收取',号),
    // 文言：此桥唯候自身子进程。汉语：未知句柄返回错误事件，不冒充通用网络或文件描述符轮询。
    豫言_异步_输入输出多路等待:(关注,超时)=>请求进程('等待',关注[0],数(超时)),
    豫言_运行于Windows:()=>process.platform==='win32',
    豫言_运行于MacOS:()=>process.platform==='darwin',
    豫言_运行于Linux:()=>process.platform==='linux',
    豫言_可绘监视面板:()=>false,
    豫言_标准输出是终端:()=>false,
    豫言_标准输入是终端:()=>false,
    豫言_打印行:值=>{写输出(1,Buffer.concat([值,Buffer.from('\n')]));},
    豫言_标准错误打印行:值=>{写输出(2,Buffer.concat([值,Buffer.from('\n')]));},
    豫言_打印字符串:值=>{写输出(1,值);},
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
    豫言_同步运行子进程并传递输出:(名,参)=>{const 果=运行子进程(名,参);写输出(1,果[1]);写输出(2,果[2]);return 果.状态;},
    豫言_同步运行子进程并继承标准流:(名,参)=>{const 果=运行子进程(名,参);写输出(1,果[1]);写输出(2,果[2]);return 果.状态;},
    豫言_退出进程:码=>{const 错=Error('客体退出');错.退出码=数(码);throw 错;}
  };
  let 调用数=0;
  // 文言：外术之名为常字，同值毋重解。汉语：编译器以不可变字面量指定原语；按 GC 对象身份弱缓存名称，不保留参数或结果。
  const 名称缓存=new WeakMap();
  const 原语次数=process.env.YY_NODE_PROFILE==='1'?Object.create(null):null;
  const 实例=new WebAssembly.Instance(模块,{'yuyan:gc-host/v1':{call:(名,参)=>{
    let 名称=名称缓存.get(名);
    if(名称===undefined){名称=文(解(名));名称缓存.set(名,名称);}
    const 参数组=解(参);调用数++;
    if(原语次数)原语次数[名称]=(原语次数[名称]??0)+1;
    if(!原语[名称])throw Error('未实现 Node 宿主原语：'+名称);
    try{return 编(原语[名称](...参数组));}catch(错){错.message=名称+': '+错.message;throw 错;}
  }}});
  try{实例.exports._start();}catch(错){if(错.退出码!==0)throw 错;}
  const 统计=JSON.stringify({轮次:轮,复用模块:!!缓存,引擎:process.versions.v8,进程:process.pid,线程:threadId,编译工作模式:process.env.YY_NODE_COMPILER_WORKERS??'threads',模块准备毫秒:编译毕-开始,执行毫秒:performance.now()-编译毕,宿主调用数:调用数,...(原语次数?{原语次数}:{})});
  if(本工.复用线程)写输出(2,Buffer.from(统计+'\n'));else console.error(统计);
  return 模块;
}
