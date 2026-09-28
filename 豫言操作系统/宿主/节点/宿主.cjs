// 文言：豫言为入口，此层唯接 Node 与客值。汉语：Node 引擎及系统接口适配，不执行编译器算法。
'use strict';
const 文件 = require('node:fs'), 路径 = require('node:path'), 系统 = require('node:os');
const 子进程 = require('node:child_process'), 终端 = require('node:tty'), 密码 = require('node:crypto'), 工具 = require('node:util');
const {Worker, MessageChannel, isMainThread, workerData, threadId, parentPort} = require('node:worker_threads');
const {接管进程, 客体请求} = require('./进程桥接.cjs');
const {建立编译线程} = require('./编译线程.cjs');
// 文言：值桥先从父宿主所传之径，次取宿主之旁（仓库目标“豫构”书之），次求于今目录，末取解于仓根之工具链包者；定则录其绝对之径于环境，子进程承之，迁目录亦不失。汉语：值桥文件依次找：父宿主经环境变量 YY_NODE_VALUE_BRIDGE 传下的绝对路径；宿主文件旁的那份（仓库目标“豫构”用当前编译器生成）；当前目录；最后是解压在仓库根目录的 Wasm 工具链包里的 yy稳定节点宿主/yy节点值桥接.wasm（新检出的仓库第一次构建时用）。找到后把绝对路径写回该环境变量，子进程继承，切换工作目录后仍能找到。
const 桥文件路径=()=>{const 候选=[process.env.YY_NODE_VALUE_BRIDGE,路径.join(__dirname,'yy节点值桥接.wasm'),路径.resolve('yy节点值桥接.wasm'),路径.resolve('yy稳定节点宿主','yy节点值桥接.wasm')];const 径=候选.find(径=>径&&文件.existsSync(径))??候选[1];process.env.YY_NODE_VALUE_BRIDGE=径;return 径;};
const 选引擎参数=参数=>参数.filter(参=>/^--(?:no-)?(?:wasm-|liftoff)/.test(参)||/^--(?:v8-pool-size|initial-heap-size|initial-old-space-size|min-semi-space-size|max-semi-space-size|max-old-space-size)=/.test(参));
if (isMainThread) {
  桥文件路径();
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
    try {码 = 执行(任务.参数, null, 1, 任务).退出码;}
    catch (错) {码 = 1; parentPort.postMessage({种: '出', 号: 2, 值: Buffer.from(String(错.stack ?? 错)+'\n')});}
    finally {任务.端口.close(); parentPort.postMessage({种: '毕', 码});}
  });
} else {
  let 码=0;
  try {
    let 缓存=null;
    const 轮数=workerData.编译线程?1:Number(process.env.YY_NODE_REPEAT??1);
    if(!Number.isInteger(轮数)||轮数<1||轮数>5)throw Error('重复次数须为一至五');
    for(let 轮=1;轮<=轮数;轮++){
      const 参数=[...workerData.参数];
      if(轮>1){const 当前=new Date();文件.utimesSync(参数[0],当前,当前);const 位=参数.indexOf('-o');if(位<0)throw Error('重复验证须指定 -o');参数[位+1]=参数[位+1]+'.第'+轮+'.wasm';}
      const 果=执行(参数,缓存,轮);缓存=果.模块;码=果.退出码;
      if(码!==0)break;
    }
  } catch (错) { console.error(错); 码=1; }
  finally {workerData.端口.close();}
  // 文言：客之退出码，经工线程之终传于主。汉语：客体的退出码经工作线程的退出码传给主线程，再成为进程退出码。
  if(码!==0)process.exit(码);
}
function 执行(参数, 缓存, 轮, 本工 = workerData) {
  const 开始=performance.now();
  const 模块路径=路径.resolve(参数[0]), 客参数=参数.slice(1);
  // 文言：客自有今目录；工线程不能迁进程之目录，故宿主代记之，诸径与子进程皆依之。汉语：每个客实例有自己的当前目录；工作线程不能调用 process.chdir，所以由宿主记录，文件路径与子进程的工作目录都以它为准。
  let 当前目录=本工.初始目录??process.cwd();
  const 桥=new WebAssembly.Instance(本工.桥模块??new WebAssembly.Module(文件.readFileSync(桥文件路径()))).exports;
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
  const 径=值=>路径.resolve(当前目录,文(值));
  const 数=值=>Number(值?.小数??值);
  const 列=诸值=>[诸值,诸值.length];
  const 可执行=名=>{try{文件.accessSync(名,文件.constants.X_OK);return 文件.statSync(名).isFile();}catch{return false;}};
  // 文言：线程之出各归其管。汉语：内部编译线程通过 Worker 标准流汇集输出，不能直接写共享进程的文件描述符。
  const 写输出=(号,值)=>本工.复用线程?parentPort.postMessage({种:'出',号,值}):本工.编译线程?(号===1?process.stdout:process.stderr).write(值):文件.writeSync(号,值);
  function 精确小数(值){const 数字=数(值);if(Object.is(数字,-0))return '-0';if(!Number.isFinite(数字))return String(数字).toLowerCase().replace('infinity','inf');const [尾,指数]=数字.toExponential(16).split('e');const 幂=Number(指数);if(幂 < -4 || 幂 >= 17)return 尾.replace(/\.?0+$/,'')+'e'+(幂>=0?'+':'-')+String(Math.abs(幂)).padStart(2,'0');return 数字.toFixed(Math.max(0,16-幂)).replace(/(\.\d*?)0+$/,'$1').replace(/\.$/,'');}
  // 文言：诸客同用引擎之制，不令调参独及调度者。汉语：传播显式 Wasm 与 V8 线程池选项；不传播调试端口或 CPU 剖析输出选项。
  const 引擎参数=本工.引擎参数??选引擎参数(process.execArgv);
  // 文言：env 与 timeout 之包装，其所行者为网页汇编模块时，由宿主径行之：环境并入，限时由宿主计之，逾时以一二四退（同 GNU timeout）。汉语：命令被 env（VAR=值…）或 timeout（[-k 宽限] [-s 信号] 秒数）包装、而最终程序是 .wasm 时，由宿主直接用 Node 运行该模块：环境变量并入子进程环境，时限由宿主计时，超时返回 124（与 GNU timeout 一致）；最终程序不是 .wasm 时原样执行包装命令。
  function 拆包装(程序,参数组){
    let 环境={},限时=0,宽限=0,余=[程序,...参数组];
    for(;;){
      const 名=路径.basename(余[0]??'');
      if(名==='env'){let 位=1;while(位<余.length&&/^[A-Za-z_][A-Za-z0-9_]*=/.test(余[位])){const 等=余[位].indexOf('=');环境[余[位].slice(0,等)]=余[位].slice(等+1);位++;}余=余.slice(位);continue;}
      if(名==='timeout'){let 位=1;while(位<余.length&&余[位].startsWith('-')){if(余[位]==='-k'){宽限=Number(余[位+1]);位+=2;}else if(余[位]==='-s')位+=2;else 位++;}限时=Number(余[位]);余=余.slice(位+1);continue;}
      break;
    }
    return {程序:余[0]??'',参数组:余.slice(1),环境,限时,宽限};
  }
  // 文言：网页汇编之模，以其首四字节 \0asm 识之，不拘其名之后缀。汉语：按文件开头 4 字节 \0asm 识别 Wasm 模块，不依赖 .wasm 后缀（豫构的 --输出 可以是任意名字）。
  const 是网页汇编=名=>{if(名.endsWith('.wasm'))return true;try{const 号=文件.openSync(路径.resolve(当前目录,名),'r');const 头=Buffer.alloc(4);const 数=文件.readSync(号,头,0,4,0);文件.closeSync(号);return 数===4&&头.equals(Buffer.from([0,0x61,0x73,0x6d]));}catch{return false;}};
  function 子进程参数(名,参){let 程序=文(名),参数组=参[0].map(文),环境=null,限时=0,宽限=0;const 拆=拆包装(程序,参数组);if(拆.程序!==程序&&是网页汇编(拆.程序)){({程序,参数组}=拆);环境=拆.环境;限时=拆.限时;宽限=拆.宽限;}const 客体=路径.resolve(当前目录,程序)===模块路径 || 是网页汇编(程序);if(客体){参数组=[...引擎参数,__filename,路径.resolve(当前目录,程序),...参数组];程序=process.execPath;}return [程序,参数组,客体,环境,限时,宽限];}
  function 运行子进程(名,参){const [程序,参数组,客体,环境,限时]=子进程参数(名,参);const 果=子进程.spawnSync(程序,参数组,{cwd:当前目录,maxBuffer:256*1024*1024,timeout:限时>0?限时*1000:undefined,killSignal:'SIGKILL',env:客体?{...process.env,...(环境??{}),YY_NODE_REPEAT:'1'}:process.env});const 超时=限时>0&&果.error?.code==='ETIMEDOUT';if(果.error&&!超时)throw 果.error;const 状态=超时?124:(果.status??(果.signal?128+(系统.constants.signals[果.signal]??0):1));const 结果=[状态===0,果.stdout??Buffer.alloc(0),果.stderr??Buffer.alloc(0)];结果.状态=状态;return 结果;}
  const 请求进程=客体请求(本工.端口,本工.信号);
  // 文言：Node 无 flock，以独占新建之“占”文为锁，书进程号；持者已亡则除其陈锁，未得则稍候复试。汉语：Node 没有 flock，用独占创建“占用文件”加锁，文件内写进程号；持有者已退出就清除陈旧的锁，拿不到锁则短暂等待后重试。
  const 小候=new Int32Array(new SharedArrayBuffer(4));
  // 文言：标准入按行读之：同步读文件号零，余者留待下次；行尾之换行与回车去之；读尽而无余则返无。汉语：按行读标准输入（与原生 getline 语义一致）：同步读取文件描述符 0，多读的部分留给下次；去掉行尾的换行与回车；读到文件尾且没有剩余内容时返回（假，空串），最后一行没有换行也照常返回。非阻塞时暂无数据就短暂等待后重读。
  let 入缓=Buffer.alloc(0),入尽=false;
  const 去行尾=行=>{if(行.length&&行[行.length-1]===13)行=行.subarray(0,行.length-1);return Buffer.from(行);};
  function 读标准输入行(){
    for(;;){
      const 位=入缓.indexOf(10);
      if(位>=0){const 行=入缓.subarray(0,位);入缓=入缓.subarray(位+1);return [true,去行尾(行)];}
      if(入尽){if(入缓.length===0)return [false,Buffer.alloc(0)];const 行=入缓;入缓=Buffer.alloc(0);return [true,去行尾(行)];}
      const 块=Buffer.alloc(65536);let 数=0;
      try{数=文件.readSync(0,块,0,块.length,null);}catch(错){if(错.code==='EAGAIN'){Atomics.wait(小候,0,0,10);continue;}if(错.code!=='EOF')throw 错;}
      if(数===0)入尽=true;else 入缓=Buffer.concat([入缓,块.subarray(0,数)]);
    }
  }
  // 文言：安全外壳之密码诸术，与原生 OpenSSL 之实同义：原始之钥以 DER 前缀装之。汉语：安全外壳的密码原语，语义与原生 OpenSSL 实现相同：32 字节原始私种与公钥分别套上 PKCS8、SPKI 的 DER 前缀后交给 node:crypto；AES-256-GCM 加密输出为密文加 16 字节标签，解密结果首字节 1 表示成功、0 表示认证失败。
  const 私钥前缀={X25519:'302e020100300506032b656e04220420',Ed25519:'302e020100300506032b657004220420'},公钥前缀={X25519:'302a300506032b656e032100',Ed25519:'302a300506032b6570032100'};
  const 三十二=(值,错)=>{if(值.length!==32)throw Error(错);return 值;};
  const 私钥对象=(种,算法)=>密码.createPrivateKey({key:Buffer.concat([Buffer.from(私钥前缀[算法],'hex'),三十二(种,算法+' 私钥长度必须为三十二字节')]),format:'der',type:'pkcs8'});
  const 公钥对象=(公,算法)=>密码.createPublicKey({key:Buffer.concat([Buffer.from(公钥前缀[算法],'hex'),三十二(公,算法+' 公钥长度必须为三十二字节')]),format:'der',type:'spki'});
  const 原始公钥=(种,算法)=>Buffer.from(密码.createPublicKey(私钥对象(种,算法)).export({format:'der',type:'spki'}).subarray(-32));
  const 验GCM=(钥,随机数)=>{if(钥.length!==32||随机数.length!==12)throw Error('AES-256-GCM 密钥须为三十二字节，IV 须为十二字节');};
  // 文言：通值之示，唯供调试。汉语：打印通用值的文本表示（仅供调试）：字节串按 UTF-8 显示为带引号的串，元组显示为方括号列表。
  const 通用表示=值=>值==null?'()':Buffer.isBuffer(值)?JSON.stringify(值.toString('utf8')):Array.isArray(值)?'['+值.map(通用表示).join(', ')+']':Object.hasOwn(Object(值),'小数')?String(值.小数):String(值);
  function 取占锁(占径){
    for(;;){
      try{const 号=文件.openSync(占径,'wx',0o600);文件.writeSync(号,String(process.pid));文件.closeSync(号);return;}
      catch(错){if(错.code!=='EEXIST')throw 错;}
      let 持者=0;try{持者=Number(文件.readFileSync(占径,'utf8'));}catch{}
      if(持者&&持者!==process.pid){try{process.kill(持者,0);}catch{try{文件.unlinkSync(占径);}catch{}continue;}}
      Atomics.wait(小候,0,0,20);
    }
  }
  const 释占锁=占径=>{try{文件.unlinkSync(占径);}catch{}};
  const 产物锁们=new Map();let 下锁号=1n;
  const 原语={
    // 文言：同文用旧号，异文另立号，既存不改（同原生 包上下文.c）。汉语：与原生 包上下文.c 相同：在当前目录的 .yybuild/豫构上下文 里逐字比较，内容相同就复用已有编号，否则新建下一个编号；加锁防并发，返回真实路径。
    豫言_存放包上下文:内容=>{
      const 目录=路径.resolve(当前目录,'.yybuild/豫构上下文');文件.mkdirSync(目录,{recursive:true,mode:0o700});
      const 占径=路径.join(目录,'锁.占');取占锁(占径);
      try{
        for(let 号=1;;号++){
          const 文径=路径.join(目录,号+'.上下文');let 旧;
          try{旧=文件.readFileSync(文径);}catch(错){if(错.code!=='ENOENT')throw 错;文件.writeFileSync(文径,内容,{flag:'wx',mode:0o600});return 文件.realpathSync(文径);}
          if(旧.equals(内容))return 文件.realpathSync(文径);
        }
      }finally{释占锁(占径);}
    },
    // 文言：原生产物之锁：返一号，释时凭号去其占文。汉语：原生产物锁：加锁后返回一个句柄号，释放时凭句柄删除占用文件。
    豫言_锁原生产物:名=>{const 占径=径(名)+'.占';取占锁(占径);const 号=下锁号++;产物锁们.set(号,占径);return 号;},
    豫言_释原生产物锁:号=>{const 键=BigInt(号);const 占径=产物锁们.get(键);if(占径!==undefined){产物锁们.delete(键);释占锁(占径);}return null;},
    豫言_获取命令行程序名:()=>模块路径,
    豫言_获取命令行参数:()=>列(客参数),
    豫言_获取当前工作目录:()=>当前目录,
    豫言_切换当前工作目录:名=>{const 目标=径(名);try{if(!文件.statSync(目标).isDirectory())return [20n,'不是目录：'+目标];当前目录=目标;return [0n,''];}catch(错){return [BigInt(Math.abs(系统.constants.errno[错.code]??2)),错.message];}},
    豫言_获取文件修改时间:名=>BigInt(Math.floor(文件.statSync(径(名)).mtimeMs/1000)),
    豫言_获取环境变量:名=>[Object.hasOwn(process.env,文(名)),process.env[文(名)]??''],
    豫言_获取当前纳秒时间:()=>({小数:Number(process.hrtime.bigint())}),
    豫言_获取当前本地日期时间字符串:()=>{const 时=new Date();return `${时.getFullYear()}-${String(时.getMonth()+1).padStart(2,'0')}-${String(时.getDate()).padStart(2,'0')} ${String(时.getHours()).padStart(2,'0')}:${String(时.getMinutes()).padStart(2,'0')}:${String(时.getSeconds()).padStart(2,'0')}`;},
    豫言_格式化当前本地日期时间:格式=>{const 时=new Date(),补=数=>String(数).padStart(2,'0'),表={'%Y':String(时.getFullYear()),'%m':补(时.getMonth()+1),'%d':补(时.getDate()),'%H':补(时.getHours()),'%M':补(时.getMinutes()),'%S':补(时.getSeconds()),'%%':'%'};return 文(格式).replace(/%./g,项=>{if(!(项 in 表))throw Error('未支持日期格式 '+项);return 表[项];});},
    豫言_同步读取文件:名=>文件.readFileSync(径(名)),
    豫言_同步读取文件字节串:名=>文件.readFileSync(径(名)),
    // 文言：字节串之术一依原生运行时：越界则止，截取以起点与长度。汉语：字节串原语与原生运行时（字节串.c）语义一致：越界即报错，截取按起点与长度。
    豫言_字节串_空:()=>Buffer.alloc(0),
    豫言_字节串_长度:值=>BigInt(值.length),
    豫言_字节串_取字节:(值,序)=>{if(序<0n||序>=BigInt(值.length))throw Error('字节串取字节：序数越界');return BigInt(值[Number(序)]);},
    豫言_字节串_从字符串:值=>值,
    豫言_字节串_单字节:值=>{if(值<0n||值>255n)throw Error('构造单字节串：字节必须在零至二百五十五之间');return Buffer.from([Number(值)]);},
    豫言_字节串_拼接:(甲,乙)=>Buffer.concat([甲,乙]),
    豫言_字节串_截取:(值,起,长)=>{if(起<0n||长<0n||起>BigInt(值.length)||长>BigInt(值.length)-起)throw Error('截取字节串：范围越界');return Buffer.from(值.subarray(Number(起),Number(起+长)));},
    豫言_同步写入文件:(名,内容)=>{文件.mkdirSync(路径.dirname(径(名)),{recursive:true});文件.writeFileSync(径(名),内容);},
    豫言_同步写入文件字节串:(名,内容)=>{文件.mkdirSync(路径.dirname(径(名)),{recursive:true});文件.writeFileSync(径(名),内容);},
    豫言_同步删除文件:名=>文件.unlinkSync(径(名)),
    豫言_同步列出文件夹:名=>列(['.','..',...文件.readdirSync(径(名))]),
    豫言_路径存在:名=>文件.existsSync(径(名)),
    豫言_路径是文件夹:名=>文件.statSync(径(名)).isDirectory(),
    豫言_路径是普通文件:名=>文件.statSync(径(名)).isFile(),
    豫言_路径为符号链接:名=>文件.lstatSync(径(名)).isSymbolicLink(),
    豫言_取得真实路径:名=>文件.realpathSync(径(名)),
    豫言_路径可执行:名=>可执行(径(名)),
    豫言_查找可执行程序:名=>{const 候选=文(名).includes('/')?[径(名)]:(process.env.PATH??'').split(':').map(径=>路径.join(径,文(名)));const 找到=候选.find(可执行);return [!!找到,找到?路径.resolve(找到):''];},
    豫言_在线处理器数量:()=>系统.availableParallelism(),
    豫言_启动异步子进程:(名,参)=>{const [程序,参数组,客体,环境,限时,宽限]=子进程参数(名,参);return 请求进程('启动',程序,参数组,客体,当前目录,环境,限时,宽限);},
    豫言_尝试收取异步子进程:号=>请求进程('收取',号),
    // 文言：此桥唯候自身子进程。汉语：未知句柄返回错误事件，不冒充通用网络或文件描述符轮询。
    豫言_异步_输入输出多路等待:(关注,超时)=>请求进程('等待',关注[0],数(超时)),
    // 文言：传输控制协议之术，由主线程持套接字而行之，皆不塞，同原生。汉语：TCP 原语：套接字由主线程持有（进程桥接.cjs），语义与原生运行时相同，均不阻塞，返回（状态，值）。
    豫言_传输控制协议_监听:(址,端口,队长)=>请求进程('网监听',文(址),数(端口),数(队长)),
    豫言_传输控制协议_开始连接:(主机,端口)=>请求进程('网开始连接',文(主机),数(端口)),
    豫言_传输控制协议_完成连接:号=>请求进程('网完成连接',数(号)),
    豫言_传输控制协议_接受:号=>请求进程('网接受',数(号)),
    豫言_传输控制协议_读取:(号,最大)=>请求进程('网读取',数(号),数(最大)),
    豫言_传输控制协议_读取字节串:(号,最大)=>请求进程('网读取',数(号),数(最大)),
    豫言_传输控制协议_从字节序数写入:(号,内容,起)=>请求进程('网写入',数(号),内容,数(起)),
    豫言_传输控制协议_从字节序数写入字节串:(号,内容,起)=>请求进程('网写入',数(号),内容,数(起)),
    豫言_传输控制协议_等待:(号,关注,超时)=>请求进程('网等待',数(号),数(关注),数(超时)),
    豫言_传输控制协议_获取本地端口:号=>请求进程('网本地端口',数(号)),
    豫言_传输控制协议_设置无延迟:(号,开)=>请求进程('网无延迟',数(号),数(开)),
    豫言_传输控制协议_关闭写入:号=>请求进程('网关闭写入',数(号)),
    豫言_传输控制协议_关闭:号=>请求进程('网关闭',数(号)),
    豫言_传输控制协议_错误消息:状态=>{const 码=数(状态);if(码<=-19900&&码>=-20100)return '地址解析失败';try{return 工具.getSystemErrorMessage(码<0?码:-码);}catch{return '未知传输控制协议错误';}},
    豫言_运行于Windows:()=>process.platform==='win32',
    豫言_运行于MacOS:()=>process.platform==='darwin',
    豫言_运行于Linux:()=>process.platform==='linux',
    豫言_可绘监视面板:()=>false,
    豫言_标准输出是终端:()=>终端.isatty(1),
    豫言_标准输入是终端:()=>终端.isatty(0),
    豫言_尝试读取标准输入行:()=>读标准输入行(),
    // 文言：常随机不求安全；安全随机取于系统之熵，一请至多一兆字节。汉语：普通随机数用 Math.random（不保证安全）；安全随机字节取自系统熵源，单次至多 1 MiB，与原生一致。
    豫言_获取随机整数:上界=>{const 界=Number(上界);if(!(界>0))throw Error('随机整数的上界须为正');return BigInt(Math.floor(Math.random()*界));},
    豫言_获取随机小数:()=>({小数:Math.random()}),
    豫言_安全随机_字节串:长=>{const 数值=Number(长);if(!Number.isInteger(数值)||数值<0||数值>1048576)throw Error('安全随机字节串：长度须在零至一兆之间');return 密码.randomBytes(数值);},
    豫言_密码_SHA256:内容=>密码.createHash('sha256').update(内容).digest(),
    豫言_密码_已知主机HMACSHA1:(盐,主机)=>{if(盐.length!==20)throw Error('已知主机 HMAC-SHA1 参数长度非法');return 密码.createHmac('sha1',盐).update(主机).digest();},
    豫言_密码_X25519公钥:种=>原始公钥(种,'X25519'),
    豫言_密码_X25519共密:(种,公)=>密码.diffieHellman({privateKey:私钥对象(种,'X25519'),publicKey:公钥对象(公,'X25519')}),
    豫言_密码_Ed25519公钥:种=>原始公钥(种,'Ed25519'),
    豫言_密码_Ed25519签:(种,正文)=>密码.sign(null,正文,私钥对象(种,'Ed25519')),
    豫言_密码_Ed25519验:(公,正文,签名)=>{if(签名.length!==64)throw Error('Ed25519 签名长度必须为六十四字节');return 密码.verify(null,正文,公钥对象(公,'Ed25519'),签名);},
    豫言_密码_AES256GCM加密:(钥,随机数,附加,明文)=>{验GCM(钥,随机数);const 器=密码.createCipheriv('aes-256-gcm',钥,随机数);if(附加.length)器.setAAD(附加);const 密=Buffer.concat([器.update(明文),器.final()]);return Buffer.concat([密,器.getAuthTag()]);},
    豫言_密码_AES256GCM解密结果:(钥,随机数,附加,密签)=>{验GCM(钥,随机数);if(密签.length<16)return Buffer.from([0]);const 器=密码.createDecipheriv('aes-256-gcm',钥,随机数);if(附加.length)器.setAAD(附加);器.setAuthTag(密签.subarray(密签.length-16));try{const 明=Buffer.concat([器.update(密签.subarray(0,密签.length-16)),器.final()]);return Buffer.concat([Buffer.from([1]),明]);}catch{return Buffer.from([0]);}},
    豫言_打印通用值:(消息,对象)=>{写输出(2,Buffer.from('[豫言通用值打印] '+文(消息)+': '+通用表示(对象)+'\n'));},
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
    // 文言：同步行之，成则阳；同原生，返爻而非退码。汉语：与原生一致：只返回是否成功（退出码为零），不返回退出码。
    豫言_同步运行子进程:(名,参)=>运行子进程(名,参).状态===0,
    豫言_同步运行子进程并传递输出:(名,参)=>{const 果=运行子进程(名,参);写输出(1,果[1]);写输出(2,果[2]);return 果.状态;},
    // 文言：承三常流而行，出不经宿主之缓；启败一二七，候败一。汉语：继承标准输入、输出与错误直接运行（输出不经宿主缓冲，交互程序可用）；启动失败返回 127，等待失败返回 1，超时返回 124。
    豫言_同步运行子进程并继承标准流:(名,参)=>{const [程序,参数组,客体,环境,限时]=子进程参数(名,参);const 果=子进程.spawnSync(程序,参数组,{cwd:当前目录,stdio:'inherit',timeout:限时>0?限时*1000:undefined,killSignal:'SIGKILL',env:客体?{...process.env,...(环境??{}),YY_NODE_REPEAT:'1'}:process.env});const 超时=限时>0&&果.error?.code==='ETIMEDOUT';if(超时)return 124;if(果.error)return 果.error.code==='ENOENT'?127:1;return 果.status??(果.signal?128+(系统.constants.signals[果.signal]??0):1);},
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
  let 退出码=0;
  try{实例.exports._start();}catch(错){if(错.退出码===undefined)throw 错;退出码=错.退出码;}
  // 文言：统计之行，唯请乃书，免乱诸器之标准错误。汉语：运行统计只在 YY_NODE_STATS=1 或 YY_NODE_PROFILE=1 时打印，避免干扰工具的标准错误输出。
  if(process.env.YY_NODE_STATS!=='1'&&!原语次数)return {模块,退出码};
  const 统计=JSON.stringify({轮次:轮,复用模块:!!缓存,引擎:process.versions.v8,进程:process.pid,线程:threadId,编译工作模式:process.env.YY_NODE_COMPILER_WORKERS??'threads',模块准备毫秒:编译毕-开始,执行毫秒:performance.now()-编译毕,宿主调用数:调用数,...(原语次数?{原语次数}:{})});
  if(本工.复用线程)写输出(2,Buffer.from(统计+'\n'));else console.error(统计);
  return {模块,退出码};
}
