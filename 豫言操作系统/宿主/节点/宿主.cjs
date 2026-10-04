// 文言：豫言为入口，此层唯接 Node 与客值。汉语：Node 引擎及系统接口适配，不执行编译器算法。
'use strict';
const 文件 = require('node:fs'), 路径 = require('node:path'), 系统 = require('node:os');
const 子进程 = require('node:child_process'), 终端 = require('node:tty'), 密码 = require('node:crypto'), 工具 = require('node:util');
const {Worker, MessageChannel, isMainThread, workerData, threadId, parentPort} = require('node:worker_threads');
const {接管进程, 客体请求} = require('./进程桥接.cjs');
const {建立编译线程} = require('./编译线程.cjs');
const {创建终端输入} = require('./终端输入.cjs');
// 文言：同一宿主工序对同一 Wasm 只散列一次。汉语：同一 Node worker 对同一路径只计算一次内容 SHA-256。
const 当前程序SHA256缓存=new Map();
const 获取程序内容SHA256=名=>{const 全径=路径.resolve(名),旧=当前程序SHA256缓存.get(全径);if(旧!==undefined)return 旧;const 摘要=密码.createHash('sha256').update(文件.readFileSync(全径)).digest('hex');当前程序SHA256缓存.set(全径,摘要);return 摘要;};
// 文言：值桥先从父宿主所传之径，次取宿主之旁（仓库目标“豫构”书之），次求于今目录，末取解于仓根之工具链包者；定则录其绝对之径于环境，子进程承之，迁目录亦不失。汉语：值桥文件依次找：父宿主经环境变量 YY_NODE_VALUE_BRIDGE 传下的绝对路径；宿主文件旁的那份（仓库目标“豫构”用当前编译器生成）；当前目录；最后是解压在仓库根目录的 Wasm 工具链包里的 yy稳定节点宿主/yy节点值桥接.wasm（新检出的仓库第一次构建时用）。找到后把绝对路径写回该环境变量，子进程继承，切换工作目录后仍能找到。
const 桥文件路径=()=>{const 候选=[process.env.YY_NODE_VALUE_BRIDGE,路径.join(__dirname,'yy节点值桥接.wasm'),路径.resolve('yy节点值桥接.wasm'),路径.resolve('yy稳定节点宿主','yy节点值桥接.wasm')];const 径=候选.find(径=>径&&文件.existsSync(径))??候选[1];process.env.YY_NODE_VALUE_BRIDGE=径;return 径;};
const 选引擎参数=参数=>参数.filter(参=>/^--(?:no-)?(?:wasm-|liftoff)/.test(参)||/^--(?:v8-pool-size|initial-heap-size|initial-old-space-size|min-semi-space-size|max-semi-space-size|max-old-space-size)=/.test(参));
// 文言：客之工作线程另带 --experimental-ffi，令系统库调用之原语可用；引擎之旗通于全进程，不可亦不必传于工作线程，先去之；诺节不识此旗、或余旗不容于工作线程，则照旧承父之旗。汉语：客体工作线程另带 --experimental-ffi，让系统库调用原语（外部库.mjs）可用，node:ffi 到第一次调用才载入。V8 引擎旗对整个进程生效，不能也不必传给工作线程，先滤掉；诺节不认此旗、或余下的旗不能用于工作线程时，照旧继承父线程的旗。
const 外部库旗='--experimental-ffi';
const 新客线程=选项=>{if(process.allowedNodeEnvironmentFlags.has(外部库旗)&&!process.execArgv.includes(外部库旗)){try{return new Worker(__filename,{...选项,execArgv:[...process.execArgv.filter(参=>!选引擎参数([参]).length&&!/^--stack-size=/.test(参)),外部库旗]});}catch(错){if(错?.code!=='ERR_WORKER_INVALID_EXEC_ARGV')throw 错;}}return new Worker(__filename,选项);};
// 文言：系统库调用之原语，惟供适配与系统库；工具链包之宿主无此文则惟报不可用。汉语：系统库调用原语（见 外部库.mjs，只供适配与系统库使用）；Wasm 工具链包里的宿主没带这个文件时，只提供报告“不可用”的原语。
const 外部库原语=文字=>{try{return require('./外部库.mjs').创建外部库能力({文字});}catch(错){if(错?.code==='MODULE_NOT_FOUND'||错?.code==='ERR_MODULE_NOT_FOUND')return {豫言_节点_外部库可用:()=>false};throw 错;}};
// 文言：边界之胶水，先求于宿主之旁（工具链之包），次求于仓中网页汇编之目；无之则不能行带型导入之模。汉语：共用的宿主边界胶水 边界.mjs：Wasm 工具链包里与宿主放在同一目录，仓库里在 ../网页汇编/；找不到时，带类型导入的模块无法运行。
const 边界胶水=(()=>{for(const 径 of ['./边界.mjs','../网页汇编/边界.mjs']){try{return require(径);}catch(错){if(错?.code!=='MODULE_NOT_FOUND'&&错?.code!=='ERR_MODULE_NOT_FOUND')throw 错;}}return null;})();
// 文言：边界回环之夹具，惟仓中有之，供测试。汉语：边界回环测试的夹具（导入模块「测试」的实现），只在仓库里有，工具链包不带。
const 边界回环=(()=>{try{return require('./边界回环.cjs');}catch(错){if(错?.code==='MODULE_NOT_FOUND')return null;throw 错;}})();
// 文言：管或不阻，一书未必尽；余者续书，遇暂不可写则稍候，与应用宿主之写出同。汉语：标准流是非阻塞管道时，writeSync 可能只写入管道容得下的部分（常为 64KB）或报 EAGAIN；循环写完，写不进就短暂等待后重写（与 应用宿主.mjs 的「写出」相同），否则父进程经管道读取时输出被截断。
const 小候=new Int32Array(new SharedArrayBuffer(4));
const 写尽=(号,值)=>{const 字节=Buffer.isBuffer(值)?值:Buffer.from(值);for(let 位=0;位<字节.length;){try{位+=文件.writeSync(号,字节,位,字节.length-位);}catch(错){if(错?.code!=='EAGAIN')throw 错;Atomics.wait(小候,0,0,5);}}};
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
  const 工 = 新客线程({workerData: {参数:process.argv.slice(2), 端口:port2, 信号}, transferList:[port2], resourceLimits:{stackSizeMb:128}});
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
  const 文=值=>Buffer.isBuffer(值)?值.toString('utf8'):String(值);
  const 径=值=>路径.resolve(当前目录,文(值));
  const 数=值=>Number(值?.小数??值);
  const 列=诸值=>[诸值,诸值.length];
  const 可执行=名=>{try{文件.accessSync(名,文件.constants.X_OK);return 文件.statSync(名).isFile();}catch{return false;}};
  // 文言：线程之出各归其管。汉语：内部编译线程通过 Worker 标准流汇集输出，不能直接写共享进程的文件描述符。
  const 写输出=(号,值)=>本工.复用线程?parentPort.postMessage({种:'出',号,值}):本工.编译线程?(号===1?process.stdout:process.stderr).write(值):写尽(号,值);
  // 文言：底层之模导入内存、数学、诺节之函或出工作线程者，交底层宿主行之；工具链包之宿主无此文，则照旧行之。汉语：底层模块导入了内存或「数学」「诺节」的函数，或导出「工作线程」时，交 底层宿主.cjs 运行（建内存、起工作线程、调 _start）；Wasm 工具链包里的宿主不带这个文件，照旧运行。
  const 底层宿主=(()=>{try{return require('./底层宿主.cjs');}catch(错){if(错?.code==='MODULE_NOT_FOUND')return null;throw 错;}})();
  if(底层宿主?.是底层模块(模块))return 底层宿主.运行底层模块(模块,文件.readFileSync(模块路径),值=>写输出(1,值));
  function 精确小数(值){const 数字=数(值);if(Object.is(数字,-0))return '-0';if(!Number.isFinite(数字))return String(数字).toLowerCase().replace('infinity','inf');const [尾,指数]=数字.toExponential(16).split('e');const 幂=Number(指数);if(幂 < -4 || 幂 >= 17)return 尾.replace(/\.?0+$/,'')+'e'+(幂>=0?'+':'-')+String(Math.abs(幂)).padStart(2,'0');return 数字.toFixed(Math.max(0,16-幂)).replace(/(\.\d*?)0+$/,'$1').replace(/\.$/,'');}
  // 文言：小数表示依 C 之 %f，理解小数依 strtod，获取随机整数取六十四位之安全随机数而取余，皆与诸宿主同义。汉语：小数表示同 C 的 %f（-0 写 -0.000000，≥1e21 展开成整数，非有限写 nan、inf、-inf）；理解小数同 strtod（跳过开头空白取最长前缀，认 inf、infinity、nan，没有可转的得 0）；获取随机整数上界不大于零时中止，否则取 64 位安全随机数对上界取余。与其他 JS 宿主统一，见 ../标准库宿主.汉语.md。
  function 小数表示(值){const 数字=数(值);if(!Number.isFinite(数字))return 精确小数(值);if(Object.is(数字,-0))return '-0.000000';return Math.abs(数字)>=1e21?BigInt(数字).toString()+'.000000':数字.toFixed(6);}
  function 理解小数(值){const 串=文(值).trim(),数字=parseFloat(串);return /^[+-]?nan/i.test(串)?NaN:/^[+-]?inf/i.test(串)?(串.startsWith('-')?-Infinity:Infinity):Number.isNaN(数字)?0:数字;}
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
  function 取占锁(占径){
    for(;;){
      try{const 号=文件.openSync(占径,'wx',0o600);文件.writeSync(号,String(process.pid));文件.closeSync(号);return;}
      // 文言：视窗上他进程方删其锁，开之则报 EPERM（删除挂起），亦视为占用而后再试。汉语：Windows 上另一个进程正在删除锁文件时，以 wx 打开会报 EPERM（删除挂起）或 EACCES、EBUSY，同样当作“被占用”，稍后重试。
      catch(错){if(错.code!=='EEXIST'&&!(process.platform==='win32'&&['EPERM','EACCES','EBUSY'].includes(错.code)))throw 错;}
      let 持者=0;try{持者=Number(文件.readFileSync(占径,'utf8'));}catch{}
      if(持者&&持者!==process.pid){try{process.kill(持者,0);}catch{try{文件.unlinkSync(占径);}catch{}continue;}}
      Atomics.wait(小候,0,0,20);
    }
  }
  const 释占锁=占径=>{try{文件.unlinkSync(占径);}catch{}};
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
    豫言_获取命令行程序名:()=>模块路径,
    豫言_获取文件SHA256:名=>获取程序内容SHA256(径(名)),
    豫言_获取当前工作目录:()=>当前目录,
    豫言_切换当前工作目录:名=>{const 目标=径(名);try{if(!文件.statSync(目标).isDirectory())return [20n,'不是目录：'+目标];当前目录=目标;return [0n,''];}catch(错){return [BigInt(Math.abs(系统.constants.errno[错.code]??2)),错.message];}},
    豫言_获取文件修改时间:名=>BigInt(Math.floor(文件.statSync(径(名)).mtimeMs/1000)),
    豫言_获取当前程序SHA256:()=>获取程序内容SHA256(模块路径),
    豫言_获取环境变量:名=>[Object.hasOwn(process.env,文(名)),process.env[文(名)]??''],
    豫言_获取当前纳秒时间:()=>({小数:Number(process.hrtime.bigint())}),
    豫言_获取当前本地日期时间字符串:()=>{const 时=new Date();return `${时.getFullYear()}-${String(时.getMonth()+1).padStart(2,'0')}-${String(时.getDate()).padStart(2,'0')} ${String(时.getHours()).padStart(2,'0')}:${String(时.getMinutes()).padStart(2,'0')}:${String(时.getSeconds()).padStart(2,'0')}`;},
    豫言_格式化当前本地日期时间:格式=>{const 时=new Date(),补=数=>String(数).padStart(2,'0'),表={'%Y':String(时.getFullYear()),'%m':补(时.getMonth()+1),'%d':补(时.getDate()),'%H':补(时.getHours()),'%M':补(时.getMinutes()),'%S':补(时.getSeconds()),'%%':'%'};return 文(格式).replace(/%./g,项=>{if(!(项 in 表))throw Error('未支持日期格式 '+项);return 表[项];});},
    豫言_同步读取文件:名=>文件.readFileSync(径(名)),
    豫言_同步读取文件字节串:名=>文件.readFileSync(径(名)),
    // 文言：字节串之术一依原生运行时：越界则止，截取以起点与长度。汉语：字节串原语与原生运行时（字节串.c）语义一致：越界即报错，截取按起点与长度。
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
    豫言_获取随机整数:上界=>{const 界=BigInt(上界);if(界<=0n)throw Error('随机整数上界须大于零');return 密码.randomBytes(8).readBigUInt64BE()%界;},
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
    豫言_打印行:值=>{写输出(1,Buffer.concat([值,Buffer.from('\n')]));},
    豫言_标准错误打印行:值=>{写输出(2,Buffer.concat([值,Buffer.from('\n')]));},
    豫言_打印字符串:值=>{写输出(1,值);},
    豫言_小数转字符串:值=>小数表示(值),
    豫言_小数精确表示:值=>精确小数(值),
    豫言_字符串转小数:值=>({小数:理解小数(值)}),
    豫言_同步运行子进程并获取输出:运行子进程,
    // 文言：同步行之，成则阳；同原生，返爻而非退码。汉语：与原生一致：只返回是否成功（退出码为零），不返回退出码。
    豫言_同步运行子进程:(名,参)=>运行子进程(名,参).状态===0,
    豫言_同步运行子进程并传递输出:(名,参)=>{const 果=运行子进程(名,参);写输出(1,果[1]);写输出(2,果[2]);return 果.状态;},
    // 文言：承三常流而行，出不经宿主之缓；启败一二七，候败一。汉语：继承标准输入、输出与错误直接运行（输出不经宿主缓冲，交互程序可用）；启动失败返回 127，等待失败返回 1，超时返回 124。
    豫言_同步运行子进程并继承标准流:(名,参)=>{const [程序,参数组,客体,环境,限时]=子进程参数(名,参);const 果=子进程.spawnSync(程序,参数组,{cwd:当前目录,stdio:'inherit',timeout:限时>0?限时*1000:undefined,killSignal:'SIGKILL',env:客体?{...process.env,...(环境??{}),YY_NODE_REPEAT:'1'}:process.env});const 超时=限时>0&&果.error?.code==='ETIMEDOUT';if(超时)return 124;if(果.error)return 果.error.code==='ENOENT'?127:1;return 果.status??(果.signal?128+(系统.constants.signals[果.signal]??0):1);},
    豫言_退出进程:码=>{const 错=Error('客体退出');错.退出码=数(码);throw 错;},
    ...外部库原语(文)
  };
  // 文言：带型之导入，以边界胶水依签名包之；无实现者给桩。汉语：带类型的导入按「豫言边界」段里的签名由共用胶水包装；没有实现的给桩，调用时报“接口函数未绑定”。
  // 文言：标准库与构建基础之带型导入借旧原语，终端键与原始制另由终端输入模块供之；拼音与绘面板仍未绑定。汉语：标准库与构建基础的带类型导入复用旧原语，终端按键与原始模式由终端输入模块提供；拼音与绘制面板仍未绑定。
  const 缓=值=>Buffer.from(值.buffer,值.byteOffset,值.byteLength);
  const 去壳=值=>值?.小数??值;
  const 串参=名=>值=>原语[名](缓(值));
  const 无参=名=>()=>原语[名]();
  const 子参=参=>[参.map(缓)];
  const 终端输入 = 创建终端输入();
  const 标准库实现={
    进入终端原始输入模式:()=>终端输入.进入(),退出终端原始输入模式:()=>终端输入.退出(),读取终端按键:()=>终端输入.读取(),
    打印行:串参('豫言_打印行'),打印字符串:串参('豫言_打印字符串'),标准错误打印行:串参('豫言_标准错误打印行'),
    尝试读取标准输入行:无参('豫言_尝试读取标准输入行'),标准输出是终端:无参('豫言_标准输出是终端'),标准输入是终端:无参('豫言_标准输入是终端'),
    同步读取文件:串参('豫言_同步读取文件'),同步读取文件字节串:串参('豫言_同步读取文件字节串'),
    同步写入文件:(名,内容)=>原语.豫言_同步写入文件(缓(名),缓(内容)),同步写入文件字节串:(名,内容)=>原语.豫言_同步写入文件字节串(缓(名),缓(内容)),
    同步删除文件:串参('豫言_同步删除文件'),同步列出文件夹:名=>原语.豫言_同步列出文件夹(缓(名))[0],
    路径存在:串参('豫言_路径存在'),路径是文件夹:串参('豫言_路径是文件夹'),路径是普通文件:串参('豫言_路径是普通文件'),路径可执行:串参('豫言_路径可执行'),
    路径为符号链接:串参('豫言_路径为符号链接'),取得真实路径:串参('豫言_取得真实路径'),获取文件修改时间:串参('豫言_获取文件修改时间'),
    获取当前工作目录:无参('豫言_获取当前工作目录'),切换当前工作目录:串参('豫言_切换当前工作目录'),
    退出进程:码=>原语.豫言_退出进程(码),获取命令行程序名:无参('豫言_获取命令行程序名'),获取命令行参数:()=>客参数,获取环境变量:串参('豫言_获取环境变量'),
    同步运行子进程并继承标准流:(名,参)=>原语.豫言_同步运行子进程并继承标准流(缓(名),子参(参)),
    同步运行子进程并传递输出:(名,参)=>原语.豫言_同步运行子进程并传递输出(缓(名),子参(参)),
    同步运行子进程:(名,参)=>原语.豫言_同步运行子进程(缓(名),子参(参)),
    同步运行子进程并获取输出:(名,参)=>原语.豫言_同步运行子进程并获取输出(缓(名),子参(参)),
    查找可执行程序:串参('豫言_查找可执行程序'),
    启动异步子进程:(名,参)=>原语.豫言_启动异步子进程(缓(名),子参(参)),
    尝试收取异步子进程:号=>原语.豫言_尝试收取异步子进程(号),
    异步_输入输出多路等待:(关注,超时)=>{const 果=原语.豫言_异步_输入输出多路等待([关注],超时);return [果[0],果[1][0]];},
    运行于Windows:无参('豫言_运行于Windows'),运行于MacOS:无参('豫言_运行于MacOS'),运行于Linux:无参('豫言_运行于Linux'),在线处理器数量:无参('豫言_在线处理器数量'),
    获取当前纳秒时间:()=>去壳(原语.豫言_获取当前纳秒时间()),获取当前本地日期时间字符串:无参('豫言_获取当前本地日期时间字符串'),
    格式化当前本地日期时间:串参('豫言_格式化当前本地日期时间'),
    获取随机整数:上界=>原语.豫言_获取随机整数(上界),获取随机小数:()=>去壳(原语.豫言_获取随机小数()),安全随机_字节串:长=>原语.豫言_安全随机_字节串(长),
    小数转字符串:值=>原语.豫言_小数转字符串(值),小数精确表示:值=>原语.豫言_小数精确表示(值),字符串转小数:值=>去壳(原语.豫言_字符串转小数(缓(值))),
    传输控制协议_监听:(址,端口,队长)=>原语.豫言_传输控制协议_监听(缓(址),端口,队长),
    传输控制协议_开始连接:(主机,端口)=>原语.豫言_传输控制协议_开始连接(缓(主机),端口),
    传输控制协议_完成连接:号=>原语.豫言_传输控制协议_完成连接(号),传输控制协议_接受:号=>原语.豫言_传输控制协议_接受(号),
    传输控制协议_读取:(号,最大)=>原语.豫言_传输控制协议_读取(号,最大),传输控制协议_读取字节串:(号,最大)=>原语.豫言_传输控制协议_读取字节串(号,最大),
    传输控制协议_从字节序数写入:(号,内容,起)=>原语.豫言_传输控制协议_从字节序数写入(号,缓(内容),起),
    传输控制协议_从字节序数写入字节串:(号,内容,起)=>原语.豫言_传输控制协议_从字节序数写入字节串(号,缓(内容),起),
    传输控制协议_等待:(号,关注,超时)=>原语.豫言_传输控制协议_等待(号,关注,超时),传输控制协议_获取本地端口:号=>原语.豫言_传输控制协议_获取本地端口(号),
    传输控制协议_设置无延迟:(号,开)=>原语.豫言_传输控制协议_设置无延迟(号,开),传输控制协议_关闭写入:号=>原语.豫言_传输控制协议_关闭写入(号),
    传输控制协议_关闭:号=>原语.豫言_传输控制协议_关闭(号),传输控制协议_错误消息:状态=>原语.豫言_传输控制协议_错误消息(状态)
  };
  const 构建基础实现={存放包上下文:串参('豫言_存放包上下文'),可绘监视面板:无参('豫言_可绘监视面板'),获取当前程序SHA256:无参('豫言_获取当前程序SHA256'),获取文件SHA256:名=>原语.豫言_获取文件SHA256(缓(名))};
  // 文言：平台接口包「系统库调用」「安全外壳密码」之带型导入，亦借旧原语：串参转 Buffer，小数之果去壳，外部值之列展为数列；旧表所无者不立，调之则报未绑定。
  // 汉语：平台接口包「系统库调用」（外部库.mjs，十八个）与「安全外壳密码」（九个）的带类型导入，同样照搬旧原语：串参数转成 Buffer，小数结果去掉 {小数} 外壳；
  //   三个调用原语的参数是外部值的列，过边界后是 [[支序, 值], …]，展成旧原语要的数值数组（支序 2 是无值，报错）。旧表没有的（工具链包里缺 外部库.mjs 时）不提供，调用时报“接口函数未绑定”。
  const 展外部值=列=>列.map(([序,值])=>{if(序===2)throw Error('系统库调用：参数不能是无值');return 值;});
  const 平台实现=(对照)=>{const 表={};for(const[字段,[旧名,转]]of Object.entries(对照))if(typeof 原语[旧名]==='function')表[字段]=转(原语[旧名]);return 表;};
  const 原样=旧=>旧,串一=旧=>值=>旧(缓(值)),整串=旧=>(号,名)=>旧(号,缓(名)),整串整=旧=>(址,类,值)=>旧(址,缓(类),值),整整串=旧=>(址,字节)=>旧(址,缓(字节)),
    调用果=收=>旧=>(址,签名,参们)=>收(旧(址,缓(签名),展外部值(参们))),串二=旧=>(甲,乙)=>旧(缓(甲),缓(乙)),串三=旧=>(甲,乙,丙)=>旧(缓(甲),缓(乙),缓(丙)),
    串四=旧=>(甲,乙,丙,丁)=>旧(缓(甲),缓(乙),缓(丙),缓(丁));
  const 系统库调用实现=平台实现({
    外部库可用原语:['豫言_节点_外部库可用',原样],外部库打开原语:['豫言_节点_外部库打开',串一],外部库取符号原语:['豫言_节点_外部库取符号',整串],
    外部调用得整数原语:['豫言_节点_外部库调用',调用果(果=>果)],外部调用得小数原语:['豫言_节点_外部库调用',调用果(去壳)],
    外部调用无返回原语:['豫言_节点_外部库调用',调用果(()=>undefined)],
    外部库分配原语:['豫言_节点_外部库分配',原样],外部库释放原语:['豫言_节点_外部库释放',原样],
    外部库读整数原语:['豫言_节点_外部库读整数',整串],外部库写整数原语:['豫言_节点_外部库写整数',整串整],
    外部库读小数原语:['豫言_节点_外部库读小数',旧=>(址,类)=>去壳(旧(址,缓(类)))],外部库写小数原语:['豫言_节点_外部库写小数',整串整],
    外部库读字节原语:['豫言_节点_外部库读字节',原样],外部库写字节原语:['豫言_节点_外部库写字节',整整串],外部库复制原语:['豫言_节点_外部库复制',原样],
    外部库读文字原语:['豫言_节点_外部库读文字',原样],外部库张量存储原语:['豫言_节点_外部库张量存储',串一],外部库张量视图原语:['豫言_节点_外部库张量视图',串一]
  });
  const 安全外壳密码实现=平台实现({
    求SHA256:['豫言_密码_SHA256',串一],求已知主机HMACSHA1:['豫言_密码_已知主机HMACSHA1',串二],求X25519公钥:['豫言_密码_X25519公钥',串一],
    求X25519共密:['豫言_密码_X25519共密',串二],求Ed25519公钥:['豫言_密码_Ed25519公钥',串一],签Ed25519:['豫言_密码_Ed25519签',串二],
    验Ed25519:['豫言_密码_Ed25519验',串三],加密AES256GCM:['豫言_密码_AES256GCM加密',串四],解密AES256GCM结果:['豫言_密码_AES256GCM解密结果',串四]
  });
  const 带型导入=边界胶水?边界胶水.造边界导入(模块,桥,{标准库:标准库实现,构建基础:构建基础实现,系统库调用:系统库调用实现,安全外壳密码:安全外壳密码实现,
    ...(边界回环?.实现表??{})}):{};
  const 实例=new WebAssembly.Instance(模块,{...带型导入});
  let 退出码=0;
  // 文言：_start 毕，应用实现启动之术者，乃调其导出。汉语：_start 初始化各模块之后，应用若实现了《豫言操作系统启动》的「启动程序」（导出 豫言操作系统启动/启动程序），就调用它；旧产物没有这个导出，照旧只跑 _start。
  try{实例.exports._start();const 导出=边界胶水?边界胶水.造边界导出(实例,模块,桥):{};const 启动=边界胶水?导出[边界胶水.启动导出名]:undefined;if(启动)启动();}catch(错){if(错.退出码===undefined)throw 错;退出码=错.退出码;}finally{终端输入.退出();}
  // 文言：承构志之工务，书定标行；显设 YY_NODE_STATS 者，仍书旧详数。旧通调已删，其调用数为零。汉语：仅继承构建日志的 worker 写 marker 行；显式 YY_NODE_STATS 仍保留详细 JSON；宿主调用数字段计旧通用调用，删除后为零。
  const 工务任务=本工.编译线程||本工.参数?.includes('--mode=worker');
  const 工务统计=工务任务&&!!process.env.YY_BUILD_LOG_RUN;
  if(!工务统计&&process.env.YY_NODE_STATS!=='1')return {模块,退出码};
  if(工务统计){
    const 统计=JSON.stringify({模块准备毫秒:Math.round(编译毕-开始),执行毫秒:Math.round(performance.now()-编译毕),宿主调用数:0});
    const 行='YY_BUILD_TASK_STATS\t'+统计;
    if(本工.复用线程)写输出(2,Buffer.from(行+'\n'));else console.error(行);
  }else{
    const 统计=JSON.stringify({轮次:轮,复用模块:!!缓存,引擎:process.versions.v8,进程:process.pid,线程:threadId,编译工作模式:process.env.YY_NODE_COMPILER_WORKERS??'threads',模块准备毫秒:编译毕-开始,执行毫秒:performance.now()-编译毕,宿主调用数:0});
    if(本工.复用线程)写输出(2,Buffer.from(统计+'\n'));else console.error(统计);
  }
  return {模块,退出码};
}
