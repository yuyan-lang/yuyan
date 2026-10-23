// 文言：豫言为入口，此层唯接 Node 与客值。汉语：Node 引擎及系统接口适配，不执行编译器算法。
'use strict';
const 文件 = require('node:fs'), 路径 = require('node:path'), 系统 = require('node:os');
const 子进程 = require('node:child_process'), 终端 = require('node:tty'), 密码 = require('node:crypto'), 工具 = require('node:util');
const {Worker, MessageChannel, isMainThread, workerData, threadId, parentPort} = require('node:worker_threads');
// 文言：稳定之具（名以 _stable.wasm 终者）所导乃旧之宿主接口名，交工具链之包自带之宿主行之，先求于其旁，次求于今目录。汉语：稳定工具（文件名以 _stable.wasm 结尾）导入的是旧的宿主接口名，交给 Wasm 工具链包自带的 稳定节点宿主/宿主.cjs 运行（提案 00013 第 4.4 节）；包内宿主先找稳定工具旁边的，再找当前目录下的。
if(isMainThread&&/_stable\.wasm$/.test(process.argv[2]??'')){const 稳定宿主=[路径.resolve(路径.dirname(process.argv[2]),'稳定节点宿主','宿主.cjs'),路径.resolve('稳定节点宿主','宿主.cjs')].find(径=>径!==__filename&&文件.existsSync(径));if(稳定宿主){require(稳定宿主);return;}}
const {接管进程, 客体请求} = require('./进程桥接.cjs');
const {建立编译线程, 线程资源限额} = require('./编译线程.cjs');
const {创建终端输入} = require('./终端输入.mjs');
const {读取标准终端尺寸} = require('./终端尺寸.mjs');
const {创建文件能力, 提取文件授权} = require('./文件能力.mjs');
const {创建传输控制协议能力} = require('./传输控制协议.mjs');
let 共享时钟;
// 汉语：公共墙钟复用原生共享核心，首次调用时载入，保留全六十四位纪元毫秒。文言：公墙钟复原生共核，初调乃载，存全六十四位纪元毫秒。
const 读取共享纪元毫秒=()=>{
  if(!共享时钟){
    const 径=[process.env.YY传输控制共享,路径.join(__dirname,'传输控制共享.wasm'),路径.resolve('传输控制共享.wasm')].find(名=>名&&文件.existsSync(名));
    共享时钟=创建传输控制协议能力({模块字节:径?文件.readFileSync(径):null});
  }
  return 共享时钟.读取当前纪元毫秒();
};
// 文言：同一宿主工序对同一 Wasm 只散列一次。汉语：同一 Node worker 对同一路径只计算一次内容 SHA-256。
const 当前程序SHA256缓存=new Map();
const 获取程序内容SHA256=名=>{const 全径=路径.resolve(名),旧=当前程序SHA256缓存.get(全径);if(旧!==undefined)return 旧;const 摘要=密码.createHash('sha256').update(文件.readFileSync(全径)).digest('hex');当前程序SHA256缓存.set(全径,摘要);return 摘要;};
// 文言：值桥先从父宿主所传之径，次取宿主之旁（仓库目标“豫构”书之），次求于今目录，末取解于仓根之工具链包者；定则录其绝对之径于环境，子进程承之，迁目录亦不失。汉语：值桥文件依次找：父宿主经环境变量 豫言诺驿值桥 传下的绝对路径；宿主文件旁的那份（仓库目标“豫构”用当前编译器生成）；当前目录；最后是解压在仓库根目录的 Wasm 工具链包里的 稳定节点宿主/节点值桥接.wasm（新检出的仓库第一次构建时用）。找到后把绝对路径写回该环境变量，子进程继承，切换工作目录后仍能找到。
const 桥文件路径=()=>{const 候选=[process.env.豫言诺驿值桥,路径.join(__dirname,'节点值桥接.wasm'),路径.resolve('节点值桥接.wasm'),路径.resolve('稳定节点宿主','节点值桥接.wasm')];const 径=候选.find(径=>径&&文件.existsSync(径))??候选[1];process.env.豫言诺驿值桥=径;return 径;};
const 选引擎参数=参数=>参数.filter(参=>/^--(?:no-)?(?:wasm-|liftoff)/.test(参)||/^--(?:v8-pool-size|initial-heap-size|initial-old-space-size|min-semi-space-size|max-semi-space-size|max-old-space-size)=/.test(参));
// 文言：客之工作线程另带 --experimental-ffi，令系统库调用之原语可用；引擎之旗通于全进程，不可亦不必传于工作线程，先去之；诺驿不识此旗、或余旗不容于工作线程，则照旧承父之旗。汉语：客体工作线程另带 --experimental-ffi，让系统库调用原语（外部库.mjs）可用，node:ffi 到第一次调用才载入。V8 引擎旗对整个进程生效，不能也不必传给工作线程，先滤掉；诺驿不认此旗、或余下的旗不能用于工作线程时，照旧继承父线程的旗。
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
  const 模式=process.env.豫言诺驿编译工作模式??'threads';
  if(!['process','threads'].includes(模式))throw Error('豫言诺驿编译工作模式 须为 process 或 threads');
  const 启动编译 = 模式==='threads'?建立编译线程(__filename,process.argv[2],选引擎参数(process.execArgv)):null;
  const 清桥 = 接管进程(port1, 信号, 启动编译);
  const 清理 = () => {清桥(); 启动编译?.清理?.();};
  let 终止码 = null;
  const 工 = 新客线程({workerData: {参数:process.argv.slice(2), 端口:port2, 信号}, transferList:[port2], resourceLimits:线程资源限额});
  工.on('error', 错 => {console.error(错); process.exitCode=1;});
  工.on('exit', 码 => {清理(); process.exitCode=终止码??码;});
  // 文言：试以时段置断标：环境 豫言打断毫秒 有正数时，得工线程之共享存后，过其毫秒置零号字。
  // 汉语：测试用：环境变量 豫言打断毫秒 为正数时，收到工作线程的共享内存后，过这么多毫秒置零号字（只供验证检查点，正常使用不设）。
  let 设标已候 = false;
  工.on('message', 消息 => {
    if (消息?.种 !== '打断内存' || 设标已候) return;
    const 毫秒 = Number(process.env.豫言打断毫秒);
    if (!(毫秒 > 0)) return;
    设标已候 = true;
    const 字 = new Int32Array(消息.缓冲);
    setTimeout(() => Atomics.store(字, 0, 1), 毫秒);
  });
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
    const 轮数=workerData.编译线程?1:Number(process.env.豫言诺驿轮数??1);
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
  // 汉语：目录授权为显式宿主参数，移出客参数；复用应用宿主同一文件能力。文言：目录之授为明示宿主之参，移出客参；共应用宿主之文件能。
  const 文件配置=提取文件授权(参数,本工.初始目录??process.cwd());参数=文件配置.参数;
  const 文件能力=创建文件能力({授权:文件配置.授权});
  const 模块路径=路径.resolve(参数[0]), 客参数=参数.slice(1);
  // 文言：客自有今目录；工线程不能迁进程之目录，故宿主代记之，诸径与子进程皆依之。汉语：每个客实例有自己的当前目录；工作线程不能调用 process.chdir，所以由宿主记录，文件路径与子进程的工作目录都以它为准。
  let 当前目录=本工.初始目录??process.cwd();
  const 桥=new WebAssembly.Instance(本工.桥模块??new WebAssembly.Module(文件.readFileSync(桥文件路径()))).exports;
  // 文言：客为装载单元之目（有“顺序.txt”）者，不读单模，后依序装诸单元（提案〇〇〇〇三）。汉语：程序是装载单元目录（里面有“顺序.txt”）时不读单个模块，稍后按顺序装入各单元（提案 00003）。
  const 单元目录=!缓存&&!本工.模块&&文件.existsSync(路径.join(模块路径,'顺序.txt'))?模块路径:null;
  const 模块=单元目录?null:缓存??本工.模块??new WebAssembly.Module(文件.readFileSync(模块路径));
  // 文言：单元之目中有“程序名.txt”者（豫构「运行」所书，即脚本之径），客所见之程名取之。汉语：单元目录里有“程序名.txt”时（豫构「运行」写的脚本路径），客体看到的程序名取它。
  const 程序名=单元目录&&文件.existsSync(路径.join(单元目录,'程序名.txt'))?文件.readFileSync(路径.join(单元目录,'程序名.txt'),'utf8'):模块路径;
  // 文言：装网汇单元行其 _start 时，暂以单元之参与程名代本程之参与名，返则复之（提案〇〇〇四三）。汉语：「装网汇单元」运行单元的 _start 时，暂把命令行参数与程序名换成单元的，返回后恢复（提案 00043）。
  let 今程序名=程序名, 今客参数=客参数;
  const 编译毕=performance.now();
  const 文=值=>Buffer.isBuffer(值)?值.toString('utf8'):String(值);
  const 径=值=>路径.resolve(当前目录,文(值));
  const 数=值=>Number(值?.小数??值);
  const 列=诸值=>[诸值,诸值.length];
  const 可执行=名=>{try{文件.accessSync(名,文件.constants.X_OK);return 文件.statSync(名).isFile();}catch{return false;}};
  // 文言：线程之出各归其管。汉语：内部编译线程通过 Worker 标准流汇集输出，不能直接写共享进程的文件描述符。
  const 写输出=(号,值)=>本工.复用线程?parentPort.postMessage({种:'出',号,值}):本工.编译线程?(号===1?process.stdout:process.stderr).write(值):写尽(号,值);
  // 文言：底层之模导入内存、数学、诺驿之函或出工作线程者，交底层宿主行之；工具链包之宿主无此文，则照旧行之。汉语：底层模块导入了内存或「数学」「诺驿」的函数，或导出「工作线程」时，交 底层宿主.cjs 运行（建内存、起工作线程、调 _start）；Wasm 工具链包里的宿主不带这个文件，照旧运行。
  const 底层宿主=(()=>{try{return require('./底层宿主.cjs');}catch(错){if(错?.code==='MODULE_NOT_FOUND')return null;throw 错;}})();
  if(模块&&底层宿主?.是底层模块(模块))return 底层宿主.运行底层模块(模块,文件.readFileSync(模块路径),值=>写输出(1,值));
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
      if(名==='env'){let 位=1;while(位<余.length&&/^[\p{L}_][\p{L}\p{N}_]*=/u.test(余[位])){const 等=余[位].indexOf('=');环境[余[位].slice(0,等)]=余[位].slice(等+1);位++;}余=余.slice(位);continue;}
      if(名==='timeout'){let 位=1;while(位<余.length&&余[位].startsWith('-')){if(余[位]==='-k'){宽限=Number(余[位+1]);位+=2;}else if(余[位]==='-s')位+=2;else 位++;}限时=Number(余[位]);余=余.slice(位+1);continue;}
      break;
    }
    return {程序:余[0]??'',参数组:余.slice(1),环境,限时,宽限};
  }
  // 文言：网页汇编之模，以其首四字节 \0asm 识之，不拘其名之后缀。汉语：按文件开头 4 字节 \0asm 识别 Wasm 模块，不依赖 .wasm 后缀（豫构的 --输出 可以是任意名字）。
  // 文言：装载单元之目（有“顺序.txt”）亦视为客，以本宿主行之（提案〇〇〇〇三第六步）。汉语：装载单元目录（里面有“顺序.txt”）也当作客体程序，由本宿主运行（提案 00003 第 6 步）。
  const 是网页汇编=名=>{if(名.endsWith('.wasm'))return true;try{if(文件.existsSync(路径.join(路径.resolve(当前目录,名),'顺序.txt')))return true;}catch{}try{const 号=文件.openSync(路径.resolve(当前目录,名),'r');const 头=Buffer.alloc(4);const 数=文件.readSync(号,头,0,4,0);文件.closeSync(号);return 数===4&&头.equals(Buffer.from([0,0x61,0x73,0x6d]));}catch{return false;}};
  function 子进程参数(名,参){let 程序=文(名),参数组=参[0].map(文),环境=null,限时=0,宽限=0;const 拆=拆包装(程序,参数组);if(拆.程序!==程序&&是网页汇编(拆.程序)){({程序,参数组}=拆);环境=拆.环境;限时=拆.限时;宽限=拆.宽限;}const 客体=路径.resolve(当前目录,程序)===模块路径 || 是网页汇编(程序);if(客体){参数组=[...引擎参数,__filename,路径.resolve(当前目录,程序),...参数组];程序=process.execPath;}return [程序,参数组,客体,环境,限时,宽限];}
  function 运行子进程(名,参){const [程序,参数组,客体,环境,限时]=子进程参数(名,参);const 果=子进程.spawnSync(程序,参数组,{cwd:当前目录,maxBuffer:256*1024*1024,timeout:限时>0?限时*1000:undefined,killSignal:'SIGKILL',env:客体?{...process.env,...(环境??{}),豫言诺驿轮数:'1'}:process.env});const 超时=限时>0&&果.error?.code==='ETIMEDOUT';if(果.error&&!超时)throw 果.error;const 状态=超时?124:(果.status??(果.signal?128+(系统.constants.signals[果.signal]??0):1));const 结果=[状态===0,果.stdout??Buffer.alloc(0),果.stderr??Buffer.alloc(0)];结果.状态=状态;return 结果;}
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
      // 文言：轩窗上他进程方删其锁，开之则报 EPERM（删除挂起），亦视为占用而后再试。汉语：Windows 上另一个进程正在删除锁文件时，以 wx 打开会报 EPERM（删除挂起）或 EACCES、EBUSY，同样当作“被占用”，稍后重试。
      catch(错){if(错.code!=='EEXIST'&&!(process.platform==='win32'&&['EPERM','EACCES','EBUSY'].includes(错.code)))throw 错;}
      let 持者=0;try{持者=Number(文件.readFileSync(占径,'utf8'));}catch{}
      if(持者&&持者!==process.pid){try{process.kill(持者,0);}catch{try{文件.unlinkSync(占径);}catch{}continue;}}
      Atomics.wait(小候,0,0,20);
    }
  }
  const 释占锁=占径=>{try{文件.unlinkSync(占径);}catch{}};
  const 原语={
    // 文言：同文用旧号，异文另立号，既存不改（同原生 包上下文.c）。汉语：与原生 包上下文.c 相同：在当前目录的 .豫构缓存/豫构上下文 里逐字比较，内容相同就复用已有编号，否则新建下一个编号；加锁防并发，返回真实路径。
    豫言_存放包上下文:内容=>{
      const 目录=路径.resolve(当前目录,'.豫构缓存/豫构上下文');文件.mkdirSync(目录,{recursive:true,mode:0o700});
      const 占径=路径.join(目录,'锁.占');取占锁(占径);
      try{
        for(let 号=1000+(()=>{let 散=5381;for(let 位=0;位<内容.length;位++)散=(Math.imul(散,33)+内容[位])>>>0;return 散%1000000;})();;号++){
          const 文径=路径.join(目录,号+'.上下文');
          // 文言：起号依其文之 djb2 散（三十二位）取一千加散除百万之余，与原生执行器同法。汉语：起始号按内容的 djb2 散列（三十二位，h×33+字节，初值 5381）取 1000 加散列除以一百万的余数，与原生执行器相同，同内容两边得同号，多数一次就中。
          // 文言：长异者不读其文，径试次号。汉语：长度不同就不读内容，直接试下一个号（目录里常有上百个旧上下文，每个任务都要从 1 号找起）。
          const 状=文件.statSync(文径,{throwIfNoEntry:false});
          if(!状){文件.writeFileSync(文径,内容,{flag:'wx',mode:0o600});return 文件.realpathSync(文径);}
          if(状.size!==内容.length)continue;
          if(文件.readFileSync(文径).equals(内容))return 文件.realpathSync(文径);
        }
      }finally{释占锁(占径);}
    },
    豫言_获取命令行程序名:()=>今程序名,
    豫言_获取文件沙256:名=>获取程序内容SHA256(径(名)),
    豫言_获取当前工作目录:()=>当前目录,
    豫言_切换当前工作目录:名=>{const 目标=径(名);try{if(!文件.statSync(目标).isDirectory())return [20n,'不是目录：'+目标];当前目录=目标;return [0n,''];}catch(错){return [BigInt(Math.abs(系统.constants.errno[错.code]??2)),错.message];}},
    豫言_获取文件修改时间:名=>BigInt(Math.floor(文件.statSync(径(名)).mtimeMs/1000)),
    豫言_获取当前程序沙256:()=>获取程序内容SHA256(模块路径),
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
    // 文言：设执行之位：所有者可读书行，余者可读可行（〇七五五）。汉语：设执行位：所有者可读写执行、其余可读执行（0o755），与原生执行器的 fchmodat 同。
    豫言_同步设置可执行:名=>文件.chmodSync(径(名),0o755),
    豫言_同步改名文件:(旧,新)=>文件.renameSync(径(旧),径(新)),
    豫言_同步列出文件夹:名=>列(['.','..',...文件.readdirSync(径(名))]),
    豫言_路径存在:名=>文件.existsSync(径(名)),
    豫言_路径是文件夹:名=>文件.statSync(径(名),{throwIfNoEntry:false})?.isDirectory()??false,
    豫言_路径是普通文件:名=>文件.statSync(径(名),{throwIfNoEntry:false})?.isFile()??false,
    豫言_路径为符号链接:名=>文件.lstatSync(径(名)).isSymbolicLink(),
    豫言_取得真实路径:名=>文件.realpathSync(径(名)),
    豫言_路径可执行:名=>可执行(径(名)),
    豫言_查找可执行程序:名=>{const 候选=文(名).includes('/')?[径(名)]:(process.env.PATH??'').split(':').map(径=>路径.join(径,文(名)));const 找到=候选.find(可执行);return [!!找到,找到?路径.resolve(找到):''];},
    豫言_在线处理器数量:()=>系统.availableParallelism(),
    '豫言_空闲物理内存块数':()=>-1,
    豫言_启动异步子进程:(名,参)=>{const [程序,参数组,客体,环境,限时,宽限]=子进程参数(名,参);return 请求进程('启动',程序,参数组,客体,当前目录,环境,限时,宽限);},
    // 文言：遣使于境：所居与环境之块随使而行；块为零终之“名=值”诸条，尽易其俗，空则承之（提案 00035）。汉语：带工作目录与环境启动（提案 00035）：环境块是若干以零结尾的“名=值”，非空时替换子进程的全部环境，空串继承；目录空串为当前目录。
    豫言_遣使于境:(名,参,目,境)=>{const [程序,参数组,客体,环境,限时,宽限]=子进程参数(名,参);const 全境=文(境);const 境对=全境?Object.fromEntries(全境.split('\0').filter(Boolean).map(项=>{const 位=项.indexOf('=');return 位<0?[项,'']:[项.slice(0,位),项.slice(位+1)];})):环境;return 请求进程('启动',程序,参数组,客体,路径.resolve(当前目录,文(目)||'.'),境对,限时,宽限);},
    豫言_风俗:()=>Buffer.from(Object.entries(process.env).map(([名,值])=>名+'='+值+'\0').join('')),
    豫言_尝试收取异步子进程:号=>请求进程('收取',号),
    豫言_终止异步子进程:号=>请求进程('终止',号),
    豫言_查询异步子进程资源:号=>请求进程('资源',号),
    // 文言：此桥唯候自身子进程。汉语：未知句柄返回错误事件，不冒充通用网络或文件描述符轮询。
    豫言_异步输入输出多路等待:(关注,超时)=>请求进程('等待',关注[0],数(超时)),
    // 文言：传输控制协议之术，由主线程持套接字而行之，皆不塞，同原生。汉语：TCP 原语：套接字由主线程持有（进程桥接.cjs），语义与原生运行时相同，均不阻塞，返回（状态，值）。
    豫言_传输控制协议监听:(址,端口,队长)=>请求进程('网监听',文(址),数(端口),数(队长)),
    豫言_传输控制协议开始连接:(主机,端口)=>请求进程('网开始连接',文(主机),数(端口)),
    豫言_传输控制协议完成连接:号=>请求进程('网完成连接',数(号)),
    豫言_传输控制协议接受:号=>请求进程('网接受',数(号)),
    豫言_传输控制协议读取:(号,最大)=>请求进程('网读取',数(号),数(最大)),
    豫言_传输控制协议读取字节串:(号,最大)=>请求进程('网读取',数(号),数(最大)),
    豫言_传输控制协议从字节序数写入:(号,内容,起)=>请求进程('网写入',数(号),内容,数(起)),
    豫言_传输控制协议从字节序数写入字节串:(号,内容,起)=>请求进程('网写入',数(号),内容,数(起)),
    豫言_传输控制协议等待:(号,关注,超时)=>请求进程('网等待',数(号),数(关注),数(超时)),
    豫言_传输控制协议获取本地端口:号=>请求进程('网本地端口',数(号)),
    豫言_传输控制协议设置无延迟:(号,开)=>请求进程('网无延迟',数(号),数(开)),
    豫言_传输控制协议关闭写入:号=>请求进程('网关闭写入',数(号)),
    豫言_传输控制协议关闭:号=>请求进程('网关闭',数(号)),
    豫言_传输控制协议错误消息:状态=>{const 码=数(状态);if(码<=-19900&&码>=-20100)return '地址解析失败';try{return 工具.getSystemErrorMessage(码<0?码:-码);}catch{return '未知传输控制协议错误';}},
    豫言_运行于视窗:()=>process.platform==='win32',
    豫言_运行于苹果:()=>process.platform==='darwin',
    豫言_运行于林纳克斯:()=>process.platform==='linux',
    豫言_运行于豫言操作系统:()=>false,
    豫言_可绘监视面板:()=>false,
    豫言_标准输出是终端:()=>终端.isatty(1),
    豫言_标准输入是终端:()=>终端.isatty(0),
    豫言_尝试读取标准输入行:()=>读标准输入行(),
    // 文言：常随机不求安全；安全随机取于系统之熵，一请至多一兆字节。汉语：普通随机数用 Math.random（不保证安全）；安全随机字节取自系统熵源，单次至多 1 MiB，与原生一致。
    豫言_获取随机整数:上界=>{const 界=BigInt(上界);if(界<=0n)throw Error('随机整数上界须大于零');return 密码.randomBytes(8).readBigUInt64BE()%界;},
    豫言_获取随机小数:()=>({小数:Math.random()}),
    豫言_安全随机字节串:长=>{const 数值=Number(长);if(!Number.isInteger(数值)||数值<0||数值>1048576)throw Error('安全随机字节串：长度须在零至一兆之间');return 密码.randomBytes(数值);},
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
    豫言_同步运行子进程并继承标准流:(名,参)=>{const [程序,参数组,客体,环境,限时]=子进程参数(名,参);const 果=子进程.spawnSync(程序,参数组,{cwd:当前目录,stdio:'inherit',timeout:限时>0?限时*1000:undefined,killSignal:'SIGKILL',env:客体?{...process.env,...(环境??{}),豫言诺驿轮数:'1'}:process.env});const 超时=限时>0&&果.error?.code==='ETIMEDOUT';if(超时)return 124;if(果.error)return 果.error.code==='ENOENT'?127:1;return 果.status??(果.signal?128+(系统.constants.signals[果.signal]??0):1);},
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
    入素:()=>终端输入.进入(),出素:()=>终端输入.退出(),听键:()=>终端输入.读取(),瞥键:()=>[false,''],/* 待办事项：诺驿上瞥键（不等待地读按键）总返回无键 */
    书:串参('豫言_打印行'),续书:串参('豫言_打印字符串'),诉:串参('豫言_标准错误打印行'),
    闻:无参('豫言_尝试读取标准输入行'),亲书:无参('豫言_标准输出是终端'),亲闻:无参('豫言_标准输入是终端'),
    阅:串参('豫言_同步读取文件'),阅帛:串参('豫言_同步读取文件字节串'),
    // 汉语：卷大小取状态里的字节数，不存在或不是普通文件时给 -1；阅帛片按偏移定位读，只读这一段（2026-10-22，议题 351）。
    // 文言：卷大小取状中之字节数，不存或非常文则给负一；阅帛片依偏定位而读，唯此一段。
    卷大小:名=>{const 状=文件.statSync(径(名),{throwIfNoEntry:false});return 状&&状.isFile()?BigInt(状.size):BigInt(-1);},
    阅帛片:(名,偏移,长)=>{const 限=Number(长);if(!(限>0))return Buffer.alloc(0);const 柄=文件.openSync(径(名),'r');try{const 缓=Buffer.alloc(限);const 读=文件.readSync(柄,缓,0,限,Number(偏移));return 读===限?缓:缓.subarray(0,读);}finally{文件.closeSync(柄);}},
    著:(名,内容)=>原语.豫言_同步写入文件(缓(名),缓(内容)),著帛:(名,内容)=>原语.豫言_同步写入文件字节串(缓(名),缓(内容)),
    焚:串参('豫言_同步删除文件'),更名:(旧,新)=>原语.豫言_同步改名文件(缓(旧),缓(新)),列目:名=>原语.豫言_同步列出文件夹(缓(名))[0],
    许行:串参('豫言_同步设置可执行'),
    实有:串参('豫言_路径存在'),为箧:串参('豫言_路径是文件夹'),为卷:串参('豫言_路径是普通文件'),能行:串参('豫言_路径可执行'),
    为影:串参('豫言_路径为符号链接'),真径:串参('豫言_取得真实路径'),改时:串参('豫言_获取文件修改时间'),
    所居:无参('豫言_获取当前工作目录'),迁:串参('豫言_切换当前工作目录'),
    辞:码=>原语.豫言_退出进程(码),自号:无参('豫言_获取命令行程序名'),受命:()=>今客参数,问俗:串参('豫言_获取环境变量'),
    亲行:(名,参)=>原语.豫言_同步运行子进程并继承标准流(缓(名),子参(参)),
    传行:(名,参)=>原语.豫言_同步运行子进程并传递输出(缓(名),子参(参)),
    成行:(名,参)=>原语.豫言_同步运行子进程(缓(名),子参(参)),
    咨:(名,参)=>原语.豫言_同步运行子进程并获取输出(缓(名),子参(参)),
    访:串参('豫言_查找可执行程序'),
    遣使:(名,参)=>原语.豫言_启动异步子进程(缓(名),子参(参)),
    遣使于境:(名,参,目,境)=>原语.豫言_遣使于境(缓(名),子参(参),缓(目),缓(境)),风俗:无参('豫言_风俗'),
    // 文言：多播之报门（提案〇〇〇三五邻籍）诺驿未备，返负五十八。汉语：多播数据报（提案 00035 邻籍）诺驿上不提供，返回 −58。
    开播报:()=>[-58,-1],投报:()=>[-58,0],收报:()=>[-58,0,0,Buffer.alloc(0)],
    收使:号=>原语.豫言_尝试收取异步子进程(号),
    召回:号=>原语.豫言_终止异步子进程(号),
    问使:号=>原语.豫言_查询异步子进程资源(号),
    群候:(关注,超时)=>{const 果=原语.豫言_异步输入输出多路等待([关注],超时);return [果[0],果[1][0]];},
    寓轩窗:无参('豫言_运行于视窗'),寓林檎:无参('豫言_运行于苹果'),寓林纳:无参('豫言_运行于林纳克斯'),寓豫言:无参('豫言_运行于豫言操作系统'),核数:无参('豫言_在线处理器数量'),闲块数:无参('豫言_空闲物理内存块数'),
    此刻:()=>去壳(原语.豫言_获取当前纳秒时间()),今时:无参('豫言_获取当前本地日期时间字符串'),
    今时依式:串参('豫言_格式化当前本地日期时间'),
    // 汉语：读墙钟秒毫（2026-10-21）：取宿主时钟的整秒与秒内毫秒；设墙钟偏移、注起居、读墙钟偏移只在豫言操作系统里有意义，诺驿上返回 58（不支持），读偏移给（0，0）。
    // 汉语：提句柄上限（2026-10-23，议题 369）：只在豫言操作系统里有实现，诺驿上返回 -1（没有这套限额）；文言：提句柄之上限唯于豫言操作系统有其实，诺驿返负一。
    读墙钟秒毫:()=>{const 毫=BigInt(Date.now());return [毫/1000n,毫%1000n];},设墙钟偏移:()=>58n,注起居:()=>58n,读墙钟偏移:()=>[0n,0n],提句柄上限:()=>-1n,
    抽数:上界=>原语.豫言_获取随机整数(上界),抽小数:()=>去壳(原语.豫言_获取随机小数()),密抽帛:长=>原语.豫言_安全随机字节串(长),
    述小数:值=>原语.豫言_小数转字符串(值),精述小数:值=>原语.豫言_小数精确表示(值),识小数:值=>去壳(原语.豫言_字符串转小数(缓(值))),
    开门:(址,端口,队长)=>原语.豫言_传输控制协议监听(缓(址),端口,队长),
    叩门:(主机,端口)=>原语.豫言_传输控制协议开始连接(缓(主机),端口),
    入门:号=>原语.豫言_传输控制协议完成连接(号),迎:号=>原语.豫言_传输控制协议接受(号),
    听:(号,最大)=>原语.豫言_传输控制协议读取(号,最大),听帛:(号,最大)=>原语.豫言_传输控制协议读取字节串(号,最大),
    续告:(号,内容,起)=>原语.豫言_传输控制协议从字节序数写入(号,缓(内容),起),
    续告帛:(号,内容,起)=>原语.豫言_传输控制协议从字节序数写入字节串(号,缓(内容),起),
    守候:(号,关注,超时)=>原语.豫言_传输控制协议等待(号,关注,超时),门号:号=>原语.豫言_传输控制协议获取本地端口(号),
    勿滞:(号,开)=>原语.豫言_传输控制协议设置无延迟(号,开),缄口:号=>原语.豫言_传输控制协议关闭写入(号),
    别:号=>原语.豫言_传输控制协议关闭(号),误由:状态=>原语.豫言_传输控制协议错误消息(状态)
  };
  // 文言：单元现况：诺驿不能装原生之单元，六十四字皆零（同浏览器宿主），可用之位零；豫构见之则改装网页汇编之单元。
  // 汉语：「单元现况」：诺驿宿主不能装原生单元，六十四字节全零（同浏览器宿主，“可用”为零）；豫构据此改装网页汇编单元（提案 00043）。
  const 构建基础实现={藏包境:串参('豫言_存放包上下文'),可绘监工板:无参('豫言_可绘监视面板'),取今程序沙二五六:无参('豫言_获取当前程序沙256'),取卷沙二五六:名=>原语.豫言_获取文件沙256(缓(名)),帛页杂凑沙二五六:(字,页)=>{const 数=缓(字),长=数.length,页数=Number(页),步=页数>0&&页数<=1073741824?页数:4096,段=长===0?0:Math.ceil(长/步),出=Buffer.alloc(段*32);for(let 序=0;序<段;序++)密码.createHash('sha256').update(数.subarray(序*步,Math.min(长,(序+1)*步))).digest().copy(出,序*32);return 出;},单元现况:()=>new Uint8Array(64)};
  // 文言：平台接口包「借道」「密壳阴书」之带型导入，亦借旧原语：串参转 Buffer，小数之果去壳，外部值之列展为数列；旧表所无者不立，调之则报未绑定。
  // 汉语：平台接口包「借道」（系统库调用，外部库.mjs，十八个）与「密壳阴书」（安全外壳密码，九个）的带类型导入，同样照搬旧原语：串参数转成 Buffer，小数结果去掉 {小数} 外壳；
  //   三个调用原语的参数是外部值的列，过边界后是 [[支序, 值], …]，展成旧原语要的数值数组（支序 2 是无值，报错）。旧表没有的（工具链包里缺 外部库.mjs 时）不提供，调用时报“接口函数未绑定”。
  const 展外部值=列=>列.map(([序,值])=>{if(序===2)throw Error('系统库调用：参数不能是无值');return 值;});
  const 平台实现=(对照)=>{const 表={};for(const[字段,[旧名,转]]of Object.entries(对照))if(typeof 原语[旧名]==='function')表[字段]=转(原语[旧名]);return 表;};
  const 原样=旧=>旧,串一=旧=>值=>旧(缓(值)),整串=旧=>(号,名)=>旧(号,缓(名)),整串整=旧=>(址,类,值)=>旧(址,缓(类),值),整整串=旧=>(址,字节)=>旧(址,缓(字节)),
    调用果=收=>旧=>(址,签名,参们)=>收(旧(址,缓(签名),展外部值(参们))),串二=旧=>(甲,乙)=>旧(缓(甲),缓(乙)),串三=旧=>(甲,乙,丙)=>旧(缓(甲),缓(乙),缓(丙)),
    串四=旧=>(甲,乙,丙,丁)=>旧(缓(甲),缓(乙),缓(丙),缓(丁));
  const 系统库调用实现=平台实现({
    外库可用本术:['豫言_节点_外部库可用',原样],外库辟本术:['豫言_节点_外部库打开',串一],外库取符号本术:['豫言_节点_外部库取符号',整串],
    外召得数本术:['豫言_节点_外部库调用',调用果(果=>果)],外召得小数本术:['豫言_节点_外部库调用',调用果(去壳)],
    外召无返回本术:['豫言_节点_外部库调用',调用果(()=>undefined)],
    外术登本术:['豫言_节点_外部库登记',整串],外召数零本术:['豫言_节点_外部库调用零',原样],外召数一本术:['豫言_节点_外部库调用一',原样],
    外召数二本术:['豫言_节点_外部库调用二',原样],外召数三本术:['豫言_节点_外部库调用三',原样],外召数四本术:['豫言_节点_外部库调用四',原样],
    外召数五本术:['豫言_节点_外部库调用五',原样],外召数六本术:['豫言_节点_外部库调用六',原样],
    外库拨本术:['豫言_节点_外部库分配',原样],外库释本术:['豫言_节点_外部库释放',原样],
    外库读数本术:['豫言_节点_外部库读整数',整串],外库写数本术:['豫言_节点_外部库写整数',整串整],
    外库读小数本术:['豫言_节点_外部库读小数',旧=>(址,类)=>去壳(旧(址,缓(类)))],外库写小数本术:['豫言_节点_外部库写小数',整串整],
    外库读字节本术:['豫言_节点_外部库读字节',原样],外库写字节本术:['豫言_节点_外部库写字节',整整串],外库摹本术:['豫言_节点_外部库复制',原样],
    外库读文字本术:['豫言_节点_外部库读文字',原样],外库算阵藏本术:['豫言_节点_外部库张量存储',串一],外库算阵观本术:['豫言_节点_外部库张量视图',串一]
  });
  const 安全外壳密码实现=平台实现({
    求沙二五六:['豫言_密码_SHA256',串一],求已知主机密钥杂凑码沙一:['豫言_密码_已知主机HMACSHA1',串二],求交换二五五一九公钥:['豫言_密码_X25519公钥',串一],
    求交换二五五一九共密:['豫言_密码_X25519共密',串二],求爱德二五五一九公钥:['豫言_密码_Ed25519公钥',串一],签爱德二五五一九:['豫言_密码_Ed25519签',串二],
    验爱德二五五一九:['豫言_密码_Ed25519验',串三],加密阴符二五六伽氏计数:['豫言_密码_AES256GCM加密',串四],解密阴符二五六伽氏计数果:['豫言_密码_AES256GCM解密结果',串四]
  });
  const 造导入=模=>边界胶水?边界胶水.造边界导入(模,桥,{豫章:标准库实现,营造:构建基础实现,借道:系统库调用实现,密壳阴书:安全外壳密码实现,诺驿东道:文件能力.实现,
    豫寓案头:{读标准终端尺寸: 读取标准终端尺寸},豫寓时辰:{读今纪元毫秒:读取共享纪元毫秒},...(边界回环?.实现表??{})}):{};
  // 文言：装载单元（提案〇〇〇〇三）：依“顺序.txt”逐单元建实例，共一零号表与二异常之标，本单元之函置于表基；导入之“单元”者取先装单元之出。汉语：装载单元（提案 00003）：按“顺序.txt”逐个实例化单元，各单元共用一张零号表与两个异常标签，本单元的函数放在表基处（表先按元素段长度增长）；“单元”导入取先装入单元的同名导出。
  function 元素数(字){
    let 位=8;const 读=()=>{let 值=0,移=0,字节;do{字节=字[位++];值+=(字节&127)*2**移;移+=7;}while(字节&128);return 值;};
    while(位<字.length){const 节=字[位++],长=读(),止=位+长;if(节===9){读();if(读()!==0)throw Error('单元元素段须为标志 0');while(字[位]!==0x0b)位++;位++;return 读();}位=止;}
    return 0;
  }
  // 文言：辍察之具（提案〇〇〇四三第六步）：断字一枚；句中之察点呼东道之断例，断例见标则抛断异。工线程有之，其缓冲递于主线程，主线程以时段置标（试而已）。
  // 汉语：打断检查点的用具（提案 00043 第六步）：一个标志字；句子里的检查点调用宿主给的打断例程，例程见到标志就抛专用异常。工作线程里有，缓冲交给主线程，主线程按时段置标志（只供测试）。
  const 打断字=new Int32Array(new SharedArrayBuffer(4));
  const 造打断错=()=>{const 错=new Error('已打断');错.打断=true;return 错;};
  let 打断已告=false;
  const 告打断=()=>{if(!打断已告){打断已告=true;parentPort?.postMessage({种:'打断内存',缓冲:打断字.buffer});}};
  const 打断之导入=模=>{
    if(!WebAssembly.Module.imports(模).some(项=>项.module==='yuyan:browser/v1'))return {};
    告打断();
    return {'yuyan:browser/v1':{interrupt:()=>{if(Atomics.load(打断字,0)!==0)throw 造打断错();}}};
  };
  // 文言：本进程之单元界：零号表、二异常之标与已装诸单元之出，诸单元共之（提案〇〇〇四三）。汉语：本进程的单元环境：零号表、两个异常标签与已装单元的导出，各单元共用（提案 00043）。
  let 单元界=null;
  const 取单元界=()=>单元界??={表:new WebAssembly.Table({element:'anyfunc',initial:0}),标签:{escape:new WebAssembly.Tag({parameters:['eqref','eqref']}),error:new WebAssembly.Tag({parameters:['eqref']})},导出们:new Map()};
  // 文言：装一单元：后出者掩前。汉语：装入一个单元并返回实例；后装的同名导出遮住先装的。
  function 装一单元(字,名){
    const 界=取单元界(),模=new WebAssembly.Module(字),基=界.表.length;
    界.表.grow(元素数(字));
    const 单元={};
    for(const 导 of WebAssembly.Module.imports(模))if(导.module==='单元'){if(!界.导出们.has(导.name))throw Error('单元导入未找到：'+导.name+'（'+名+'）');单元[导.name]=界.导出们.get(导.name);}
    const 实例=new WebAssembly.Instance(模,{...造导入(模),运行时:{表:界.表,表基:new WebAssembly.Global({value:'i32',mutable:false},基),escape:界.标签.escape,error:界.标签.error},单元,...打断之导入(模)});
    for(const [出名,值] of Object.entries(实例.exports))界.导出们.set(出名,值);
    return 实例;
  }
  function 装入单元们(目录){
    const 行们=文件.readFileSync(路径.join(目录,'顺序.txt'),'utf8').split('\n').filter(行=>行.length).map(行=>行.split('\t'));
    for(const [名] of 行们){
      const 实例=装一单元(文件.readFileSync(路径.join(目录,名)),名);
      // 文言：装一单元即行其 _start（初置顶层数据，试与脚本并行程序之体），后装者乃得读之。汉语：每装入一个单元就运行它的 _start（初始化顶层数据，测试与脚本还运行程序体），后装的单元才能读到它的顶层数据。
      实例.exports._start();
    }
  }
  // 文言：装网汇单元（构建宿主，提案〇〇〇四三）：装一单元于本进程，暂以所予之参与程名行其 _start，返则复之；零为成，不能装或抛非豫言之误者印其由而返一，退出照传；为察点所断则返二。
  // 汉语：「装网汇单元」（构建宿主服务，提案 00043）：把一个单元装进本进程，暂以给定的参数与程序名运行它的 _start，返回后恢复；零为成功，装不进或抛出宿主错误时印出原因、返回一，退出照常往外传；被检查点打断时返回二（提案 00043 第六步）。
  构建基础实现.装网汇单元=(字,参们,名)=>{
    const 旧名=今程序名,旧参=今客参数;
    今程序名=文(缓(名));今客参数=参们.map(项=>文(缓(项)));
    try{
      Atomics.store(打断字,0,0);
      装一单元(缓(字),今程序名).exports._start();return 0;
    }
    catch(错){
      if(错?.退出码!==undefined)throw 错;
      if(错?.打断){Atomics.store(打断字,0,0);return 2;}
      写输出(2,Buffer.from('装网汇单元：'+(错?.stack??错)+'\n'));return 1;
    }
    finally{今程序名=旧名;今客参数=旧参;}
  };
  let 退出码=0;
  // 文言：_start 先行静态之初置，乃召入口之函。汉语：_start 先做静态初始化，再调用入口函数「入口」（提案 00006）。
  try{if(单元目录)装入单元们(单元目录);else new WebAssembly.Instance(模块,{...造导入(模块),...打断之导入(模块)}).exports._start();}catch(错){if(错.退出码===undefined)throw 错;退出码=错.退出码;}finally{文件能力.清理();终端输入.退出();}
  // 文言：承构志之工务，书定标行；显设 豫言诺驿统计 者，仍书旧详数。旧通调已删，其调用数为零。汉语：仅继承构建日志的 worker 写 marker 行；显式 豫言诺驿统计 仍保留详细 JSON；宿主调用数字段计旧通用调用，删除后为零。
  const 工务任务=本工.编译线程||本工.参数?.includes('--mode=worker');
  const 工务统计=工务任务&&!!process.env.豫言构建日志运行;
  if(!工务统计&&process.env.豫言诺驿统计!=='1')return {模块,退出码};
  if(工务统计){
    const 统计=JSON.stringify({模块准备毫秒:Math.round(编译毕-开始),执行毫秒:Math.round(performance.now()-编译毕),宿主调用数:0});
    const 行='YY_BUILD_TASK_STATS\t'+统计;
    if(本工.复用线程)写输出(2,Buffer.from(行+'\n'));else console.error(行);
  }else{
    const 统计=JSON.stringify({轮次:轮,复用模块:!!缓存,引擎:process.versions.v8,进程:process.pid,线程:threadId,编译工作模式:process.env.豫言诺驿编译工作模式??'threads',模块准备毫秒:编译毕-开始,执行毫秒:performance.now()-编译毕,宿主调用数:0});
    if(本工.复用线程)写输出(2,Buffer.from(统计+'\n'));else console.error(统计);
  }
  return {模块,退出码};
}
