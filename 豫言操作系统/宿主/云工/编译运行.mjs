// 文言：编译之司，居于宿主；客惟起、候、取、终四术，业务与限额之决在豫言。汉语：编译运行接口（豫言操作系统编译运行 0.1.0）的云工宿主实现：在内存文件系统里运行豫言编译器 Wasm，Binaryen 组装回调由外壳传入；每次运行把阶段与输出排成事件队列，由豫言应用逐个拉取。本模块只做“运行编译器”这一件事，不解析 HTTP、不决定响应形状。
// 文言：本模块无外部依赖，惟借浏览器编译器宿主之内存文件系与执行器；二者与本模块同处操作系统目录。汉语：依赖同仓的 ../浏览器/编译器/宿主.mjs 与 组装.mjs（云仓的“准备宿主”把整个操作系统目录复制到云仓，相对位置不变）。
import {内存文件系统, 执行模块} from '../浏览器/编译器/宿主.mjs';
import {创建组装器} from '../浏览器/编译器/组装.mjs';

// 文言：诸限皆载于规范，此处强制之，以防应用有误。汉语：与规范一致的硬限额；应用可以更严，宿主不得更宽。
export const 编译运行限额 = Object.freeze({
  请求字节: 2 * 1024 * 1024,
  文件数: 256,
  路径长度: 256,
  内容字节: 262144,
  产物字节: 8 * 1024 * 1024,
  事件字节: 16 * 1024 * 1024,
  并发运行: 8,
  保留运行: 32,
  存活毫秒: 5 * 60 * 1000,
  等待毫秒: 60000,
  资料加载毫秒: 60000,
  锁租约毫秒: 3 * 60 * 1000,
  轮询毫秒: 15
});

const 编码 = new TextEncoder();
const 十六进制 = 字节 => Array.from(new Uint8Array(字节), 位 => 位.toString(16).padStart(2, '0')).join('');
// 文言：路径循规：不逾长、无斜反与控、不以斜起、诸段非空非点。汉语：与云端编译服务历来的路径规则逐项相同（UTF-16 长度 ≤ 256；无反斜线与 U+0000 至 U+001F；不以 / 起首；各段非空且不是 . 或 ..）。
const 合法路径 = 名 => typeof 名 === 'string' && 名.length <= 编译运行限额.路径长度 && !/[\\\u0000-\u001f]/.test(名) &&
  !名.startsWith('/') && 名.split('/').every(项 => 项 && 项 !== '.' && 项 !== '..');

// 文言：请求须先验后用；违者以中文言其故。汉语：解析并校验编译请求 JSON：{"files":{路径:文字…},"entry":路径}，entry 缺省或为空值时取 入口。豫。
function 解析请求(文) {
  if (typeof 文 !== 'string') throw Error('编译请求须为 JSON 文字');
  if (编码.encode(文).length > 编译运行限额.请求字节) throw Error('编译请求超过 2 MiB');
  let 值;
  try { 值 = JSON.parse(文); } catch { throw Error('编译请求不是有效的 JSON'); }
  if (!值 || typeof 值 !== 'object' || Array.isArray(值)) throw Error('编译请求须为 JSON 对象');
  const {files} = 值;
  if (!files || typeof files !== 'object' || Array.isArray(files)) throw Error('须提供 files 文件映射');
  const 项 = Object.entries(files);
  if (!项.length || 项.length > 编译运行限额.文件数) throw Error('项目须含 1 至 256 个文件');
  if (项.some(([名, 内容]) => !合法路径(名) || typeof 内容 !== 'string')) throw Error('项目路径或内容无效');
  if (项.reduce((总, [, 内容]) => 总 + 编码.encode(内容).length, 0) > 编译运行限额.内容字节) throw Error('项目超过 256 KiB');
  const entry = 值.entry ?? '入口。豫';
  if (!合法路径(entry) || !Object.hasOwn(files, entry)) throw Error('入口文件不存在');
  return {files, entry};
}

// 文言：产物 Base64 分块而成，免展开之栈溢。汉语：与旧云桥相同的分块 String.fromCharCode + btoa。
function 编成Base64(字节) {
  let 串 = '';
  for (let 位 = 0; 位 < 字节.length; 位 += 8192) 串 += String.fromCharCode(...字节.subarray(位, 位 + 8192));
  return btoa(串);
}

/**
 * 文言：造运行器之工厂；器、桥、组装恒定，标准库资料随资产绑定取之而恒缓存。
 * 汉语：参数 {binaryen, 编译模块, 桥模块, 取标准库资料?}。binaryen 是已初始化的 Binaryen API（第三方胶水，由外壳静态导入）；编译模块与桥模块是已编译的 WebAssembly.Module；
 * 取标准库资料 可选，返回（或解析出）标准库资料对象 {路径:内容|{内容,时间}}；缺省时经返回函数传入的资产绑定读取 标准库.json.gz。
 * 返回函数 (资产绑定) → 运行器；同一进程内所有运行器共享运行表与标准库缓存。运行器方法（成败以异常表示）：
 *   启动(请求文) → 运行号；读事件(运行号, 毫秒) → Promise<{状态, 事件}>（状态 0 得事、1 候满无事、2 运已终而事尽；事件为 JSON 文字或空串）；取产物(运行号) → Uint8Array；取产物Base64(运行号) → 文字；结束(运行号) → true。
 */
export function 创建编译运行器工厂({binaryen, 编译模块, 桥模块, 取标准库资料 = null}) {
  if (!binaryen || !(编译模块 instanceof WebAssembly.Module) || !(桥模块 instanceof WebAssembly.Module)) throw Error('编译运行器缺少 binaryen 或编译器 Wasm 模块');
  const 组装 = 创建组装器(binaryen);
  const 运行表 = new Map();
  let 资产绑定 = null;
  let 标准资料 = null;
  let 加载中 = null;
  // 文言：候之有期，逾则弃其承诺而重来，免首请被撤而众皆悬。汉语：给一个承诺加期限；期限由调用者自己的定时器计（在调用者自己的请求上下文里）。
  const 限时 = (承诺, 毫秒, 说明) => new Promise((成, 败) => {
    const 计时 = setTimeout(() => 败(Error(说明)), 毫秒);
    承诺.then(值 => { clearTimeout(计时); 成(值); }, 错 => { clearTimeout(计时); 败(错); });
  });
  // 文言：标准库唯一份，随进程而存；败则重取。汉语：加载成功后缓存资料对象本身（不再依赖某个请求创建的承诺）；并发的首批运行共享同一次加载；失败或逾期清空，下次重试。Worker 的异步工作与请求上下文绑定，首个请求若被撤销，别的请求不会永远等下去。
  const 取资料 = async () => {
    if (标准资料) return 标准资料;
    if (取标准库资料) return (标准资料 = await 取标准库资料());
    if (!加载中) {
      加载中 = (async () => {
        if (!资产绑定) throw Error('标准库资源不可用');
        const 回 = await 资产绑定.fetch(new Request('https://资源/标准库.json.gz'));
        if (!回.ok) throw Error('标准库资源不可用');
        return await new Response(回.body.pipeThrough(new DecompressionStream('gzip'))).json();
      })().then(值 => { 标准资料 = 值; 加载中 = null; return 值; }, 错 => { 加载中 = null; throw 错; });
    }
    try { return await 限时(加载中, 编译运行限额.资料加载毫秒, '标准库资源加载超时'); }
    catch (错) { 加载中 = null; throw 错; }
  };
  // 文言：一时一编，免二室并存而竭其库；各运各以己之定时器候锁，不借他请求之承诺链。汉语：编译阶段（造文件系统、执行编译器、取产物）互斥：同一时刻至多一份标准库文件系统副本。互斥用“锁 + 各运自己的定时器轮询”实现，而不是把所有运行串成一条承诺链——Worker 的异步工作与创建它的请求上下文绑定，链在别的请求上的续行会跑进别的请求的上下文，那个请求一旦结束或被撤销，链就断了；轮询让每个运行的全部工作都在自己请求的上下文里进行。锁带租约（超过则视为持有者已失踪，可被夺取），持有者失踪不会使后来者永远等待。
  let 持锁 = null;
  const 取运行 = 号 => {
    const 运行 = 运行表.get(String(号));
    if (!运行) throw Error('编译运行不存在或已结束');
    return 运行;
  };
  // 文言：事件先入队，候者在则直交。汉语：事件序列化为 JSON 文字；有等待者时直接交付，否则入队；结束后不再收事件。
  const 发 = (运行, 事件, 终止 = false) => {
    if (运行.已终止 || 运行.已完) return;
    const 文 = JSON.stringify(事件);
    运行.事件字节 += 编码.encode(文).length;
    // 文言：终止之事必达，余者逾限则弃。汉语：终止事件（finished、failed）不受事件字节限额约束，保证应用总能看到结局。
    if (!终止 && 运行.事件字节 > 编译运行限额.事件字节) return;
    if (运行.等待者) 运行.等待者(文);
    else 运行.队列.push(文);
  };
  const 收尾 = (运行, 事件) => {
    发(运行, 事件, true);
    运行.已完 = true;
  };
  const 编译 = async 运行 => {
    let 文件 = null;
    try {
      发(运行, {type: 'stage', label: '正在加载标准库与项目文件'});
      const 资料 = await 取资料();
      if (运行.已终止) return;
      // 文言：每稿独室，官书只读其本。汉语：缓存仅含固定标准库，每次运行新建文件系统；不加载前次用户产物。
      文件 = new 内存文件系统(资料);
      文件.删('/用户程序/用户程序。包。豫');
      文件.写('/用户程序/云项目。包。豫', '');
      文件.写('/包上下文', '豫构包上下文二\n豫言\t标准库\t/库/标准库/标准库。包。豫\n访客\t云项目\t/用户程序/云项目。包。豫\t豫言\t标准库');
      for (const [名, 内容] of Object.entries(运行.请求.files)) 文件.写('/用户程序/' + 名, 内容);
      发(运行, {type: 'stage', label: '正在执行豫言编译器'});
      const 结果 = await 执行模块(编译模块, 桥模块, 文件, ['/用户程序/' + 运行.请求.entry, '--package-context', '/包上下文', '--target=wasmgc', '-o', '/程序.wasm'],
        {编译: true, 组装, 报告: 事 => 发(运行, 事)});
      if (!结果.ok) { 文件 = null; 收尾(运行, {type: 'finished', result: 结果}); return; }
      const 产物 = 文件.读('/程序.wasm');
      文件 = null;
      if (产物.length > 编译运行限额.产物字节) throw Error('编译产物超过 8 MiB');
      const 摘要 = 十六进制(await crypto.subtle.digest('SHA-256', 产物));
      运行.产物 = 产物;
      收尾(运行, {type: 'finished', result: 结果, artifact: {format: 'wasmgc', bytes: 产物.length, sha256: 摘要}});
    } catch (错) {
      文件 = null;
      收尾(运行, {type: 'failed', error: String(错?.message ?? 错)});
    }
  };
  // 文言：久不结束者收之；未终之运不逾八，既终待取者不逾三十二，溢则弃其最旧。汉语：回收超过存活期的运行；并发限额只算尚未终止的运行（编译中或排队中），已终止而等待读取的运行另限 32 个，超出时丢弃最旧的已终止运行——事件异常退出遗留的运行因此既不会永久占用并发额度，也不会无限堆积。
  const 清过期 = () => {
    const 现 = Date.now();
    for (const [号, 运行] of 运行表) if (现 - 运行.创建于 > 编译运行限额.存活毫秒) { 运行.已终止 = true; 运行表.delete(号); }
  };
  const 让位 = () => {
    let 未终 = 0;
    for (const 运行 of 运行表.values()) if (!运行.已完 && !运行.已终止) 未终++;
    if (未终 >= 编译运行限额.并发运行) throw Error('编译运行过多');
    for (const [号, 运行] of 运行表) {
      if (运行表.size < 编译运行限额.保留运行) break;
      if (运行.已完) { 运行.已终止 = true; 运行表.delete(号); }
    }
  };
  const 轮到 = 运行 => {
    if (运行.已终止) return;
    const 现 = Date.now();
    if (持锁 && !持锁.运行.已终止 && !持锁.运行.已完 && 现 < 持锁.到期) {
      setTimeout(() => 轮到(运行), 编译运行限额.轮询毫秒);
      return;
    }
    持锁 = {运行, 到期: 现 + 编译运行限额.锁租约毫秒};
    编译(运行).finally(() => { if (持锁?.运行 === 运行) 持锁 = null; });
  };
  const 运行器 = {
    启动(请求文) {
      const 请求 = 解析请求(请求文);
      清过期();
      让位();
      const 号 = crypto.randomUUID();
      const 运行 = {号, 请求, 队列: [], 事件字节: 0, 等待者: null, 已完: false, 已终止: false, 产物: null, 创建于: Date.now()};
      运行表.set(号, 运行);
      // 文言：起而不即行，令应用先入候位。汉语：编译在下一个宏任务里由这个运行自己的定时器起头（轮到才编），应用先取得运行号并开始等待事件。
      setTimeout(() => 轮到(运行), 0);
      return 号;
    },
    读事件(号, 毫秒) {
      const 运行 = 取运行(号);
      const 时 = Number(毫秒);
      if (!Number.isSafeInteger(时) || 时 < 1 || 时 > 编译运行限额.等待毫秒) throw Error('等待毫秒须在 1 至 60000');
      if (运行.队列.length) return Promise.resolve({状态: 0, 事件: 运行.队列.shift()});
      if (运行.已完 || 运行.已终止) return Promise.resolve({状态: 2, 事件: ''});
      if (运行.等待者) throw Error('同一运行已有等待者');
      return new Promise(完成 => {
        const 计时 = setTimeout(() => { 运行.等待者 = null; 完成({状态: 1, 事件: ''}); }, 时);
        运行.等待者 = 文 => { clearTimeout(计时); 运行.等待者 = null; 完成(文 === null ? {状态: 2, 事件: ''} : {状态: 0, 事件: 文}); };
      });
    },
    取产物(号) {
      const 运行 = 取运行(号);
      if (!运行.产物) throw Error('编译产物不可用');
      return 运行.产物;
    },
    取产物Base64(号) { return 编成Base64(运行器.取产物(号)); },
    // 文言：诊断之术：现存之运几何。汉语：诊断与测试用：宿主进程里当前登记的运行个数（应为 0 表示无残留）。
    运行数() { return 运行表.size; },
    结束(号) {
      const 运行 = 运行表.get(String(号));
      if (!运行) return true;
      运行.已终止 = true;
      运行表.delete(String(号));
      if (运行.等待者) { const 交 = 运行.等待者; 运行.等待者 = null; 交(null); }
      return true;
    }
  };
  return 资产 => { 资产绑定 = 资产 ?? 资产绑定; return 运行器; };
}
