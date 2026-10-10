// 文言：云工薄宿主：惟译名与值，平台之义皆在豫言；一程序一实例，诸事交错于 JSPI 挂起之点。
// 汉语：云工薄宿主：只翻译名字和值，平台语义都在豫言里（库/云工宿主 与 豫言操作系统/适配）。
//   每个程序在一个隔离体里（持久对象则每个对象）只建一个 Wasm 实例，多个事件在 JSPI 挂起点交错执行；宿主不保存任何按事件的状态，
//   请求、上下文都作入口参数显式传入，JS 对象以资源（externref）交给豫言，生命周期交给引擎的垃圾回收。
import {创建豫言实例, 平台导入旧名} from './值桥.mjs';
import {造边界导出} from './边界.mjs';
import {创建物桥} from './物桥.mjs';

// 文言：类型化之入口：事类所对之应用导出。汉语：类型化入口：平台事件对应的应用接口导出（应用实现，宿主调用）。
export const 类型化入口 = Object.freeze({
  'fetch': '豫言操作系统网页服务/处理入站网页请求',
  'service-fetch': '豫言操作系统网页服务/处理入站网页请求',
  'durable-fetch': '豫言操作系统网页服务/处理入站网页请求',
  'queue': '豫言操作系统消息队列/处理队列批次',
  'scheduled': '豫言操作系统定时事件/处理定时事件',
  'durable-alarm': '豫言操作系统持久告警/处理持久告警'
});
// 文言：JS 回呼豫言之唯一入口，云工宿主库供之。汉语：JS 回调豫言的唯一入口，由云工宿主库提供（库/云工宿主/回调。豫）。
export const 回调入口 = '云工宿主/执行云工回调';

// 文言：客可见之全局唯此表所列；fetch、WebSocket、Function 之属皆不与，外发须经许可（豫言公网取、豫言授权取与平台之绑）。
// 汉语：豫言用「云工全局」只能取得下表列出的全局对象与构造器；fetch、WebSocket、EventSource、Function、WebAssembly、globalThis 等不在表内，
//   外发请求只能走宿主对象 豫言公网取、豫言授权取（都核对 OUTBOUND_ORIGINS）或平台绑定（核对许可清单）。
export const 可用全局 = new Set([
  'Array', 'ArrayBuffer', 'BigInt', 'Blob', 'Boolean', 'CompressionStream', 'DataView', 'Date', 'DecompressionStream',
  'File', 'FormData', 'Headers', 'Intl', 'JSON', 'Map', 'Math', 'Number', 'Object', 'RegExp', 'ReadableStream', 'Response', 'Set',
  'String', 'Symbol', 'TextDecoder', 'TextDecoderStream', 'TextEncoder', 'TextEncoderStream', 'TransformStream',
  'URL', 'URLSearchParams', 'Uint8Array', 'WritableStream', 'AbortController', 'AbortSignal', 'atob', 'btoa',
  'decodeURI', 'decodeURIComponent', 'encodeURI', 'encodeURIComponent', 'isFinite', 'isNaN', 'parseFloat', 'parseInt',
  'structuredClone', 'Request', 'Promise', 'Error', 'TypeError', 'RangeError', 'caches', 'scheduler', 'console', 'crypto',
  'performance', 'setTimeout', 'clearTimeout', 'setInterval', 'clearInterval', 'queueMicrotask'
]);

// 文言：公网 HTTPS 之动态上游，惟许可 OUTBOUND_ORIGINS 含 https://* 乃许，址须 https、无用户名与口令，转址一律 manual。
// 汉语：动态公网 HTTPS 上游（宿主对象 豫言公网取）：程序许可 OUTBOUND_ORIGINS 含 https://* 才放行，网址须是 https、不带用户名或密码，redirect 一律改为 manual。
export function 造公网取({许可 = {}, 全局 = globalThis} = {}) {
  const 允许 = Array.isArray(许可.OUTBOUND_ORIGINS) && 许可.OUTBOUND_ORIGINS.includes('https://*');
  return (网址, 选项 = {}) => {
    if (!允许) throw Error('云工宿主不开放此全局：fetch');
    const 是请求 = typeof 全局.Request === 'function' && 网址 instanceof 全局.Request;
    let 目标;
    try { 目标 = new URL(是请求 ? 网址.url : String(网址)); } catch { throw Error('公网 fetch 的网址无效'); }
    if (目标.protocol !== 'https:') throw Error('公网 fetch 只许 https 网址：' + 目标.protocol);
    if (目标.username || 目标.password) throw Error('公网 fetch 的网址不得带用户名或密码');
    if (选项 === null || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('公网 fetch 的选项须为对象');
    return Reflect.apply(全局.fetch, 全局, [是请求 ? 网址 : 目标.href, {...选项, redirect: 'manual'}]);
  };
}

// 文言：许可来源之外发：址须 https、无用户名与口令，其源（方案、主机、端口）须列于许可 OUTBOUND_ORIGINS，转址一律 manual；通配之 https://* 不许任何真源。
//   其 已授权(址) 惟验不发，网页上游之适配先验其源，乃验余参。
// 汉语：许可来源的外发请求（宿主对象 豫言授权取，网页上游适配的静态来源请求用）：网址须是 https、不带用户名或密码，
//   来源（方案、主机、端口）须在程序许可 OUTBOUND_ORIGINS 里，redirect 一律改为 manual；通配来源 https://* 不放行任何真实来源。
//   函数属性 已授权(网址) 只核对不发请求：适配先核对来源，再校验其余参数（已授权('https://*') 即“许可是否声明了通配来源”）。
export function 造授权取({许可 = {}, 全局 = globalThis} = {}) {
  const 来源们 = Array.isArray(许可.OUTBOUND_ORIGINS) ? 许可.OUTBOUND_ORIGINS : [];
  const 来源 = 网址 => {
    let 目标;
    try { 目标 = new URL(String(网址)); } catch { return ''; }
    return 目标.protocol === 'https:' && !目标.username && !目标.password ? 目标.origin : '';
  };
  const 已授权 = 网址 => { const 源 = 来源(网址); return 源 !== '' && 来源们.includes(源); };
  const 取 = (网址, 选项 = {}) => {
    const 是请求 = typeof 全局.Request === 'function' && 网址 instanceof 全局.Request;
    const 源 = 来源(是请求 ? 网址.url : 网址);
    if (源 === '' || 源 === 'https://*' || !来源们.includes(源)) throw Error('上游网址未获授权');
    if (选项 === null || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('授权 fetch 的选项须为对象');
    return Reflect.apply(全局.fetch, 全局, [网址, {...选项, redirect: 'manual'}]);
  };
  取.已授权 = 已授权;
  return 取;
}

// 文言：云工所供之平台接口包。汉语：云工宿主实现的平台接口包（导入模块名即包名）。
export const 云工平台包 = Object.freeze(['云工宿主', '中央张量宿主']);
const 异步函数 = (async () => {}).constructor;

// 文言：造云工宿主。汉语：创建云工宿主，得各平台事件的处理器。
//   动态资源 {模块源码, 值桥字节} 供隔离运行造子 Worker；中央张量内核模块 供张量计算。
//   待办事项：原来按事件种类的墙钟时限（执行配置）已不再使用，由平台的 CPU 限额兜底。
export function 创建云工宿主({程序模块, 值桥模块, 许可 = {}, 动态资源 = null, 全局 = globalThis, 输出 = () => {}, 错误输出 = 文 => 全局.console?.error?.(文), 中央张量内核模块 = null}) {
  // 文言：平台之 env，同一隔离体恒为一物；事至则记之。汉语：平台的 env 在同一个隔离体里是同一个对象，每个事件到来时记下它，供「云工绑定」查。
  let 环境 = {};
  // 文言：未授者部署之误，径抛；已授而阙者归未定义，由适配定之（必需之绑以「云工中止」止之）。
  // 汉语：未授权是部署错误，直接抛出（中止本次事件）；已授权而环境里没有时得 undefined，由适配决定：可选的绑定当作“没有配置”，必需的绑定用「云工中止」中止本次事件。
  const 取绑定 = (类, 名) => {
    if (!许可[类]?.includes(名)) throw Error('未授权的' + 类 + '绑定：' + 名);
    return 名 in 环境 ? 环境[名] : undefined;
  };
  // 文言：宿主之物：惟 JS 能为之组件，挂于全局之名下。汉语：宿主对象：只能用 JS 写的组件，以「豫言」开头的名字挂在「云工全局」下。
  const 宿主对象 = new Map([
    ['豫言公网取', 造公网取({许可, 全局})],
    ['豫言授权取', 造授权取({许可, 全局})],
    ['豫言子工', 造子工({取绑定, 动态资源})],
    ['豫言平台资料', {取: () => import('./平台资料.mjs').then(模块 => 模块.创建平台资料({全局}))}]
  ]);
  const 取全局 = 名 => {
    if (宿主对象.has(名)) return 宿主对象.get(名);
    if (!可用全局.has(名)) throw Error('云工宿主不开放此全局：' + 名);
    if (!(名 in 全局)) throw Error('云工全局能力不存在：' + 名);
    return 全局[名];
  };
  // 文言：中央张量之能，一宿主一份，初用乃载。汉语：中央张量（导入模块 中央张量宿主）：每个宿主一份，第一次「启用」时动态导入 中央张量.mjs 并载入内核。
  let 中央张量 = null, 中央张量载入 = null;
  const 载中央张量 = () => 中央张量载入 ??= import('./中央张量.mjs').then(模块 => {
    中央张量 = 模块.创建中央张量能力({
      取内核模块: async () => { if (!中央张量内核模块) throw Error('构建产物没有中央张量内核模块'); return 中央张量内核模块; },
      多线程: false, 最大页: 1024, 全局
    });
  });
  const 须中央张量 = () => { if (!中央张量) throw Error('中央张量：尚未启用'); return 中央张量; };
  const 裸数 = 值 => (值 !== null && typeof 值 === 'object' && Object.hasOwn(值, '小数') ? 值.小数 : 值);
  const 中央张量函 = 旧名 => 旧名 === '豫言_中央张量_启用' ? async () => { await 载中央张量(); return 中央张量[旧名](); }
    : 旧名 === '豫言_中央张量_读单精' || 旧名 === '豫言_中央张量_数学' ? (...参) => 裸数(须中央张量()[旧名](...参))
    : (...参) => 须中央张量()[旧名](...参);

  // 文言：造实例：接导入，行 _start，取导出。汉语：建实例：接好导入（云工宿主 用物桥，中央张量宿主 用张量能力），跑一次 _start（静态初始化），取应用接口导出。
  const 造实例 = async () => {
    let 导出 = null;
    // 文言：豫言应用导出：本实例之应用接口导出，适配得以物调候呼应用所定之入口（如 执行持久事务回调）。
    // 汉语：宿主对象 豫言应用导出 是本实例的应用接口导出表（键为“包名/函数名”），适配可以用「物调候」调用应用定义的入口（如持久事务里调 执行持久事务回调）。
    const 物桥 = 创建物桥({
      取全局: 名 => 名 === '豫言应用导出' ? 导出 : 取全局(名), 取绑定,
      回调: (号, 参们) => {
        const 函 = 导出?.[回调入口];
        if (!函) throw Error('程序没有回调入口 ' + 回调入口);
        return 函(BigInt(号), 参们);
      }
    });
    const 平台 = {};
    for (const 项 of WebAssembly.Module.imports(程序模块)) {
      if (项.kind !== 'function') continue;
      let 函;
      if (项.module === '云工宿主') 函 = 物桥[项.name];
      else if (项.module === '中央张量宿主' && Object.hasOwn(平台导入旧名.中央张量宿主, 项.name)) 函 = 中央张量函(平台导入旧名.中央张量宿主[项.name]);
      if (typeof 函 !== 'function') continue;
      if (函 instanceof 异步函数) 函.异步 = true;
      (平台[项.module] ??= {})[项.name] = 函;
    }
    const {运行, 实例, 桥} = 创建豫言实例(程序模块, 值桥模块, {输出, 错误输出, 时限毫秒: Infinity, 平台});
    await 运行();
    导出 = 造边界导出(实例, 程序模块, 桥.原, {异步: true});
    return 导出;
  };
  // 文言：一宿主一实例；入口抛异常则弃之，后事另建。汉语：每个宿主一个实例，第一个事件到来时建；入口把异常抛出 Wasm 时（程序缺陷），作废这个实例，后来的事件另建新实例，在途的事件照常跑完。
  let 实例承诺 = null;
  const 调入口 = async (入口名, 参数们) => {
    const 承诺 = 实例承诺 ??= 造实例();
    let 导出;
    try { 导出 = await 承诺; }
    catch (错) { if (实例承诺 === 承诺) 实例承诺 = null; throw 错; }
    const 函 = 导出[入口名];
    if (typeof 函 !== 'function') throw Error('程序没有导出入口 ' + 入口名);
    try { return await 函(...参数们); }
    catch (错) { if (实例承诺 === 承诺) 实例承诺 = null; throw 错; }
  };
  // 文言：先交之答：网页之事，入口未返而豫言可先交其答，宿主即以之答，入口续行而其返值弃之；待交者以请求之物为键，存于弱表，随请而收。
  // 汉语：先行交付响应（网页事件流用）：网页事件里，豫言可在入口返回之前用宿主对象 豫言先交响应(请求, 响应) 把 Response 交给本请求，
  //   宿主立即以它作答，入口继续运行，入口的返回值随之忽略；得到入口运行毕的 Promise（正常返回得 true，抛出得 false），适配据此在事件结束时收尾。
  //   待交付的请求以 Request 对象为键记在 WeakMap 里，随请求回收，每个请求只能交付一次。
  const 待先交 = new WeakMap();
  宿主对象.set('豫言先交响应', (请求, 响应) => {
    const 交 = 待先交.get(请求);
    if (!交) throw Error('先行交付响应须在本请求的网页事件里，且只能交付一次');
    if (!(响应 instanceof 全局.Response)) throw Error('先行交付的不是 Response');
    待先交.delete(请求);
    return 交(响应);
  });
  // 文言：网页之事：入站请求为〔0, 请求, 上下文, 事类〕，答为〔0, Response 或其 Promise〕；入口未返而先交其答者，即以所先交者答之。
  // 汉语：网页事件：入口参数 入站网页请求 =〔0, 请求物, 上下文物, 种类〕，返回 出站网页响应 =〔0, Response 或 Promise<Response>〕。
  //   入口返回之前用 豫言先交响应 先行交付了响应的，立即以它作答，入口继续运行（有 waitUntil 的上下文等它跑完）；此后入口抛出只写错误输出。
  const 网页 = async (种类, 请求, 环境参, 上下文) => {
    if (环境参) 环境 = 环境参;
    let 交付, 已先交 = false;
    const 先交 = new Promise(解 => { 交付 = 解; });
    待先交.set(请求, 回 => {
      已先交 = true;
      交付(回);
      上下文?.waitUntil?.(运行毕);
      return 运行毕;
    });
    const 运行 = 调入口(类型化入口[种类], [[0, 请求, 上下文 ?? {}, 种类]]);
    const 运行毕 = 运行.then(() => true, 错 => {
      if (已先交) 错误输出('[豫言] 响应已先行交付后运行失败：' + String(错?.stack ?? 错));
      return false;
    });
    try {
      const 已交回 = await Promise.race([运行.then(() => null), 先交]);
      if (已交回) return 已交回;
    } finally { 待先交.delete(请求); }
    const 果 = await 运行;
    let 回 = 果?.[1];
    if (回 && typeof 回.then === 'function') 回 = await 回;
    if (!(回 instanceof 全局.Response)) throw Error('入口返回的不是 Response');
    return 回;
  };
  // 文言：余事：参数为〔0, 事物, 上下文〕，无答。汉语：其余事件：入口参数为〔0, 事件物, 上下文物〕，没有返回值。
  const 其事 = async (种类, 事物, 环境参, 上下文) => {
    if (环境参) 环境 = 环境参;
    await 调入口(类型化入口[种类], [[0, 事物, 上下文 ?? {}]]);
  };
  return {
    fetch(请求, 环境参, 上下文) { return 网页('fetch', 请求, 环境参, 上下文); },
    serviceFetch(请求, 环境参, 上下文) { return 网页('service-fetch', 请求, 环境参, 上下文); },
    durableFetch(请求, 环境参, 状态) { return 网页('durable-fetch', 请求, 环境参, 状态); },
    queue(批次, 环境参, 上下文) { return 其事('queue', 批次, 环境参, 上下文); },
    scheduled(控制, 环境参, 上下文) { return 其事('scheduled', 控制, 环境参, 上下文); },
    durableAlarm(告警, 环境参, 状态) { return 其事('durable-alarm', 告警 ?? {}, 环境参, 状态); }
  };
}

// 文言：子工之物：以 LOADER 之绑定载豫言客器，env 空、外发禁。汉语：宿主对象 豫言子工：按 LOADER 许可的绑定加载子 Worker，子 Worker 只运行传入的 Wasm，env 为空、globalOutbound 为 null。
//   按号取(绑定名, 标识, 主模块, 程序字节, cpuMs, subRequests) 得子 Worker 的默认入口；主模块是 隔离入口.mjs（豫言客器）或 隔离运行客.mjs（文件式客器）。
//   相同标识必须始终对应相同的程序字节、限额与主模块（平台按标识缓存子 Worker）。
function 造子工({取绑定, 动态资源}) {
  const 附源码 = {'隔离入口.mjs': ['宿主.mjs', '物桥.mjs', '值桥.mjs', '边界.mjs'], '隔离运行客.mjs': ['编译宿主.mjs', '边界.mjs']};
  const 造码 = (主模块, 程序字节, cpuMs, subRequests) => {
    if (!动态资源) throw Error('缺少动态 Worker 资源');
    if (!Object.hasOwn(附源码, 主模块)) throw Error('子 Worker 主模块无效：' + 主模块);
    const 字节 = 程序字节 instanceof Uint8Array ? 程序字节 : new Uint8Array(程序字节);
    if (!WebAssembly.validate(字节)) throw Error('动态 Worker 程序不是有效 Wasm');
    const CPU = Number(cpuMs), 次数 = Number(subRequests);
    if (!Number.isSafeInteger(CPU) || CPU < 1 || !Number.isSafeInteger(次数) || 次数 < 0) throw Error('动态 Worker 资源限额无效');
    const 模块 = {};
    for (const 名 of [主模块, ...附源码[主模块]]) {
      if (typeof 动态资源.模块源码[名] !== 'string') throw Error('缺少动态 Worker 模块源码：' + 名);
      模块[名] = {js: 动态资源.模块源码[名]};
    }
    模块['程序.wasm'] = {wasm: 字节.slice().buffer};
    模块['值桥.wasm'] = {wasm: 动态资源.值桥字节.slice(0)};
    return {compatibilityDate: '2026-09-10', mainModule: 主模块, modules: 模块, globalOutbound: null, env: {}, limits: {cpuMs: CPU, subRequests: 次数}};
  };
  return {
    按号取(绑定名, 标识, 主模块, 程序字节, cpuMs, subRequests) {
      const 加载器 = 取绑定('LOADER', String(绑定名));
      const 码 = 造码(String(主模块), 程序字节, cpuMs, subRequests);
      return 加载器.get(String(标识), () => 码).getEntrypoint();
    }
  };
}
