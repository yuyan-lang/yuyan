// 文言：单应用开机之主线程网页外务（云仓提案〇〇〇〇五）：网页导航（豫寓向导）、网页请求（豫寓网索）、网页事件之统一队列（豫寓网事，今惟历史遍历）
//   与剪贴板（豫寓夹袋）。诸术皆同步问于工作线程，故可延迟作答以成阻塞：取址、待事、索文皆直还其果。
// 汉语：单应用开机的主线程网页外务（云仓提案 C00005）：网页导航（豫寓向导）、网页请求（豫寓网索）、网页事件的统一队列（豫寓网事，现在只有历史遍历）
//   与剪贴板（豫寓夹袋）。这些操作由工作线程同步提问，所以可以延后作答来做出阻塞语义：读地址、等事件、取网页文字都直接把结果还给调用方。
//   语义照《豫寓榫卯》各包规范：导航只许 http、https，压入与易史限同源；请求限同源、有超时与正文上限；事件队列按序编号。
const 编码器 = new TextEncoder();
const 严解码器 = new TextDecoder('utf-8', {fatal: true, ignoreBOM: true});
const 控制字符 = /[\u0000-\u001f\u007f]/u;

// 文言：解析导航之址：依今页而解，惟许 http 与 https；空、逾八千一百九十二码元、含控制之符或反斜者，皆告事故。
// 汉语：解析导航地址：按当前页面解析，只许 http 与 https；为空、超过 8192 个 UTF-16 码元、含控制字符或反斜杠都报事故。
const 解析址 = (址, 基) => {
  const 文 = String(址 ?? '');
  if (!文) throw Error('网址不能为空');
  if (文.length > 8192) throw Error('网址至多 8192 个 UTF-16 码元');
  if (控制字符.test(文) || 文.includes('\\')) throw Error('网址不得含控制字符或反斜杠');
  let 解;
  try { 解 = new URL(文, 基); } catch { throw Error('网址无法解析：' + 文.slice(0, 64)); }
  if (解.protocol !== 'http:' && 解.protocol !== 'https:') throw Error('只许 http 与 https 网址：' + 解.protocol);
  return 解;
};

// 文言：解请求之结绳：字段照《网页请求》规范，网址必，余可省；不识之字段、类型不合、越限者皆告事故；网址须同源。
// 汉语：解析请求 JSON：字段照《网页请求》规范，网址必填，其余可省；未知字段、类型不合、超出限额都报事故；网址必须同源。
const 请求字段 = new Set(['网址', '方法', '标头', '正文', '缓存', '超时毫秒', '正文上限', '标记']);
const 解析请求 = (请求文, 基) => {
  let 求;
  try { 求 = JSON.parse(String(请求文 ?? '')); } catch { throw Error('请求不是有效的 JSON'); }
  if (!求 || typeof 求 !== 'object' || Array.isArray(求)) throw Error('请求须是 JSON 对象');
  for (const 键 of Object.keys(求)) if (!请求字段.has(键)) throw Error('请求里有未知字段：' + 键);
  if (typeof 求.网址 !== 'string') throw Error('请求缺少「网址」');
  const 解 = 解析址(求.网址, 基);
  if (解.origin !== 基.origin) throw Error('网页请求只许同源网址');
  解.hash = '';
  const 方法 = 求.方法 ?? 'GET';
  if (!['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'DELETE'].includes(方法)) throw Error('请求方法无效：' + 方法);
  const 标头 = {};
  if (求.标头 !== undefined) {
    if (!求.标头 || typeof 求.标头 !== 'object' || Array.isArray(求.标头)) throw Error('「标头」须是对象');
    const 项 = Object.entries(求.标头);
    if (项.length > 32) throw Error('标头至多 32 项');
    for (const [名, 值] of 项) {
      if (!/^[A-Za-z0-9-]{1,64}$/u.test(名)) throw Error('标头名称无效：' + 名);
      if (typeof 值 !== 'string' || 值.length > 4096 || 控制字符.test(值)) throw Error('标头值无效：' + 名);
      标头[名] = 值;
    }
  }
  const 正文 = 求.正文;
  if (正文 !== undefined) {
    if (typeof 正文 !== 'string') throw Error('「正文」须是文字');
    if (方法 === 'GET' || 方法 === 'HEAD') throw Error('GET 与 HEAD 不带正文');
    if (编码器.encode(正文).length > 8 * 1024 * 1024) throw Error('请求正文至多 8 MiB');
  }
  const 缓存 = 求.缓存 ?? '默认';
  if (缓存 !== '默认' && 缓存 !== '不缓存') throw Error('「缓存」只能是 默认 或 不缓存');
  const 超时毫秒 = 求.超时毫秒 ?? 60000, 正文上限 = 求.正文上限 ?? 8388608, 标记 = 求.标记 ?? '';
  if (!Number.isInteger(超时毫秒) || 超时毫秒 < 1 || 超时毫秒 > 600000) throw Error('「超时毫秒」须在一到六十万之间');
  if (!Number.isInteger(正文上限) || 正文上限 < 1 || 正文上限 > 16777216) throw Error('「正文上限」须在一到一千六百七十七万七千二百一十六之间');
  if (typeof 标记 !== 'string' || 标记.length > 256) throw Error('「标记」须是至多 256 码元的文字');
  return {网址: 解.href, 方法, 标头, 正文, 缓存, 超时毫秒, 正文上限, 标记};
};

// 文言：一请求之果：成则状、状文、终址、小写之标头、正文、字节数与上传字节、经过毫秒；败则原因与说明。二者皆记标记。
// 汉语：一次请求的结果：成功是状态、状态文、最终网址、小写标头、正文、字节数与上传字节、经过毫秒；失败是原因与说明。两者都带标记。
const 请求之果 = async ({网址, 方法, 标头, 正文, 缓存, 超时毫秒, 正文上限, 标记}, 起时) => {
  const 控 = new AbortController();
  const 钟 = setTimeout(() => 控.abort(), 超时毫秒);
  try {
    const 参 = {method: 方法, headers: 标头, credentials: 'same-origin', redirect: 'follow', mode: 'same-origin', signal: 控.signal};
    if (缓存 === '不缓存') 参.cache = 'no-store';
    if (正文 !== undefined) 参.body = 正文;
    // 待办事项：正文整段读入再比上限，超大正文会先占内存；改成读流累计、逾限即中止。
    const 答 = await fetch(网址, 参);
    if (答.type === 'opaqueredirect' || (答.redirected && new URL(答.url).origin !== new URL(网址).origin))
      return {成: false, 原因: '网络错误', 说明: '请求被重定向到别的来源', 标记};
    const 字 = new Uint8Array(await 答.arrayBuffer());
    if (字.length > 正文上限) return {成: false, 原因: '正文过大', 说明: '响应正文超过 ' + 正文上限 + ' 字节', 标记};
    let 文;
    try { 文 = 严解码器.decode(字); } catch { return {成: false, 原因: '正文不是有效UTF-8', 说明: '响应正文不是有效的 UTF-8', 标记}; }
    const 头们 = {};
    for (const [名, 值] of 答.headers) { const 小 = 名.toLowerCase(); 头们[小] = 头们[小] ? 头们[小] + ', ' + 值 : 值; }
    return {成: true, 状态: 答.status, 状态文: 答.statusText || '', 网址: 答.url || 网址, 标头: 头们, 正文: 文, 字节数: 字.length,
      上传字节: 正文 === undefined ? 0 : 编码器.encode(正文).length, 经过毫秒: Math.max(1, Math.round(performance.now() - 起时)), 标记};
  } catch (错) {
    if (控.signal.aborted) return {成: false, 原因: '超时', 说明: '请求超过 ' + 超时毫秒 + ' 毫秒未完成', 标记};
    return {成: false, 原因: '网络错误', 说明: String(错?.message ?? 错), 标记};
  } finally { clearTimeout(钟); }
};

// 文言：造网页外务：返回诸操作以交进程表之外务表。根为页之 document，全局为 window。
// 汉语：造网页外务：返回交给进程表外务表的各个操作。根是页面的 document，全局是 window。
export function 造网页外务({根 = document, 全局 = globalThis} = {}) {
  const 基 = new URL(全局.location.href);
  const 剪贴 = {值: null};
  // 文言：事件之列：一列先进先出，等候者至则以事付之；事之种今惟历史遍历，请求之果亦入此列。
  // 汉语：事件队列：一个先进先出队列，等候者到来时把事件交给它；事件的种类现在只有历史遍历，请求的结果也进这个队列。
  const 事列 = [];
  let 序 = 0, 事候 = null, 弃数 = 0;
  const 投事 = 事 => {
    事.序 = ++序;
    if (事候) { const 成 = 事候; 事候 = null; 成(JSON.stringify(事)); return; }
    if (事列.length >= 1024) { 事列.shift(); 弃数++; }
    事列.push(事);
  };
  const 待事 = () => {
    if (事列.length) return JSON.stringify(事列.shift());
    return new Promise(成 => { 事候 = 成; });
  };
  // 文言：门面之订阅：今惟窗口之 popstate 与 hashchange（历史类）。订阅号自一起，罢订则去其录。
  // 汉语：门面订阅：现在只有窗口的 popstate 与 hashchange（历史类）。订阅号从一起，取消订阅就去掉记录。
  const 订阅们 = new Map();
  let 下订号 = 1;
  const 历史订阅 = () => [...订阅们.values()];
  const 订阅门面事 = (目标, 名, 策) => {
    if (目标 !== '窗口' && 目标 !== '文档') throw Error('此宿主只订阅窗口与文档上的历史事件');
    if (名 !== 'popstate' && 名 !== 'hashchange') throw Error('此宿主只订阅 popstate 与 hashchange');
    if (策) { try { JSON.parse(String(策)); } catch { throw Error('订阅策略不是有效的 JSON'); } }
    const 号 = 下订号++;
    订阅们.set(号, {号, 名});
    const 听 = () => {
      for (const 订 of 历史订阅()) if (订.名 === 名) 投事({类型: '历史', 订阅号: 订.号, 名称: 名, 详情: {网址: 全局.location.href, 状态: null}});
    };
    全局.addEventListener(名, 听);
    订阅们.get(号).去听 = () => 全局.removeEventListener(名, 听);
    return 号;
  };
  const 罢订阅门面事 = 号 => {
    const 订 = 订阅们.get(Number(号));
    if (!订) return;
    订.去听?.();
    订阅们.delete(Number(号));
  };
  // 文言：导航诸术：读址、前往、替换当前、重载、压入、易史、移动；压入与易史须同源。
  // 汉语：导航各操作：读地址、前往、替换当前、重载、压入、替换历史、移动；压入与替换历史必须同源。
  const 导航 = (操作, 甲, 乙) => {
    const 址 = () => 全局.location.href;
    switch (操作) {
      case '读址': return 址();
      case '前往': { const 解 = 解析址(甲, 址()); 全局.location.assign(解.href); return undefined; }
      case '替换': { const 解 = 解析址(甲, 址()); 全局.location.replace(解.href); return undefined; }
      case '重载': 全局.location.reload(); return undefined;
      case '压入': {
        const 解 = 解析址(甲, 址());
        if (解.origin !== 基.origin) throw Error('历史记录只允许同源网址');
        try { 全局.history.pushState(null, '', 解.href); } catch (错) { throw Error('浏览器拒绝压入历史：' + String(错?.message ?? 错)); }
        return undefined;
      }
      case '易史': {
        const 解 = 解析址(甲, 址());
        if (解.origin !== 基.origin) throw Error('历史记录只允许同源网址');
        try { 全局.history.replaceState(null, '', 解.href); } catch (错) { throw Error('浏览器拒绝替换历史：' + String(错?.message ?? 错)); }
        return undefined;
      }
      case '移动': {
        const 步 = Number(甲);
        if (!Number.isInteger(步) || 步 === 0 || Math.abs(步) > 50) throw Error('历史步数须是非零整数，绝对值至多五十');
        全局.history.go(步); return undefined;
      }
      default: throw Error('未知的导航操作：' + 操作);
    }
  };
  // 文言：网页请求：文字者阻塞待其果；发起者还号而后投事；罢者撤之。在途之数不逾三十二。
  // 汉语：网页请求：文字版阻塞等结果；发起版返回请求号然后投事件；取消版撤回。在途之数不超过 32。
  const 在途 = new Map();
  let 下索号 = 1;
  const 网页索 = async (操作, 请求文) => {
    if (操作 === '文字') {
      const 求 = 解析请求(请求文, 基);
      return JSON.stringify(await 请求之果(求, performance.now()));
    }
    if (操作 === '发起') {
      const 求 = 解析请求(请求文, 基);
      if (在途.size >= 32) throw Error('在途网页请求已达 32 个');
      const 号 = 下索号++, 控 = {已罢: false};
      在途.set(号, 控);
      请求之果(求, performance.now()).then(果 => {
        在途.delete(号);
        if (控.已罢) return;
        投事({类型: '请求', 订阅号: 号, 名称: 果.成 ? '完成' : '失败', 详情: 果});
      });
      return 号;
    }
    if (操作 === '罢') {
      const 号 = Number(请求文), 控 = 在途.get(号);
      if (!控) return false;
      控.已罢 = true; 在途.delete(号);
      return true;
    }
    throw Error('未知的网页请求操作：' + 操作);
  };
  // 文言：剪贴板：写则记于页中一份，并试以 navigator.clipboard 写系统之夹袋（不成亦无妨）；读则取此份，未有则报未获授权。
  // 汉语：剪贴板：写入时在页面里记一份，并试用 navigator.clipboard 写入系统剪贴板（不成也无妨）；读取取这一份，没有时报未获授权。
  const 剪贴板写 = 文 => {
    剪贴.值 = String(文 ?? '');
    try { 全局.navigator?.clipboard?.writeText?.(剪贴.值)?.catch?.(() => {}); } catch { /* 文言：系统之夹袋不可用则略。汉语：系统剪贴板不可用就略过。 */ }
  };
  const 剪贴板读 = () => [剪贴.值 !== null, 剪贴.值 ?? ''];
  const 网页事 = (操作, 甲, 乙, 丙) => {
    switch (操作) {
      case '待': return 待事();
      case '订阅': return 订阅门面事(甲, 乙, 丙);
      case '罢订': 罢订阅门面事(甲); return undefined;
      case '弃数': return 弃数;
      case '类弃数': return 0;
      default: throw Error('未知的网页事件操作：' + 操作);
    }
  };
  return {导航, 网页索, 网页事, 剪贴板写, 剪贴板读};
}
