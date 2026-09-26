// 文言：客裁界面，宿主但施文树、待事与网求。汉语：浏览器宿主只执行 DOM、事件和网络原语。
import {创建豫言实例, 文字} from './值桥.mjs';
import {创建句柄表} from './句柄.mjs';

// 文言：同源径不得逸境，改址与巨文俱拒。汉语：静态资源只从当前 origin 读取，禁止重定向并逐块限制正文大小。
export async function 读取同源资源文字(路径文, 基址文, 网络) {
  if (!路径文.startsWith('/') || 路径文.startsWith('//') || 路径文.includes('\\'))
    throw Error('页面资源路径无效');
  const 基址 = new URL(基址文);
  const 网址 = new URL(路径文, 基址);
  if (!['http:', 'https:'].includes(网址.protocol) || 网址.origin !== 基址.origin)
    throw Error('页面资源不得跨站');
  const 回应 = await 网络(网址.href, {method: 'GET', redirect: 'error', credentials: 'same-origin'});
  if (回应.status !== 200) throw Error('页面资源不可用');
  if (!回应.body) return '';
  const 读器 = 回应.body.getReader();
  const 诸块 = [];
  let 总数 = 0;
  try {
    for (;;) {
      const {value, done} = await 读器.read();
      if (done) break;
      总数 += value.byteLength;
      if (总数 > 2 * 1024 * 1024) throw Error('页面资源超过二 MiB');
      诸块.push(value);
    }
  } catch (错误) {
    await 读器.cancel().catch(() => {});
    throw 错误;
  } finally { 读器.releaseLock(); }
  const 合 = new Uint8Array(总数);
  let 位 = 0;
  for (const 块 of 诸块) { 合.set(块, 位); 位 += 块.byteLength; }
  return new TextDecoder('utf-8', {fatal: true}).decode(合);
}

// ============================================================================
// 文言：以下三块（事件列、界面订阅器、页面控制）皆为纯工厂，凭注入之根与全局而行，可离 Wasm 独测；创建浏览器宿主惟接线。
// 汉语：下面三块（统一事件队列、界面订阅器、页面控制）都是纯工厂函数，依赖注入的 document 与全局对象，可以脱离 Wasm 单独测试；
//       创建浏览器宿主只负责把它们接到豫言原语上。
// ============================================================================

const 编码器 = new TextEncoder();
// 文言：宿主术之败，归（阴，因）而不越桥。汉语：把宿主原语的同步异常转成（爻, 字符串）结果，避免异常穿过 Wasm 边界而无法被豫言捕获。
const 安全结果 = 函 => {
  try {
    const 果 = 函();
    return [true, 果 === undefined ? '' : String(果)];
  } catch (错) { return [false, String(错?.message ?? 错)]; }
};
// 文言：文之字节数逾限则真；先以码元数粗判，免巨文全编。汉语：判断字符串的 UTF-8 字节数是否超过上限，先用码元数快速判断，只在灰区才真正编码。
const 字节超限 = (文, 限) => {
  if (文.length > 限) return true;
  if (文.length * 3 <= 限) return false;
  return 编码器.encode(文).length > 限;
};
const 字节数 = 文 => 编码器.encode(文).length;

const 标签名 = 目标 => (目标 && typeof 目标.localName === 'string' ? 目标.localName : '');
const 元素标识 = 目标 => (目标 && typeof 目标.id === 'string' ? 目标.id : '');
// 文言：自触发之节上溯，取最近带 data-yy 者之值，至边界（含）而止。汉语：从事件目标向上找最近带 data-yy 属性的元素（含自身），到订阅边界为止；无则空字符串。
const 找操作键 = (目标, 边界) => {
  for (let 节 = 目标; 节; 节 = 节.parentNode) {
    if (节.nodeType === 1 && 节.hasAttribute('data-yy')) return 节.getAttribute('data-yy');
    if (节 === 边界) break;
  }
  return '';
};
const 是键盘事件 = 事件 => typeof 事件.key === 'string' && typeof 事件.code === 'string' && typeof 事件.ctrlKey === 'boolean';
const 是鼠标事件 = 事件 => typeof 事件.button === 'number' && typeof 事件.ctrlKey === 'boolean' && typeof 事件.clientX === 'number';
// 文言：豫言串以码点计，DOM 以码元计；二者互换于宿主。汉语：豫言字符串按 Unicode 码点计数，DOM 按 UTF-16 码元计数，光标位置在宿主里互相换算。
const 码点转码元 = (串, 码点位) => {
  let 码元 = 0;
  let 点 = 0;
  while (点 < 码点位 && 码元 < 串.length) { 码元 += 串.codePointAt(码元) > 0xffff ? 2 : 1; 点++; }
  if (点 < 码点位) throw Error('位置超出文字长度');
  return 码元;
};
const 码元转码点 = (串, 码元位) => {
  let 点 = 0;
  for (let 位 = 0; 位 < 码元位 && 位 < 串.length; 位 += 串.codePointAt(位) > 0xffff ? 2 : 1) 点++;
  return 点;
};

// ---------------------------------------------------------------------------
// 一、统一事件队列
// ---------------------------------------------------------------------------
// 文言：诸源之事同入一列，各类自有其限；溢则去其最旧而记其数，「宿主」类独拒新事，免失待答之请。
// 汉语：所有事件来源共用一个先进先出队列，事件带类型；每个类型独立上限（默认 1024），超限丢弃该类型最旧事件并计数。
//       「宿主」类（低层宿主库的原始事件，如公开操作）保持旧行为：满则报错，因为丢弃其中的调用会让页面侧 Promise 永不返回。
export const 网页事件类型 = Object.freeze(['界面', '消息', '定时', '事件流', '请求', '编译', '可见性', '联机', '历史', '关闭', '宿主']);
const 默认事件队列上限 = 1024;

// 文言：旧式原始事件依其名而归类；已带类型之新式事件从其所标。汉语：给尚未带类型的旧式原始事件分类；新式事件（含合法「类型」字段）保持自己的类型。
export const 分类网页事件 = 事件 => {
  if (typeof 事件.类型 === 'string' && 事件.类型 !== '宿主' && 网页事件类型.includes(事件.类型)) return 事件.类型;
  if (事件.名称 === '定时') return '定时';
  if (事件.名称 === '关闭') return '关闭';
  if (['click', 'input', 'change', 'submit'].includes(事件.名称) && typeof 事件.标识 === 'string' && !('来源句柄' in 事件)) return '界面';
  return '宿主';
};

// 文言：统一之形：类型、序、订阅号、名称及各类所需之字段。汉语：把队列项转成统一事件对象；旧式原始事件在此映射，新式事件本身已是统一形状。
export const 统一事件体 = 项 => {
  const 事 = 项.事件;
  if (项.新式) return {类型: 项.类型, 序: 项.序, ...事};
  switch (项.类型) {
    case '界面': {
      const 体 = {类型: '界面', 序: 项.序, 订阅号: 0, 标识: String(事.标识 ?? ''), 名称: String(事.名称 ?? '')};
      if (项.附加?.操作键) 体.操作键 = 项.附加.操作键;
      体.值 = String(事.值 ?? '');
      体.选中 = Boolean(事.选中);
      return 体;
    }
    case '定时':
      return {类型: '定时', 序: 项.序, 订阅号: Number(事.定时号) || 0, 名称: '定时', 详情: {种类: 事.种类, 标记: 事.标记, 时刻: 事.时刻}};
    case '关闭':
      return {类型: '关闭', 序: 项.序, 订阅号: 0, 名称: '关闭'};
    default:
      return {类型: '宿主', 序: 项.序, 订阅号: 0, 名称: String(事.名称 ?? ''), 详情: 事};
  }
};
// 文言：详情文本身已是癸象，直接拼入，不再解析。汉语：消息事件的详情本来就是 JSON 文本，直接拼接进事件文本，避免对最大 1 MiB 的正文做一次解析与重编码。
export const 统一事件文 = 项 => {
  if (项.详情文 !== undefined || 项.详情错误 !== undefined) {
    const 头 = JSON.stringify({类型: 项.类型, 序: 项.序, ...项.事件});
    const 尾 = 项.详情错误 !== undefined
      ? ',"详情":null,"详情错误":' + JSON.stringify(项.详情错误)
      : ',"详情":' + 项.详情文;
    return 头.slice(0, -1) + 尾 + '}';
  }
  return JSON.stringify(统一事件体(项));
};
// 文言：旧法取事，其形如旧。汉语：旧的 等待浏览器事件 看到的仍是原来的原始 JSON；新式事件没有旧形状，直接用统一形状。
export const 原始事件文 = 项 => (项.新式 ? 统一事件文(项) : JSON.stringify(项.事件));
const 消息错误文 = 错 => '网页消息详情' + 错;

export function 创建事件队列({上限 = {}, 离队钩子 = () => {}, 就绪钩子 = () => {}} = {}) {
  const 队列 = [];
  const 各类计数 = new Map();
  const 各类丢弃 = new Map();
  const 类型上限 = new Map();
  const 等待者 = [];
  let 总丢弃 = 0;
  let 下序 = 0;
  let 已关闭 = false;
  const 取上限 = 类 => 类型上限.get(类) ?? 默认事件队列上限;
  const 设上限 = (类, 限) => {
    if (!网页事件类型.includes(类)) throw Error('网页事件类型无效：' + 类);
    const 数 = Number(限);
    if (!Number.isSafeInteger(数) || 数 < 1 || 数 > 65536) throw Error('网页事件队列上限须为 1 至 65536 的整数');
    类型上限.set(类, 数);
  };
  if (!上限 || typeof 上限 !== 'object' || Array.isArray(上限)) throw Error('事件队列上限须为对象');
  for (const [类, 限] of Object.entries(上限)) 设上限(类, 限);
  const 离队 = 项 => { 各类计数.set(项.类型, (各类计数.get(项.类型) ?? 1) - 1); 离队钩子(项); };
  const 记丢弃 = 类 => { 总丢弃++; 各类丢弃.set(类, (各类丢弃.get(类) ?? 0) + 1); };
  const 入队 = 项 => {
    const 类 = 项.类型;
    // 文言：同订阅同标识之高频事件相接，则以新代旧，序位不移。汉语：合并——尾项与新项同类型且合并键相同，就地替换，保持与其他事件的先后次序。
    if (项.合并键 !== undefined && 队列.length) {
      const 尾 = 队列[队列.length - 1];
      if (尾.类型 === 类 && 尾.合并键 === 项.合并键) { 队列[队列.length - 1] = 项; return; }
    }
    const 限 = 取上限(类);
    if (类 === '宿主') {
      if ((各类计数.get(类) ?? 0) >= 限) throw Error('浏览器事件队列已满');
    } else {
      while ((各类计数.get(类) ?? 0) >= 限) {
        const 位 = 队列.findIndex(旧 => 旧.类型 === 类);
        if (位 < 0) break;
        const [旧] = 队列.splice(位, 1);
        离队(旧);
        记丢弃(类);
      }
    }
    队列.push(项);
    各类计数.set(类, (各类计数.get(类) ?? 0) + 1);
  };
  // 文言：新事至，先付待者；无待者则入列。汉语：投递事件：分配序号；有接受该类型的等待者就直接交付，否则入队并执行背压。
  const 投递 = 项 => {
    if (已关闭) return false;
    项.序 = ++下序;
    const 位 = 等待者.findIndex(者 => 者.接受(项.类型));
    if (位 >= 0) {
      const [者] = 等待者.splice(位, 1);
      离队钩子(项);
      者.完成(者.格式(项));
      return true;
    }
    入队(项);
    return true;
  };
  // 文言：候事：先取列中首个合类者；无则待。已关则返关闭标记。汉语：等待事件：先从队列取第一个类型被接受的事件（不动别的类型）；没有就挂起。
  //       粘滞=真时，关闭后每次调用都返回关闭标记（异步返回，避免不检查关闭的循环霸占线程）。
  const 等待 = (接受, 格式, 关闭值, 粘滞) => {
    就绪钩子();
    for (let 位 = 0; 位 < 队列.length; 位++) {
      if (!接受(队列[位].类型)) continue;
      const [项] = 队列.splice(位, 1);
      离队(项);
      return 格式(项);
    }
    if (已关闭) return 粘滞 ? new Promise(完成 => setTimeout(() => 完成(关闭值()), 0)) : new Promise(() => {});
    return new Promise(完成 => { 等待者.push({接受, 格式, 完成, 关闭值}); });
  };
  const 删除若 = 谓词 => {
    let 数 = 0;
    for (let 位 = 队列.length - 1; 位 >= 0; 位--) {
      if (!谓词(队列[位])) continue;
      const [项] = 队列.splice(位, 1);
      离队(项);
      数++;
    }
    return 数;
  };
  const 关闭 = () => {
    if (已关闭) return;
    已关闭 = true;
    for (const 者 of 等待者.splice(0)) 者.完成(者.关闭值());
  };
  return {
    投递, 等待, 删除若, 关闭, 设上限,
    取上限,
    丢弃数: 类 => (类 === undefined ? 总丢弃 : 各类丢弃.get(类) ?? 0),
    已关闭: () => 已关闭,
    状态: () => ({积压: 队列.length, 各类积压: Object.fromEntries(各类计数), 丢弃: 总丢弃, 各类丢弃: Object.fromEntries(各类丢弃), 等待者: 等待者.length, 序: 下序})
  };
}

// 文言：旧法取界面事，惟返名与标识；取消息，返名、详情与错。汉语：旧的等待界面事件、等待网页消息各自需要的元组形状。
export const 界面事件元组 = 项 => [String(项.事件.名称 ?? ''), String(项.事件.标识 ?? '')];
export const 消息事件元组 = 项 => [
  String(项.事件.名称 ?? ''),
  项.详情错误 !== undefined ? 'null' : (项.详情文 ?? 'null'),
  项.详情错误 !== undefined ? 消息错误文(项.详情错误) : ''
];

// ---------------------------------------------------------------------------
// 二、界面订阅器：委托事件、同步键规则、组字与滚动等载荷
// ---------------------------------------------------------------------------
// 文言：事不冒泡者，必于捕获相而听，方能委托于祖；高频者相接则合。
// 汉语：不冒泡的事件（focus、blur、scroll、close、cancel……）一律在捕获阶段监听，才能在祖先上委托；高频事件默认合并相邻同源事件。
const 不冒泡事件 = new Set(['focus', 'blur', 'scroll', 'close', 'cancel', 'load', 'error', 'toggle', 'invalid', 'mouseenter', 'mouseleave', 'pointerenter', 'pointerleave']);
const 高频事件 = new Set(['scroll', 'resize', 'mousemove', 'pointermove', 'touchmove', 'wheel', 'dragover', 'selectionchange']);
const 无值事件 = new Set([...高频事件, 'mouseover', 'mouseout', 'mouseenter', 'mouseleave', 'pointerover', 'pointerout', 'pointerenter', 'pointerleave']);
const 被动默认事件 = new Set(['scroll', 'wheel', 'touchstart', 'touchmove']);
const 布尔属性名 = new Set(['hidden', 'disabled', 'readonly', 'checked', 'selected', 'open']);
const 策略字段 = new Set(['阻止默认', '停止传播', '停止同处', '捕获', '被动', '合并', '一次', '仅命中', '键规则', '选择器', '带属性', '带矩形']);
const 键规则字段 = new Set(['键', '代码', 'ctrl', 'meta', 'alt', 'shift', '含组字', '仅选区为空', '仅目标标识', '仅当属性']);
const 键事件名 = new Set(['keydown', 'keyup', 'keypress']);

const 检布尔字段 = (对象, 名单, 说明) => {
  for (const 名 of 名单) if (对象[名] !== undefined && typeof 对象[名] !== 'boolean') throw Error(说明 + '须为布尔：' + 名);
};
const 检键规则项 = 律 => {
  if (!律 || typeof 律 !== 'object' || Array.isArray(律)) throw Error('键规则项须为对象');
  for (const 名 of Object.keys(律)) if (!键规则字段.has(名)) throw Error('键规则含未知字段：' + 名);
  if (律.键 === undefined && 律.代码 === undefined) throw Error('键规则须含 键 或 代码');
  for (const 名 of ['键', '代码']) {
    if (律[名] !== undefined && (typeof 律[名] !== 'string' || !律[名] || 律[名].length > 64)) throw Error('键规则字段须为 1 至 64 字符的文字：' + 名);
  }
  检布尔字段(律, ['ctrl', 'meta', 'alt', 'shift', '含组字', '仅选区为空'], '键规则字段');
  if (律.仅目标标识 !== undefined && (typeof 律.仅目标标识 !== 'string' || !律.仅目标标识 || 律.仅目标标识.length > 256)) throw Error('键规则字段 仅目标标识 无效');
  const 条 = 律.仅当属性;
  if (条 !== undefined) {
    if (!条 || typeof 条 !== 'object' || Array.isArray(条)) throw Error('键规则字段 仅当属性 须为对象');
    for (const 名 of Object.keys(条)) if (!['标识', '属性', '值'].includes(名)) throw Error('仅当属性含未知字段：' + 名);
    if (typeof 条.标识 !== 'string' || !条.标识 || 条.标识.length > 256) throw Error('仅当属性.标识 无效');
    if (typeof 条.属性 !== 'string' || !/^[a-z][a-z0-9:_-]{0,63}$/u.test(条.属性)) throw Error('仅当属性.属性 无效');
    if (条.值 !== null && typeof 条.值 !== 'string') throw Error('仅当属性.值 须为文字或 null');
  }
};
// 文言：策略以癸象文定，先验后用；未识之字段必拒，免笔误默失。汉语：解析并严格校验订阅策略 JSON：未知字段、类型错误和自相矛盾的组合都在订阅时报错。
export const 解析界面策略 = (文, 事件名) => {
  let 策;
  try { 策 = 文 === '' ? {} : JSON.parse(文); } catch { throw Error('界面事件策略不是有效 JSON'); }
  if (!策 || typeof 策 !== 'object' || Array.isArray(策)) throw Error('界面事件策略须为 JSON 对象');
  for (const 名 of Object.keys(策)) if (!策略字段.has(名)) throw Error('界面事件策略含未知字段：' + 名);
  检布尔字段(策, ['阻止默认', '停止传播', '停止同处', '捕获', '被动', '合并', '一次', '仅命中', '带矩形'], '界面事件策略字段');
  const 律们 = 策.键规则 ?? [];
  if (!Array.isArray(律们) || 律们.length > 32) throw Error('键规则须为不超过 32 条的数组');
  律们.forEach(检键规则项);
  if (律们.length && !键事件名.has(事件名)) throw Error('键规则只适用于 keydown、keyup、keypress 事件');
  if (策.仅命中 && !律们.length) throw Error('仅命中 须配合非空的键规则');
  // 文言：选择器、带属性、带矩形，订时严验，笔误不默失。汉语：0.2.0 新增字段的静态校验；选择器的语法在订阅时用 DOM 再验一次。
  if (策.选择器 !== undefined && (typeof 策.选择器 !== 'string' || !策.选择器.trim() || 策.选择器.length > 512)) throw Error('选择器须为 1 至 512 个字符的文字');
  if (策.带属性 !== undefined) {
    if (!Array.isArray(策.带属性) || 策.带属性.length > 8) throw Error('带属性须为不超过 8 个属性名的数组');
    for (const 名 of 策.带属性) {
      if (typeof 名 !== 'string' || !/^[a-z][a-z0-9:_-]{0,63}$/u.test(名) || 名.startsWith('on') || 名 === 'style' || 名 === 'srcdoc') throw Error('带属性含无效的属性名：' + String(名).slice(0, 64));
    }
    if (new Set(策.带属性).size !== 策.带属性.length) throw Error('带属性含重复的属性名');
  }
  const 显式动作 = ['阻止默认', '停止传播', '停止同处'].some(名 => 策[名] !== undefined);
  const 会阻止 = 律们.length ? (显式动作 ? Boolean(策.阻止默认) : true) : Boolean(策.阻止默认);
  if (策.被动 === true && (会阻止 || 策.停止传播 || 策.停止同处)) throw Error('被动订阅不能阻止默认或停止传播');
  return {
    阻止默认: 会阻止, 停止传播: Boolean(策.停止传播), 停止同处: Boolean(策.停止同处),
    捕获: Boolean(策.捕获) || 不冒泡事件.has(事件名), 被动: 策.被动, 合并: 策.合并 ?? 高频事件.has(事件名),
    一次: Boolean(策.一次), 仅命中: Boolean(策.仅命中), 键规则: 律们,
    选择器: 策.选择器, 带属性: 策.带属性 ?? [], 带矩形: Boolean(策.带矩形)
  };
};

const 键相同 = (律键, 实键) => 律键 === 实键 || (律键.length === 1 && 实键.length === 1 && 律键.toLowerCase() === 实键.toLowerCase());
const 事件类型之别 = 名 => (名 === 'visibilitychange' ? '可见性' : 名 === 'online' || 名 === 'offline' ? '联机' : 名 === 'popstate' || 名 === 'hashchange' ? '历史' : '界面');
const 度量元素 = 元 => ({顶: Math.round(元.scrollTop), 总高: Math.round(元.scrollHeight), 可视高: Math.round(元.clientHeight)});

export function 创建界面订阅器({根, 全局, 投递, 已关闭 = () => false, 删除若 = () => {}}) {
  const 订阅表 = new Map();
  const 消息表 = new Map();
  let 下号 = 1;
  let 下消息号 = 1;
  const 页面选区为空 = () => {
    const 活 = 根.activeElement;
    if (活 && (标签名(活) === 'input' || 标签名(活) === 'textarea') && typeof 活.selectionStart === 'number' && 活.selectionStart !== 活.selectionEnd) return false;
    const 选 = typeof 根.getSelection === 'function' ? 根.getSelection() : typeof 全局.getSelection === 'function' ? 全局.getSelection() : null;
    return !选 || 选.isCollapsed !== false;
  };
  const 属性条件成立 = 条 => {
    const 元 = 根.getElementById?.(条.标识);
    if (!元) return false;
    const 有 = 元.hasAttribute(条.属性);
    if (条.值 === null) return !有;
    if (布尔属性名.has(条.属性)) return 条.值 === 'true' ? 有 : 条.值 === 'false' ? !有 : false;
    return 有 && 元.getAttribute(条.属性) === 条.值;
  };
  // 文言：键律相合，须诸条件皆备；组字之际，回车方为字选，故默认不合。汉语：一条键规则命中的全部条件；输入法组字期间的按键默认不匹配（含组字=真才匹配）。
  const 匹配键律 = (律, 事件) => {
    if (!律.含组字 && (事件.isComposing || 事件.keyCode === 229)) return false;
    if (律.键 !== undefined && !键相同(律.键, String(事件.key))) return false;
    if (律.代码 !== undefined && 律.代码 !== 事件.code) return false;
    for (const [名, 属] of [['ctrl', 'ctrlKey'], ['meta', 'metaKey'], ['alt', 'altKey'], ['shift', 'shiftKey']]) {
      if (律[名] !== undefined && 律[名] !== Boolean(事件[属])) return false;
    }
    if (律.仅选区为空 && !页面选区为空()) return false;
    if (律.仅目标标识 !== undefined) {
      const 域 = 根.getElementById?.(律.仅目标标识);
      const 目标 = 事件.target;
      if (!域) return false;
      if (!(目标 === 域 || (目标 && typeof 目标.nodeType === 'number' && typeof 域.contains === 'function' && 域.contains(目标)))) return false;
    }
    if (律.仅当属性 !== undefined && !属性条件成立(律.仅当属性)) return false;
    return true;
  };
  const 状态JSON = 状 => {
    if (状 === undefined) return null;
    try {
      const 文 = JSON.stringify(状);
      return typeof 文 === 'string' && 文.length <= 65536 ? JSON.parse(文) : null;
    } catch { return null; }
  };
  // 文言：委托之合：自事目标向上求最近合选择器者，必在订阅之界内；无则此事不与此订阅相涉。
  // 汉语：选择器过滤——取原始目标（文本节点取其父元素）向上（含自身）第一个匹配选择器的元素，且必须在订阅边界之内；没有则返回 null。
  const 找匹配 = (目标, 选择器, 边界) => {
    let 元 = 目标;
    if (元 && 元.nodeType === 3) 元 = 元.parentElement;
    if (!元 || 元.nodeType !== 1 || typeof 元.closest !== 'function') return null;
    let 合;
    try { 合 = 元.closest(选择器); } catch { return null; }
    if (!合) return null;
    if (边界 && !边界.contains(合)) return null;
    return 合;
  };
  const 读属性们 = (元, 名们) => {
    const 果 = {};
    for (const 名 of 名们) {
      if (!元.hasAttribute(名)) continue;
      if (名 === 'value' && 标签名(元) === 'input' && 元.type === 'password') continue;
      const 值 = 元.getAttribute(名);
      if (typeof 值 === 'string' && 值.length <= 4096) 果[名] = 值;
    }
    return 果;
  };
  const 造界面体 = (订阅, 事件, 匹配 = null) => {
    const 原目标 = 事件.target;
    const 目标 = 匹配 ?? 原目标;
    const 名 = 事件.type;
    const 体 = {订阅号: 订阅.号};
    if (订阅.类型 === '界面') {
      const 标 = 标签名(目标);
      体.标识 = 元素标识(目标);
      体.名称 = 名;
      if (匹配) 体.目标标识 = 元素标识(原目标);
      const 键 = 找操作键(目标, 订阅.边界);
      if (键 !== '') 体.操作键 = 键;
      const 属性元 = 目标 && 目标.nodeType === 1 ? 目标 : null;
      if (订阅.策.带属性.length && 属性元) 体.属性 = 读属性们(属性元, 订阅.策.带属性);
      if (订阅.策.带矩形 && 属性元 && typeof 属性元.getBoundingClientRect === 'function') {
        const 框 = 属性元.getBoundingClientRect();
        体.矩形 = {左: Math.round(框.left), 上: Math.round(框.top), 右: Math.round(框.right), 下: Math.round(框.bottom)};
      }
      // 文言：口令之框，其值不入事列。汉语：type=password 的输入框不带值（避免口令流入事件队列与日志）；应用需要时显式用读取节点值。
      if (!无值事件.has(名) && (标 === 'input' || 标 === 'textarea' || 标 === 'select') && !(标 === 'input' && 目标.type === 'password')) {
        const 值 = String(目标.value ?? '');
        if (值.length > 65536) 体.值过长 = true; else 体.值 = 值;
        if (标 === 'input' && (目标.type === 'checkbox' || 目标.type === 'radio')) 体.选中 = Boolean(目标.checked);
      }
      if (是键盘事件(事件)) {
        Object.assign(体, {键: 事件.key, 代码: 事件.code, ctrl: 事件.ctrlKey, meta: 事件.metaKey, alt: 事件.altKey, shift: 事件.shiftKey,
          重复: Boolean(事件.repeat), 组字中: Boolean(事件.isComposing)});
      } else if (是鼠标事件(事件)) {
        Object.assign(体, {ctrl: 事件.ctrlKey, meta: 事件.metaKey, alt: 事件.altKey, shift: 事件.shiftKey, 按钮: 事件.button});
      }
      if (名 === 'compositionstart' || 名 === 'compositionupdate' || 名 === 'compositionend') {
        体.组字中 = 名 !== 'compositionend';
        体.详情 = {数据: String(事件.data ?? '')};
      } else if (名 === 'input' && typeof 事件.inputType === 'string') {
        体.组字中 = Boolean(事件.isComposing);
        体.详情 = {输入类型: 事件.inputType, 数据: 事件.data ?? null};
      } else if (名 === 'focus' || 名 === 'blur' || 名 === 'focusin' || 名 === 'focusout') {
        体.详情 = {关联标识: 元素标识(事件.relatedTarget)};
      } else if (名 === 'scroll') {
        const 元 = 目标 && 目标.nodeType === 9 ? (目标.scrollingElement ?? 目标.documentElement) : 目标;
        if (元 && typeof 元.scrollTop === 'number') 体.详情 = 度量元素(元);
      } else if ((名 === 'close' || 名 === 'cancel') && 标签名(目标) === 'dialog') {
        体.详情 = {返回值: String(目标.returnValue ?? '')};
      }
      体.已阻止默认 = Boolean(事件.defaultPrevented);
      return 体;
    }
    体.名称 = 名;
    if (订阅.类型 === '可见性') 体.详情 = {可见: !(根.hidden ?? false)};
    else if (订阅.类型 === '联机') 体.详情 = {联机: 名 === 'online'};
    else if (名 === 'popstate') 体.详情 = {网址: String(全局.location?.href ?? ''), 状态: 状态JSON(事件.state)};
    else 体.详情 = {旧网址: String(事件.oldURL ?? ''), 新网址: String(事件.newURL ?? '')};
    return 体;
  };

  // 文言：主动撤订并去其未取之事；一次订阅自撤，其已投之事须留。汉语：取消订阅；主动取消同时移除该订阅尚未取走的事件，“一次”订阅自动撤销时则保留刚投递的那个事件。
  const 取消 = (号, 清队 = true) => {
    const 项 = 订阅表.get(号);
    if (!项) return false;
    项.清理();
    订阅表.delete(号);
    if (清队) 删除若(队列项 => 队列项.来源键 === '界面:' + 号);
    return true;
  };
  // 文言：订而后听；键规则、止默认、止传播皆于原生回调即行，豫言不能追改。汉语：订阅界面事件——所有同步决定（键规则、preventDefault、stopPropagation）都在原生回调里做完。
  const 订阅 = (目标名, 事件名, 策略文) => {
    if (已关闭()) throw Error('豫言浏览器宿主已关闭');
    if (订阅表.size >= 256) throw Error('界面事件订阅数量达到上限');
    if (!/^[A-Za-z][A-Za-z0-9_.:-]{0,63}$/u.test(事件名)) throw Error('界面事件名无效：' + 事件名.slice(0, 64));
    const 策 = 解析界面策略(策略文, 事件名);
    if (策.选择器 !== undefined) {
      try { 根.createDocumentFragment().querySelector(策.选择器); }
      catch { throw Error('选择器无效：' + 策.选择器.slice(0, 128)); }
    }
    let 目标;
    let 边界 = null;
    if (目标名 === '文档') 目标 = 根;
    else if (目标名 === '窗口') 目标 = 全局;
    else {
      目标 = 根.getElementById?.(目标名);
      if (!目标) throw Error('网页元素不存在：' + 目标名.slice(0, 128));
      边界 = 目标;
    }
    if (typeof 目标?.addEventListener !== 'function') throw Error('界面事件目标不可订阅：' + 目标名.slice(0, 128));
    const 号 = 下号++;
    const 项 = {号, 名: 事件名, 类型: 事件类型之别(事件名), 边界, 目标, 策, 清理: null};
    const 处理 = 事件 => {
      if (已关闭()) return;
      try {
        let 匹配 = null;
        if (策.选择器 !== undefined) {
          匹配 = 找匹配(事件.target, 策.选择器, 项.边界);
          if (!匹配) return;
        }
        const 有律 = 策.键规则.length > 0;
        const 命中 = 有律 && 是键盘事件(事件) && 策.键规则.some(律 => 匹配键律(律, 事件));
        const 动作 = 有律 ? 命中 : true;
        if (动作 && 策.阻止默认 && 事件.cancelable) 事件.preventDefault();
        if (策.仅命中 && !命中) return;
        const 体 = 造界面体(项, 事件, 匹配);
        // 文言：欲止其默认而事不可取，则明告之。汉语：订阅要求阻止默认，但浏览器不允许取消这个事件时（如未经用户激活的 dialog cancel），载荷带 可取消:false，以解释“已阻止默认”为假。
        if (动作 && 策.阻止默认 && !事件.cancelable) 体.可取消 = false;
        投递({
          类型: 项.类型, 事件: 体, 新式: true, 来源键: '界面:' + 号,
          合并键: 策.合并 ? 号 + ':' + (体.标识 ?? '') : undefined
        });
        if (动作) {
          if (策.停止同处) 事件.stopImmediatePropagation();
          else if (策.停止传播) 事件.stopPropagation();
        }
        if (策.一次) 取消(号, false);
      } catch (错) { 全局.console?.error?.(错); }
    };
    const 选项 = {capture: 策.捕获};
    if (策.被动 !== undefined) 选项.passive = 策.被动;
    else if (被动默认事件.has(事件名)) 选项.passive = !策.阻止默认 && !策.停止传播 && !策.停止同处;
    目标.addEventListener(事件名, 处理, 选项);
    项.清理 = () => 目标.removeEventListener(事件名, 处理, {capture: 策.捕获});
    订阅表.set(号, 项);
    return 号;
  };
  // 文言：旧法只听有标识之元素；祖先已委托同名之事，则旧听让之，免一事二报。
  // 汉语：旧的隐式监听（只报带 id 元素）遇到已被新订阅覆盖的事件就让路，避免同一次点击报两遍。
  const 覆盖 = 事件 => {
    if (!订阅表.size) return false;
    const 目标 = 事件.target;
    for (const 项 of 订阅表.values()) {
      if (项.类型 !== '界面' || 项.名 !== 事件.type) continue;
      if (项.边界 === null) return true;
      if (目标 && typeof 目标.nodeType === 'number' && typeof 项.边界.contains === 'function' && 项.边界.contains(目标)) return true;
    }
    return false;
  };

  const 订阅消息 = 名 => {
    if (typeof 名 !== 'string' || !名.startsWith('豫言') || 字节数(名) > 128) throw Error('网页消息名称无效');
    if (已关闭()) throw Error('豫言浏览器宿主已关闭');
    if (消息表.has(名)) return;
    if (消息表.size >= 128) throw Error('网页消息订阅数量达到上限');
    const 号 = 下消息号++;
    const 处理 = 事件 => {
      if (已关闭()) return;
      try {
        let 详情文;
        let 错;
        try { 详情文 = JSON.stringify(事件.detail === undefined ? null : 事件.detail); } catch { 错 = '不是有效 JSON'; }
        if (错 === undefined && typeof 详情文 !== 'string') 错 = '不是有效 JSON';
        if (错 === undefined && 字节超限(详情文, 1048576)) 错 = '超过一 MiB';
        投递({类型: '消息', 事件: {订阅号: 号, 名称: 名}, 新式: true, 来源键: '消息:' + 号, 详情文: 错 === undefined ? 详情文 : undefined, 详情错误: 错});
      } catch (错) { 全局.console?.error?.(错); }
    };
    根.addEventListener(名, 处理);
    消息表.set(名, {号, 清理: () => 根.removeEventListener(名, 处理)});
  };
  const 取消消息 = 名 => {
    const 项 = 消息表.get(名);
    if (!项) return false;
    项.清理();
    消息表.delete(名);
    删除若(队列项 => 队列项.来源键 === '消息:' + 项.号);
    return true;
  };
  const 清空 = () => {
    for (const 项 of 订阅表.values()) 项.清理();
    订阅表.clear();
    for (const 项 of 消息表.values()) 项.清理();
    消息表.clear();
  };
  return {订阅, 取消, 覆盖, 订阅消息, 取消消息, 清空, 订阅数: () => 订阅表.size, 消息订阅数: () => 消息表.size};
}


// ---------------------------------------------------------------------------
// 三、页面控制：界面读回与控制；受限文树（白册、一次建树、句柄管理）
// ---------------------------------------------------------------------------
// 文言：白册之外，一概不造；危险之签，不许取、不许改。汉语：标签、属性、链接的白册是这里的唯一来源，所有文树函数（含旧函数）都经它校验。
const 危险标签 = new Set(['script', 'style', 'iframe', 'object', 'embed', 'link', 'meta', 'base', 'frame', 'frameset', 'applet']);
export const 允许标签 = new Set([
  'a', 'span', 'small', 'h1', 'h2', 'h3', 'h4', 'h5', 'p', 'section', 'div', 'article', 'b', 'strong', 'br',
  'button', 'table', 'thead', 'tbody', 'tr', 'th', 'td', 'pre', 'code', 'time', 'ul', 'ol', 'li', 'label',
  'select', 'option', 'nav', 'form', 'input', 'textarea', 'details', 'summary'
]);
const 空元素 = new Set(['br', 'input']);
const 输入类型 = new Set(['button', 'submit', 'reset', 'checkbox', 'radio', 'text', 'search', 'number', 'email', 'url', 'tel', 'password', 'range', 'date', 'time', 'datetime-local']);
const 按钮类型 = new Set(['button', 'submit', 'reset']);
const 文字上限 = 8 * 1024 * 1024;
// 文言：可设之属性名，去除与读回皆循此册。汉语：属性白册的名称集合（布尔属性另见 布尔属性名；aria-*、data-* 按前缀判）。
const 白册属性名 = new Set(['class', 'id', 'role', 'type', 'title', 'placeholder', 'tabindex', 'datetime', 'maxlength', 'rows', 'colspan', 'rowspan', 'for', 'value', 'href', 'target']);
const 页面标识式 = /^[^\s"'<>&`\\\u0000-\u001f\u007f]{1,128}$/u;
const 控制字符式 = /[\u0000-\u0008\u000b\u000c\u000e-\u001f\u007f]/u;

// 文言：DOM 之异常译为华言，使豫言得读其因。汉语：把常见 DOMException 翻成中文错误，其余原样抛出。
const 翻译DOM错误 = 错 => {
  switch (错?.name) {
    case 'HierarchyRequestError': return Error('页面节点层级无效：不能把节点放入自身或其后代');
    case 'NotFoundError': return Error('参照节点不是父节点的子节点');
    case 'InvalidStateError': return Error('页面元素当前状态不允许此操作：' + String(错.message ?? ''));
    default: return 错;
  }
};
const 校验类名 = 串 => {
  if (typeof 串 !== 'string') throw Error('页面类名须为文字');
  if (串.length > 512) throw Error('页面类名过长');
  if (/[^\p{L}\p{N}_ -]/u.test(串)) throw Error('页面类名含不允许的字符：仅允许字母、数字、下划线、短横线与空格');
  const 令牌 = 串.split(' ').filter(项 => 项 !== '');
  if (令牌.length > 32) throw Error('页面类名过多（至多 32 个）');
  if (令牌.some(项 => 项.length > 64)) throw Error('单个页面类名过长（至多 64 个字符）');
  return 令牌.join(' ');
};
// 文言：链接限同源径、锚、二种网络址；含空白、控制、反斜杠者拒。汉语：a[href] 允许 /路径（不含 //）、#锚、https:// 与 http://；含空白、控制字符或反斜杠一律拒绝。
const 校验链接 = 值 => {
  if (值.length > 8192 || /[\s\u0000-\u001f\u007f\\]/u.test(值)) throw Error('页面链接不受支持');
  if (值.startsWith('#')) return;
  if (值.startsWith('/') && !值.startsWith('//')) return;
  if (/^https?:\/\/[^/?#]+/iu.test(值)) return;
  throw Error('页面链接不受支持');
};
const 整数参 = (串, 名) => {
  if (!/^-?\d{1,15}$/u.test(串)) throw Error(名 + '须为整数');
  return Number(串);
};
const 布尔参 = 串 => {
  if (串 === 'true') return true;
  if (串 === 'false') return false;
  throw Error('布尔参数须为 true 或 false');
};

export function 创建页面控制({根, 全局, 路径, 网络, 句柄, 释放句柄全部}) {
  const 找元素 = (标识, 含框架 = false) => {
    if (!标识) throw Error('网页元素标识为空');
    const 元 = 根.getElementById?.(标识);
    if (!元) throw Error('网页元素不存在：' + 标识.slice(0, 128));
    const 标 = 标签名(元);
    if (危险标签.has(标) && !(含框架 && 标 === 'iframe')) throw Error('网页元素类型不允许操作：' + 标);
    return 元;
  };
  const 输入控件 = 元 => {
    const 标 = 标签名(元);
    if (标 !== 'input' && 标 !== 'textarea' && 标 !== 'select') throw Error('网页元素不是输入控件：' + 元素标识(元));
    return 元;
  };
  const 文本框 = 元 => {
    const 标 = 标签名(元);
    if (标 !== 'input' && 标 !== 'textarea') throw Error('网页元素不是文本输入框：' + 元素标识(元));
    return 元;
  };
  const 应用类名 = (元, 串) => {
    const 净 = 校验类名(串);
    if (净 === '') 元.removeAttribute('class'); else 元.setAttribute('class', 净);
  };
  const 设文字 = (元, 文) => {
    if (文.length > 文字上限) throw Error('页面文字超过八 MiB');
    if (空元素.has(标签名(元)) && 文 !== '') throw Error('空元素不能含文字：' + 标签名(元));
    元.textContent = 文;
  };
  // 文言：属性验于此一处，名值皆严；宿主留名不许客改。汉语：属性白册——class role type title hidden disabled readonly placeholder tabindex datetime
  //       maxlength rows aria-* data-* 及 id for value checked selected open colspan rowspan href target；`data-yy-` 前缀留给宿主。
  const 应用属性 = (元, 名, 值) => {
    if (typeof 名 !== 'string' || !/^[a-z][a-z0-9-]{0,63}$/u.test(名)) throw Error('页面属性不受支持：' + String(名).slice(0, 64));
    if (typeof 值 !== 'string' || 值.length > 65536) throw Error('页面属性值无效或过长：' + 名);
    if (控制字符式.test(值)) throw Error('页面属性值含控制字符：' + 名);
    const 标 = 标签名(元);
    if (危险标签.has(标) && 标 !== 'iframe') throw Error('页面标签不受支持：' + 标);
    if (名.startsWith('data-yy-')) throw Error('页面属性名为宿主保留：' + 名);
    if (名 === 'data-yy' || /^data-[a-z0-9-]+$/u.test(名) || /^aria-[a-z0-9-]+$/u.test(名)) { 元.setAttribute(名, 值); return; }
    if (标 === 'iframe' && !['class', 'title', 'hidden'].includes(名)) throw Error('页面属性不受支持：iframe 的 ' + 名);
    if (布尔属性名.has(名)) {
      if (值 === '' || 值 === 'true') 元.setAttribute(名, '');
      else if (值 === 'false') 元.removeAttribute(名);
      else throw Error('页面属性值不受支持：' + 名);
      return;
    }
    const 限定 = (可用签, 说明) => { if (!可用签.includes(标)) throw Error('页面属性不受支持：' + 说明); };
    switch (名) {
      case 'class': 应用类名(元, 值); return;
      case 'id': {
        if (值 === '') { 元.removeAttribute('id'); return; }
        if (!页面标识式.test(值)) throw Error('页面标识无效');
        const 旧 = 根.getElementById?.(值);
        if (旧 && 旧 !== 元) throw Error('页面标识已存在：' + 值);
        元.setAttribute('id', 值);
        return;
      }
      case 'role':
        if (!/^[a-z][a-z-]{0,31}$/u.test(值)) throw Error('页面属性值不受支持：role');
        元.setAttribute(名, 值);
        return;
      case 'type':
        限定(['button', 'input'], '仅 button 与 input 可设 type');
        if (!(标 === 'button' ? 按钮类型 : 输入类型).has(值)) throw Error('页面属性值不受支持：type=' + 值.slice(0, 32));
        元.setAttribute(名, 值);
        return;
      case 'title':
      case 'placeholder':
        if (名 === 'placeholder') 限定(['input', 'textarea'], '仅 input 与 textarea 可设 placeholder');
        if (值.length > 4096) throw Error('页面属性值过长：' + 名);
        元.setAttribute(名, 值);
        return;
      case 'tabindex':
        if (值 !== '-1' && 值 !== '0') throw Error('页面属性值不受支持：tabindex 仅允许 -1 与 0');
        元.setAttribute(名, 值);
        return;
      case 'datetime':
        限定(['time'], '仅 time 可设 datetime');
        if (!/^[0-9TZ:.+\- ]{1,64}$/u.test(值)) throw Error('页面属性值不受支持：datetime');
        元.setAttribute(名, 值);
        return;
      case 'maxlength':
      case 'rows':
      case 'colspan':
      case 'rowspan':
        限定(名 === 'maxlength' ? ['input', 'textarea'] : 名 === 'rows' ? ['textarea'] : ['td', 'th'], '此元素不可设 ' + 名);
        if (!/^[0-9]{1,6}$/u.test(值)) throw Error('页面属性值不受支持：' + 名);
        元.setAttribute(名, 值);
        return;
      case 'for':
        限定(['label'], '仅 label 可设 for');
        if (!页面标识式.test(值)) throw Error('页面标识无效');
        元.setAttribute(名, 值);
        return;
      case 'value':
        限定(['input', 'option', 'button'], '仅 input、option、button 可设 value');
        元.setAttribute(名, 值);
        return;
      case 'href':
        限定(['a'], '仅 a 可设 href');
        校验链接(值);
        元.setAttribute(名, 值);
        return;
      case 'target':
        限定(['a'], '仅 a 可设 target');
        if (值 !== '_blank' && 值 !== '_self') throw Error('页面属性值不受支持：target 仅允许 _blank 与 _self');
        元.setAttribute(名, 值);
        if (值 === '_blank') 元.setAttribute('rel', 'noopener noreferrer');
        return;
      default:
        throw Error('页面属性不受支持：' + 名);
    }
  };

  // ---- 界面操作：以元素标识为址，不产生句柄 ----
  const 页面状态 = () => {
    const 隐 = Boolean(根.hidden ?? (根.visibilityState === 'hidden'));
    const 联机 = typeof 全局.navigator?.onLine === 'boolean' ? 全局.navigator.onLine : true;
    let 窄屏 = false;
    try { 窄屏 = Boolean(全局.matchMedia?.('(max-width: 700px)')?.matches); } catch { 窄屏 = false; }
    return JSON.stringify({可见: !隐, 联机, 窄屏});
  };
  const 选区文字 = () => {
    const 活 = 根.activeElement;
    if (活 && (标签名(活) === 'input' || 标签名(活) === 'textarea') && typeof 活.selectionStart === 'number' && 活.selectionStart !== 活.selectionEnd) {
      return String(活.value).slice(活.selectionStart, 活.selectionEnd);
    }
    const 选 = typeof 根.getSelection === 'function' ? 根.getSelection() : typeof 全局.getSelection === 'function' ? 全局.getSelection() : null;
    return 选 ? String(选.toString()) : '';
  };
  const 界面操作表 = {
    // 文言：读逾八 MiB 则拒，免越值桥之界而客不能捕。汉语：读回的文字按 UTF-8 字节至多 8 MiB，超过则报错（值桥单次交换上限 16 MiB，越界会变成无法捕获的宿主异常）。
    读取值: 标识 => {
      const 值 = String(输入控件(找元素(标识)).value);
      if (字节超限(值, 文字上限)) throw Error('网页控件值超过八 MiB，不能读回');
      return 值;
    },
    设置值: (标识, 值) => {
      if (值.length > 文字上限) throw Error('网页控件值超过八 MiB');
      const 元 = 输入控件(找元素(标识));
      if (标签名(元) === 'input' && 元.type === 'file') throw Error('不得设置文件输入框的值');
      元.value = 值;
      if (标签名(元) === 'select' && 元.value !== 值) throw Error('选择框没有此选项值');
    },
    设置显示: (标识, 显) => { 找元素(标识, true).hidden = !布尔参(显); },
    设置禁用: (标识, 是) => { 找元素(标识).toggleAttribute('disabled', 布尔参(是)); },
    设置只读: (标识, 是) => { 找元素(标识).toggleAttribute('readonly', 布尔参(是)); },
    设置类名: (标识, 类) => { 应用类名(找元素(标识, true), 类); },
    设置类标记: (标识, 类, 有) => {
      if (!/^[\p{L}\p{N}_-]{1,64}$/u.test(类)) throw Error('页面类标记须为单个类名：仅允许字母、数字、下划线与短横线，至多 64 个字符');
      找元素(标识, true).classList.toggle(类, 布尔参(有));
    },
    聚焦: 标识 => {
      const 元 = 找元素(标识);
      if (typeof 元.focus !== 'function') throw Error('网页元素不可聚焦');
      元.focus();
    },
    读取聚焦标识: () => 元素标识(根.activeElement),
    包含焦点: 标识 => {
      const 元 = 找元素(标识);
      const 活 = 根.activeElement;
      return 活 && 元.contains(活) ? 'true' : 'false';
    },
    设置光标: (标识, 起串, 止串) => {
      const 元 = 文本框(找元素(标识));
      const 起 = 整数参(起串, '光标起点');
      const 止 = 整数参(止串, '光标止点');
      if (起 < 0 || 止 < 起) throw Error('光标位置无效：须 0 ≤ 起 ≤ 止');
      const 值 = String(元.value);
      try { 元.setSelectionRange(码点转码元(值, 起), 码点转码元(值, 止)); } catch (错) { throw 翻译DOM错误(错); }
    },
    读取光标起: 标识 => {
      const 元 = 文本框(找元素(标识));
      if (typeof 元.selectionStart !== 'number') throw Error('此输入框类型不支持光标');
      return String(码元转码点(String(元.value), 元.selectionStart));
    },
    读取光标止: 标识 => {
      const 元 = 文本框(找元素(标识));
      if (typeof 元.selectionEnd !== 'number') throw Error('此输入框类型不支持光标');
      return String(码元转码点(String(元.value), 元.selectionEnd));
    },
    读取选区文字: () => {
      const 文 = 选区文字();
      if (字节超限(文, 文字上限)) throw Error('选区文字超过八 MiB，不能读回');
      return 文;
    },
    读取滚动度量: 标识 => JSON.stringify(度量元素(找元素(标识))),
    设置滚动顶: (标识, 顶) => { 找元素(标识).scrollTop = 整数参(顶, '滚动位置'); },
    打开模态框: 标识 => {
      const 元 = 找元素(标识);
      if (标签名(元) !== 'dialog') throw Error('网页元素不是 dialog：' + 标识);
      if (typeof 元.showModal !== 'function') throw Error('宿主不支持 dialog 模态框');
      try { 元.showModal(); } catch (错) { throw 翻译DOM错误(错); }
    },
    关闭模态框: 标识 => {
      const 元 = 找元素(标识);
      if (标签名(元) !== 'dialog') throw Error('网页元素不是 dialog：' + 标识);
      if (typeof 元.close !== 'function') throw Error('宿主不支持 dialog 模态框');
      元.close();
    },
    读取页面状态: () => 页面状态(),
    读取节点可见: 标识 => {
      const 元 = 找元素(标识, true);
      const 窗 = typeof 全局.getComputedStyle === 'function' ? 全局 : 根.defaultView;
      if (!窗 || typeof 窗.getComputedStyle !== 'function') throw Error('宿主不支持 getComputedStyle');
      if (!元.isConnected) return 'false';
      for (let 节 = 元; 节 && 节.nodeType === 1; 节 = 节.parentElement) {
        if (窗.getComputedStyle(节).display === 'none') return 'false';
      }
      return 'true';
    },
    // ---- 0.3.0：属性读回与移除、文字读回、点击、位置与矩形 ----
    // 文言：读回之名式与设置同严；“文档根”“页体”乃根与身之保留字。汉语：读取属性原文，返回“1”加值或“0”；标识可为保留字 文档根（html）与 页体（body）。
    读取属性: (标识, 名) => {
      const 元 = 标识 === '文档根' ? 根.documentElement : 标识 === '页体' ? 根.body : 找元素(标识, true);
      if (!元) throw Error('网页元素不存在：' + 标识);
      if (typeof 名 !== 'string' || !/^[a-z][a-z0-9:_-]{0,63}$/u.test(名) || 名.startsWith('on') || 名 === 'style' || 名 === 'srcdoc') throw Error('页面属性不受支持：' + String(名).slice(0, 64));
      if (标签名(元) === 'iframe' && !['class', 'title', 'hidden'].includes(名) && !/^(?:data|aria)-/u.test(名)) throw Error('页面属性不受支持：iframe 的 ' + 名);
      if (!元.hasAttribute(名)) return '0';
      const 值 = String(元.getAttribute(名));
      if (值.length > 65536) throw Error('页面属性值过长：' + 名);
      return '1' + 值;
    },
    移除属性: (标识, 名) => {
      const 元 = 找元素(标识, true);
      if (typeof 名 !== 'string' || !/^[a-z][a-z0-9-]{0,63}$/u.test(名)) throw Error('页面属性不受支持：' + String(名).slice(0, 64));
      if (名.startsWith('data-yy-')) throw Error('页面属性名为宿主保留：' + 名);
      const 通配 = 名 === 'data-yy' || /^data-[a-z0-9-]+$/u.test(名) || /^aria-[a-z0-9-]+$/u.test(名);
      if (!通配 && !布尔属性名.has(名) && !白册属性名.has(名)) throw Error('页面属性不受支持：' + 名);
      if (标签名(元) === 'iframe' && !通配 && !['class', 'title', 'hidden'].includes(名)) throw Error('页面属性不受支持：iframe 的 ' + 名);
      元.removeAttribute(名);
    },
    读取文字: 标识 => {
      const 文 = String(找元素(标识).textContent ?? '');
      if (字节超限(文, 文字上限)) throw Error('网页元素文字超过八 MiB，不能读回');
      return 文;
    },
    // 文言：程序之点，文件框与不合规之链不许。汉语：HTMLElement.click()；文件输入框与 href 不满足链接规则的 a 抛错。
    点击: 标识 => {
      const 元 = 找元素(标识);
      const 标 = 标签名(元);
      if (标 === 'input' && 元.type === 'file') throw Error('不得点击文件输入框');
      if (标 === 'a' && 元.hasAttribute('href')) 校验链接(String(元.getAttribute('href')));
      if (typeof 元.click !== 'function') throw Error('网页元素不可点击');
      元.click();
    },
    设置位置: (标识, 左串, 上串) => {
      const 元 = 找元素(标识);
      const 左 = 整数参(左串, '左位置'), 上 = 整数参(上串, '上位置');
      if (Math.abs(左) > 100000 || Math.abs(上) > 100000) throw Error('元素位置越界：须在 -100000 至 100000 之间');
      元.style.left = 左 + 'px';
      元.style.top = 上 + 'px';
    },
    读取矩形: 标识 => {
      const 框 = 找元素(标识).getBoundingClientRect();
      return JSON.stringify({左: Math.round(框.left), 上: Math.round(框.top), 右: Math.round(框.right), 下: Math.round(框.bottom)});
    },
    设置文字: (标识, 文) => { 设文字(找元素(标识), 文); },
    设置属性: (标识, 名, 值) => { 应用属性(找元素(标识, true), 名, 值); }
  };

  // ---- 文树操作：以句柄为址；句柄由本组产生，须释放 ----
  const 登记节点 = 元 => {
    try { return 句柄.登记(元); } catch { throw Error('页面节点句柄达到上限（' + 句柄.上限 + '），请及时释放不再使用的节点'); }
  };
  const 取节点 = 号 => {
    let 值;
    try { 值 = 句柄.取得(号); } catch { throw Error('页面节点句柄无效：' + 号.slice(0, 32)); }
    if (!值 || typeof 值.nodeType !== 'number') throw Error('句柄不是页面节点：' + 号.slice(0, 32));
    return 值;
  };
  const 取父元素 = 号 => {
    const 节 = 取节点(号);
    if (节.nodeType !== 1) throw Error('父节点须为元素：' + 号.slice(0, 32));
    return 节;
  };
  // 文言：节点既离文树，其身与其下诸柄同释，免柄表渐满。汉语：从文档里移除的子树，宿主同步释放子树内所有节点的句柄（含节点自身），防止句柄表被泄漏填满。
  const 释放子树句柄 = 节点 => {
    for (const [号, 值] of 句柄.条目()) {
      if (值 && typeof 值.nodeType === 'number' && (值 === 节点 || (typeof 节点.contains === 'function' && 节点.contains(值)))) 释放句柄全部(String(号));
    }
  };
  const 建树 = 描述文 => {
    if (描述文.length > 2 * 1024 * 1024) throw Error('页面节点树描述超过 2 MiB');
    let 描述;
    try { 描述 = JSON.parse(描述文); } catch { throw Error('页面节点树描述不是有效 JSON'); }
    const 计 = {节点: 0, 文字字节: 0, 属性字符: 0, 标识: new Set()};
    const 记文字 = 文 => {
      计.文字字节 += 字节数(文);
      if (计.文字字节 > 1048576) throw Error('页面节点树文字总量超过 1 MiB');
    };
    const 造 = (项, 深, 位置) => {
      if (深 > 32) throw Error('页面节点树深度超过 32：' + 位置);
      if (++计.节点 > 2000) throw Error('页面节点树节点数超过 2000');
      if (typeof 项 === 'string') { 记文字(项); return 根.createTextNode(项); }
      if (!项 || typeof 项 !== 'object' || Array.isArray(项)) throw Error('页面节点描述须为对象或文字：' + 位置);
      for (const 键 of Object.keys(项)) {
        if (!['标签', '类', '属性', '文字', '子'].includes(键)) throw Error('页面节点描述含未知字段：' + 键 + '（' + 位置 + '）');
      }
      const 签 = 项.标签;
      if (typeof 签 !== 'string' || !允许标签.has(签)) throw Error('页面标签不受支持：' + String(签).slice(0, 32) + '（' + 位置 + '）');
      const 元 = 根.createElement(签);
      if (项.类 !== undefined) 应用类名(元, 项.类);
      if (项.属性 !== undefined) {
        if (!项.属性 || typeof 项.属性 !== 'object' || Array.isArray(项.属性)) throw Error('页面节点属性须为对象：' + 位置);
        for (const [名, 原值] of Object.entries(项.属性)) {
          const 值 = typeof 原值 === 'boolean' || typeof 原值 === 'number' ? String(原值) : 原值;
          if (typeof 值 !== 'string') throw Error('页面节点属性值须为文字、数字或布尔：' + 名 + '（' + 位置 + '）');
          计.属性字符 += 名.length + 值.length;
          if (计.属性字符 > 1048576) throw Error('页面节点树属性总量超过 1 MiB');
          if (名 === 'id' && 值 !== '') {
            if (计.标识.has(值)) throw Error('页面节点树内标识重复：' + 值.slice(0, 64));
            计.标识.add(值);
          }
          应用属性(元, 名, 值);
        }
      }
      if (项.文字 !== undefined) {
        if (typeof 项.文字 !== 'string') throw Error('页面节点文字须为文字：' + 位置);
        记文字(项.文字);
        if (空元素.has(签) && 项.文字 !== '') throw Error('空元素不能含文字：' + 签 + '（' + 位置 + '）');
        if (项.文字 !== '') 元.appendChild(根.createTextNode(项.文字));
      }
      if (项.子 !== undefined) {
        if (!Array.isArray(项.子)) throw Error('页面节点子项须为数组：' + 位置);
        if (空元素.has(签) && 项.子.length) throw Error('空元素不能含子节点：' + 签 + '（' + 位置 + '）');
        项.子.forEach((子, 序) => { 元.appendChild(造(子, 深 + 1, 位置 + '.子[' + 序 + ']')); });
      }
      return 元;
    };
    if (typeof 描述 === 'string') throw Error('页面节点树的根须为元素描述');
    return 登记节点(造(描述, 1, '根'));
  };
  const 设框架地址 = (标识, 网址) => {
    const 框 = 找元素(标识, true);
    if (标签名(框) !== 'iframe') throw Error('网页元素不是 iframe：' + 标识.slice(0, 128));
    if (!框.hasAttribute('sandbox')) throw Error('iframe 未声明 sandbox，拒绝设置地址');
    if (网址 === '') { 框.removeAttribute('src'); return; }
    if (网址.length > 2048 || /[\s\u0000-\u001f\u007f\\]/u.test(网址)) throw Error('框架地址无效');
    const 页面源 = (() => { try { return new URL(路径).origin; } catch { return 'null'; } })();
    let 解析;
    try { 解析 = new URL(网址, 页面源 === 'null' ? undefined : 路径); } catch { throw Error('框架地址无效'); }
    if (网址.startsWith('/') && !网址.startsWith('//')) {
      if (页面源 === 'null' || 解析.origin !== 页面源) throw Error('框架地址不得跨站');
    } else if (/^https:\/\//iu.test(网址)) {
      const 允许 = (框.getAttribute('data-yy-frame-origins') ?? '').split(' ').filter(项 => 项 !== '')
        .map(项 => { try { const 址 = new URL(项); return 址.protocol === 'https:' ? 址.origin : ''; } catch { return ''; } });
      if (解析.origin !== 页面源 && !允许.includes(解析.origin)) throw Error('框架地址来源未获页面声明：' + 解析.origin);
    } else throw Error('框架地址无效');
    框.setAttribute('src', 网址);
  };
  const 清洗模板子树 = 根元素 => {
    const 全部 = [根元素, ...根元素.querySelectorAll('*')];
    for (const 元 of 全部) {
      if (!根元素.contains(元)) continue;
      if (危险标签.has(标签名(元)) || 标签名(元) === 'template') { 元.remove(); continue; }
      for (const 属 of Array.from(元.attributes)) {
        const 名 = 属.name.toLowerCase();
        const 值 = 属.value.replace(/[\u0000- ]/gu, '');
        if (名.startsWith('on') || 名 === 'srcdoc' || 名 === 'formaction' ||
            (['href', 'src', 'action', 'xlink:href', 'poster'].includes(名) && /^(javascript|vbscript):/iu.test(值))) 元.removeAttribute(属.name);
      }
      if (标签名(元) === 'a' && 元.getAttribute('target') === '_blank') 元.setAttribute('rel', 'noopener noreferrer');
    }
  };
  // 文言：取同源之页，惟采其一节，去脚本与事处而后装之。汉语：取同源 HTML 模板，按标识选出一个元素，剔除脚本类元素与 on* 等危险属性，作为目标元素的唯一子节点装入。
  const 装入模板 = async (资源路径, 选择标识, 目标标识) => {
    const 目标 = 找元素(目标标识);
    const 文 = await 读取同源资源文字(资源路径, 路径, 网络);
    const 模板 = 根.createElement('template');
    模板.innerHTML = 文;
    const 选 = 模板.content.getElementById?.(选择标识);
    if (!选) throw Error('页面模板中没有标识：' + 选择标识.slice(0, 128));
    if (危险标签.has(标签名(选)) || 标签名(选) === 'template') throw Error('页面模板根元素类型不允许：' + 标签名(选));
    const 副本 = 根.importNode(选, true);
    清洗模板子树(副本);
    for (const 子 of Array.from(目标.childNodes)) if (子.nodeType === 1) 释放子树句柄(子);
    目标.replaceChildren(副本);
  };
  const 文树操作表 = {
    取得: 标识 => 登记节点(找元素(标识)),
    新建: (签, 文, 类) => {
      if (!允许标签.has(签)) throw Error('页面标签不受支持：' + 签.slice(0, 32));
      const 元 = 根.createElement(签);
      if (文 !== '') 设文字(元, 文);
      if (类 !== '') 应用类名(元, 类);
      return 登记节点(元);
    },
    设置文字: (号, 文) => { 设文字(取节点(号), 文); },
    设置属性: (号, 名, 值) => { 应用属性(取父元素(号), 名, 值); },
    添加子: (父, 子) => {
      const 父元素 = 取父元素(父);
      const 子节点 = 取节点(子);
      try { 父元素.appendChild(子节点); } catch (错) { throw 翻译DOM错误(错); }
    },
    清空子: 号 => {
      const 元 = 取父元素(号);
      const 旧 = Array.from(元.childNodes);
      元.replaceChildren();
      for (const 子 of 旧) if (子.nodeType === 1) 释放子树句柄(子);
    },
    插入子前: (父, 子, 参照) => {
      const 父元素 = 取父元素(父);
      const 子节点 = 取节点(子);
      const 参照节点 = 参照 === '' ? null : 取节点(参照);
      try { 父元素.insertBefore(子节点, 参照节点); } catch (错) { throw 翻译DOM错误(错); }
    },
    替换子: (父, 子们文) => {
      const 父元素 = 取父元素(父);
      let 诸号;
      try { 诸号 = JSON.parse(子们文); } catch { throw Error('子节点句柄列表不是有效 JSON'); }
      if (!Array.isArray(诸号) || 诸号.length > 2000 || 诸号.some(项 => typeof 项 !== 'string')) throw Error('子节点句柄列表须为不超过 2000 项的文字数组');
      const 新 = 诸号.map(取节点);
      if (new Set(新).size !== 新.length) throw Error('子节点句柄列表含重复项');
      const 旧 = Array.from(父元素.childNodes).filter(节 => !新.includes(节));
      try { 父元素.replaceChildren(...新); } catch (错) { throw 翻译DOM错误(错); }
      for (const 节 of 旧) if (节.nodeType === 1) 释放子树句柄(节);
    },
    移除: 号 => {
      const 节 = 取节点(号);
      节.remove();
      释放子树句柄(节);
    },
    释放: 号 => { 取节点(号); 释放句柄全部(号); },
    追加文字: (号, 文) => {
      const 元 = 取父元素(号);
      if (文.length > 文字上限) throw Error('页面文字超过八 MiB');
      if (空元素.has(标签名(元)) && 文 !== '') throw Error('空元素不能含文字：' + 标签名(元));
      const 末 = 元.lastChild;
      if (末 && 末.nodeType === 3) {
        if (末.length + 文.length > 文字上限) throw Error('页面节点文字超过八 MiB');
        末.appendData(文);
      } else 元.appendChild(根.createTextNode(文));
    },
    构建树: 描述文 => 建树(描述文),
    设置框架地址: (标识, 网址) => { 设框架地址(标识, 网址); },
    读取页面网址: () => String(全局.location?.href ?? 路径)
  };
  const 运行表 = (表, 名, 参) => {
    if (typeof 名 !== 'string' || !Object.hasOwn(表, 名)) throw Error('网页操作不受支持：' + String(名).slice(0, 64));
    return 表[名](...参);
  };
  return {界面操作表, 文树操作表, 装入模板, 运行表, 应用属性, 校验链接};
}


// ---------------------------------------------------------------------------
// 四、网页能力：定时、储存、导航、环境、请求、事件源、编译、应用
// ---------------------------------------------------------------------------
// 文言：诸新包之行术皆归此一表，操作以名，参皆文；校验于此一处，败则抛，由外层归为（阴，因）。
// 汉语：网页定时、网页储存、网页导航、网页环境、网页请求、网页事件源、网页编译、网页应用八个接口的宿主实现。所有操作按“操作名 + 文字参数”调用，
//       由 `豫言_浏览器_网页能力`（同步）与 `豫言_浏览器_网页能力异步` 两个原语进入；校验与限额只在这里写一处。纯工厂函数，可脱离 Wasm 用 JSDOM 单独测试。

// 文言：SSE 之解，依 WHATWG 之规，逐块而喂；行终可为 LF、CR、CRLF；流首 BOM 略之；未毕之事于断连时弃之。
// 汉语：服务器推送事件流解析器：喂入字节块，返回 {事件们, 重试, 致命}；跨块的 CR LF、多字节字符与半行都由它处理。
export function 创建SSE解析器({起始事件号 = '', 单事上限 = 1048576} = {}) {
  let 解码 = new TextDecoder('utf-8');
  let 缓冲 = '';
  let 跳过LF = false;
  let 数据行 = [];
  let 事名 = '';
  let 号缓冲 = 起始事件号;
  let 数据字节 = 0;
  let 重试 = null;
  let 致命 = null;
  const 事们 = [];
  const 分发 = () => {
    if (数据行.length) 事们.push({事件号: 号缓冲, 事件名: 事名 || 'message', 数据: 数据行.join('\n')});
    数据行 = [];
    事名 = '';
    数据字节 = 0;
  };
  const 处理行 = 行 => {
    if (行 === '') { 分发(); return; }
    if (行[0] === ':') return;
    const 冒 = 行.indexOf(':');
    let 字段 = 行;
    let 值 = '';
    if (冒 >= 0) {
      字段 = 行.slice(0, 冒);
      值 = 行.slice(冒 + 1);
      if (值[0] === ' ') 值 = 值.slice(1);
    }
    switch (字段) {
      case 'event': 事名 = 值; break;
      case 'data':
        数据字节 += 字节数(值) + 1;
        if (数据字节 > 单事上限 + 1) { 致命 = '事件过大'; return; }
        数据行.push(值);
        break;
      case 'id': if (!值.includes('\u0000')) 号缓冲 = 值; break;
      case 'retry': if (/^[0-9]{1,9}$/u.test(值)) 重试 = Number(值); break;
      default: break;
    }
  };
  const 喂 = (字节, 结束 = false) => {
    缓冲 += 解码.decode(字节, {stream: !结束});
    let 起 = 0;
    for (let i = 0; i < 缓冲.length && 致命 === null; i++) {
      const 字 = 缓冲[i];
      if (跳过LF) {
        跳过LF = false;
        if (字 === '\n') { 起 = i + 1; continue; }
      }
      if (字 === '\n' || 字 === '\r') {
        处理行(缓冲.slice(起, i));
        起 = i + 1;
        if (字 === '\r') {
          if (缓冲[i + 1] === '\n') { i++; 起 = i + 1; } else if (i + 1 >= 缓冲.length) 跳过LF = true;
        }
      }
    }
    缓冲 = 缓冲.slice(起);
    if (缓冲.length > 单事上限 + 65536) 致命 ??= '事件过大';
    const 出 = {事件们: 事们.splice(0), 重试, 致命};
    重试 = null;
    return 出;
  };
  // 文言：重连则半行与未毕之事皆弃，末事件号独存。汉语：重新连接时丢弃半行和未派发的事件，只保留最后事件号。
  const 重置连接 = () => {
    解码 = new TextDecoder('utf-8');
    缓冲 = ''; 跳过LF = false; 数据行 = []; 事名 = ''; 数据字节 = 0; 重试 = null; 致命 = null;
    事们.length = 0;
  };
  return {喂, 重置连接, 最后号: () => 号缓冲};
}

const 网页能力允许字段 = (对象, 名单, 说明) => {
  for (const 名 of Object.keys(对象)) if (!名单.includes(名)) throw Error(说明 + '含未知字段：' + 名.slice(0, 64));
};
const 检整数串 = (串, 名, 下, 上) => {
  if (typeof 串 !== 'string' || !/^-?\d{1,16}$/u.test(串)) throw Error(名 + '须为整数');
  const 数 = Number(串);
  if (!Number.isSafeInteger(数) || 数 < 下 || 数 > 上) throw Error(名 + '越界：须在 ' + 下 + ' 至 ' + 上 + ' 之间');
  return 数;
};
const 检整数值 = (值, 名, 下, 上) => {
  if (!Number.isSafeInteger(值) || 值 < 下 || 值 > 上) throw Error(名 + '须为 ' + 下 + ' 至 ' + 上 + ' 之间的整数');
  return 值;
};
// 文言：fetch 规范所禁之标头，宿主一概拒之，免默被浏览器吞。汉语：fetch 规范的禁用标头名（含 sec-、proxy- 前缀）；宿主明确拒绝，而不是让浏览器悄悄忽略。
const 禁用标头名 = new Set(['accept-charset', 'accept-encoding', 'access-control-request-headers', 'access-control-request-method', 'connection',
  'content-length', 'cookie', 'cookie2', 'date', 'dnt', 'expect', 'host', 'keep-alive', 'origin', 'referer', 'set-cookie', 'te', 'trailer',
  'transfer-encoding', 'upgrade', 'user-agent', 'via']);
const 请求方法集 = new Set(['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'DELETE']);
const 十兆 = 10 * 1024 * 1024;
const 八兆 = 8 * 1024 * 1024;
const 十六兆 = 16 * 1024 * 1024;
const 背压阈值 = 8;

export function 创建网页能力({根, 全局, 网络, 路径, 储存 = null, 队列, 已关闭 = () => false, 定时 = null,
  编译客户端 = null, 导入模块 = null, 页面应用超时 = 30000}) {
  const 导入 = 导入模块 ?? (地址 => import(地址));
  const 基址 = () => {
    try { return new URL(String(全局.location?.href ?? 路径)); } catch { return new URL(String(路径)); }
  };
  const 检开着 = () => { if (已关闭()) throw Error('豫言浏览器宿主已关闭'); };
  const 睡 = (毫秒, 登记) => new Promise(完成 => {
    const 计时 = 全局.setTimeout(() => { 登记(null); 完成(); }, 毫秒);
    登记(() => { 全局.clearTimeout(计时); 完成(); });
  });

  // 文言：站内之径或同源之绝对址，余者一概不受。汉语：网页请求、事件源用的网址规则：以 / 开头（不以 // 开头）的站内路径，或与当前页面同源的 http(s) 绝对网址。
  const 解析同源网址 = (原, 说明) => {
    if (typeof 原 !== 'string' || !原) throw Error(说明 + '网址不能为空');
    if (原.length > 8192) throw Error(说明 + '网址过长（至多 8192 个 UTF-16 码元）');
    if (/[\u0000-\u001f\u007f\\]/u.test(原)) throw Error(说明 + '网址含控制字符或反斜杠');
    if (!(原.startsWith('/') && !原.startsWith('//')) && !/^https?:\/\//iu.test(原)) throw Error(说明 + '网址须为站内路径（以 / 开头）或同源的绝对网址');
    const 基 = 基址();
    let 址;
    try { 址 = new URL(原, 基); } catch { throw Error(说明 + '网址无法解析'); }
    if (基.origin === 'null' || 址.origin !== 基.origin) throw Error(说明 + '只允许同源网址');
    return 址;
  };
  // 文言：导航之址可相对当前页而解，惟许 http、https。汉语：导航用的网址规则：可以是相对当前页面的引用，解析后协议只能是 http 或 https。
  const 解析导航网址 = 原 => {
    if (typeof 原 !== 'string' || !原) throw Error('导航网址不能为空');
    if (原.length > 8192) throw Error('导航网址过长（至多 8192 个 UTF-16 码元）');
    if (/[\u0000-\u001f\u007f\\]/u.test(原)) throw Error('导航网址含控制字符或反斜杠');
    let 址;
    try { 址 = new URL(原, 基址()); } catch { throw Error('导航网址无法解析'); }
    if (址.protocol !== 'http:' && 址.protocol !== 'https:') throw Error('导航网址只允许 http 或 https');
    return 址;
  };

  // ---- 背压管线：按来源限制未取走的事件数；超限时先缓存，待应用取走后续投 ----
  // 文言：一源之事，列中至多阈数；余者暂存，取一则补一，故不失事亦不塞列。
  // 汉语：事件源与编译进度都经它投递：队列里该来源未取走的事件达到阈值（8）时，新事件暂存在宿主里，应用每取走一个再补投一个；
  //       事件源据 等余量() 暂停读取网络流，从而不丢事件、也不撑爆队列。
  const 管线表 = new Map();
  const 造管线 = (来源键, 类型, 号) => {
    const 待发 = [];
    let 在队 = 0;
    let 等待 = null;
    let 已停 = false;
    const 泵 = () => {
      while (!已停 && 待发.length && 在队 < 背压阈值) {
        const {名称, 详情} = 待发.shift();
        在队++;
        const 成 = 队列.投递({类型, 事件: {订阅号: 号, 名称, 详情}, 新式: true, 来源键});
        if (!成) { 已停 = true; 待发.length = 0; }
      }
      if (等待 && (已停 || 待发.length + 在队 < 背压阈值)) { const 醒 = 等待; 等待 = null; 醒(); }
    };
    const 管 = {
      推: (名称, 详情) => { if (已停) return; 待发.push({名称, 详情}); 泵(); },
      离队: () => { 在队 = Math.max(0, 在队 - 1); 泵(); },
      等余量: () => (已停 || 待发.length + 在队 < 背压阈值 ? Promise.resolve() : new Promise(醒 => { 等待 = 醒; })),
      待数: () => 待发.length + 在队,
      停: () => { 已停 = true; 待发.length = 0; if (等待) { const 醒 = 等待; 等待 = null; 醒(); } }
    };
    管线表.set(来源键, 管);
    return 管;
  };
  const 拆批 = (项们, 计字节) => {
    const 批们 = [];
    let 现 = [];
    let 量 = 0;
    for (const 项 of 项们) {
      const 字节 = 计字节(项);
      if (现.length && (现.length >= 256 || 量 + 字节 > 1048576)) { 批们.push(现); 现 = []; 量 = 0; }
      现.push(项);
      量 += 字节;
    }
    if (现.length) 批们.push(现);
    return 批们;
  };

  // ---- 定时 ----
  const 检标记 = 标记 => {
    if (typeof 标记 !== 'string' || 标记.length > 1024) throw Error('定时标记至多 1024 个 UTF-16 码元');
  };
  const 定时操作 = {
    一次: (毫秒串, 标记) => {
      检开着();
      const 毫秒 = 检整数串(毫秒串, '定时延时毫秒', 0, 2147483647);
      检标记(标记);
      return 定时.造(毫秒, 标记, false);
    },
    重复: (毫秒串, 标记) => {
      检开着();
      const 毫秒 = 检整数串(毫秒串, '定时间隔毫秒', 10, 2147483647);
      检标记(标记);
      return 定时.造(毫秒, 标记, true);
    },
    取消: 号串 => String(定时.取消(String(检整数串(号串, '定时号', 1, Number.MAX_SAFE_INTEGER))))
  };

  // ---- 储存 ----
  const 取储存区 = 区 => {
    if (区 !== '本地' && 区 !== '会话') throw Error('网页储存区域无效：' + String(区).slice(0, 32) + '（只能是 会话 或 本地）');
    return 区;
  };
  const 检储存键 = 键 => {
    if (typeof 键 !== 'string' || !键) throw Error('网页储存键不能为空');
    if (键.length > 512) throw Error('网页储存键至多 512 个 UTF-16 码元');
  };
  const 开储存 = 区 => (区 === '本地' ? (储存 ?? 全局.localStorage) : 全局.sessionStorage);
  const 储存操作 = {
    读取: (区, 键) => {
      取储存区(区); 检储存键(键);
      try {
        const 值 = 开储存(区).getItem(键);
        return 值 === null || 值 === undefined ? '0' : '1' + 值;
      } catch { return '0'; }
    },
    写入: (区, 键, 值) => {
      取储存区(区); 检储存键(键);
      if (typeof 值 !== 'string' || 值.length > 1048576) throw Error('网页储存值至多 1048576 个 UTF-16 码元');
      try { 开储存(区).setItem(键, 值); return 'true'; } catch { return 'false'; }
    },
    删除: (区, 键) => {
      取储存区(区); 检储存键(键);
      try { 开储存(区).removeItem(键); } catch { /* 文言：储存不可用则无事。汉语：储存不可用时删除什么也不做。 */ }
      return '';
    }
  };

  // ---- 导航 ----
  const 导航操作 = {
    前往: 址 => { 全局.location.assign(解析导航网址(址).href); return ''; },
    替换当前: 址 => { 全局.location.replace(解析导航网址(址).href); return ''; },
    重载: () => { 全局.location.reload(); return ''; },
    压入: 址 => {
      const 解 = 解析导航网址(址);
      if (解.origin !== 基址().origin) throw Error('历史记录只允许同源网址');
      try { 全局.history.pushState(null, '', 解.href); } catch (错) { throw Error('浏览器拒绝压入历史：' + String(错?.message ?? 错)); }
      return '';
    },
    替换历史: 址 => {
      const 解 = 解析导航网址(址);
      if (解.origin !== 基址().origin) throw Error('历史记录只允许同源网址');
      try { 全局.history.replaceState(null, '', 解.href); } catch (错) { throw Error('浏览器拒绝替换历史：' + String(错?.message ?? 错)); }
      return '';
    },
    移动: 数串 => {
      const 步 = 检整数串(数串, '历史步数', -50, 50);
      if (步 === 0) throw Error('历史步数不能为 0（重载请用重载页面）');
      全局.history.go(步);
      return '';
    }
  };

  // ---- 环境 ----
  const 环境操作 = {
    时区偏移: 毫秒串 => {
      const 毫秒 = 检整数串(毫秒串, '时间毫秒', -8640000000000000, 8640000000000000);
      const 偏 = -new (全局.Date ?? Date)(毫秒).getTimezoneOffset();
      return String(Math.round(偏) + 0);
    },
    饼: 名 => {
      if (typeof 名 !== 'string' || !名 || 名.length > 256 || /[=;\s\u0000-\u001f\u007f]/u.test(名)) throw Error('饼名称无效：不得为空、超过 256 个 UTF-16 码元，或含等号、分号、空白、控制字符');
      let 全 = '';
      try { 全 = String(根.cookie ?? ''); } catch { return '0'; }
      for (const 段 of 全.split(';')) {
        const 项 = 段.trim();
        const 等 = 项.indexOf('=');
        if (等 > 0 && 项.slice(0, 等) === 名) return '1' + 项.slice(等 + 1);
      }
      return '0';
    }
  };

  // ---- 请求 ----
  const 请求表 = new Map();
  let 下请求号 = 1;
  const 检请求 = 文 => {
    if (typeof 文 !== 'string' || 文.length > 十兆) throw Error('网页请求 JSON 过大（至多 10 MiB）');
    let 对象;
    try { 对象 = JSON.parse(文); } catch { throw Error('网页请求不是有效 JSON'); }
    if (!对象 || typeof 对象 !== 'object' || Array.isArray(对象)) throw Error('网页请求须为 JSON 对象');
    网页能力允许字段(对象, ['网址', '方法', '标头', '正文', '缓存', '超时毫秒', '正文上限', '标记'], '网页请求');
    const 址 = 解析同源网址(对象.网址, '网页请求');
    const 方法 = 对象.方法 ?? 'GET';
    if (typeof 方法 !== 'string' || !请求方法集.has(方法)) throw Error('网页请求方法只能是 GET、HEAD、POST、PUT、PATCH、DELETE');
    const 标头 = {};
    if (对象.标头 !== undefined) {
      const 头们 = 对象.标头;
      if (!头们 || typeof 头们 !== 'object' || Array.isArray(头们)) throw Error('网页请求标头须为对象');
      const 名们 = Object.keys(头们);
      if (名们.length > 32) throw Error('网页请求标头至多 32 项');
      for (const 名 of 名们) {
        const 值 = 头们[名];
        if (!/^[A-Za-z0-9-]{1,64}$/u.test(名)) throw Error('网页请求标头名无效：' + 名.slice(0, 64));
        const 小 = 名.toLowerCase();
        if (禁用标头名.has(小) || 小.startsWith('sec-') || 小.startsWith('proxy-')) throw Error('网页请求标头名被禁用：' + 名);
        if (typeof 值 !== 'string' || 值.length > 4096 || /[\u0000-\u001f\u007f]/u.test(值)) throw Error('网页请求标头值无效：' + 名);
        标头[名] = 值;
      }
    }
    let 正文;
    if (对象.正文 !== undefined) {
      if (typeof 对象.正文 !== 'string') throw Error('网页请求正文须为文字');
      if (方法 === 'GET' || 方法 === 'HEAD') throw Error('GET 与 HEAD 请求不能带正文');
      if (字节超限(对象.正文, 八兆)) throw Error('网页请求正文至多 8 MiB');
      正文 = 对象.正文;
    }
    const 缓存 = 对象.缓存 ?? '默认';
    if (缓存 !== '默认' && 缓存 !== '不缓存') throw Error('网页请求缓存只能是 默认 或 不缓存');
    const 超时毫秒 = 对象.超时毫秒 === undefined ? 60000 : 检整数值(对象.超时毫秒, '网页请求超时毫秒', 1, 600000);
    const 正文上限 = 对象.正文上限 === undefined ? 八兆 : 检整数值(对象.正文上限, '网页请求正文上限', 1, 十六兆);
    const 标记 = 对象.标记 ?? '';
    if (typeof 标记 !== 'string' || 标记.length > 256) throw Error('网页请求标记至多 256 个 UTF-16 码元');
    return {址, 方法, 标头, 正文, 缓存, 超时毫秒, 正文上限, 标记};
  };
  const 登记请求 = (阻塞) => {
    检开着();
    if (请求表.size >= 32) throw Error('网页请求并发达到上限（32）');
    const 号 = 下请求号++;
    const 项 = {号, 阻塞, 控制器: new AbortController(), 原因: null, 已投: false};
    请求表.set(号, 项);
    return 项;
  };
  const 读正文 = async (响应, 上限, 项) => {
    if (!响应.body) return new Uint8Array(0);
    const 读器 = 响应.body.getReader();
    const 诸块 = [];
    let 总数 = 0;
    try {
      for (;;) {
        const {value, done} = await 读器.read();
        if (done) break;
        总数 += value.byteLength;
        if (总数 > 上限) { 项.原因 = '正文过大'; await 读器.cancel().catch(() => {}); throw Error('正文过大'); }
        诸块.push(value);
      }
    } finally { try { 读器.releaseLock(); } catch { /* 文言：已释。汉语：读取器已被取消时释放会抛错，忽略。 */ } }
    const 合 = new Uint8Array(总数);
    let 位 = 0;
    for (const 块 of 诸块) { 合.set(块, 位); 位 += 块.byteLength; }
    return 合;
  };
  // 文言：成败皆归一癸象，网败不发事故；取消与宿主闭则无果。汉语：执行一个请求，返回响应对象（成为真或假）；因取消或宿主关闭而中止时返回 null（不产生任何结果）。
  const 执行请求 = async (规, 项) => {
    const 计时 = 全局.setTimeout(() => { 项.原因 ??= '超时'; 项.控制器.abort(); }, 规.超时毫秒);
    try {
      const 初 = {method: 规.方法, headers: 规.标头, credentials: 'same-origin', mode: 'same-origin', redirect: 'follow', signal: 项.控制器.signal};
      if (规.缓存 === '不缓存') 初.cache = 'no-store';
      if (规.正文 !== undefined) 初.body = 规.正文;
      const 响应 = await 网络(规.址.href, 初);
      const 字节 = 规.方法 === 'HEAD' ? new Uint8Array(0) : await 读正文(响应, 规.正文上限, 项);
      let 正文;
      try { 正文 = new TextDecoder('utf-8', {fatal: true}).decode(字节); }
      catch { return {成: false, 原因: '正文不是有效UTF-8', 说明: '响应正文不是有效的 UTF-8 文字', 标记: 规.标记}; }
      const 标头 = {};
      for (const [名, 值] of 响应.headers ?? []) 标头[String(名).toLowerCase()] = String(值);
      return {成: true, 状态: 响应.status, 状态文: String(响应.statusText ?? ''), 网址: String(响应.url || 规.址.href), 标头, 正文, 字节数: 字节.byteLength, 标记: 规.标记};
    } catch (错) {
      if (项.原因 === '取消' || 项.原因 === '关闭') return null;
      if (项.原因 === '超时') return {成: false, 原因: '超时', 说明: '请求超过 ' + 规.超时毫秒 + ' 毫秒未完成', 标记: 规.标记};
      if (项.原因 === '正文过大') return {成: false, 原因: '正文过大', 说明: '响应正文超过 ' + 规.正文上限 + ' 字节上限', 标记: 规.标记};
      return {成: false, 原因: '网络错误', 说明: '网络请求失败：' + String(错?.message ?? 错).slice(0, 200), 标记: 规.标记};
    } finally { 全局.clearTimeout(计时); }
  };
  const 请求操作 = {
    发起: 文 => {
      const 规 = 检请求(文);
      const 项 = 登记请求(false);
      const 键 = '请求:' + 项.号;
      执行请求(规, 项).then(响应 => {
        if (响应 === null || 项.原因 === '取消' || 项.原因 === '关闭') { 请求表.delete(项.号); return; }
        项.已投 = true;
        const 成 = 队列.投递({类型: '请求', 事件: {订阅号: 项.号, 名称: 响应.成 ? '完成' : '失败', 详情: 响应}, 新式: true, 来源键: 键});
        if (!成) 请求表.delete(项.号);
      }, () => { 请求表.delete(项.号); });
      return String(项.号);
    },
    取消: 号串 => {
      const 号 = 检整数串(号串, '请求号', 1, Number.MAX_SAFE_INTEGER);
      const 项 = 请求表.get(号);
      if (!项 || 项.阻塞) return 'false';
      项.原因 = '取消';
      项.控制器.abort();
      队列.删除若(队列项 => 队列项.来源键 === '请求:' + 号);
      请求表.delete(号);
      return 'true';
    }
  };
  const 同步请求 = async 文 => {
    const 规 = 检请求(文);
    const 项 = 登记请求(true);
    try {
      const 响应 = await 执行请求(规, 项);
      return JSON.stringify(响应 ?? {成: false, 原因: '网络错误', 说明: '豫言浏览器宿主已关闭', 标记: 规.标记});
    } finally { 请求表.delete(项.号); }
  };

  // ---- 事件源 ----
  const 事件源表 = new Map();
  let 下事件源号 = 1;
  const 解析事件源选项 = 文 => {
    if (typeof 文 !== 'string') throw Error('事件源选项须为文字');
    let 对象 = {};
    if (文.trim() !== '') {
      try { 对象 = JSON.parse(文); } catch { throw Error('事件源选项不是有效 JSON'); }
    }
    if (!对象 || typeof 对象 !== 'object' || Array.isArray(对象)) throw Error('事件源选项须为 JSON 对象');
    网页能力允许字段(对象, ['起始事件号', '重连毫秒'], '事件源选项');
    const 起始事件号 = 对象.起始事件号 ?? '';
    if (typeof 起始事件号 !== 'string' || 起始事件号.length > 256 || /[\r\n\u0000]/u.test(起始事件号)) throw Error('事件源起始事件号无效：至多 256 个 UTF-16 码元，不含换行与 NUL');
    const 重连毫秒 = 对象.重连毫秒 === undefined ? 3000 : 检整数值(对象.重连毫秒, '事件源重连毫秒', 100, 60000);
    return {起始事件号, 重连毫秒};
  };
  const 事件源终清 = 项 => {
    if (项.终止 && 项.管.待数() === 0) { 事件源表.delete(项.号); 管线表.delete(项.键); }
  };
  const 运行事件源 = async (项, 址, 选项) => {
    const 解析器 = 创建SSE解析器({起始事件号: 选项.起始事件号});
    let 重连 = 选项.重连毫秒;
    let 首次 = true;
    const 投批 = 事们 => {
      for (const 批 of 拆批(事们, 事 => 字节数(事.数据) + 64)) 项.管.推('消息', {批});
    };
    while (!项.已关) {
      项.控制器 = new AbortController();
      let 致命 = null;
      let 状态码;
      let 原因 = '网络中断';
      try {
        const 头 = {Accept: 'text/event-stream', 'Cache-Control': 'no-cache'};
        if (解析器.最后号()) 头['Last-Event-ID'] = 解析器.最后号();
        const 响应 = await 网络(址.href, {method: 'GET', headers: 头, credentials: 'same-origin', mode: 'same-origin', cache: 'no-store', signal: 项.控制器.signal});
        if (项.已关) { try { await 响应.body?.cancel(); } catch { /* 忽略 */ } return; }
        const 类型头 = String(响应.headers?.get?.('content-type') ?? '');
        if (响应.status !== 200) {
          致命 = 'HTTP 状态 ' + 响应.status; 状态码 = 响应.status;
          try { await 响应.body?.cancel(); } catch { /* 忽略 */ }
        } else if (!/^\s*text\/event-stream\s*(?:;|$)/iu.test(类型头)) {
          致命 = '内容类型不是 text/event-stream';
          try { await 响应.body?.cancel(); } catch { /* 忽略 */ }
        } else {
          解析器.重置连接();
          项.管.推('打开', {重连: !首次});
          首次 = false;
          const 读器 = 响应.body.getReader();
          try {
            for (;;) {
              const {value, done} = await 读器.read();
              if (项.已关) return;
              const 出 = 解析器.喂(done ? new Uint8Array(0) : value, done);
              投批(出.事件们);
              if (出.重试 !== null) 重连 = Math.min(60000, Math.max(100, 出.重试));
              if (出.致命) { 致命 = 出.致命; await 读器.cancel().catch(() => {}); break; }
              if (done) break;
              await 项.管.等余量();
            }
          } finally { try { 读器.releaseLock(); } catch { /* 忽略 */ } }
          if (!致命) 原因 = '服务端关闭';
        }
      } catch (错) {
        if (项.已关) return;
        原因 = '网络中断';
      }
      if (项.已关) return;
      if (致命) {
        项.终止 = true;
        项.管.推('错误', 状态码 === undefined ? {状态: '已关闭', 原因: 致命} : {状态: '已关闭', 原因: 致命, HTTP状态: 状态码});
        事件源终清(项);
        return;
      }
      项.管.推('错误', {状态: '重连中', 原因});
      await 睡(重连, 唤 => { 项.唤醒睡 = 唤; });
    }
  };
  const 事件源操作 = {
    打开: (址串, 选项文) => {
      检开着();
      const 址 = 解析同源网址(址串, '事件源');
      const 选项 = 解析事件源选项(选项文);
      if (事件源表.size >= 8) throw Error('事件源数量达到上限（8）');
      const 号 = 下事件源号++;
      const 键 = '事件源:' + 号;
      const 项 = {号, 键, 管: 造管线(键, '事件流', 号), 已关: false, 终止: false, 控制器: null, 唤醒睡: null};
      事件源表.set(号, 项);
      运行事件源(项, 址, 选项).catch(错 => { 全局.console?.error?.(错); });
      return String(号);
    },
    关闭: 号串 => {
      const 号 = 检整数串(号串, '事件源号', 1, Number.MAX_SAFE_INTEGER);
      const 项 = 事件源表.get(号);
      if (!项) return 'false';
      const 有事 = 项.管.待数() > 0;
      项.已关 = true;
      try { 项.控制器?.abort(); } catch { /* 忽略 */ }
      项.唤醒睡?.();
      项.管.停();
      队列.删除若(队列项 => 队列项.来源键 === 项.键);
      事件源表.delete(号);
      管线表.delete(项.键);
      return String(!项.终止 || 有事);
    }
  };

  // ---- 编译 ----
  let 下编译号 = 1;
  let 当前编译 = null;
  const 编译可用 = () => typeof 全局.WebAssembly?.promising === 'function' && typeof 全局.Worker === 'function' && typeof 全局.DecompressionStream === 'function';
  const 检编译文件 = 文 => {
    if (typeof 文 !== 'string') throw Error('本地编译文件须为 JSON 文字');
    let 对象;
    try { 对象 = JSON.parse(文); } catch { throw Error('本地编译文件不是有效 JSON'); }
    if (!对象 || typeof 对象 !== 'object' || Array.isArray(对象)) throw Error('本地编译文件须为 {"路径":"内容"} 对象');
    const 名们 = Object.keys(对象);
    if (名们.length === 0) throw Error('本地编译至少需要一个文件');
    if (名们.length > 4096) throw Error('本地编译文件至多 4096 个');
    let 总 = 0;
    for (const 名 of 名们) {
      if (!名 || 名.length > 512 || 名.includes('\u0000')) throw Error('本地编译文件路径无效：1 至 512 个 UTF-16 码元且不含 NUL');
      if (typeof 对象[名] !== 'string') throw Error('本地编译文件内容须为文字：' + 名.slice(0, 64));
      总 += 字节数(对象[名]);
      if (总 > 32 * 1024 * 1024) throw Error('本地编译内容总量至多 32 MiB');
    }
    return 对象;
  };
  const 取编译客户端 = async () => {
    if (编译客户端 && typeof 编译客户端 === 'object') return 编译客户端;
    const 地址 = typeof 编译客户端 === 'string' ? new URL(编译客户端, 基址()).href : new URL('编译/客户端.mjs', 基址()).href;
    const 模块 = await 导入(地址);
    if (typeof 模块?.浏览器编译 !== 'function' || typeof 模块?.停止编译 !== 'function') throw Error('缺少导出 浏览器编译 或 停止编译');
    return 模块;
  };
  const 映射编译事件 = 事 => {
    if (事?.type === 'stage') return {类: '阶段', 阶段: String(事.phase ?? ''), 标签: String(事.label ?? '')};
    if (事?.type === 'output') return {类: '输出', 流: 事.stream === 'stdout' ? 'stdout' : 'stderr', 文字: String(事.text ?? '')};
    if (事?.type === 'diagnostic') return {类: '诊断', 文字: String(事.text ?? '')};
    return null;
  };
  const 运行编译 = async (项, 文件) => {
    let 待批 = [];
    let 批计时 = null;
    const 冲 = () => {
      if (批计时 !== null) { 全局.clearTimeout(批计时); 批计时 = null; }
      if (!待批.length) return;
      for (const 批 of 拆批(待批, 事 => 字节数(事.文字 ?? '') + 字节数(事.标签 ?? '') + 64)) 项.管.推('进度', {批});
      待批 = [];
    };
    const 报告 = 事 => {
      const 映 = 映射编译事件(事);
      if (!映) return;
      待批.push(映);
      if (待批.length >= 64) 冲();
      else if (批计时 === null) 批计时 = 全局.setTimeout(冲, 50);
    };
    let 结果;
    try {
      项.客户端 = await 取编译客户端();
      if (项.请求停止) 结果 = {ok: false, error: '已停止'};
      else 结果 = await 项.客户端.浏览器编译({files: 文件}, true, 报告);
    } catch (错) {
      结果 = {ok: false, error: '编译客户端不可用：' + String(错?.message ?? 错).slice(0, 300)};
    }
    冲();
    if (当前编译 === 项) 当前编译 = null;
    项.管.推('完成', {
      成: 结果?.ok === true, 标准输出: String(结果?.stdout ?? ''), 标准错误: String(结果?.stderr ?? ''),
      错误: String(结果?.error ?? ''), 阶段: String(结果?.phase ?? ''), 产物摘要: String(结果?.artifact?.sha256 ?? '')
    });
  };
  const 编译操作 = {
    可用: () => String(编译可用()),
    启动: 文 => {
      检开着();
      const 文件 = 检编译文件(文);
      if (当前编译) throw Error('已有本地编译在运行');
      const 号 = 下编译号++;
      const 键 = '编译:' + 号;
      const 项 = {号, 键, 管: 造管线(键, '编译', 号), 客户端: null, 请求停止: false};
      当前编译 = 项;
      运行编译(项, 文件).catch(错 => { 全局.console?.error?.(错); });
      return String(号);
    },
    停止: () => {
      const 项 = 当前编译;
      if (!项) return 'false';
      项.请求停止 = true;
      try { 项.客户端?.停止编译(); } catch { /* 文言：客户端自败则以完成事报之。汉语：客户端停止时抛错则忽略，编译最终仍会以“完成”事件收尾。 */ }
      return 'true';
    }
  };

  // ---- 应用 ----
  const 已启应用 = new Map();
  const 检应用路径 = 串 => {
    if (typeof 串 !== 'string' || !串) throw Error('页面应用路径不能为空');
    if (串.length > 512) throw Error('页面应用路径过长（至多 512 个 UTF-16 码元）');
    if (!串.startsWith('/') || 串.startsWith('//')) throw Error('页面应用路径须以单个 / 开头');
    if (!串.endsWith('/入口.mjs')) throw Error('页面应用路径须以 /入口.mjs 结尾');
    if (/[?#\\%\u0000-\u001f\u007f]/u.test(串)) throw Error('页面应用路径不得含查询、片段、反斜杠、百分号编码与控制字符');
    if (串.split('/').some(段 => 段 === '.' || 段 === '..')) throw Error('页面应用路径不得含 . 或 .. 目录段');
    return 串;
  };
  const 启动页面应用 = async 串 => {
    检开着();
    const 路 = 检应用路径(串);
    if (已启应用.has(路)) throw Error('页面应用已启动：' + 路);
    if (已启应用.size >= 8) throw Error('页面应用数量达到上限（8）');
    已启应用.set(路, null);
    try {
      const 模块 = await 导入(new URL(路, 基址()).href);
      if (typeof 模块?.启动豫言浏览器应用 !== 'function') throw Error('模块没有导出 启动豫言浏览器应用');
      const 实例 = await 模块.启动豫言浏览器应用();
      已启应用.set(路, 实例);
      实例?.完成?.catch?.(() => {});
      let 计时;
      const 超时 = new Promise((_, 拒) => { 计时 = 全局.setTimeout(() => 拒(Error('页面应用 ' + 页面应用超时 + ' 毫秒内未就绪')), 页面应用超时); });
      try { await Promise.race([实例.就绪, 超时]); } finally { 全局.clearTimeout(计时); }
    } catch (错) {
      const 半启 = 已启应用.get(路);
      if (半启) { try { 半启.关闭?.(); } catch { /* 文言：半启之应用尽力而闭。汉语：启动失败时尽力关闭已创建的子应用。 */ } }
      已启应用.delete(路);
      throw Error('页面应用启动失败：' + String(错?.message ?? 错).slice(0, 300));
    }
    return '';
  };

  // ---- 分派 ----
  const 表们 = {定时: 定时操作, 储存: 储存操作, 导航: 导航操作, 环境: 环境操作, 请求: 请求操作, 事件源: 事件源操作, 编译: 编译操作};
  const 找操作 = 名 => {
    const 点 = typeof 名 === 'string' ? 名.indexOf('.') : -1;
    const 表 = 点 > 0 ? 表们[名.slice(0, 点)] : undefined;
    const 函 = 表 && Object.hasOwn(表, 名.slice(点 + 1)) ? 表[名.slice(点 + 1)] : undefined;
    if (typeof 函 !== 'function') throw Error('网页能力操作不受支持：' + String(名).slice(0, 64));
    return 函;
  };
  const 运行 = (名, 参 = []) => {
    const 果 = 找操作(名)(...参);
    return 果 === undefined ? '' : String(果);
  };
  const 异步运行 = async (名, 参 = []) => {
    if (名 === '请求.同步') return 同步请求(参[0]);
    if (名 === '应用.启动') return 启动页面应用(参[0]);
    throw Error('网页能力异步操作不受支持：' + String(名).slice(0, 64));
  };
  // 文言：事出列则通其管线，请求之表亦削。汉语：队列项离开队列（被取走、丢弃、撤销）时调用：给背压管线放行，并清理请求登记。
  const 离队 = 项 => {
    if (!项.来源键) return;
    const 管 = 管线表.get(项.来源键);
    if (管) {
      管.离队();
      for (const 源 of 事件源表.values()) if (源.键 === 项.来源键) 事件源终清(源);
      return;
    }
    if (项.来源键.startsWith('请求:')) 请求表.delete(Number(项.来源键.slice(3)));
  };
  const 清理 = () => {
    for (const 项 of 请求表.values()) { 项.原因 = '关闭'; try { 项.控制器.abort(); } catch { /* 忽略 */ } }
    请求表.clear();
    for (const 项 of 事件源表.values()) {
      项.已关 = true;
      try { 项.控制器?.abort(); } catch { /* 忽略 */ }
      项.唤醒睡?.();
      项.管.停();
    }
    事件源表.clear();
    if (当前编译) {
      当前编译.请求停止 = true;
      try { 当前编译.客户端?.停止编译(); } catch { /* 忽略 */ }
      当前编译.管.停();
      当前编译 = null;
    }
    for (const 管 of 管线表.values()) 管.停();
    管线表.clear();
    for (const 实例 of 已启应用.values()) { try { 实例?.关闭?.(); } catch { /* 忽略 */ } }
    已启应用.clear();
  };
  const 状态 = () => ({请求数: 请求表.size, 事件源数: 事件源表.size, 编译中: 当前编译 !== null, 页面应用数: 已启应用.size, 管线数: 管线表.size});
  return {运行, 异步运行, 离队, 清理, 状态};
}


// ---------------------------------------------------------------------------
// 五、浏览器宿主：把上面的工厂接到豫言原语
// ---------------------------------------------------------------------------
export function 创建浏览器宿主({程序模块, 值桥模块, 根 = globalThis.document ?? globalThis, 网络 = fetch, 储存 = null, 全局 = globalThis, 路径 = 全局.document?.baseURI ?? 全局.location?.href ?? import.meta.url, 输出 = () => {}, 队列上限 = {}, 编译客户端 = null, 导入模块 = null, 页面应用超时 = 30000}) {
  let 网页能力 = null;
  const 定时器 = new Map();
  const 定时待处理 = new Set();
  // 文言：诸事归一列；事离列则销其定时待办之记。汉语：统一事件队列；事件离开队列（被取走、丢弃或直接交付）时清理定时器「待处理」标记，使周期定时器能继续投递。
  const 队列 = 创建事件队列({
    上限: 队列上限,
    离队钩子: 项 => {
      if (项.事件?.名称 === '定时' && 项.事件.定时号 !== undefined) 定时待处理.delete(项.事件.定时号);
      网页能力?.离队(项);
    },
    就绪钩子: () => 标记就绪()
  });
  let 下定时号 = 1;
  const 动画帧 = new Map();
  let 下帧号 = 1;
  const 空闲回调 = new Map();
  let 下空闲号 = 1;
  const 句柄 = 创建句柄表();
  const 订阅 = new Map();
  let 下订阅号 = 1;
  const 观察器 = new Map();
  const 公开空间 = new Map();
  const 待答调用 = new Map();
  let 下调用号 = 1;
  const 精听 = new WeakMap();
  const 可写流 = new Map();
  const 套接字 = new Map();
  const 消息端口 = new Map();
  const 广播频道 = new Map();
  const 工作线程 = new Map();
  const 事件源 = new Map();
  const 文件读取器 = new Map();
  const 登记文件读取器 = () => {
    const 读取器 = new 全局.FileReader();
    const 号 = 句柄.登记(读取器);
    const 态 = {读取器, 队列: [], 唤醒: null, 清理: []};
    const 推送 = 事 => {
      if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成(事); }
      else if (态.队列.length < 1024) 态.队列.push(事);
      else throw Error('文件读取器事件队列已满');
    };
    for (const 名 of ['loadstart', 'progress', 'load', 'loadend', 'error', 'abort']) {
      const 处理 = 事 => 推送({种类: 名, 已读: 事.loaded ?? 0, 总量: 事.total ?? 0,
        可计算: Boolean(事.lengthComputable), 状态: 读取器.readyState,
        错误名: 读取器.error?.name ?? '', 错误文: 读取器.error?.message ?? ''});
      读取器.addEventListener(名, 处理);
      态.清理.push(() => 读取器.removeEventListener(名, 处理));
    }
    文件读取器.set(号, 态);
    return 号;
  };
  const 等文件读取事 = (号, 时限) => {
    const 态 = 文件读取器.get(文字(号));
    if (!态) throw Error('文件读取器句柄无效');
    if (态.队列.length) return Promise.resolve(态.队列.shift());
    if (态.唤醒) throw Error('同一文件读取器已有等待者');
    const 毫秒 = Number(时限);
    if (!Number.isSafeInteger(毫秒) || 毫秒 < 1 || 毫秒 > 30000) throw Error('文件读取器等待时限无效');
    return new Promise(完成 => {
      const 计时 = setTimeout(() => { 态.唤醒 = null; 完成({种类: 'timeout'}); }, 毫秒);
      态.唤醒 = 事 => { clearTimeout(计时); 完成(事); };
    });
  };
  const 转换压缩字节 = async (种类, 格式, 内容) => {
    const 构造 = 种类 === '压缩' ? 全局.CompressionStream : 全局.DecompressionStream;
    if (typeof 构造 !== 'function') throw Error('宿主不支持' + 种类 + '流');
    const 输入 = new 全局.ReadableStream({start(控制器) { 控制器.enqueue(内容.slice()); 控制器.close(); }});
    const 转换 = new 构造(文字(格式));
    const 写入 = 输入.pipeTo(转换.writable);
    const 读取 = new 全局.Response(转换.readable).arrayBuffer();
    const [写果, 读果] = await Promise.allSettled([写入, 读取]);
    if (写果.status === 'rejected') throw 写果.reason;
    if (读果.status === 'rejected') throw 读果.reason;
    return new Uint8Array(读果.value);
  };
  let 关闭 = false;
  let 报就绪;
  let 报就绪失败;
  let 已就绪 = false;
  const 就绪 = new Promise((完成, 失败) => { 报就绪 = 完成; 报就绪失败 = 失败; });
  就绪.catch(() => {});
  const 标记就绪 = () => { if (!已就绪) { 已就绪 = true; 报就绪(); } };
  // 文言：旧式事件由此入列，依其名归类；新式事件自带类型。汉语：所有原始事件仍从推事件进入；这里给旧式事件分类，再交给统一队列。
  const 推事件 = (事件, 附加 = {}) => {
    const 类型 = 分类网页事件(事件);
    队列.投递({类型, 事件, 附加, 新式: 事件.类型 === 类型, 合并键: 附加.合并键});
  };
  const 造定时 = (延时, 标记, 重复) => {
    const 毫秒 = Number(延时);
    if (!Number.isSafeInteger(毫秒) || 毫秒 < 0 || 毫秒 > 2147483647) throw Error('浏览器定时毫秒无效');
    if (定时器.size >= 64) throw Error('浏览器定时器达到上限');
    const 号 = String(下定时号++);
    const 标记文 = 文字(标记);
    const 项 = {原号: null, 重复};
    const 触发 = () => {
      if (!定时器.has(号) || 关闭) return;
      if (!重复) 定时器.delete(号);
      if (定时待处理.has(号)) return;
      定时待处理.add(号);
      try { 推事件({名称: '定时', 定时号: 号, 种类: 重复 ? '重复' : '一次', 标记: 标记文, 时刻: 全局.Date.now()}); }
      catch (错) {
        定时待处理.delete(号);
        if (重复) { 全局.clearInterval(项.原号); 定时器.delete(号); }
        全局.console?.error?.(错);
      }
    };
    项.原号 = 重复 ? 全局.setInterval(触发, 毫秒) : 全局.setTimeout(触发, 毫秒);
    定时器.set(号, 项);
    return 号;
  };
  const 撤定时 = 号 => {
    const 名 = 文字(号);
    const 项 = 定时器.get(名);
    if (!项) return false;
    if (项.重复) 全局.clearInterval(项.原号);
    else 全局.clearTimeout(项.原号);
    定时器.delete(名);
    定时待处理.delete(名);
    队列.删除若(项 => 项.类型 === '定时' && 项.事件.定时号 === 名);
    return true;
  };
  // 文言：新接口之取消，事已入列而未取者亦去之，故其后必无此号之事。汉语：网页定时的取消：连同队列里该定时号尚未取走的事件一并移除（一次定时已到期但事件未取时也能取消）。
  const 取消定时全 = 名 => {
    const 有期 = 撤定时(名);
    定时待处理.delete(名);
    const 删数 = 队列.删除若(项 => 项.类型 === '定时' && 项.事件?.定时号 === 名);
    return 有期 || 删数 > 0;
  };
  // 文言：网页定时等八包之行术，一处成之。汉语：网页定时、储存、导航、环境、请求、事件源、编译、应用的宿主实现（见 创建网页能力）。
  网页能力 = 创建网页能力({
    根, 全局, 网络, 路径, 储存, 队列, 已关闭: () => 关闭, 编译客户端, 导入模块, 页面应用超时,
    定时: {造: 造定时, 取消: 取消定时全}
  });
  // 文言：帧时由浏览器原生驱动，客唯候事件而裁绘。汉语：原生动画帧回调只投递时间戳和标记，绘制逻辑留在豫言。
  const 造动画帧 = 标记 => {
    if (typeof 全局.requestAnimationFrame !== 'function') throw Error('宿主不支持动画帧');
    if (动画帧.size >= 64) throw Error('浏览器动画帧达到上限');
    const 号 = String(下帧号++), 标记文 = 文字(标记);
    const 原号 = 全局.requestAnimationFrame(时刻 => {
      if (!动画帧.has(号) || 关闭) return;
      动画帧.delete(号);
      try { 推事件({名称: '动画帧', 帧号: 号, 标记: 标记文, 时刻: Number(时刻)}); }
      catch (错) { 全局.console?.error?.(错); }
    });
    动画帧.set(号, 原号);
    return 号;
  };
  const 撤动画帧 = 号 => {
    const 名 = 文字(号), 原号 = 动画帧.get(名);
    if (原号 === undefined) return false;
    全局.cancelAnimationFrame(原号);
    动画帧.delete(名);
    return true;
  };
  // 文言：闲暇既至，即取其余时；越回调而后问之，则所得失真。汉语：在原生回调中快照剩余时间，再投递给豫言。
  const 造空闲回调 = (标记, 超时毫秒 = null) => {
    if (typeof 全局.requestIdleCallback !== 'function') throw Error('宿主不支持空闲回调');
    if (空闲回调.size >= 64) throw Error('浏览器空闲回调达到上限');
    if (超时毫秒 !== null && (!Number.isSafeInteger(Number(超时毫秒)) || Number(超时毫秒) < 0 || Number(超时毫秒) > 2147483647))
      throw Error('浏览器空闲回调超时毫秒无效');
    const 号 = String(下空闲号++), 标记文 = 文字(标记);
    const 触发 = 截止 => {
      if (!空闲回调.has(号) || 关闭) return;
      空闲回调.delete(号);
      const 已超时 = Boolean(截止.didTimeout), 剩余毫秒 = Number(截止.timeRemaining());
      try { 推事件({名称: '空闲', 空闲号: 号, 标记: 标记文, 已超时, 剩余毫秒}); }
      catch (错) { 全局.console?.error?.(错); }
    };
    const 原号 = 超时毫秒 === null ? 全局.requestIdleCallback(触发) :
      全局.requestIdleCallback(触发, {timeout: Number(超时毫秒)});
    空闲回调.set(号, 原号);
    return 号;
  };
  const 撤空闲回调 = 号 => {
    const 名 = 文字(号), 原号 = 空闲回调.get(名);
    if (原号 === undefined) return false;
    全局.cancelIdleCallback(原号);
    空闲回调.delete(名);
    return true;
  };
  const 订阅器 = 创建界面订阅器({根, 全局, 投递: 项 => 队列.投递(项), 已关闭: () => 队列.已关闭(), 删除若: 谓词 => 队列.删除若(谓词)});
  // 文言：旧听惟报有标识之元素；祖先已委托同名之事者，旧听让之。汉语：隐式监听保持旧行为（只报带 id 的元素），并额外带上操作键；若同名事件已被 订阅界面事件 覆盖则让路，避免重复。
  const 监听 = 事件 => {
    const 目标 = 事件.target;
    if (!目标 || typeof 目标.id !== 'string' || !目标.id) return;
    if (精听.get(目标)?.has(事件.type)) return;
    if (订阅器.覆盖(事件)) return;
    推事件({名称: 事件.type, 标识: 目标.id, 值: 'value' in 目标 ? String(目标.value) : '', 选中: Boolean(目标.checked)}, {操作键: 找操作键(目标, null)});
  };
  for (const 名 of ['click', 'input', 'change', 'submit']) 根.addEventListener(名, 监听);
  const 取元素 = 标识 => {
    const 元素 = 根.getElementById(文字(标识));
    if (!元素) throw Error('网页元素不存在：' + 文字(标识));
    return 元素;
  };
  const 是受限文档饼 = (对象, 名) => 名 === 'cookie' && (对象 === 根 || 对象 === 全局.document);
  const 是受限历史改态 = (对象, 名) => 对象 === 全局.history && (名 === 'pushState' || 名 === 'replaceState');
  const 是权限拒绝 = 错 => 错?.name === 'SecurityError';
  const 取储存 = 区域 => {
    const 名 = 文字(区域);
    if (名 === '本地') return 储存 ?? 全局.localStorage;
    if (名 === '会话') return 全局.sessionStorage;
    throw Error('浏览器储存区域无效：' + 名);
  };
  const 注册事件 = (号, 名, 选项 = false, 多听 = false) => {
    const 标识 = 文字(号), 事件名 = 句柄.允名(文字(名));
    const 对象 = 句柄.取得(标识);
    if (typeof 对象.addEventListener !== 'function') throw Error('宿主句柄不能订阅事件');
    const 策 = typeof 选项 === 'boolean' ? {阻止默认: 选项} : 选项;
    if (!策 || typeof 策 !== 'object' || Array.isArray(策)) throw Error('事件订阅选项无效');
    const 捕获 = Boolean(策.capture), 一次 = Boolean(策.once), 被动 = Boolean(策.passive);
    const 阻止默认 = Boolean(策.阻止默认), 止传播 = Boolean(策.止传播), 止同处 = Boolean(策.止同处);
    // 文言：键禁之策由客先定，宿主于原生事中即行之。汉语：豫言预先声明按键条件，宿主在同步监听回调中阻止默认动作。
    const 键盘阻止默认 = 策.键盘阻止默认 ?? [];
    if (!Array.isArray(键盘阻止默认) || 键盘阻止默认.length > 32 ||
        键盘阻止默认.some(律 => !律 || typeof 律 !== 'object' || Array.isArray(律) || typeof 律.key !== 'string' || !律.key ||
          ['ctrlKey', 'metaKey', 'altKey', 'shiftKey'].some(名 => 律[名] !== undefined && typeof 律[名] !== 'boolean'))) throw Error('键盘阻止默认规则无效');
    const 信号 = 策.signal === undefined ? undefined : 句柄.取得(文字(策.signal));
    if (信号 && !(信号 instanceof 全局.AbortSignal)) throw Error('事件中断信号句柄无效');
    const 键 = 多听 ? '多:' + String(下订阅号++) : 标识 + ':' + 事件名;
    const 旧项 = 多听 ? null : 订阅.get(键);
    if (旧项) { 旧项.清理(); 订阅.delete(键); }
    if (信号?.aborted) return '';
    const 项 = {目标号: 标识, 清理: null};
    const 处理 = 事件 => {
      const 是键盘 = typeof 全局.KeyboardEvent === 'function' && 事件 instanceof 全局.KeyboardEvent;
      const 符合键律 = 是键盘 && 键盘阻止默认.some(律 => 律.key === 事件.key &&
        ['ctrlKey', 'metaKey', 'altKey', 'shiftKey'].every(名 => 律[名] === undefined || 律[名] === 事件[名]));
      if ((阻止默认 || 符合键律) && 事件.cancelable) 事件.preventDefault();
      const 载荷 = {名称: 事件名, 来源句柄: 标识, 订阅号: 键, 事件: 句柄.出(事件), 可取消: 事件.cancelable, 已阻止默认: 事件.defaultPrevented,
        冒泡: 事件.bubbles, 组合: 事件.composed, 阶段: 事件.eventPhase, 是否可信: 事件.isTrusted, 时间戳: 事件.timeStamp,
        目标句柄: 事件.target ? 句柄.登记(事件.target) : null,
        当前句柄: 事件.currentTarget ? 句柄.登记(事件.currentTarget) : null,
        路径: 事件.composedPath().map(项 => 句柄.登记(项))};
      if (是键盘) Object.assign(载荷, {键: 事件.key, 代码: 事件.code, 控制键: 事件.ctrlKey,
        命令键: 事件.metaKey, 选项键: 事件.altKey, 换挡键: 事件.shiftKey,
        重复: 事件.repeat, 正在组字: 事件.isComposing});
      if (typeof 全局.MessageEvent === 'function' && 事件 instanceof 全局.MessageEvent)
        Object.assign(载荷, {数据: 句柄.出(事件.data), 来源: 事件.origin ?? '',
          端口: Array.from(事件.ports ?? [], 项 => 句柄.登记(项))});
      if (typeof 全局.ErrorEvent === 'function' && 事件 instanceof 全局.ErrorEvent)
        Object.assign(载荷, {消息: String(事件.message ?? ''), 文件: String(事件.filename ?? ''),
          行: Number(事件.lineno ?? 0), 列: Number(事件.colno ?? 0)});
      if (事件名 === 'storage' && typeof 全局.StorageEvent === 'function' && 事件 instanceof 全局.StorageEvent) {
        // 文言：他页改仓，空键与空文皆如实传客。汉语：StorageEvent 保留 null 与空字符串，并标记共享的储存区域。
        Object.assign(载荷, {区域: 事件.storageArea === 全局.localStorage ? '本地' : 事件.storageArea === 全局.sessionStorage ? '会话' : '',
          键: 事件.key, 旧值: 事件.oldValue, 新值: 事件.newValue, 网址: 事件.url});
      }
      推事件(载荷);
      if (止同处) 事件.stopImmediatePropagation();
      else if (止传播) 事件.stopPropagation();
      if (一次) { 项.清理(); 订阅.delete(键); }
    };
    const 中断 = () => { 项.清理(); 订阅.delete(键); };
    对象.addEventListener(事件名, 处理, {capture: 捕获, passive: 被动, once: 一次, signal: 信号});
    if (信号) 信号.addEventListener('abort', 中断, {once: true});
    const 诸名 = 精听.get(对象) ?? new Map();
    诸名.set(事件名, (诸名.get(事件名) ?? 0) + 1);
    精听.set(对象, 诸名);
    项.清理 = () => {
      对象.removeEventListener(事件名, 处理, 捕获);
      if (信号) 信号.removeEventListener('abort', 中断);
      const 余数 = (诸名.get(事件名) ?? 1) - 1;
      if (余数 > 0) 诸名.set(事件名, 余数);
      else 诸名.delete(事件名);
      if (诸名.size === 0) 精听.delete(对象);
    };
    订阅.set(键, 项);
    return 键;
  };
  const 取消事件 = (号, 名) => {
    const 键 = 文字(号) + ':' + 句柄.允名(文字(名));
    const 项 = 订阅.get(键);
    if (项) { 项.清理(); 订阅.delete(键); }
  };
  const 端口监听 = 号 => {
    const 名 = 文字(号);
    const 端口 = 句柄.取得(名);
    if (!(端口 instanceof 全局.MessagePort)) throw Error('句柄不是消息端口');
    const 旧态 = 消息端口.get(名);
    if (旧态) return 旧态;
    const 态 = {端口, 队列: [], 唤醒: null};
    const 推送 = 事 => {
      if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成(事); }
      else if (态.队列.length < 1024) 态.队列.push(事);
      else throw Error('消息端口事件队列已满');
    };
    端口.addEventListener('message', 事 => 推送({种类: 'message', 数据: 事.data, 端口: Array.from(事.ports, 项 => 句柄.登记(项))}));
    端口.addEventListener('messageerror', 事 => 推送({种类: 'messageerror', 错误: String(事?.message ?? '')}));
    端口.addEventListener('close', () => 推送({种类: 'close'}));
    端口.start();
    消息端口.set(名, 态);
    return 态;
  };
  const 等端口事 = (号, 时限) => {
    const 态 = 端口监听(号);
    if (态.队列.length) return Promise.resolve(态.队列.shift());
    if (态.唤醒) throw Error('同一消息端口已有等待者');
    const 毫秒 = Number(时限);
    if (!Number.isSafeInteger(毫秒) || 毫秒 < 1 || 毫秒 > 30000) throw Error('消息端口等待时限无效');
    return new Promise(完成 => {
      const 计时 = setTimeout(() => { 态.唤醒 = null; 完成({种类: 'timeout'}); }, 毫秒);
      态.唤醒 = 事 => { clearTimeout(计时); 完成(事); };
    });
  };
  const 登记事件源 = 来源 => {
    const 号 = 句柄.登记(来源);
    const 态 = {来源, 队列: [], 唤醒: null, 已订阅: new Set()};
    const 推送 = 事 => {
      if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成(事); }
      else if (态.队列.length < 1024) 态.队列.push(事);
      else { 来源.close(); throw Error('事件源队列已满'); }
    };
    const 订阅 = 名 => {
      if (态.已订阅.has(名)) return;
      来源.addEventListener(名, 事 => 推送({种类: 名, 数据: String(事.data ?? ''), 事件号: String(事.lastEventId ?? ''), 来源: String(事.origin ?? '')}));
      态.已订阅.add(名);
    };
    for (const 名 of ['open', 'message', 'error']) 订阅(名);
    态.订阅 = 订阅;
    事件源.set(号, 态);
    return 号;
  };
  const 等事件源事 = (号, 时限) => {
    const 态 = 事件源.get(文字(号));
    if (!态) throw Error('事件源句柄无效');
    if (态.队列.length) return Promise.resolve(态.队列.shift());
    if (态.唤醒) throw Error('同一事件源已有等待者');
    const 毫秒 = Number(时限);
    if (!Number.isSafeInteger(毫秒) || 毫秒 < 1 || 毫秒 > 30000) throw Error('事件源等待时限无效');
    return new Promise(完成 => {
      const 计时 = setTimeout(() => { 态.唤醒 = null; 完成({种类: 'timeout'}); }, 毫秒);
      态.唤醒 = 事 => { clearTimeout(计时); 完成(事); };
    });
  };
  // 文言：释句柄并撤其观察与订阅。汉语：释放一个句柄，同时断开它上面的 MutationObserver 和通用事件订阅。
  const 释放句柄全部 = 标识 => {
    const 观察 = 观察器.get(标识);
    if (观察) { 观察.disconnect(); 观察器.delete(标识); }
    for (const [键, 项] of 订阅) if (项.目标号 === 标识) { 项.清理(); 订阅.delete(键); }
    句柄.释放(标识);
  };
  const 页面 = 创建页面控制({根, 全局, 路径, 网络, 句柄, 释放句柄全部});
  const 能力 = {
    豫言_浏览器_等待事件: () => 队列.等待(() => true, 原始事件文, () => JSON.stringify({名称: '关闭'}), false),
    // 文言：新法按类取事，不吞他类；界面、消息之旧术亦然。汉语：统一等待点与按类型等待：只取自己关心的类型，其余事件留在队列里。
    豫言_浏览器_等待网页事件: () => 队列.等待(() => true, 统一事件文, () => JSON.stringify({类型: '关闭', 订阅号: 0, 名称: '关闭'}), true),
    豫言_浏览器_等待界面事件: () => 队列.等待(类 => 类 === '界面', 界面事件元组, () => ['关闭', ''], true),
    豫言_浏览器_等待网页消息: () => 队列.等待(类 => 类 === '消息', 消息事件元组, () => ['关闭', 'null', ''], true),
    豫言_浏览器_订阅界面事件: (目标, 事件名, 策略) => 安全结果(() => String(订阅器.订阅(文字(目标), 文字(事件名), 文字(策略)))),
    豫言_浏览器_取消订阅界面事件: 号 => { 订阅器.取消(Number(号)); },
    豫言_浏览器_订阅网页消息: 名 => 安全结果(() => { 订阅器.订阅消息(文字(名)); return ''; }),
    豫言_浏览器_取消订阅网页消息: 名 => 安全结果(() => { 订阅器.取消消息(文字(名)); return ''; }),
    豫言_浏览器_事件丢弃数: () => 队列.丢弃数(),
    豫言_浏览器_事件类型丢弃数: 类 => 队列.丢弃数(文字(类)),
    豫言_浏览器_设置事件队列上限: (类, 限) => 安全结果(() => { 队列.设上限(文字(类), Number(限)); return ''; }),
    // 文言：界面与文树诸术，失败归阴与因，不使异常越桥。汉语：界面操作（按元素标识）与文树操作（按句柄）：返回（成功, 结果或错误文）。
    豫言_浏览器_界面操作: (操作, 标识, 一, 二) => 安全结果(() => 页面.运行表(页面.界面操作表, 文字(操作), [文字(标识), 文字(一), 文字(二)])),
    豫言_浏览器_文树操作: (操作, 一, 二, 三) => 安全结果(() => 页面.运行表(页面.文树操作表, 文字(操作), [文字(一), 文字(二), 文字(三)])),
    豫言_浏览器_文树装入模板: async (资源路径, 选择标识, 目标标识) => {
      try { await 页面.装入模板(文字(资源路径), 文字(选择标识), 文字(目标标识)); return [true, '']; }
      catch (错) { return [false, String(错?.message ?? 错)]; }
    },
    // 文言：计时事入客列，客自决更新；撤时清其未交之事。汉语：定时器只投递事件，刷新业务由豫言决定；取消时清除尚未交付的事件。
    豫言_浏览器_定时一次: (毫秒, 标记) => 造定时(毫秒, 标记, false),
    豫言_浏览器_定时重复: (毫秒, 标记) => 造定时(毫秒, 标记, true),
    豫言_浏览器_取消定时: 撤定时,
    // 文言：新八包之行术两口而入：同步者返（成，果），异步者候其毕。汉语：网页定时、储存、导航、环境、请求、事件源、编译的同步入口与 请求.同步、应用.启动 的异步入口；失败以（阴，错误文）返回，不让异常越桥。
    豫言_浏览器_网页能力: (操作, 一, 二, 三) => 安全结果(() => 网页能力.运行(文字(操作), [文字(一), 文字(二), 文字(三)])),
    豫言_浏览器_网页能力异步: async (操作, 一) => {
      try { return [true, await 网页能力.异步运行(文字(操作), [文字(一)])]; }
      catch (错) { return [false, String(错?.message ?? 错)]; }
    },
    豫言_浏览器_请求动画帧: 造动画帧,
    豫言_浏览器_取消动画帧: 撤动画帧,
    豫言_浏览器_请求空闲回调: 标记 => 造空闲回调(标记),
    豫言_浏览器_请求限时空闲回调: (毫秒, 标记) => 造空闲回调(标记, 毫秒),
    豫言_浏览器_取消空闲回调: 撤空闲回调,
    // 文言：造工者即听其信，速报就绪亦不失；诸应皆归客列。汉语：创建 Worker 时立即安装监听，ready、消息和错误统一进入豫言事件队列。
    豫言_浏览器_工作线程新建: (网址, 选项文) => {
      if (typeof 全局.Worker !== 'function') throw Error('宿主不支持 Worker');
      if (工作线程.size >= 64) throw Error('浏览器工作线程达到上限');
      const 策 = JSON.parse(文字(选项文));
      if (!策 || typeof 策 !== 'object' || Array.isArray(策)) throw Error('工作线程选项无效');
      const 类型 = 策.type ?? 'module', 凭据 = 策.credentials ?? 'same-origin', 名称 = 策.name ?? '';
      if (!['module', 'classic'].includes(类型) || !['omit', 'same-origin', 'include'].includes(凭据) || typeof 名称 !== 'string') throw Error('工作线程选项无效');
      const 工者 = new 全局.Worker(new URL(文字(网址), 路径), {type: 类型, credentials: 凭据, name: 名称});
      let 号;
      try { 号 = 句柄.登记(工者); }
      catch (错) { 工者.terminate(); throw 错; }
      const 清理 = [];
      const 监听工事 = (名, 处理) => { 工者.addEventListener(名, 处理); 清理.push(() => 工者.removeEventListener(名, 处理)); };
      监听工事('message', 事 => 推事件({名称: '工作线程', 线程号: 号, 种类: 'message', 数据: 句柄.出(事.data),
        来源: 事.origin ?? '', 端口: Array.from(事.ports ?? [], 项 => 句柄.登记(项))}));
      监听工事('messageerror', () => 推事件({名称: '工作线程', 线程号: 号, 种类: 'messageerror'}));
      监听工事('error', 事 => 推事件({名称: '工作线程', 线程号: 号, 种类: 'error',
        消息: String(事.message ?? ''), 文件: String(事.filename ?? ''), 行: Number(事.lineno ?? 0), 列: Number(事.colno ?? 0)}));
      工作线程.set(号, {工者, 清理});
      return 号;
    },
    豫言_浏览器_工作线程发值: (号, 值文, 转移文) => {
      const 态 = 工作线程.get(文字(号));
      if (!态) throw Error('工作线程句柄无效');
      const 转移 = 句柄.参数(文字(转移文));
      if (转移.length > 32) throw Error('工作线程转移列表过长');
      态.工者.postMessage(句柄.入(JSON.parse(文字(值文))), 转移);
    },
    豫言_浏览器_工作线程终止: 号 => {
      const 名 = 文字(号), 态 = 工作线程.get(名);
      if (!态) return false;
      for (const 清理 of 态.清理) 清理();
      态.工者.terminate();
      工作线程.delete(名);
      for (const [键, 项] of 订阅) if (项.目标号 === 名) { 项.清理(); 订阅.delete(键); }
      队列.删除若(项 => 项.类型 === '宿主' && 项.事件.名称 === '工作线程' && 项.事件.线程号 === 名);
      句柄.释放(名);
      return true;
    },
    // 文言：工内之客执自身柄，候主信而答之；页中不得冒称工。汉语：仅 Dedicated Worker 中可取得自身句柄并向页面发送结构化消息。
    豫言_浏览器_工作线程自身句柄: () => {
      if (typeof 全局.DedicatedWorkerGlobalScope !== 'function' || !(全局 instanceof 全局.DedicatedWorkerGlobalScope))
        throw Error('当前环境不是 Dedicated Worker');
      return 句柄.登记(全局);
    },
    豫言_浏览器_工作线程向主线程发值: (值文, 转移文) => {
      if (typeof 全局.DedicatedWorkerGlobalScope !== 'function' || !(全局 instanceof 全局.DedicatedWorkerGlobalScope))
        throw Error('当前环境不是 Dedicated Worker');
      const 转移 = 句柄.参数(文字(转移文));
      if (转移.length > 32) throw Error('工作线程转移列表过长');
      全局.postMessage(句柄.入(JSON.parse(文字(值文))), 转移);
    },
    豫言_浏览器_工作线程自身名称: () => {
      if (typeof 全局.DedicatedWorkerGlobalScope !== 'function' || !(全局 instanceof 全局.DedicatedWorkerGlobalScope))
        throw Error('当前环境不是 Dedicated Worker');
      return String(全局.name);
    },
    豫言_浏览器_工作线程自行关闭: () => {
      if (typeof 全局.DedicatedWorkerGlobalScope !== 'function' || !(全局 instanceof 全局.DedicatedWorkerGlobalScope))
        throw Error('当前环境不是 Dedicated Worker');
      全局.close();
    },
    // 文言：页可转移诸物，客以柄列明授；消息候得而返，毋使页之事序失。汉语：浏览器端支持 transfer list，豫言可等待消息并取得转移后的端口句柄。
    豫言_浏览器_消息通道新建: () => {
      const 通道 = new 全局.MessageChannel();
      const 左 = 句柄.登记(通道.port1), 右 = 句柄.登记(通道.port2);
      端口监听(左);
      端口监听(右);
      return [左, 右];
    },
    豫言_浏览器_消息端口发值: (号, 值文, 转移文) => {
      const 转移 = 句柄.参数(文字(转移文));
      if (转移.length > 32) throw Error('消息端口转移列表过长');
      句柄.取得(文字(号)).postMessage(句柄.入(JSON.parse(文字(值文))), 转移);
    },
    豫言_浏览器_消息端口发字节: (号, 内容, 转移) => {
      const 字节 = 内容.slice();
      句柄.取得(文字(号)).postMessage(字节, 转移 ? [字节.buffer] : []);
    },
    豫言_浏览器_消息端口启动: 号 => { 端口监听(号); 句柄.取得(文字(号)).start(); },
    豫言_浏览器_消息端口关闭: 号 => { 句柄.取得(文字(号)).close(); },
    豫言_浏览器_消息端口等事文: async (号, 时限) => {
      const 事 = await 等端口事(号, 时限);
      return JSON.stringify(事.种类 === 'message' ?
        {种类: 'message', 数据: 句柄.出(事.数据), 端口: 事.端口} : 事);
    },
    豫言_浏览器_消息端口等字节: async (号, 时限) => {
      const 事 = await 等端口事(号, 时限);
      if (事.种类 !== 'message') return [false, new Uint8Array()];
      const 值 = 事.数据;
      if (值 instanceof ArrayBuffer) return [true, new Uint8Array(值).slice()];
      if (ArrayBuffer.isView(值)) return [true, new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice()];
      throw Error('消息端口收到的不是字节');
    },
    豫言_浏览器_消息端口等转入端口: async (号, 时限) => {
      const 事 = await 等端口事(号, 时限);
      if (事.种类 !== 'message' || 事.端口.length !== 1) return [false, ''];
      const 新号 = 事.端口[0];
      端口监听(新号);
      return [true, 新号];
    },
    // 文言：同名诸客得信，发者自身不得；宿主唯传信，客自裁其义。汉语：原生 BroadcastChannel 收发，事件由豫言处理。
    豫言_浏览器_广播频道新建: 名 => {
      if (typeof 全局.BroadcastChannel !== 'function') throw Error('宿主不支持 BroadcastChannel');
      if (广播频道.size >= 64) throw Error('浏览器广播频道达到上限');
      const 频道 = new 全局.BroadcastChannel(文字(名));
      let 号;
      try { 号 = 句柄.登记(频道); }
      catch (错) { 频道.close(); throw 错; }
      const 收信 = 事 => 推事件({名称: '广播频道', 频道号: 号, 频道名: 频道.name,
        种类: 'message', 数据: 句柄.出(事.data), 来源: String(事.origin ?? '')});
      const 收错 = 事 => 推事件({名称: '广播频道', 频道号: 号, 频道名: 频道.name,
        种类: 'messageerror', 来源: String(事.origin ?? '')});
      频道.addEventListener('message', 收信);
      频道.addEventListener('messageerror', 收错);
      广播频道.set(号, {频道, 收信, 收错});
      return 号;
    },
    豫言_浏览器_广播频道名称: 号 => {
      const 态 = 广播频道.get(文字(号));
      if (!态) throw Error('广播频道句柄无效');
      return 态.频道.name;
    },
    豫言_浏览器_广播频道发值: (号, 值文) => {
      const 态 = 广播频道.get(文字(号));
      if (!态) throw Error('广播频道句柄无效');
      态.频道.postMessage(句柄.入(JSON.parse(文字(值文))));
    },
    豫言_浏览器_广播频道发字节: (号, 内容) => {
      const 态 = 广播频道.get(文字(号));
      if (!态) throw Error('广播频道句柄无效');
      态.频道.postMessage(内容.slice());
    },
    豫言_浏览器_广播频道事件字节: 事件文 => {
      const 事 = JSON.parse(文字(事件文));
      if (事?.名称 !== '广播频道' || 事.种类 !== 'message' || typeof 事.数据?.$句柄 !== 'string')
        throw Error('广播频道事件不含字节');
      const 值 = 句柄.取得(事.数据.$句柄);
      if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
      if (ArrayBuffer.isView(值)) return new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice();
      throw Error('广播频道事件不含字节');
    },
    豫言_浏览器_广播频道关闭: 号 => {
      const 名 = 文字(号), 态 = 广播频道.get(名);
      if (!态) return false;
      态.频道.removeEventListener('message', 态.收信);
      态.频道.removeEventListener('messageerror', 态.收错);
      态.频道.close();
      广播频道.delete(名);
      句柄.释放(名);
      return true;
    },
    // 文言：服务器推事入页，由客候而判；断联由客定。汉语：浏览器 EventSource 的连接和事件处理均由豫言控制。
    豫言_浏览器_事件源连接: (网址, 凭据) => 登记事件源(new 全局.EventSource(文字(网址), {withCredentials: Boolean(凭据)})),
    豫言_浏览器_事件源订阅: (号, 名) => {
      const 态 = 事件源.get(文字(号));
      if (!态) throw Error('事件源句柄无效');
      const 事件名 = 文字(名);
      if (!事件名 || 事件名.length > 256) throw Error('事件源事件名无效');
      态.订阅(事件名);
    },
    豫言_浏览器_事件源等事文: async (号, 时限) => JSON.stringify(await 等事件源事(号, 时限)),
    豫言_浏览器_事件源状态文: 号 => {
      const 来源 = 事件源.get(文字(号))?.来源;
      if (!来源) throw Error('事件源句柄无效');
      return JSON.stringify({网址: 来源.url, 状态: 来源.readyState, 含凭据: 来源.withCredentials});
    },
    豫言_浏览器_事件源关闭: 号 => {
      const 名 = 文字(号);
      const 态 = 事件源.get(名);
      if (!态) throw Error('事件源句柄无效');
      态.来源.close();
      if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成({种类: 'closed'}); }
      事件源.delete(名);
      句柄.释放(名);
    },
    // 文言：压缩之端留页，客可执柄接流；短字亦可一呼成之。汉语：原生压缩流提供读写端句柄，短字节转换保留原始二进制。
    豫言_浏览器_压缩流创建: 格式 => 句柄.登记(new 全局.CompressionStream(文字(格式))),
    豫言_浏览器_解压流创建: 格式 => 句柄.登记(new 全局.DecompressionStream(文字(格式))),
    豫言_浏览器_压缩流读端: 号 => 句柄.登记(句柄.取得(文字(号)).readable),
    豫言_浏览器_压缩流写端: 号 => 句柄.登记(句柄.取得(文字(号)).writable),
    // 文言：文转字与字转文皆守原生流义；豫言执端柄而逐块读写。汉语：TextEncoderStream/TextDecoderStream 保持跨块状态和原生背压。
    豫言_浏览器_文字编码流新建: () => 句柄.登记(new 全局.TextEncoderStream()),
    豫言_浏览器_文字解码流新建: (标记, 严格, 略首) =>
      句柄.登记(new 全局.TextDecoderStream(文字(标记), {fatal: Boolean(严格), ignoreBOM: Boolean(略首)})),
    豫言_浏览器_文字转换流信息文: 号 => {
      const 流 = 句柄.取得(文字(号));
      return JSON.stringify({编码: 流.encoding, 严格: 'fatal' in 流 ? 流.fatal : null, 略首: 'ignoreBOM' in 流 ? 流.ignoreBOM : null});
    },
    豫言_浏览器_文字转换流读端: 号 => 句柄.登记(句柄.取得(文字(号)).readable),
    豫言_浏览器_文字转换流写端: 号 => 句柄.登记(句柄.取得(文字(号)).writable),
    豫言_浏览器_压缩字节安全: async (格式, 内容) => {
      try { return [true, await 转换压缩字节('压缩', 格式, 内容)]; }
      catch (错) { return [false, new TextEncoder().encode(String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错))]; }
    },
    豫言_浏览器_解压字节安全: async (格式, 内容) => {
      try { return [true, await 转换压缩字节('解压', 格式, 内容)]; }
      catch (错) { return [false, new TextEncoder().encode(String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错))]; }
    },
    豫言_浏览器_设置文字: (标识, 内容) => { 取元素(标识).textContent = 文字(内容); },
    豫言_浏览器_设置属性: (标识, 名, 值) => {
      const 属性 = 文字(名).toLowerCase();
      const 内容 = 文字(值);
      if (/^on/u.test(属性) || 属性 === 'srcdoc' ||
          (['href', 'src', 'action', 'formaction'].includes(属性) && /^\s*javascript:/iu.test(内容))) {
        throw Error('网页属性不得包含可执行脚本');
      }
      取元素(标识).setAttribute(属性, 内容);
    },
    豫言_浏览器_读取输入: 标识 => String(取元素(标识).value ?? ''),
    豫言_浏览器_文档句柄: () => 句柄.登记(根),
    豫言_浏览器_请求文字: async (方法, 网址, 正文) => {
      const 回应 = await 网络(文字(网址), {method: 文字(方法), body: 文字(方法) === 'GET' ? undefined : 文字(正文)});
      return JSON.stringify({状态: 回应.status, 正文: await 回应.text()});
    },
    豫言_浏览器_同源资源文字: 路径文 => 读取同源资源文字(文字(路径文), 路径, 网络),
    // 文言：先发求而归待柄，客得以断信号，后候回应。汉语：豫言先取得在途 fetch Promise，随后可中止，再领取 Response 句柄或错误。
    豫言_浏览器_请求发起可中断: (网址, 选项文, 信号号) => {
      const 信号 = 句柄.取得(文字(信号号));
      if (!(信号 instanceof 全局.AbortSignal)) throw Error('请求中断信号句柄无效');
      const 选项 = 句柄.入(JSON.parse(文字(选项文)));
      if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('请求选项须为对象');
      let 待;
      try { 待 = Promise.resolve(网络(文字(网址), {...选项, signal: 信号})); }
      catch (错) { 待 = Promise.reject(错); }
      待.catch(() => {});
      return 句柄.登记(待);
    },
    豫言_浏览器_请求候回应安全: async 等号 => {
      const 名 = 文字(等号);
      let 已取 = false;
      try { const 待 = 句柄.取得(名); 已取 = true; return [true, 句柄.登记(await 待)]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
      finally { if (已取) 句柄.释放(名); }
    },
    豫言_浏览器_回应文字安全: async 号 => {
      try { return [true, await 句柄.取得(文字(号)).text()]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    // 文言：断网无答，与 HTTP 已答而非二百者别。汉语：网络或读取失败返回阴和错误文字；HTTP 4xx/5xx 仍作为有响应返回状态与正文。
    豫言_浏览器_请求文字安全: async (方法, 网址, 正文) => {
      try { return [true, await 能力.豫言_浏览器_请求文字(方法, 网址, 正文)]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    // 文言：癸象请专用法，置其类型而不改旧 GET。汉语：JSON 写请求显式设置 Content-Type，原安全文本请求保持原样。
    豫言_浏览器_请求JSON安全: async (方法, 网址, 正文) => {
      try {
        const 法 = 文字(方法).toUpperCase();
        if (!['POST', 'PUT', 'PATCH'].includes(法)) throw Error('JSON 写请求仅支持 POST、PUT、PATCH');
        const 回应 = await 网络(文字(网址), {
          method: 法, headers: {'content-type': 'application/json; charset=utf-8'}, body: 文字(正文)
        });
        return [true, JSON.stringify({状态: 回应.status, 正文: await 回应.text()})];
      } catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    // 文言：接长联而推诸事入客队，文字与字节各守其形。汉语：浏览器 WebSocket 事件进入豫言队列，业务消息由豫言决定。
    豫言_浏览器_套接字连接: (网址, 协议文) => {
      const 协议 = JSON.parse(文字(协议文));
      if (!Array.isArray(协议) || 协议.some(项 => typeof 项 !== 'string')) throw Error('WebSocket 协议须为字符串数组');
      const 连接 = new 全局.WebSocket(文字(网址), 协议.length ? 协议 : undefined);
      连接.binaryType = 'arraybuffer';
      const 号 = 句柄.登记(连接);
      const 打开 = () => 推事件({名称: '套接字打开', 来源句柄: 号, 网址: 连接.url, 协议: 连接.protocol});
      const 收信 = 事件 => {
        const 数据 = 事件.data;
        if (typeof 数据 === 'string') 推事件({名称: '套接字消息', 来源句柄: 号, 种类: '文字', 正文: 数据});
        else if (数据 instanceof ArrayBuffer) 推事件({名称: '套接字消息', 来源句柄: 号, 种类: '字节', 字节句柄: 句柄.登记(new Uint8Array(数据))});
        else 推事件({名称: '套接字错误', 来源句柄: 号, 错误: '消息数据类型不支持'});
      };
      const 断联 = 事件 => 推事件({名称: '套接字关闭', 来源句柄: 号, 代码: 事件.code, 原因: 事件.reason, 正常: 事件.wasClean});
      const 报错 = () => 推事件({名称: '套接字错误', 来源句柄: 号, 错误: 'WebSocket 连接错误'});
      for (const [名, 函数] of [['open', 打开], ['message', 收信], ['close', 断联], ['error', 报错]]) 连接.addEventListener(名, 函数);
      套接字.set(号, {连接, 清理: () => {
        for (const [名, 函数] of [['open', 打开], ['message', 收信], ['close', 断联], ['error', 报错]]) 连接.removeEventListener(名, 函数);
      }});
      return 号;
    },
    豫言_浏览器_套接字发送文字: (号, 内容) => { 句柄.取得(文字(号)).send(文字(内容)); },
    豫言_浏览器_套接字发送字节: (号, 内容) => { 句柄.取得(文字(号)).send(内容.slice()); },
    豫言_浏览器_套接字关闭: (号, 代码, 原因) => { 句柄.取得(文字(号)).close(Number(代码), 文字(原因)); },
    豫言_浏览器_套接字状态: 号 => 句柄.取得(文字(号)).readyState,
    豫言_浏览器_套接字协议: 号 => String(句柄.取得(文字(号)).protocol),
    豫言_浏览器_套接字缓冲字节: 号 => 句柄.取得(文字(号)).bufferedAmount,
    豫言_浏览器_套接字释放: 号 => {
      const 名 = 文字(号);
      const 项 = 套接字.get(名);
      if (!项) throw Error('WebSocket 句柄无效');
      项.清理();
      if (项.连接.readyState < 2) 项.连接.close(1000, '豫言释放');
      套接字.delete(名);
      句柄.释放(名);
    },
    豫言_浏览器_读取储存: 键 => 取储存('本地').getItem(文字(键)) ?? '',
    豫言_浏览器_写入储存: (键, 值) => { 取储存('本地').setItem(文字(键), 文字(值)); },
    // 文言：饼与史遭沙箱拒绝，则归阴；余异常仍显。汉语：显式安全入口保留 SecurityError 与空 Cookie 的区别。
    豫言_浏览器_读取饼安全: () => {
      try { return [true, String(根.cookie)]; }
      catch (错) { if (是权限拒绝(错)) return [false, '']; throw 错; }
    },
    豫言_浏览器_写饼安全: 值 => {
      try { 根.cookie = 文字(值); return true; }
      catch (错) { if (是权限拒绝(错)) return false; throw 错; }
    },
    豫言_浏览器_历史改态安全: (名, 状态文, 网址) => {
      const 方法名 = 文字(名);
      if (方法名 !== 'pushState' && 方法名 !== 'replaceState') throw Error('历史方法无效');
      try {
        全局.history[方法名](JSON.parse(文字(状态文)), '', 文字(网址));
        return true;
      } catch (错) { if (是权限拒绝(错)) return false; throw 错; }
    },
    // 文言：二域同具六术，空文与无值不混。汉语：localStorage 和 sessionStorage 共用 Storage 接口，保留缺失值语义。
    豫言_浏览器_储存读取: (区域, 键) => {
      const 值 = 取储存(区域).getItem(文字(键));
      return 值 === null ? [false, ''] : [true, 值];
    },
    豫言_浏览器_储存写入: (区域, 键, 值) => { 取储存(区域).setItem(文字(键), 文字(值)); },
    豫言_浏览器_储存删除: (区域, 键) => { 取储存(区域).removeItem(文字(键)); },
    豫言_浏览器_储存清空: 区域 => { 取储存(区域).clear(); },
    豫言_浏览器_储存计数: 区域 => 取储存(区域).length,
    豫言_浏览器_储存序键: (区域, 序数) => {
      const 序 = Number(序数);
      if (!Number.isSafeInteger(序) || 序 < 0) throw Error('储存序数无效');
      const 键 = 取储存(区域).key(序);
      return 键 === null ? [false, ''] : [true, 键];
    },
    // 文言：索引库升级之仓、索引与事务序列皆由客定；宿主于同一活期排诸请。汉语：豫言给出 schema 和事务操作，宿主只在 IndexedDB 的活动事务期间同步排队。
    豫言_浏览器_索引库打开: async (名, 版本, 方案文) => {
      const 序 = Number(版本);
      if (!Number.isSafeInteger(序) || 序 < 0) throw Error('索引库版本无效');
      const 方案 = JSON.parse(文字(方案文));
      if (!方案 || typeof 方案 !== 'object' || !Array.isArray(方案.stores)) throw Error('索引库方案须含 stores 数组');
      return new Promise((完成, 失败) => {
        const 请求 = 序 === 0 ? 全局.indexedDB.open(文字(名)) : 全局.indexedDB.open(文字(名), 序);
        let 已阻塞 = false;
        请求.onupgradeneeded = () => {
          try {
            const 库 = 请求.result;
            for (const 项 of 方案.stores) {
              if (!项 || typeof 项.name !== 'string') throw Error('索引库仓名无效');
              const 仓 = 库.objectStoreNames.contains(项.name)
                ? 请求.transaction.objectStore(项.name)
                : 库.createObjectStore(项.name, 项.options ?? {});
              for (const 索引 of 项.indexes ?? []) {
                if (!索引 || typeof 索引.name !== 'string') throw Error('索引库索引名无效');
                if (!仓.indexNames.contains(索引.name)) 仓.createIndex(索引.name, 索引.keyPath, 索引.options ?? {});
              }
            }
          } catch (错) { 请求.transaction.abort(); 失败(错); }
        };
        请求.onblocked = () => { 已阻塞 = true; 失败(Error('索引库升级被其他连接阻塞')); };
        请求.onerror = () => 失败(请求.error ?? Error('索引库打开失败'));
        请求.onsuccess = () => {
          if (已阻塞) 请求.result.close();
          else 完成(句柄.登记(请求.result));
        };
      });
    },
    豫言_浏览器_索引库事务: async (库号, 仓名, 模式, 操作文) => {
      const 库 = 句柄.取得(文字(库号));
      if (!(库 instanceof 全局.IDBDatabase)) throw Error('句柄不是索引库');
      const 诸操作 = JSON.parse(文字(操作文));
      if (!Array.isArray(诸操作)) throw Error('索引库操作须为数组');
      const 权 = 文字(模式);
      if (权 !== 'readonly' && 权 !== 'readwrite') throw Error('索引库事务模式无效');
      const 事务 = 库.transaction(文字(仓名), 权);
      const 仓 = 事务.objectStore(文字(仓名));
      const 结果 = new Array(诸操作.length);
      const 允术 = new Set(['add', 'put', 'get', 'getKey', 'getAll', 'getAllKeys', 'delete', 'clear', 'count']);
      return new Promise((完成, 失败) => {
        事务.oncomplete = () => 完成(JSON.stringify(结果));
        事务.onabort = () => 失败(事务.error ?? Error('索引库事务已中止'));
        try {
          for (let 序数 = 0; 序数 < 诸操作.length; 序数++) {
            const 操作 = 诸操作[序数];
            if (!操作 || !允术.has(操作.method) || !Array.isArray(操作.args)) throw Error('索引库操作无效');
            const 来源 = 操作.index === undefined ? 仓 : 仓.index(操作.index);
            if (操作.index !== undefined && ['add', 'put', 'delete', 'clear'].includes(操作.method)) throw Error('索引不可写');
            const 请求 = Reflect.apply(来源[操作.method], 来源, 操作.args.map(项 => 句柄.入(项)));
            请求.onsuccess = () => { 结果[序数] = 句柄.出(请求.result); };
          }
        } catch (错) { try { 事务.abort(); } catch {} 失败(错); }
      });
    },
    豫言_浏览器_索引库事务安全: async (库号, 仓名, 模式, 操作文) => {
      try { return [true, await 能力.豫言_浏览器_索引库事务(库号, 仓名, 模式, 操作文)]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    // 文言：键界留宿主，以柄授客；游标于成功回调内续行。汉语：IDBKeyRange 保持原生对象，游标在请求回调中推进并返回有序快照。
    豫言_浏览器_索引库键界: 选项文 => {
      const 选项 = JSON.parse(文字(选项文));
      if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('索引库键界选项无效');
      const 入键 = 值 => 句柄.入(值);
      let 范围;
      switch (选项.kind) {
        case 'only': 范围 = 全局.IDBKeyRange.only(入键(选项.key)); break;
        case 'lowerBound': 范围 = 全局.IDBKeyRange.lowerBound(入键(选项.lower), Boolean(选项.lowerOpen)); break;
        case 'upperBound': 范围 = 全局.IDBKeyRange.upperBound(入键(选项.upper), Boolean(选项.upperOpen)); break;
        case 'bound': 范围 = 全局.IDBKeyRange.bound(入键(选项.lower), 入键(选项.upper), Boolean(选项.lowerOpen), Boolean(选项.upperOpen)); break;
        default: throw Error('索引库键界种类无效');
      }
      return 句柄.登记(范围);
    },
    豫言_浏览器_索引库键界含: (范围号, 键文) => 句柄.取得(文字(范围号)).includes(句柄.入(JSON.parse(文字(键文)))),
    豫言_浏览器_索引库键界信息文: 范围号 => {
      const 范围 = 句柄.取得(文字(范围号));
      if (!(范围 instanceof 全局.IDBKeyRange)) throw Error('句柄不是索引库键界');
      return JSON.stringify(句柄.出({lower: 范围.lower, upper: 范围.upper, lowerOpen: 范围.lowerOpen, upperOpen: 范围.upperOpen}));
    },
    豫言_浏览器_索引库游标文: async (库号, 仓名, 索引名, 范围号, 方向, 唯键, 限数) => {
      const 库 = 句柄.取得(文字(库号));
      if (!(库 instanceof 全局.IDBDatabase)) throw Error('句柄不是索引库');
      const 数 = Number(限数);
      if (!Number.isSafeInteger(数) || 数 < 1 || 数 > 4096) throw Error('索引库游标限数无效');
      const 向 = 文字(方向);
      if (!['next', 'prev', 'nextunique', 'prevunique'].includes(向)) throw Error('索引库游标方向无效');
      const 事务 = 库.transaction(文字(仓名), 'readonly');
      const 仓 = 事务.objectStore(文字(仓名));
      const 来源 = 文字(索引名) ? 仓.index(文字(索引名)) : 仓;
      const 范围 = 文字(范围号) ? 句柄.取得(文字(范围号)) : undefined;
      const 请求 = Boolean(唯键) ? 来源.openKeyCursor(范围, 向) : 来源.openCursor(范围, 向);
      const 诸项 = [];
      return new Promise((完成, 失败) => {
        事务.oncomplete = () => 完成(JSON.stringify(诸项));
        事务.onabort = () => 失败(事务.error ?? Error('索引库游标事务已中止'));
        请求.onsuccess = () => {
          const 游标 = 请求.result;
          if (!游标 || 诸项.length >= 数) return;
          诸项.push(句柄.出({key: 游标.key, primaryKey: 游标.primaryKey,
            value: Boolean(唯键) ? undefined : 游标.value}));
          if (诸项.length < 数) 游标.continue();
        };
      });
    },
    豫言_浏览器_索引库关闭: 库号 => {
      句柄.取得(文字(库号)).close();
      句柄.释放(文字(库号));
    },
    豫言_浏览器_索引库删除: async 名 => new Promise((完成, 失败) => {
      const 请求 = 全局.indexedDB.deleteDatabase(文字(名));
      请求.onsuccess = () => 完成();
      请求.onerror = () => 失败(请求.error ?? Error('索引库删除失败'));
      请求.onblocked = () => 失败(Error('索引库删除被其他连接阻塞'));
    }),
    豫言_浏览器_索引库列表文: async () => JSON.stringify(await 全局.indexedDB.databases()),
    豫言_浏览器_索引库比键: (左文, 右文) => 全局.indexedDB.cmp(句柄.入(JSON.parse(文字(左文))), 句柄.入(JSON.parse(文字(右文)))),
    豫言_浏览器_全局句柄: 名 => {
      const 名称 = 句柄.允名(文字(名));
      if (!(名称 in 全局)) throw Error('浏览器全局能力不存在：' + 名称);
      return 句柄.登记(全局[名称]);
    },
    豫言_浏览器_读取属性: (号, 名) => {
      const 对象 = 句柄.取得(文字(号));
      const 属性 = 句柄.允名(文字(名));
      try { return JSON.stringify(句柄.出(对象[属性])); }
      catch (错) {
        // 文言：沙箱禁饼，犹可易页语；余错不掩。汉语：Cookie SecurityError 按空 Cookie 处理，供旧豫言 Wasm 继续运行。
        if (是受限文档饼(对象, 属性) && 是权限拒绝(错)) return JSON.stringify('');
        throw 错;
      }
    },
    豫言_浏览器_设置对象属性: (号, 名, 值文) => {
      const 对象 = 句柄.取得(文字(号));
      const 属性 = 句柄.允名(文字(名));
      try { 对象[属性] = 句柄.入(JSON.parse(文字(值文))); }
      catch (错) {
        if (是受限文档饼(对象, 属性) && 是权限拒绝(错)) return;
        throw 错;
      }
    },
    豫言_浏览器_调用方法: async (号, 名, 参数文) => {
      const 对象 = 句柄.取得(文字(号));
      const 名称 = 句柄.允名(文字(名));
      const 方法 = 对象[名称];
      if (typeof 方法 !== 'function') throw Error('宿主成员不是方法');
      try { return JSON.stringify(句柄.出(await Reflect.apply(方法, 对象, 句柄.参数(文字(参数文))))); }
      catch (错) {
        if (是受限历史改态(对象, 名称) && 是权限拒绝(错)) return JSON.stringify({$未定义: true});
        throw 错;
      }
    },
    // 文言：同步异常与异步拒绝皆归客值，不使 JSPI 越桥抛出。汉语：为需要自行处理拒绝的 OS 能力提供安全方法调用。
    豫言_浏览器_调用方法安全: async (号, 名, 参数文) => {
      try {
        const 对象 = 句柄.取得(文字(号));
        const 方法 = 对象[句柄.允名(文字(名))];
        if (typeof 方法 !== 'function') throw Error('宿主成员不是方法');
        return [true, JSON.stringify(句柄.出(await Reflect.apply(方法, 对象, 句柄.参数(文字(参数文)))))];
      } catch (错) { return [false, String(错?.message ?? 错)]; }
    },
    豫言_浏览器_调用方法原始: (号, 名, 参数文) => {
      const 对象 = 句柄.取得(文字(号));
      const 方法 = 对象[句柄.允名(文字(名))];
      if (typeof 方法 !== 'function') throw Error('宿主成员不是方法');
      return JSON.stringify(句柄.出(Reflect.apply(方法, 对象, 句柄.参数(文字(参数文)))));
    },
    豫言_浏览器_等待句柄: async 号 => JSON.stringify(句柄.出(await 句柄.取得(文字(号)))),
    豫言_浏览器_构造对象: (名, 参数文) => {
      const 构造 = 全局[句柄.允名(文字(名))];
      if (typeof 构造 !== 'function') throw Error('浏览器构造器不存在');
      return JSON.stringify(句柄.出(Reflect.construct(构造, 句柄.参数(文字(参数文)))));
    },
    豫言_浏览器_调用全局: async (名, 参数文) => {
      const 函数 = 全局[句柄.允名(文字(名))];
      if (typeof 函数 !== 'function') throw Error('浏览器全局函数不存在');
      return JSON.stringify(句柄.出(await Reflect.apply(函数, 全局, 句柄.参数(文字(参数文)))));
    },
    豫言_浏览器_调用全局安全: async (名, 参数文) => {
      try {
        const 函数 = 全局[句柄.允名(文字(名))];
        if (typeof 函数 !== 'function') throw Error('浏览器全局函数不存在');
        return [true, JSON.stringify(句柄.出(await Reflect.apply(函数, 全局, 句柄.参数(文字(参数文)))))];
      } catch (错) { return [false, JSON.stringify({名称: String(错?.name ?? 'Error'), 消息: String(错?.message ?? 错)})]; }
    },
    豫言_浏览器_网址编解码安全: (方法, 内容) => {
      const 名 = 文字(方法);
      if (!['encodeURI', 'encodeURIComponent', 'decodeURI', 'decodeURIComponent'].includes(名)) return [false, '方法不受支持'];
      try { return [true, 全局[名](文字(内容))]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    豫言_浏览器_调用全局原始: (名, 参数文) => {
      const 函数 = 全局[句柄.允名(文字(名))];
      if (typeof 函数 !== 'function') throw Error('浏览器全局函数不存在');
      return JSON.stringify(句柄.出(Reflect.apply(函数, 全局, 句柄.参数(文字(参数文)))));
    },
    豫言_浏览器_订阅事件: (号, 名) => { 注册事件(号, 名, false); },
    豫言_浏览器_事件目标新建: () => 句柄.登记(new 全局.EventTarget()),
    豫言_浏览器_事件新建: (名, 选项文) => 句柄.登记(new 全局.Event(文字(名), JSON.parse(文字(选项文)))),
    豫言_浏览器_事件发送: (目标号, 事件号) => 句柄.取得(文字(目标号)).dispatchEvent(句柄.取得(文字(事件号))),
    豫言_浏览器_事件订阅选项: (号, 名, 选项文) => { 注册事件(号, 名, JSON.parse(文字(选项文))); },
    豫言_浏览器_事件订阅多: (号, 名, 选项文) => 注册事件(号, 名, JSON.parse(文字(选项文)), true),
    豫言_浏览器_事件取消订阅号: 号 => {
      const 键 = 文字(号), 项 = 订阅.get(键);
      if (!项) return false;
      项.清理(); 订阅.delete(键); return true;
    },
    豫言_浏览器_事件状态文: 号 => {
      const 事 = 句柄.取得(文字(号));
      if (!(事 instanceof 全局.Event)) throw Error('句柄不是 Event');
      return JSON.stringify({名称: 事.type, 冒泡: 事.bubbles, 可取消: 事.cancelable, 组合: 事.composed,
        已阻止默认: 事.defaultPrevented, 阶段: 事.eventPhase, 是否可信: 事.isTrusted, 时间戳: 事.timeStamp,
        目标句柄: 事.target ? 句柄.登记(事.target) : null, 当前句柄: 事.currentTarget ? 句柄.登记(事.currentTarget) : null,
        路径: 事.composedPath().map(项 => 句柄.登记(项))});
    },
    豫言_浏览器_事件旧式初始化: (号, 名, 冒泡, 可取消) => 句柄.取得(文字(号)).initEvent(文字(名), Boolean(冒泡), Boolean(可取消)),
    // 文言：中断理由与原生信号留柄，客可察其态而联诸信号。汉语：AbortSignal 对象和 reason 留在宿主，豫言只持句柄并查询状态。
    豫言_浏览器_中断控制器新建: () => 句柄.登记(new 全局.AbortController()),
    豫言_浏览器_中断控制器信号: 号 => 句柄.登记(句柄.取得(文字(号)).signal),
    豫言_浏览器_中断控制器中断: 号 => 句柄.取得(文字(号)).abort(),
    豫言_浏览器_中断控制器带理由: (号, 理由文) => 句柄.取得(文字(号)).abort(句柄.入(JSON.parse(文字(理由文)))),
    豫言_浏览器_中断信号已中断: 号 => Boolean(句柄.取得(文字(号)).aborted),
    豫言_浏览器_中断信号状态文: 号 => {
      const 信号 = 句柄.取得(文字(号));
      if (!(信号 instanceof 全局.AbortSignal)) throw Error('句柄不是 AbortSignal');
      const 理由 = 信号.reason;
      return JSON.stringify({已中断: 信号.aborted, 理由: 句柄.出(理由), 理由名: String(理由?.name ?? ''), 理由消息: String(理由?.message ?? '')});
    },
    豫言_浏览器_中断信号检查安全: 号 => {
      try { 句柄.取得(文字(号)).throwIfAborted(); return [true, '']; }
      catch (错) { return [false, JSON.stringify({名称: String(错?.name ?? 'Error'), 消息: String(错?.message ?? 错), 值: 句柄.出(错)})]; }
    },
    豫言_浏览器_中断信号立断: 理由文 => 句柄.登记(全局.AbortSignal.abort(句柄.入(JSON.parse(文字(理由文))))),
    豫言_浏览器_中断信号立断无理由: () => 句柄.登记(全局.AbortSignal.abort()),
    豫言_浏览器_中断信号限时安全: 毫秒 => {
      try { return [true, 句柄.登记(全局.AbortSignal.timeout(Number(毫秒)))]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    豫言_浏览器_中断信号合一安全: 诸号文 => {
      try { return [true, 句柄.登记(全局.AbortSignal.any(句柄.参数(文字(诸号文))))]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    豫言_浏览器_中断信号等事文: async (号, 时限) => {
      const 信号 = 句柄.取得(文字(号)), 毫秒 = Number(时限);
      if (!(信号 instanceof 全局.AbortSignal)) throw Error('句柄不是 AbortSignal');
      if (!Number.isSafeInteger(毫秒) || 毫秒 < 1 || 毫秒 > 30000) throw Error('中断事件等待时限无效');
      const 种类 = 信号.aborted ? 'abort' : await new Promise(完成 => {
        let 计时;
        const 处理 = () => { clearTimeout(计时); 信号.removeEventListener('abort', 处理); 完成('abort'); };
        信号.addEventListener('abort', 处理, {once: true});
        计时 = setTimeout(() => { 信号.removeEventListener('abort', 处理); 完成('timeout'); }, 毫秒);
        if (信号.aborted) 处理();
      });
      return JSON.stringify({种类, 已中断: 信号.aborted,
        理由名: String(信号.reason?.name ?? ''), 理由消息: String(信号.reason?.message ?? '')});
    },
    豫言_浏览器_订阅并阻止默认: (号, 名) => { 注册事件(号, 名, true); },
    豫言_浏览器_取消订阅事件: 取消事件,
    // 文言：页呼公开术，惟投请求入客事列；客之答以号返之。汉语：页面调用公开方法只产生事件，结果由豫言按调用号回复。
    豫言_浏览器_公开操作: (空间名, 操作名) => {
      const 空间 = 句柄.允名(文字(空间名));
      const 操作 = 句柄.允名(文字(操作名));
      let 对象 = 公开空间.get(空间);
      if (!对象) {
        if (空间 in 全局) throw Error('公开空间已存在：' + 空间);
        对象 = Object.create(null);
        Object.defineProperty(全局, 空间, {value: 对象, configurable: true, enumerable: true});
        公开空间.set(空间, 对象);
      }
      if (Object.hasOwn(对象, 操作)) throw Error('公开操作已存在：' + 空间 + '.' + 操作);
      Object.defineProperty(对象, 操作, {
        configurable: true,
        enumerable: true,
        value: (...参数) => new Promise((完成, 失败) => {
          try {
            if (关闭) throw Error('豫言浏览器宿主已关闭');
            if (待答调用.size >= 1024) throw Error('公开操作待答已满');
            const 调用号 = String(下调用号++);
            const 参数值 = 参数.map(项 => 句柄.出(项));
            待答调用.set(调用号, {完成, 失败, 空间, 操作});
            try { 推事件({名称: '公开操作', 空间, 操作, 调用号, 参数: 参数值}); }
            catch (错) { 待答调用.delete(调用号); throw 错; }
          } catch (错) { 失败(错); }
        })
      });
    },
    豫言_浏览器_完成公开操作: (号, 值文) => {
      const 标识 = 文字(号);
      const 项 = 待答调用.get(标识);
      if (!项) throw Error('公开操作调用号无效：' + 标识);
      const 值 = 句柄.入(JSON.parse(文字(值文)));
      待答调用.delete(标识);
      项.完成(值);
    },
    豫言_浏览器_拒绝公开操作: (号, 原因) => {
      const 标识 = 文字(号);
      const 项 = 待答调用.get(标识);
      if (!项) throw Error('公开操作调用号无效：' + 标识);
      待答调用.delete(标识);
      项.失败(Error(文字(原因)));
    },
    豫言_浏览器_取消公开操作: (空间名, 操作名) => {
      const 空间 = 句柄.允名(文字(空间名));
      const 操作 = 句柄.允名(文字(操作名));
      const 对象 = 公开空间.get(空间);
      if (!对象 || !Object.hasOwn(对象, 操作)) throw Error('公开操作不存在：' + 空间 + '.' + 操作);
      delete 对象[操作];
      for (const [号, 项] of 待答调用) if (项.空间 === 空间 && 项.操作 === 操作) {
        待答调用.delete(号);
        项.失败(Error('公开操作已取消'));
      }
      if (!Object.keys(对象).length) {
        if (全局[空间] === 对象) delete 全局[空间];
        公开空间.delete(空间);
      }
    },
    // 文言：文树变动由宿主候而入事件队列，诸节点仍以柄传。汉语：MutationObserver 回调只投递记录句柄，处理规则留给豫言。
    豫言_浏览器_观察变动: (根号, 选项文) => {
      const 节点 = 句柄.取得(文字(根号));
      const 选项 = 句柄.入(JSON.parse(文字(选项文)));
      const 观察 = new 全局.MutationObserver(记录 => {
        推事件({名称: '文树变动', 观察器句柄: 号, 记录: 记录.map(项 => 句柄.出(项))});
      });
      观察.observe(节点, 选项);
      const 号 = 句柄.登记(观察);
      观察器.set(号, 观察);
      return 号;
    },
    豫言_浏览器_停止观察变动: 号 => {
      const 标识 = 文字(号);
      const 观察 = 观察器.get(标识);
      if (!观察) throw Error('文树观察器句柄无效');
      观察.disconnect();
      观察器.delete(标识);
      句柄.释放(标识);
    },
    豫言_浏览器_释放句柄: 号 => { 释放句柄全部(文字(号)); },
    豫言_浏览器_句柄取字节: async 号 => {
      const 值 = 句柄.取得(文字(号));
      if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
      if (ArrayBuffer.isView(值)) return new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice();
      if (typeof Blob !== 'undefined' && 值 instanceof Blob) return new Uint8Array(await 值.arrayBuffer());
      throw Error('句柄不是二进制对象');
    },
    豫言_浏览器_字节成句柄: 内容 => 句柄.登记(内容.slice()),
    // 文言：物字之部件及名类皆由客定，宿主但造 Blob、File 并归原字。汉语：豫言控制 Blob/File 组成，宿主保留原生类型、MIME 和流语义。
    豫言_浏览器_物字造字节: (内容, 类别) => 句柄.登记(new 全局.Blob([内容.slice()], {type: 文字(类别)})),
    豫言_浏览器_物字造组合: (部件文, 选项文) => 句柄.登记(new 全局.Blob(句柄.参数(文字(部件文)), 句柄.入(JSON.parse(文字(选项文))))),
    豫言_浏览器_文件造字节: (内容, 名称, 类别, 修改时) => 句柄.登记(new 全局.File([内容.slice()], 文字(名称), {type: 文字(类别), lastModified: Number(修改时)})),
    豫言_浏览器_文件造组合: (部件文, 名称, 选项文) => 句柄.登记(new 全局.File(句柄.参数(文字(部件文)), 文字(名称), 句柄.入(JSON.parse(文字(选项文))))),
    豫言_浏览器_物字信息文: 号 => {
      const 值 = 句柄.取得(文字(号));
      if (!(值 instanceof 全局.Blob)) throw Error('句柄不是 Blob 或 File');
      const 是文件 = typeof 全局.File === 'function' && 值 instanceof 全局.File;
      return JSON.stringify({字节数: 值.size, 类别: 值.type, 文件名: 是文件 ? 值.name : null,
        修改毫秒: 是文件 ? 值.lastModified : null, 相对路径: 是文件 ? String(值.webkitRelativePath ?? '') : null});
    },
    豫言_浏览器_物字切片: (号, 起, 止, 类别) => 句柄.登记(句柄.取得(文字(号)).slice(Number(起), Number(止), 文字(类别))),
    豫言_浏览器_物字切片至尾: (号, 起, 类别) => 句柄.登记(句柄.取得(文字(号)).slice(Number(起), undefined, 文字(类别))),
    豫言_浏览器_物字原字: async 号 => new Uint8Array(await 句柄.取得(文字(号)).arrayBuffer()),
    豫言_浏览器_物字原生字节安全: async 号 => {
      try { return [true, await 句柄.取得(文字(号)).bytes()]; }
      catch (错) { return [false, new TextEncoder().encode(String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错))]; }
    },
    豫言_浏览器_物字文字: async 号 => await 句柄.取得(文字(号)).text(),
    豫言_浏览器_物字流: 号 => 句柄.登记(句柄.取得(文字(号)).stream()),
    豫言_浏览器_物字文字流安全: 号 => {
      try { return [true, 句柄.登记(句柄.取得(文字(号)).textStream())]; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    // 文言：文件读取诸事按本义投客，结果之原字毋经文字桥。汉语：FileReader 的进度事件和二进制结果由豫言读取，保留 0x00、0xff。
    豫言_浏览器_文件读取器新建: 登记文件读取器,
    豫言_浏览器_文件读取字节: (读号, 文件号) => 文件读取器.get(文字(读号)).读取器.readAsArrayBuffer(句柄.取得(文字(文件号))),
    豫言_浏览器_文件读取文字: (读号, 文件号, 编码名) => 文件读取器.get(文字(读号)).读取器.readAsText(句柄.取得(文字(文件号)), 文字(编码名)),
    豫言_浏览器_文件读取网址: (读号, 文件号) => 文件读取器.get(文字(读号)).读取器.readAsDataURL(句柄.取得(文字(文件号))),
    豫言_浏览器_文件读取旧二进制: (读号, 文件号) => 文件读取器.get(文字(读号)).读取器.readAsBinaryString(句柄.取得(文字(文件号))),
    豫言_浏览器_文件读取中止: 读号 => 文件读取器.get(文字(读号)).读取器.abort(),
    豫言_浏览器_文件读取等事文: async (读号, 时限) => JSON.stringify(await 等文件读取事(读号, 时限)),
    豫言_浏览器_文件读取状态文: 读号 => {
      const 读取器 = 文件读取器.get(文字(读号))?.读取器;
      if (!读取器) throw Error('文件读取器句柄无效');
      const 值 = 读取器.result;
      return JSON.stringify({状态: 读取器.readyState, 结果种类: 值 === null ? '无' : typeof 值 === 'string' ? '文字' : 值 instanceof ArrayBuffer ? '字节' : '其他',
        错误名: 读取器.error?.name ?? '', 错误文: 读取器.error?.message ?? ''});
    },
    豫言_浏览器_文件读取结果字节安全: 读号 => {
      const 值 = 文件读取器.get(文字(读号))?.读取器.result;
      return 值 instanceof ArrayBuffer ? [true, new Uint8Array(值).slice()] : [false, new Uint8Array()];
    },
    豫言_浏览器_文件读取结果文字安全: 读号 => {
      const 值 = 文件读取器.get(文字(读号))?.读取器.result;
      return typeof 值 === 'string' ? [true, 值] : [false, ''];
    },
    豫言_浏览器_文件读取旧二进制字节安全: 读号 => {
      const 值 = 文件读取器.get(文字(读号))?.读取器.result;
      if (typeof 值 !== 'string') return [false, new Uint8Array()];
      return [true, Uint8Array.from(值, 字 => 字.charCodeAt(0) & 255)];
    },
    豫言_浏览器_文件读取器释放: 读号 => {
      const 名 = 文字(读号), 态 = 文件读取器.get(名);
      if (!态) throw Error('文件读取器句柄无效');
      if (态.读取器.readyState === 1) 态.读取器.abort();
      for (const 清理 of 态.清理) 清理();
      if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成({种类: 'closed'}); }
      文件读取器.delete(名);
      句柄.释放(名);
    },
    豫言_浏览器_文件列表从输入: 标识 => {
      const 输入 = 取元素(标识);
      if (输入.tagName !== 'INPUT' || 输入.type !== 'file' || !输入.files) throw Error('目标不是文件输入框');
      return 句柄.登记(输入.files);
    },
    豫言_浏览器_文件列表数量: 号 => 句柄.取得(文字(号)).length,
    豫言_浏览器_文件列表取项安全: (号, 序) => {
      const 值 = 句柄.取得(文字(号)).item(Number(序));
      return 值 ? [true, 句柄.登记(值)] : [false, ''];
    },
    // 文言：表单诸值按原序归客；文件留柄，毋以象文伤原字。汉语：保留 FormData 重名项顺序，文件通过句柄交给豫言。
    豫言_浏览器_表单新建: () => 句柄.登记(new 全局.FormData()),
    豫言_浏览器_表单订阅生成: 表号 => { 注册事件(表号, 'formdata', false); },
    豫言_浏览器_表单取消订阅生成: 表号 => { 取消事件(表号, 'formdata'); },
    豫言_浏览器_表单拦截提交: 表号 => { 注册事件(表号, 'submit', true); },
    豫言_浏览器_表单取消拦截提交: 表号 => { 取消事件(表号, 'submit'); },
    豫言_浏览器_表单提交钮从队列安全: 事文 => {
      const 载荷 = JSON.parse(文字(事文));
      if (载荷?.名称 !== 'submit' || typeof 载荷.事件?.$句柄 !== 'string') return [false, ''];
      const 事 = 句柄.取得(载荷.事件.$句柄);
      if (!(事 instanceof 全局.SubmitEvent) || !事.submitter) return [false, ''];
      return [true, 句柄.登记(事.submitter)];
    },
    豫言_浏览器_表单事件新建: 表号 => 句柄.登记(new 全局.FormDataEvent('formdata', {formData: 句柄.取得(文字(表号))})),
    豫言_浏览器_表单事件自定: (类型, 表号, 选项文) => 句柄.登记(new 全局.FormDataEvent(文字(类型),
      {...句柄.入(JSON.parse(文字(选项文))), formData: 句柄.取得(文字(表号))})),
    豫言_浏览器_表单事件数据: 事件号 => {
      const 事 = 句柄.取得(文字(事件号));
      if (!(事 instanceof 全局.FormDataEvent)) throw Error('句柄不是 FormDataEvent');
      return 句柄.登记(事.formData);
    },
    豫言_浏览器_表单事件从队列: 事文 => {
      const 载荷 = JSON.parse(文字(事文));
      if (载荷?.名称 !== 'formdata' || typeof 载荷.事件?.$句柄 !== 'string') throw Error('队列项不是 formdata 事件');
      const 事 = 句柄.取得(载荷.事件.$句柄);
      if (!(事 instanceof 全局.FormDataEvent)) throw Error('队列事件不是 FormDataEvent');
      return 句柄.登记(事.formData);
    },
    豫言_浏览器_表单从表格: (表号, 提交号) => {
      const 表 = 句柄.取得(文字(表号));
      if (!(表 instanceof 全局.HTMLFormElement)) throw Error('句柄不是表单元素');
      return 句柄.登记(new 全局.FormData(表, 文字(提交号) ? 句柄.取得(文字(提交号)) : undefined));
    },
    豫言_浏览器_表单追加文字: (号, 名, 值) => 句柄.取得(文字(号)).append(文字(名), 文字(值)),
    豫言_浏览器_表单追加文件: (号, 名, 文件号, 文件名) => 句柄.取得(文字(号)).append(文字(名), 句柄.取得(文字(文件号)), 文字(文件名)),
    豫言_浏览器_表单追加原文件: (号, 名, 文件号) => 句柄.取得(文字(号)).append(文字(名), 句柄.取得(文字(文件号))),
    豫言_浏览器_表单设置文字: (号, 名, 值) => 句柄.取得(文字(号)).set(文字(名), 文字(值)),
    豫言_浏览器_表单设置文件: (号, 名, 文件号, 文件名) => 句柄.取得(文字(号)).set(文字(名), 句柄.取得(文字(文件号)), 文字(文件名)),
    豫言_浏览器_表单设置原文件: (号, 名, 文件号) => 句柄.取得(文字(号)).set(文字(名), 句柄.取得(文字(文件号))),
    豫言_浏览器_表单删除: (号, 名) => 句柄.取得(文字(号)).delete(文字(名)),
    豫言_浏览器_表单含名: (号, 名) => 句柄.取得(文字(号)).has(文字(名)),
    豫言_浏览器_表单首项文: (号, 名) => {
      const 值 = 句柄.取得(文字(号)).get(文字(名));
      return JSON.stringify(值 === null ? null : typeof 值 === 'string' ? {种类: '文字', 值} : {种类: '文件', 句柄: 句柄.登记(值)});
    },
    豫言_浏览器_表单首文件安全: (号, 名) => {
      const 值 = 句柄.取得(文字(号)).get(文字(名));
      return 值 instanceof 全局.File ? [true, 句柄.登记(值)] : [false, ''];
    },
    豫言_浏览器_表单同名诸项文: (号, 名) => JSON.stringify(句柄.取得(文字(号)).getAll(文字(名)).map(值 =>
      typeof 值 === 'string' ? {种类: '文字', 值} : {种类: '文件', 句柄: 句柄.登记(值)})),
    豫言_浏览器_表单诸项文: 号 => JSON.stringify(Array.from(句柄.取得(文字(号)).entries(), ([名, 值]) =>
      [名, typeof 值 === 'string' ? {种类: '文字', 值} : {种类: '文件', 句柄: 句柄.登记(值)}])),
    豫言_浏览器_表单键列文: 号 => JSON.stringify(Array.from(句柄.取得(文字(号)).keys())),
    豫言_浏览器_表单值列文: 号 => JSON.stringify(Array.from(句柄.取得(文字(号)).values(), 值 =>
      typeof 值 === 'string' ? {种类: '文字', 值} : {种类: '文件', 句柄: 句柄.登记(值)})),
    豫言_浏览器_表单解析正文: async 号 => 句柄.登记(await 句柄.取得(文字(号)).formData()),
    // 文言：密钥留宿主，惟以柄用之。汉语：CryptoKey 不出宿主，豫言只持不透明句柄。
    豫言_浏览器_密码随机识别: () => 全局.crypto.randomUUID(),
    豫言_浏览器_密码随机字节: 长度 => {
      const 数 = Number(长度);
      if (!Number.isSafeInteger(数) || 数 < 0 || 数 > 65536) throw Error('随机字节长度无效');
      return 全局.crypto.getRandomValues(new Uint8Array(数));
    },
    豫言_浏览器_密码摘要: async (算法, 内容) => new Uint8Array(await 全局.crypto.subtle.digest(文字(算法), 内容)),
    豫言_浏览器_密码导入AES: async 钥字节 => 句柄.登记(await 全局.crypto.subtle.importKey('raw', 钥字节, 'AES-GCM', false, ['encrypt', 'decrypt'])),
    豫言_浏览器_密码生成AES: async (位数, 可导) =>
      句柄.登记(await 全局.crypto.subtle.generateKey({name: 'AES-GCM', length: Number(位数)}, Boolean(可导), ['encrypt', 'decrypt'])),
    豫言_浏览器_密码导出AES: async 钥号 =>
      new Uint8Array(await 全局.crypto.subtle.exportKey('raw', 句柄.取得(文字(钥号)))),
    豫言_浏览器_密码AES加密: async (钥号, 随机数, 附加文, 明文) =>
      new Uint8Array(await 全局.crypto.subtle.encrypt({name: 'AES-GCM', iv: 随机数, additionalData: 附加文}, 句柄.取得(文字(钥号)), 明文)),
    豫言_浏览器_密码AES解密: async (钥号, 随机数, 附加文, 密文) =>
      new Uint8Array(await 全局.crypto.subtle.decrypt({name: 'AES-GCM', iv: 随机数, additionalData: 附加文}, 句柄.取得(文字(钥号)), 密文)),
    豫言_浏览器_密码导入HMAC: async (散列, 钥字节) =>
      句柄.登记(await 全局.crypto.subtle.importKey('raw', 钥字节, {name: 'HMAC', hash: 文字(散列)}, false, ['sign', 'verify'])),
    豫言_浏览器_密码HMAC签: async (钥号, 内容) =>
      new Uint8Array(await 全局.crypto.subtle.sign('HMAC', 句柄.取得(文字(钥号)), 内容)),
    豫言_浏览器_密码HMAC验: async (钥号, 签文, 内容) =>
      全局.crypto.subtle.verify('HMAC', 句柄.取得(文字(钥号)), 签文, 内容),
    豫言_浏览器_密码PBKDF2派生字节: async (口令, 盐, 轮数, 散列, 位数) => {
      const 基钥 = await 全局.crypto.subtle.importKey('raw', 口令, 'PBKDF2', false, ['deriveBits']);
      return new Uint8Array(await 全局.crypto.subtle.deriveBits({name: 'PBKDF2', salt: 盐, iterations: Number(轮数), hash: 文字(散列)}, 基钥, Number(位数)));
    },
    豫言_浏览器_密码HKDF派生字节: async (原钥, 盐, 用途, 散列, 位数) => {
      const 基钥 = await 全局.crypto.subtle.importKey('raw', 原钥, 'HKDF', false, ['deriveBits']);
      return new Uint8Array(await 全局.crypto.subtle.deriveBits({name: 'HKDF', salt: 盐, info: 用途, hash: 文字(散列)}, 基钥, Number(位数)));
    },
    豫言_浏览器_密码PBKDF2派生AES: async (口令, 盐, 轮数, 散列, 位数) => {
      const 基钥 = await 全局.crypto.subtle.importKey('raw', 口令, 'PBKDF2', false, ['deriveKey']);
      return 句柄.登记(await 全局.crypto.subtle.deriveKey({name: 'PBKDF2', salt: 盐, iterations: Number(轮数), hash: 文字(散列)}, 基钥, {name: 'AES-GCM', length: Number(位数)}, false, ['encrypt', 'decrypt']));
    },
    豫言_浏览器_密码HKDF派生AES: async (原钥, 盐, 用途, 散列, 位数) => {
      const 基钥 = await 全局.crypto.subtle.importKey('raw', 原钥, 'HKDF', false, ['deriveKey']);
      return 句柄.登记(await 全局.crypto.subtle.deriveKey({name: 'HKDF', salt: 盐, info: 用途, hash: 文字(散列)}, 基钥, {name: 'AES-GCM', length: Number(位数)}, false, ['encrypt', 'decrypt']));
    },
    豫言_浏览器_打开可读流: 号 => 句柄.登记(句柄.取得(文字(号)).getReader()),
    豫言_浏览器_读取流块: async 号 => {
      const 结果 = await 句柄.取得(文字(号)).read();
      if (结果.done) return [true, new Uint8Array()];
      const 值 = 结果.value;
      if (值 instanceof ArrayBuffer) return [false, new Uint8Array(值).slice()];
      if (ArrayBuffer.isView(值)) return [false, new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice()];
      throw Error('可读流块不是字节');
    },
    豫言_浏览器_读取文字流块: async 号 => {
      const 结果 = await 句柄.取得(文字(号)).read();
      if (结果.done) return [true, ''];
      if (typeof 结果.value !== 'string') throw Error('可读流块不是文字');
      return [false, 结果.value];
    },
    // 文言：严解有失则归阴与错文，毋使 JSPI 异常越桥。汉语：安全读取文字流，将流错误变成豫言可检查的结果。
    豫言_浏览器_读取文字流块安全: async 号 => {
      try {
        const 结果 = await 句柄.取得(文字(号)).read();
        if (结果.done) return [true, '{"已终":true}'];
        if (typeof 结果.value !== 'string') throw Error('可读流块不是文字');
        return [true, JSON.stringify({已终: false, 文字: 结果.value})];
      } catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    豫言_浏览器_释放流读取器: 号 => {
      句柄.取得(文字(号)).releaseLock();
      句柄.释放(文字(号));
    },
    // 文言：写器之待与读器可并行；异步写闭皆归柄，客可后候其果。汉语：写入与读取可并发，豫言取得 Promise 句柄后自行决定等待时机。
    豫言_浏览器_打开可写流: 号 => 句柄.登记(句柄.取得(文字(号)).getWriter()),
    豫言_浏览器_可写流已锁: 号 => Boolean(句柄.取得(文字(号)).locked),
    豫言_浏览器_写器容量文: 号 => JSON.stringify(句柄.取得(文字(号)).desiredSize),
    豫言_浏览器_写器就绪安全: async 号 => {
      try { await 句柄.取得(文字(号)).ready; return [true, '']; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    豫言_浏览器_写器已闭安全: async 号 => {
      try { await 句柄.取得(文字(号)).closed; return [true, '']; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
    },
    豫言_浏览器_写器发字节: (号, 内容) => {
      const 待 = 句柄.取得(文字(号)).write(内容.slice());
      待.catch(() => {});
      return 句柄.登记(待);
    },
    豫言_浏览器_写器发文字: (号, 内容) => {
      const 待 = 句柄.取得(文字(号)).write(文字(内容));
      待.catch(() => {});
      return 句柄.登记(待);
    },
    豫言_浏览器_写器发关闭: 号 => {
      const 待 = 句柄.取得(文字(号)).close();
      待.catch(() => {});
      return 句柄.登记(待);
    },
    豫言_浏览器_写器候操作安全: async 号 => {
      const 名 = 文字(号);
      let 已取 = false;
      try { const 待 = 句柄.取得(名); 已取 = true; await 待; return [true, '']; }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
      finally { if (已取) 句柄.释放(名); }
    },
    豫言_浏览器_写器中断安全: async (号, 原因) => {
      const 名 = 文字(号);
      let 写器;
      try {
        写器 = 句柄.取得(名);
        if (typeof 写器.abort !== 'function' || typeof 写器.releaseLock !== 'function') throw Error('句柄不是流写器');
        await 写器.abort(文字(原因));
        return [true, ''];
      }
      catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
      finally { if (写器) { if (typeof 写器.releaseLock === 'function') 写器.releaseLock(); 句柄.释放(名); } }
    },
    豫言_浏览器_释放流写器: 号 => {
      const 名 = 文字(号);
      句柄.取得(名).releaseLock();
      句柄.释放(名);
    },
    豫言_浏览器_创建可读流: () => {
      const 态 = {控制器: null, 唤醒: null, 已关闭: false};
      const 流 = new ReadableStream({
        start(控制器) { 态.控制器 = 控制器; },
        pull() { if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成(); } },
        cancel() { 态.已关闭 = true; if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成(); } }
      }, {highWaterMark: 1});
      const 号 = 句柄.登记(流);
      可写流.set(号, 态);
      return 号;
    },
    豫言_浏览器_写入流块: async (号, 内容) => {
      const 态 = 可写流.get(文字(号));
      if (!态) throw Error('可写流句柄无效');
      while (!态.已关闭 && 态.控制器.desiredSize <= 0) await new Promise(完成 => { 态.唤醒 = 完成; });
      if (态.已关闭) throw Error('可写流已关闭');
      态.控制器.enqueue(内容.slice());
    },
    豫言_浏览器_关闭可读流: 号 => {
      const 态 = 可写流.get(文字(号));
      if (!态) throw Error('可写流句柄无效');
      if (!态.已关闭) { 态.已关闭 = true; 态.控制器.close(); }
      可写流.delete(文字(号));
    }
  };
  const {运行} = 创建豫言实例(程序模块, 值桥模块, 能力, {输出, 时限毫秒: Number.POSITIVE_INFINITY});
  const 完成 = 运行();
  const 关闭宿主 = () => {
      if (关闭) return;
      关闭 = true;
      for (const 名 of ['click', 'input', 'change', 'submit']) 根.removeEventListener(名, 监听);
      for (const 项 of 订阅.values()) 项.清理();
      订阅.clear();
      for (const 态 of 工作线程.values()) {
        for (const 清理 of 态.清理) 清理();
        态.工者.terminate();
      }
      工作线程.clear();
      for (const [空间, 对象] of 公开空间) if (全局[空间] === 对象) delete 全局[空间];
      公开空间.clear();
      for (const 项 of 待答调用.values()) 项.失败(Error('豫言浏览器宿主已关闭'));
      待答调用.clear();
      for (const 观察 of 观察器.values()) 观察.disconnect();
      观察器.clear();
      for (const 项 of 套接字.values()) {
        项.清理();
        if (项.连接.readyState < 2) 项.连接.close(1000, '豫言宿主关闭');
      }
      套接字.clear();
      for (const 项 of 定时器.values()) {
        if (项.重复) 全局.clearInterval(项.原号);
        else 全局.clearTimeout(项.原号);
      }
      定时器.clear();
      定时待处理.clear();
      for (const 原号 of 动画帧.values()) 全局.cancelAnimationFrame(原号);
      动画帧.clear();
      for (const 原号 of 空闲回调.values()) 全局.cancelIdleCallback(原号);
      空闲回调.clear();
      for (const 态 of 消息端口.values()) {
        if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成({种类: 'close'}); }
        try { 态.端口.close(); } catch { /* 文言：转移后旧端已废。汉语：转移后原端口可能不可再关闭。 */ }
      }
      消息端口.clear();
      for (const 态 of 广播频道.values()) {
        态.频道.removeEventListener('message', 态.收信);
        态.频道.removeEventListener('messageerror', 态.收错);
        态.频道.close();
      }
      广播频道.clear();
      for (const 态 of 事件源.values()) {
        态.来源.close();
        if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成({种类: 'closed'}); }
      }
      事件源.clear();
      for (const 态 of 文件读取器.values()) {
        if (态.读取器.readyState === 1) 态.读取器.abort();
        for (const 清理 of 态.清理) 清理();
        if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成({种类: 'closed'}); }
      }
      文件读取器.clear();
      for (const 态 of 可写流.values()) {
        if (!态.已关闭) { 态.已关闭 = true; 态.控制器.error(Error('浏览器宿主已关闭')); }
        if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成(); }
      }
      可写流.clear();
      订阅器.清空();
      网页能力.清理();
      队列.关闭();
  };
  完成.then(
    () => { if (!已就绪) { 已就绪 = true; 报就绪(); } 关闭宿主(); },
    错 => { if (!已就绪) { 已就绪 = true; 报就绪失败(错); } 关闭宿主(); }
  );
  // 文言：状态供验与察，不为业务所用。汉语：返回当前队列积压、丢弃数、句柄数与订阅数，供测试和诊断使用。
  const 状态 = () => ({...队列.状态(), 句柄数: 句柄.数量(), 界面订阅数: 订阅器.订阅数(), 消息订阅数: 订阅器.消息订阅数(), 定时数: 定时器.size, ...网页能力.状态()});
  return {就绪, 完成, 关闭: 关闭宿主, 状态};
}
