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

export function 创建浏览器宿主({程序模块, 值桥模块, 根 = globalThis.document ?? globalThis, 网络 = fetch, 储存 = null, 全局 = globalThis, 路径 = 全局.document?.baseURI ?? 全局.location?.href ?? import.meta.url, 输出 = () => {}}) {
  const 事件队列 = [];
  const 定时器 = new Map();
  const 定时待处理 = new Set();
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
  let 唤醒;
  let 关闭 = false;
  let 报就绪;
  let 报就绪失败;
  let 已就绪 = false;
  const 就绪 = new Promise((完成, 失败) => { 报就绪 = 完成; 报就绪失败 = 失败; });
  就绪.catch(() => {});
  const 推事件 = 事件 => {
    if (关闭) return;
    const 文 = JSON.stringify(事件);
    if (唤醒) {
      if (事件.名称 === '定时') 定时待处理.delete(事件.定时号);
      const 完成 = 唤醒; 唤醒 = null; 完成(文);
    }
    else if (事件队列.length < 1024) 事件队列.push(文);
    else throw Error('浏览器事件队列已满');
  };
  const 取队列事件 = () => {
    const 文 = 事件队列.shift();
    if (文) {
      const 事 = JSON.parse(文);
      if (事.名称 === '定时') 定时待处理.delete(事.定时号);
    }
    return 文;
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
    for (let 序 = 事件队列.length - 1; 序 >= 0; 序--) {
      const 事 = JSON.parse(事件队列[序]);
      if (事.名称 === '定时' && 事.定时号 === 名) 事件队列.splice(序, 1);
    }
    return true;
  };
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
  const 监听 = 事件 => {
    const 目标 = 事件.target;
    if (!目标 || typeof 目标.id !== 'string' || !目标.id) return;
    if (精听.get(目标)?.has(事件.type)) return;
    推事件({名称: 事件.type, 标识: 目标.id, 值: 'value' in 目标 ? String(目标.value) : '', 选中: Boolean(目标.checked)});
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
  const 能力 = {
    豫言_浏览器_等待事件: () => {
      if (!已就绪) { 已就绪 = true; 报就绪(); }
      return 事件队列.length ? 取队列事件() : new Promise(完成 => { 唤醒 = 完成; });
    },
    // 文言：计时事入客列，客自决更新；撤时清其未交之事。汉语：定时器只投递事件，刷新业务由豫言决定；取消时清除尚未交付的事件。
    豫言_浏览器_定时一次: (毫秒, 标记) => 造定时(毫秒, 标记, false),
    豫言_浏览器_定时重复: (毫秒, 标记) => 造定时(毫秒, 标记, true),
    豫言_浏览器_取消定时: 撤定时,
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
      for (let 序 = 事件队列.length - 1; 序 >= 0; 序--) {
        const 事 = JSON.parse(事件队列[序]);
        if (事.名称 === '工作线程' && 事.线程号 === 名) 事件队列.splice(序, 1);
      }
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
    豫言_浏览器_释放句柄: 号 => {
      const 标识 = 文字(号);
      const 观察 = 观察器.get(标识);
      if (观察) { 观察.disconnect(); 观察器.delete(标识); }
      for (const [键, 项] of 订阅) if (项.目标号 === 标识) { 项.清理(); 订阅.delete(键); }
      句柄.释放(标识);
    },
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
      if (唤醒) { const 完毕 = 唤醒; 唤醒 = null; 完毕(JSON.stringify({名称: '关闭'})); }
  };
  完成.then(
    () => { if (!已就绪) { 已就绪 = true; 报就绪(); } 关闭宿主(); },
    错 => { if (!已就绪) { 已就绪 = true; 报就绪失败(错); } 关闭宿主(); }
  );
  return {就绪, 完成, 关闭: 关闭宿主};
}
