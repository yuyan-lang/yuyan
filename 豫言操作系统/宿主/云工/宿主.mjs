// 文言：云工之桥但行受授诸能；路由、权限与事序皆归豫言。汉语：Worker 桥只执行授权的平台调用，业务决策由豫言程序完成。
import {创建豫言实例, 文字} from './值桥.mjs';
import {创建句柄表} from './句柄.mjs';

const 控制台诸法 = new Set([
  'debug', 'error', 'info', 'log', 'warn', 'clear', 'count', 'group', 'table', 'trace',
  'assert', 'countReset', 'dir', 'dirxml', 'groupCollapsed', 'groupEnd', 'profile',
  'profileEnd', 'time', 'timeEnd', 'timeLog', 'timeStamp', 'createTask'
]);

const 限文 = async 回应 => {
  const 文 = await 回应.text();
  if (new TextEncoder().encode(文).length > 2 * 1024 * 1024) throw Error('宿主响应超过 2 MiB');
  return 文;
};
const 绑定 = (环境, 许可, 名, 种类) => {
  const 名称 = 文字(名);
  if (!许可[种类]?.includes(名称)) throw Error('未授权的' + 种类 + '绑定：' + 名称);
  const 资源 = 环境[名称];
  if (!资源) throw Error('绑定不存在：' + 名称);
  return 资源;
};
const 取值绑定 = (环境, 许可, 名, 种类) => {
  const 名称 = 文字(名);
  if (!许可[种类]?.includes(名称)) throw Error('未授权的' + 种类 + '绑定：' + 名称);
  if (!Object.hasOwn(环境, 名称)) throw Error('绑定不存在：' + 名称);
  return 环境[名称];
};

export function 创建云工宿主({程序模块, 值桥模块, 许可 = {}, 动态资源 = null, 网络 = fetch, 全局 = globalThis, 输出 = () => {}}) {
  const 已授外发网址 = 原文 => {
    let 目标;
    try { 目标 = new URL(原文); } catch { return false; }
    return 目标.protocol === 'https:' && !目标.username && !目标.password &&
      Array.isArray(许可.OUTBOUND_ORIGINS) && 许可.OUTBOUND_ORIGINS.includes(目标.origin);
  };
  // 文言：同一持久对象之诸调用共持频道，所发皆原字，报文之义全由客定。汉语：同一 Durable Object 实例共享原始字节广播；业务事件和 SSE 编码由豫言负责。
  const 广播频道 = new Map();
  const 广播名 = 值 => {
    const 名 = 文字(值);
    if (!名 || 名.length > 128) throw Error('云工广播频道名无效');
    return 名;
  };
  // 文言：隔离客器只受程序字节及空许可。汉语：子 Worker 只能运行传入的豫言 Wasm，不继承父 Worker 的绑定。
  const 造隔离客码 = (程序字节, cpuMs, subRequests) => {
    if (!动态资源) throw Error('缺少动态 Worker 资源');
    if (!(程序字节 instanceof Uint8Array) || !WebAssembly.validate(程序字节)) throw Error('动态 Worker 程序不是有效 Wasm');
    const CPU = Number(cpuMs);
    const 次数 = Number(subRequests);
    if (!Number.isSafeInteger(CPU) || CPU < 1 || !Number.isSafeInteger(次数) || 次数 < 0) throw Error('动态 Worker 资源限额无效');
    const 源 = 动态资源.模块源码;
    return {
      compatibilityDate: '2026-09-10',
      mainModule: '隔离入口.mjs',
      modules: {
        '隔离入口.mjs': {js: 源['隔离入口.mjs']},
        '宿主.mjs': {js: 源['宿主.mjs']},
        '句柄.mjs': {js: 源['句柄.mjs']},
        '值桥.mjs': {js: 源['值桥.mjs']},
        '程序.wasm': {wasm: 程序字节.slice().buffer},
        '值桥.wasm': {wasm: 动态资源.值桥字节.slice(0)}
      },
      globalOutbound: null,
      env: {},
      limits: {cpuMs: CPU, subRequests: 次数}
    };
  };
  const 执行 = async (种类, 载荷, 环境, 上下文, 对象状态 = null, 工作流步 = null, 事务仓 = null) => {
      const 请求 = 种类 === 'fetch' || 种类 === 'durable-fetch' || 种类 === 'service-fetch' ? 载荷 : null;
      const 批次 = 种类 === 'queue' ? 载荷 : null;
      const 定时 = 种类 === 'scheduled' ? 载荷 : null;
      const 邮件 = 种类 === 'email' ? 载荷 : null;
      const 告警 = 种类 === 'durable-alarm' ? 载荷 : null;
      const HTML事件 = 种类.startsWith('html-') ? 载荷 : null;
      const 套接字事件 = 种类.startsWith('websocket-') || 种类.startsWith('durable-websocket-') ? 载荷 : null;
      const 流回调 = 种类 === 'stream-pull' || 种类 === 'stream-cancel' ? 载荷 : null;
      let 响应;
      let 通知响应;
      const 有响应 = 种类 === 'fetch' || 种类 === 'durable-fetch' || 种类 === 'service-fetch';
      const 需工作流输出 = 种类 === 'workflow' || 种类 === 'workflow-step';
      const 需流块输出 = 种类 === 'stream-pull';
      const 需事务输出 = 种类 === 'durable-transaction';
      const 是工作流 = 需工作流输出 || 种类 === 'workflow-rollback';
      let 工作流输出;
      let 已设工作流输出 = false;
      let 流块输出;
      let 已设流块输出 = false;
      let 事务输出;
      let 已设事务输出 = false;
      const 响应已备 = 有响应 ? new Promise(完成 => { 通知响应 = 完成; }) : null;
      const 句柄 = 创建句柄表();
      const 可写流 = new Map();
      const 消息端口 = new Map();
      const 事件源 = new Map();
      const 定时器 = new Map();
      const 定时队列 = [];
      const 定时待交 = new Set();
      let 下定时号 = 1;
      let 定时唤醒 = null;
      const 取定时事 = () => {
        const 事 = 定时队列.shift();
        if (事) 定时待交.delete(事.定时号);
        return 事;
      };
      const 推定时事 = 事 => {
        if (定时唤醒) {
          const 完成 = 定时唤醒;
          定时唤醒 = null;
          定时待交.delete(事.定时号);
          完成(事);
        } else if (定时队列.length < 1024) 定时队列.push(事);
        else throw Error('云工定时事件队列已满');
      };
      const 造定时 = (延时, 标记, 重复) => {
        const 毫秒 = Number(延时);
        if (!Number.isSafeInteger(毫秒) || 毫秒 < 0 || 毫秒 > 2147483647) throw Error('云工定时毫秒无效');
        if (定时器.size >= 64) throw Error('云工定时器达到上限');
        const 号 = String(下定时号++);
        const 标记文 = 文字(标记);
        const 项 = {原号: null, 重复};
        const 触发 = () => {
          if (!定时器.has(号)) return;
          if (!重复) 定时器.delete(号);
          if (定时待交.has(号)) return;
          定时待交.add(号);
          try { 推定时事({种类: 重复 ? '重复' : '一次', 定时号: 号, 标记: 标记文, 时刻: 全局.Date.now()}); }
          catch (错) {
            定时待交.delete(号);
            if (重复) { 全局.clearInterval(项.原号); 定时器.delete(号); }
            全局.console?.error?.(错);
          }
        };
        项.原号 = 重复 ? 全局.setInterval(触发, 毫秒) : 全局.setTimeout(触发, 毫秒);
        定时器.set(号, 项);
        return 号;
      };
      const 撤定时 = 号 => {
        const 名 = 文字(号), 项 = 定时器.get(名);
        if (!项) return false;
        if (项.重复) 全局.clearInterval(项.原号);
        else 全局.clearTimeout(项.原号);
        定时器.delete(名);
        定时待交.delete(名);
        for (let 序 = 定时队列.length - 1; 序 >= 0; 序--)
          if (定时队列[序].定时号 === 名) 定时队列.splice(序, 1);
        return true;
      };
      const 等定时事 = 时限 => {
        if (定时队列.length) return Promise.resolve(取定时事());
        if (定时唤醒) throw Error('当前调用已有定时事件等待者');
        const 毫秒 = Number(时限);
        if (!Number.isSafeInteger(毫秒) || 毫秒 < 1 || 毫秒 > 30000) throw Error('云工定时等待时限无效');
        return new Promise(完成 => {
          const 计时 = 全局.setTimeout(() => { 定时唤醒 = null; 完成({种类: 'timeout'}); }, 毫秒);
          定时唤醒 = 事 => { 全局.clearTimeout(计时); 完成(事); };
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
      const 端口监听 = 号 => {
        const 名 = 文字(号);
        const 端口 = 句柄.取得(名);
        if (!(端口 instanceof 全局.MessagePort)) throw Error('句柄不是消息端口');
        const 旧态 = 消息端口.get(名);
        if (旧态) return 旧态;
        const 态 = {队列: [], 唤醒: null};
        const 推送 = 事 => {
          if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成(事); }
          else if (态.队列.length < 1024) 态.队列.push(事);
          else throw Error('消息端口事件队列已满');
        };
        端口.addEventListener('message', 事 => 推送({种类: 'message', 数据: 事.data}));
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
      const 取流字节 = 值 => {
        if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
        if (ArrayBuffer.isView(值)) return new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice();
        throw Error('可读流块不是字节');
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
      let 已登记HTML尾签 = false;
      const 设响应 = 值 => {
        if (!有响应) throw Error('仅 HTTP 事件可设置响应');
        if (响应) throw Error('响应只能设置一次');
        响应 = 值;
        通知响应(值);
      };
      const 取回应对象 = 号 => {
        const 回应 = 句柄.取得(文字(号));
        if (!(回应 instanceof 全局.Response)) throw Error('宿主句柄不是 Response');
        return 回应;
      };
      const 输入 = async () => {
        if (!请求) throw Error('当前事件没有 HTTP 请求');
        return JSON.stringify({方法: 请求.method, 网址: 请求.url, 标头: Object.fromEntries(请求.headers), 正文: await 限文(请求.clone())});
      };
      const 取批次 = () => {
        if (!批次) throw Error('当前事件没有队列批次');
        return 批次;
      };
      const 取定时 = () => {
        if (!定时) throw Error('当前事件没有定时控制器');
        return 定时;
      };
      const 取邮件 = () => {
        if (!邮件) throw Error('当前事件没有入站邮件');
        return 邮件;
      };
      const 取持久状态 = () => {
        if (!对象状态) throw Error('当前事件不属于持久对象');
        return 对象状态;
      };
      const 取持久仓 = () => 取持久状态().storage;
      const 取键值仓 = () => 事务仓 ?? 取持久仓();
      const 取SQL仓 = () => {
        const 仓 = 取持久仓().sql;
        if (!仓 || typeof 仓.exec !== 'function') throw Error('持久对象没有 SQLite 存储');
        return 仓;
      };
      const 取恢复仓 = () => {
        if (事务仓) throw Error('持久事务回调内不可操作恢复书签');
        const 仓 = 取持久仓();
        if (!仓.sql || typeof 仓.sql.exec !== 'function') throw Error('持久对象没有 SQLite 存储');
        return 仓;
      };
      const 取SQL游标 = 号 => {
        const 游标 = 句柄.取得(文字(号));
        if (!游标 || typeof 游标.next !== 'function' || typeof 游标.toArray !== 'function' || typeof 游标.raw !== 'function') throw Error('SQL 游标句柄无效');
        return 游标;
      };
      const 取同步键值仓 = () => {
        const 仓 = 取持久仓().kv;
        if (!仓 || typeof 仓.get !== 'function') throw Error('持久对象没有 SQLite 同步 KV');
        return 仓;
      };
      const 取告警信息 = () => {
        if (种类 !== 'durable-alarm') throw Error('当前事件不是持久对象告警');
        return 告警 ?? {};
      };
      const 验套接字协议 = 名 => {
        const 协议 = 文字(名);
        if (协议 && !(请求?.headers.get('Sec-WebSocket-Protocol') ?? '').split(',').map(项 => 项.trim()).includes(协议)) {
          throw Error('选定的 WebSocket 协议不在请求列表');
        }
        return 协议;
      };
      const 取消息 = 序 => {
        const 项 = Number(序);
        const 诸消息 = 取批次().messages;
        if (!Number.isSafeInteger(项) || 项 < 0 || 项 >= 诸消息.length) throw Error('队列消息序号无效');
        return 诸消息[项];
      };
      const 登记出站套接字 = (连接, 已接纳 = false) => {
        连接.binaryType = 'arraybuffer';
        let 完成存续;
        const 存续 = new Promise(完成 => { 完成存续 = 完成; });
        if (上下文?.waitUntil) 上下文.waitUntil(存续);
        const 投递 = (名, 值 = {}) => 执行('websocket-' + 名, {套接字: 连接, 来源: '出站', ...值}, 环境, null)
          .catch(错 => { 全局.console?.error?.(错); if (连接.readyState === 1) 连接.close(1011, '豫言回调失败'); 输出(String(错)); });
        let 已报开 = false;
        const 报开 = () => { if (!已报开) { 已报开 = true; void 投递('open'); } };
        连接.addEventListener('open', 报开);
        连接.addEventListener('message', 事件 => { void 投递('message', {数据: 事件.data}); });
        连接.addEventListener('close', 事件 => { void 投递('close', {代码: 事件.code, 原因: 事件.reason, 正常: 事件.wasClean}).finally(完成存续); });
        连接.addEventListener('error', 事件 => { void 投递('error', {错误: String(事件?.message ?? 'WebSocket error')}); });
        if (已接纳) void Promise.resolve().then(报开);
        return 句柄.登记(连接);
      };
      const 能力 = {
        豫言_云工_事件种类: () => 种类,
        // 文言：客定记志之法与参数，宿主只循平台之 Console；无为之法仍依平台。汉语：豫言选择标准 Console 方法及参数，保留 Workers 对部分方法的 no-op 语义。
        豫言_云工_控制台文字: (方法, 内容) => {
          const 名 = 文字(方法);
          if (!['debug', 'error', 'info', 'log', 'warn'].includes(名)) throw Error('控制台文字级别无效');
          全局.console[名](文字(内容));
        },
        豫言_云工_控制台调用: (方法, 参数文) => {
          const 名 = 文字(方法);
          if (!控制台诸法.has(名)) throw Error('控制台方法不在标准清单');
          const 函数 = 全局.console?.[名];
          if (typeof 函数 !== 'function') throw Error('控制台方法在当前运行时不可用：' + 名);
          return JSON.stringify(句柄.出(Reflect.apply(函数, 全局.console, 句柄.参数(文字(参数文)))));
        },
        // 文言：两端同器通信，客以候事复得消息；转移诸物之法云工今未许。汉语：Workers MessageChannel 在同一事件内以 JSPI 等待，平台不支持 transfer list。
        豫言_云工_消息通道新建: () => {
          const 通道 = new 全局.MessageChannel();
          const 左 = 句柄.登记(通道.port1), 右 = 句柄.登记(通道.port2);
          端口监听(左);
          端口监听(右);
          return [左, 右];
        },
        豫言_云工_消息端口发值: (号, 值文) => { 句柄.取得(文字(号)).postMessage(句柄.入(JSON.parse(文字(值文)))); },
        豫言_云工_消息端口发字节: (号, 内容) => { 句柄.取得(文字(号)).postMessage(内容.slice()); },
        豫言_云工_消息端口启动: 号 => { 端口监听(号); 句柄.取得(文字(号)).start(); },
        豫言_云工_消息端口关闭: 号 => { 句柄.取得(文字(号)).close(); },
        豫言_云工_消息端口等事文: async (号, 时限) => {
          const 事 = await 等端口事(号, 时限);
          return JSON.stringify(事.种类 === 'message' ? {种类: 'message', 数据: 句柄.出(事.数据)} : 事);
        },
        豫言_云工_消息端口等字节: async (号, 时限) => {
          const 事 = await 等端口事(号, 时限);
          if (事.种类 !== 'message') return [false, new Uint8Array()];
          return [true, 取流字节(事.数据)];
        },
        // 文言：事源之网联或流联由客择之，诸事件悉以文归客。汉语：EventSource 可连 URL、授权服务绑定或既有流，事件由 JSPI 等待交给豫言。
        豫言_云工_事件源连接: (网址, 凭据) => 登记事件源(new 全局.EventSource(文字(网址), {withCredentials: Boolean(凭据)})),
        豫言_云工_事件源连接服务: (网址, 名) => 登记事件源(new 全局.EventSource(文字(网址), {fetcher: 绑定(环境, 许可, 名, 'SERVICE')})),
        豫言_云工_事件源接流: 流号 => 登记事件源(全局.EventSource.from(句柄.取得(文字(流号)))),
        豫言_云工_事件源订阅: (号, 名) => {
          const 态 = 事件源.get(文字(号));
          if (!态) throw Error('事件源句柄无效');
          const 事件名 = 文字(名);
          if (!事件名 || 事件名.length > 256) throw Error('事件源事件名无效');
          态.订阅(事件名);
        },
        豫言_云工_事件源等事文: async (号, 时限) => JSON.stringify(await 等事件源事(号, 时限)),
        豫言_云工_事件源状态文: 号 => {
          const 来源 = 事件源.get(文字(号))?.来源;
          if (!来源) throw Error('事件源句柄无效');
          return JSON.stringify({网址: 来源.url, 状态: 来源.readyState, 含凭据: 来源.withCredentials});
        },
        豫言_云工_事件源关闭: 号 => {
          const 名 = 文字(号);
          const 态 = 事件源.get(名);
          if (!态) throw Error('事件源句柄无效');
          态.来源.close();
          if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成({种类: 'closed'}); }
          事件源.delete(名);
          句柄.释放(名);
        },
        // 文言：升级、收信、断联皆归客器裁；宿主只持原生套接字。汉语：WebSocketPair 及事件回调由宿主适配，响应和消息处理都交给豫言 Wasm。
        豫言_云工_WebSocket请求协议文: () => {
          if (!请求) throw Error('当前事件没有 HTTP 请求');
          return 请求.headers.get('Sec-WebSocket-Protocol') ?? '';
        },
        豫言_云工_WebSocket建对并应答协议: 协议名 => {
          if (!有响应 || !请求 || 请求.headers.get('Upgrade')?.toLowerCase() !== 'websocket') throw Error('当前请求不是 WebSocket 升级');
          if (typeof 全局.WebSocketPair !== 'function') throw Error('宿主不支持 WebSocketPair');
          const 协议 = 验套接字协议(协议名);
          const [客户, 服务] = Object.values(new 全局.WebSocketPair());
          const 投递 = (名, 值) => 执行('websocket-' + 名, {套接字: 服务, ...值}, 环境, null)
            .catch(错 => { 全局.console?.error?.(错); if (服务.readyState === 1) 服务.close(1011, '豫言回调失败'); 输出(String(错)); });
          服务.addEventListener('message', 事件 => { void 投递('message', {数据: 事件.data}); });
          服务.addEventListener('close', 事件 => { void 投递('close', {代码: 事件.code, 原因: 事件.reason, 正常: 事件.wasClean}); });
          服务.addEventListener('error', 事件 => { void 投递('error', {错误: String(事件?.message ?? 'WebSocket error')}); });
          服务.accept();
          设响应(new Response(null, {status: 101, webSocket: 客户, headers: 协议 ? {'Sec-WebSocket-Protocol': 协议} : {}}));
          return 句柄.登记(服务);
        },
        豫言_云工_WebSocket建对并应答: () => 能力.豫言_云工_WebSocket建对并应答协议(''),
        豫言_云工_WebSocket出站连接: (网址, 协议文) => {
          const 协议 = JSON.parse(文字(协议文));
          if (!Array.isArray(协议) || 协议.some(项 => typeof 项 !== 'string')) throw Error('WebSocket 协议须为字符串数组');
          const 连接 = new 全局.WebSocket(文字(网址), 协议.length ? 协议 : undefined);
          return 登记出站套接字(连接);
        },
        豫言_云工_WebSocket请求出站: async (网址, 半开) => {
          const 回应 = await 网络(文字(网址), {headers: {Upgrade: 'websocket'}});
          if (!回应.webSocket) throw Error('出站 WebSocket 升级未获接受：HTTP ' + 回应.status);
          const 连接 = 回应.webSocket;
          连接.accept({allowHalfOpen: Boolean(半开)});
          return 登记出站套接字(连接, true);
        },
        // 文言：持久客器以态接长联，事后可休而重启；附件附于联本身。汉语：Durable Object 通过 ctx.acceptWebSocket 接纳连接，消息由类回调重入豫言 Wasm。
        豫言_云工_持久套接字建对并应答协议: (标签文, 附件文, 协议名) => {
          if (种类 !== 'durable-fetch' || !请求 || 请求.headers.get('Upgrade')?.toLowerCase() !== 'websocket') throw Error('当前事件不是持久对象 WebSocket 升级');
          const 标签 = JSON.parse(文字(标签文));
          if (!Array.isArray(标签) || 标签.length > 10 || 标签.some(项 => typeof 项 !== 'string' || 项.length > 256)) throw Error('持久套接字标签无效');
          const 协议 = 验套接字协议(协议名);
          const [客户, 服务] = Object.values(new 全局.WebSocketPair());
          取持久状态().acceptWebSocket(服务, 标签);
          服务.serializeAttachment(句柄.入(JSON.parse(文字(附件文))));
          设响应(new Response(null, {status: 101, webSocket: 客户, headers: 协议 ? {'Sec-WebSocket-Protocol': 协议} : {}}));
          return 句柄.登记(服务);
        },
        豫言_云工_持久套接字建对并应答: (标签文, 附件文) =>
          能力.豫言_云工_持久套接字建对并应答协议(标签文, 附件文, ''),
        豫言_云工_持久套接字诸柄文: 标签 => {
          const 名 = 文字(标签);
          return JSON.stringify(取持久状态().getWebSockets(名 || undefined).map(项 => 句柄.登记(项)));
        },
        豫言_云工_持久套接字标签文: 号 => JSON.stringify(取持久状态().getTags(句柄.取得(文字(号)))),
        豫言_云工_持久套接字附件文: 号 => JSON.stringify(句柄.出(句柄.取得(文字(号)).deserializeAttachment())),
        豫言_云工_持久套接字设附件: (号, 附件文) => {
          句柄.取得(文字(号)).serializeAttachment(句柄.入(JSON.parse(文字(附件文))));
        },
        豫言_云工_持久套接字设自动回复: (请求文, 回应文) => {
          const 请 = 文字(请求文), 答 = 文字(回应文);
          if (请.length > 2048 || 答.length > 2048) throw Error('持久套接字自动回复超过 2048 字符');
          取持久状态().setWebSocketAutoResponse(new 全局.WebSocketRequestResponsePair(请, 答));
        },
        豫言_云工_持久套接字清自动回复: () => { 取持久状态().setWebSocketAutoResponse(); },
        豫言_云工_持久套接字自动回复文: () => {
          const 配对 = 取持久状态().getWebSocketAutoResponse();
          if (配对 == null) return [false, ''];
          const 请求 = typeof 配对.getRequest === 'function' ? 配对.getRequest() : 配对.request;
          const 回应 = typeof 配对.getResponse === 'function' ? 配对.getResponse() : 配对.response;
          if (typeof 请求 !== 'string' || typeof 回应 !== 'string') throw Error('自动回复配对接口无效');
          return [true, JSON.stringify({请求, 回应})];
        },
        豫言_云工_持久套接字自动回复时刻: 号 => {
          const 时刻 = 取持久状态().getWebSocketAutoResponseTimestamp(句柄.取得(文字(号)));
          return 时刻 == null ? [false, ''] : [true, 时刻.toISOString()];
        },
        豫言_云工_持久套接字设事件时限: 毫秒 => {
          const 时 = Number(文字(毫秒));
          if (!Number.isSafeInteger(时) || 时 < 0 || 时 > 604800000) throw Error('持久套接字事件时限无效');
          取持久状态().setHibernatableWebSocketEventTimeout(时);
        },
        豫言_云工_持久套接字事件时限文: () => {
          const 时 = 取持久状态().getHibernatableWebSocketEventTimeout();
          return 时 == null ? [false, ''] : [true, String(时)];
        },
        豫言_云工_WebSocket当前句柄: () => {
          if (!套接字事件) throw Error('当前事件不是 WebSocket');
          return 句柄.登记(套接字事件.套接字);
        },
        豫言_云工_WebSocket事件文: () => {
          if (!套接字事件) throw Error('当前事件不是 WebSocket');
          return JSON.stringify({种类, 来源: 套接字事件.来源 ?? '入站', 代码: 套接字事件.代码 ?? null, 原因: 套接字事件.原因 ?? '', 正常: 套接字事件.正常 ?? null, 错误: 套接字事件.错误 ?? ''});
        },
        豫言_云工_WebSocket消息文字: () => {
          if (!套接字事件 || !种类.endsWith('websocket-message') || typeof 套接字事件.数据 !== 'string') throw Error('当前消息不是 WebSocket 文字');
          return 套接字事件.数据;
        },
        豫言_云工_WebSocket消息种类: () => {
          if (!套接字事件 || !种类.endsWith('websocket-message')) throw Error('当前事件不是 WebSocket 消息');
          return typeof 套接字事件.数据 === 'string' ? '文字' : '字节';
        },
        豫言_云工_WebSocket消息字节: () => {
          if (!套接字事件 || !种类.endsWith('websocket-message')) throw Error('当前事件不是 WebSocket 消息');
          const 值 = 套接字事件.数据;
          if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
          if (ArrayBuffer.isView(值)) return new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice();
          throw Error('当前消息不是 WebSocket 字节');
        },
        豫言_云工_WebSocket发送文字: (号, 内容) => { 句柄.取得(文字(号)).send(文字(内容)); },
        豫言_云工_WebSocket发送字节: (号, 内容) => { 句柄.取得(文字(号)).send(内容.slice()); },
        豫言_云工_WebSocket状态: 号 => Number(句柄.取得(文字(号)).readyState),
        豫言_云工_WebSocket协议: 号 => String(句柄.取得(文字(号)).protocol ?? ''),
        豫言_云工_WebSocket关闭: (号, 代码, 原因) => { 句柄.取得(文字(号)).close(Number(代码), 文字(原因)); },
        // 文言：下游索块则复启客器；客还字、后态与终否。汉语：ReadableStream 每次 pull 重启豫言 Wasm，状态和数据块由豫言决定。
        豫言_云工_流回调状态文: () => {
          if (!流回调) throw Error('当前事件不是流回调');
          return 流回调.状态;
        },
        豫言_云工_流回调取消原因文: () => {
          if (种类 !== 'stream-cancel') throw Error('当前事件不是流取消');
          return 流回调.原因;
        },
        豫言_云工_流回调供块: (内容, 后态, 已终) => {
          if (!需流块输出 || 已设流块输出) throw Error('流拉取回调只能供块一次');
          if (!(内容 instanceof Uint8Array) || 内容.length > 2 * 1024 * 1024) throw Error('流块须为不超过 2 MiB 的字节串');
          const 状态 = 文字(后态);
          if (状态.length > 65536) throw Error('流回调状态过长');
          流块输出 = {内容: 内容.slice(), 状态, 已终: Boolean(已终)};
          已设流块输出 = true;
        },
        豫言_云工_创建回调可读流: 初态 => {
          const 初 = 文字(初态);
          if (初.length > 65536) throw Error('流回调初始状态过长');
          let 状态 = 初;
          let 已终 = false;
          const 流 = new ReadableStream({
            async pull(控制器) {
              if (已终) return;
              try {
                const 结果 = await 执行('stream-pull', {状态}, 环境, null);
                状态 = 结果.状态;
                if (结果.内容.length) 控制器.enqueue(结果.内容);
                if (结果.已终) { 已终 = true; 控制器.close(); }
              } catch (错) { 已终 = true; 控制器.error(错); }
            },
            async cancel(原因) {
              if (已终) return;
              已终 = true;
              await 执行('stream-cancel', {状态, 原因: String(原因 ?? '')}, 环境, null);
            }
          }, {highWaterMark: 0});
          return 句柄.登记(流);
        },
        // 文言：每中一元，复启豫言客器；客执元素柄自定更易。汉语：HTMLRewriter 的异步元素回调逐次运行豫言 Wasm，所有修改由豫言发起。
        豫言_云工_HTML选择器: () => {
          if (!HTML事件) throw Error('当前事件不是 HTML 改写');
          return HTML事件.选择器;
        },
        豫言_云工_HTML元素句柄: () => {
          if (种类 !== 'html-element') throw Error('当前事件不是 HTML 元素');
          return 句柄.登记(HTML事件.值);
        },
        豫言_云工_HTML当前句柄: () => {
          if (!HTML事件) throw Error('当前事件不是 HTML 改写');
          return 句柄.登记(HTML事件.值);
        },
        豫言_云工_HTML属性列文: () => {
          if (种类 !== 'html-element') throw Error('仅 HTML 元素回调可枚举属性');
          return JSON.stringify(Array.from(HTML事件.值.attributes, ([名, 值]) => [名, 值]));
        },
        豫言_云工_HTML登记尾签: () => {
          if (种类 !== 'html-element') throw Error('仅 HTML 元素回调可登记尾标签');
          if (已登记HTML尾签) throw Error('同一元素尾标签只能登记一次');
          已登记HTML尾签 = true;
          HTML事件.值.onEndTag(尾签 => 执行('html-end-tag', {选择器: HTML事件.选择器, 值: 尾签}, 环境, null));
        },
        豫言_云工_HTML改写响应规则: (回应号, 规则文, 篇事件文) => {
          if (!有响应) throw Error('HTML 改写须在 HTTP 事件中登记');
          const 原回应 = 句柄.取得(文字(回应号));
          if (!(原回应 instanceof Response)) throw Error('HTML 改写源须为 Response');
          if (typeof 全局.HTMLRewriter !== 'function') throw Error('宿主不支持 HTMLRewriter');
          const 规则 = JSON.parse(文字(规则文));
          const 篇事件 = JSON.parse(文字(篇事件文));
          const 合法事件 = (诸, 允许) => Array.isArray(诸) && 诸.length <= 允许.size &&
            诸.every(项 => 允许.has(项)) && new Set(诸).size === 诸.length;
          const 元允许 = new Set(['element', 'text', 'comments']);
          const 篇允许 = new Set(['doctype', 'text', 'comments', 'end']);
          if (!Array.isArray(规则) || 规则.length > 32 || !合法事件(篇事件, 篇允许) ||
              !规则.length && !篇事件.length || 规则.some(项 =>
                !项 || typeof 项 !== 'object' || Array.isArray(项) ||
                typeof 项.selector !== 'string' || !项.selector || 项.selector.length > 4096 ||
                !合法事件(项.events, 元允许) || !项.events.length)) throw Error('HTML 改写规则无效');
          const 应 = (种, 值, 选择) => 执行('html-' + 种, {选择器: 选择, 值}, 环境, null);
          let 改写器 = new 全局.HTMLRewriter();
          for (const 项 of 规则) {
            const 元处理 = {};
            if (项.events.includes('element')) 元处理.element = 值 => 应('element', 值, 项.selector);
            if (项.events.includes('text')) 元处理.text = 值 => 应('text', 值, 项.selector);
            if (项.events.includes('comments')) 元处理.comments = 值 => 应('comments', 值, 项.selector);
            改写器 = 改写器.on(项.selector, 元处理);
          }
          const 篇处理 = {};
          if (篇事件.includes('doctype')) 篇处理.doctype = 值 => 应('doctype', 值, '');
          if (篇事件.includes('text')) 篇处理.text = 值 => 应('document-text', 值, '');
          if (篇事件.includes('comments')) 篇处理.comments = 值 => 应('document-comments', 值, '');
          if (篇事件.includes('end')) 篇处理.end = 值 => 应('end', 值, '');
          if (Object.keys(篇处理).length) 改写器 = 改写器.onDocument(篇处理);
          return 句柄.登记(改写器.transform(原回应));
        },
        豫言_云工_HTML改写响应全: (回应号, 选择器, 事件文) => {
          const 诸事件 = JSON.parse(文字(事件文));
          const 元事件 = Array.isArray(诸事件) ? 诸事件.filter(项 => ['element', 'text', 'comments'].includes(项)) : null;
          const 篇事件 = Array.isArray(诸事件) ? 诸事件.filter(项 => ['doctype', 'document-text', 'document-comments', 'end'].includes(项)) : null;
          if (!Array.isArray(诸事件) || 诸事件.length !== (元事件.length + 篇事件.length) ||
              new Set(诸事件).size !== 诸事件.length) throw Error('HTML 回调事件无效');
          return 能力.豫言_云工_HTML改写响应规则(回应号,
            JSON.stringify(元事件.length ? [{selector: 文字(选择器), events: 元事件}] : []),
            JSON.stringify(篇事件.map(项 => 项.replace('document-', ''))));
        },
        豫言_云工_HTML改写响应: (回应号, 选择器) =>
          能力.豫言_云工_HTML改写响应全(回应号, 选择器, '["element"]'),
        // 文言：取云工当时之纪元毫秒，云上无 I/O 时其钟可止。汉语：Date.now 供豫言计算绝对期限，遵循 Workers 的计时限制。
        豫言_云工_当前Unix毫秒: () => 全局.Date.now(),
        // 文言：性能时与原点循云工原生数而归文，客可明察精度。汉语：保留 performance 原始数值的文本表示，避免桥接时截断小数。
        豫言_云工_性能时刻文: () => String(全局.performance.now()),
        豫言_云工_性能原点文: () => String(全局.performance.timeOrigin),
        // 文言：诸定时事仅在本次调用之界内候，客执标与号自裁所行。汉语：定时器只投递事件，事件逻辑和取消由豫言控制；调用结束清理余项。
        豫言_云工_定时一次: (毫秒, 标记) => 造定时(毫秒, 标记, false),
        豫言_云工_定时重复: (毫秒, 标记) => 造定时(毫秒, 标记, true),
        豫言_云工_取消定时: 撤定时,
        豫言_云工_等待定时事件文: async 时限 => JSON.stringify(await 等定时事(时限)),
        // 文言：每步复入客器；客定步骤与输出，云工唯持久缓存步骤之果。汉语：Workflows 步骤回调重启豫言 Wasm，控制流及值由豫言决定。
        豫言_云工_工作流事件文: () => {
          if (!是工作流) throw Error('当前事件不是工作流');
          const 事 = 种类 === 'workflow' ? 载荷 : 载荷.事件;
          return JSON.stringify({载荷: 事.payload, 实例号: 事.instanceId, 工作流名: 事.workflowName,
            时刻: 事.timestamp?.toISOString?.() ?? null, 定时: 事.schedule ?? null});
        },
        豫言_云工_工作流步骤名: () => {
          if (种类 !== 'workflow-step') throw Error('当前事件不是工作流步骤');
          return 载荷.步骤名;
        },
        豫言_云工_工作流步骤输入文: () => {
          if (种类 !== 'workflow-step') throw Error('当前事件不是工作流步骤');
          return JSON.stringify(载荷.输入);
        },
        豫言_云工_工作流步骤上下文文: () => {
          if (种类 !== 'workflow-step') throw Error('当前事件不是工作流步骤');
          const 值 = 载荷.步骤上下文;
          return JSON.stringify({名称: 值.step.name, 次数: 值.step.count, 尝试: 值.attempt, 配置: 值.config});
        },
        豫言_云工_工作流回滚信息文: () => {
          if (种类 !== 'workflow-rollback') throw Error('当前事件不是工作流回滚');
          const 值 = 载荷.步骤上下文;
          return JSON.stringify({步骤名: 载荷.步骤名, 输入: 载荷.输入,
            上下文: {名称: 值.step.name, 次数: 值.step.count, 尝试: 值.attempt, 配置: 值.config},
            输出: 载荷.步骤输出, 错误: {名称: 载荷.错误?.name ?? 'Error', 消息: 载荷.错误?.message ?? ''}});
        },
        豫言_云工_工作流设输出: 结果文 => {
          if (!需工作流输出 || 已设工作流输出) throw Error('工作流输出只能设置一次');
          工作流输出 = JSON.parse(文字(结果文));
          已设工作流输出 = true;
        },
        豫言_云工_工作流步做: async (步骤名, 配置文, 输入文) => {
          if (种类 !== 'workflow' || !工作流步) throw Error('当前事件不能执行工作流步骤');
          const 名 = 文字(步骤名);
          const 配置 = JSON.parse(文字(配置文));
          const 输入 = JSON.parse(文字(输入文));
          if (!配置 || typeof 配置 !== 'object' || Array.isArray(配置)) throw Error('工作流步骤配置须为对象');
          const 回调 = 步骤上下文 => 执行('workflow-step', {事件: 载荷, 步骤名: 名, 输入, 步骤上下文}, 环境, null);
          const 结果 = Object.keys(配置).length ? await 工作流步.do(名, 配置, 回调) : await 工作流步.do(名, 回调);
          return JSON.stringify(结果);
        },
        豫言_云工_工作流步做可回滚: async (步骤名, 配置文, 输入文, 回滚配置文) => {
          if (种类 !== 'workflow' || !工作流步) throw Error('当前事件不能执行可回滚步骤');
          const 名 = 文字(步骤名);
          const 配置 = JSON.parse(文字(配置文));
          const 输入 = JSON.parse(文字(输入文));
          const 回滚配置 = JSON.parse(文字(回滚配置文));
          if (!配置 || typeof 配置 !== 'object' || Array.isArray(配置) ||
              !回滚配置 || typeof 回滚配置 !== 'object' || Array.isArray(回滚配置)) throw Error('工作流步骤配置须为对象');
          const 回调 = 步骤上下文 => 执行('workflow-step', {事件: 载荷, 步骤名: 名, 输入, 步骤上下文}, 环境, null);
          const 回滚 = async ({ctx, output, error}) => {
            await 执行('workflow-rollback', {事件: 载荷, 步骤名: 名, 输入, 步骤上下文: ctx, 步骤输出: output, 错误: error}, 环境, null);
          };
          const 选项 = {rollback: 回滚};
          if (Object.keys(回滚配置).length) 选项.rollbackConfig = 回滚配置;
          const 结果 = Object.keys(配置).length ? await 工作流步.do(名, 配置, 回调, 选项) : await 工作流步.do(名, 回调, 选项);
          return JSON.stringify(结果);
        },
        豫言_云工_工作流休眠: async (步骤名, 时长文) => {
          if (种类 !== 'workflow' || !工作流步) throw Error('当前事件不能休眠工作流');
          const 时长 = JSON.parse(文字(时长文));
          if (typeof 时长 !== 'string' && !(typeof 时长 === 'number' && Number.isFinite(时长))) throw Error('工作流休眠时长无效');
          await 工作流步.sleep(文字(步骤名), 时长);
        },
        豫言_云工_工作流休眠至: async (步骤名, 时刻文) => {
          if (种类 !== 'workflow' || !工作流步) throw Error('当前事件不能定时唤醒工作流');
          const 时刻 = Number(文字(时刻文));
          if (!Number.isSafeInteger(时刻) || 时刻 < 0) throw Error('工作流绝对时刻无效');
          await 工作流步.sleepUntil(文字(步骤名), 时刻);
        },
        豫言_云工_工作流候事件: async (步骤名, 选项文) => {
          if (种类 !== 'workflow' || !工作流步) throw Error('当前事件不能等待工作流事件');
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) || typeof 选项.type !== 'string') throw Error('工作流等待选项无效');
          return JSON.stringify(await 工作流步.waitForEvent(文字(步骤名), 选项));
        },
        豫言_云工_工作流创建: async (名, 选项文) => {
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('工作流创建选项须为对象');
          return (await 绑定(环境, 许可, 名, 'WORKFLOW').create(选项)).id;
        },
        豫言_云工_工作流批量创建文: async (名, 批文) => {
          const 批次 = JSON.parse(文字(批文));
          if (!Array.isArray(批次) || 批次.length < 1 || 批次.length > 100 ||
              批次.some(项 => !项 || typeof 项 !== 'object' || Array.isArray(项) || typeof 项.id !== 'string' || !Object.hasOwn(项, 'params'))) {
            throw Error('工作流批量创建选项无效');
          }
          const 实例 = await 绑定(环境, 许可, 名, 'WORKFLOW').createBatch(批次);
          return JSON.stringify(实例.map(项 => 项.id));
        },
        豫言_云工_工作流批量删除文: async (名, 批文) => {
          const 诸号 = JSON.parse(文字(批文));
          if (!Array.isArray(诸号) || 诸号.length < 1 || 诸号.length > 100 || 诸号.some(项 => typeof 项 !== 'string')) {
            throw Error('工作流批量删除实例号无效');
          }
          return JSON.stringify(await 绑定(环境, 许可, 名, 'WORKFLOW').deleteBatch(诸号));
        },
        豫言_云工_工作流状态文: async (名, 实例号) => {
          const 实例 = await 绑定(环境, 许可, 名, 'WORKFLOW').get(文字(实例号));
          return JSON.stringify(await 实例.status());
        },
        豫言_云工_工作流送事件: async (名, 实例号, 选项文) => {
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) || typeof 选项.type !== 'string') throw Error('工作流事件选项无效');
          const 实例 = await 绑定(环境, 许可, 名, 'WORKFLOW').get(文字(实例号));
          await 实例.sendEvent(选项);
        },
        // 文言：实例之停、复、重起、终与删，皆须逐名受授。汉语：实例管理只经 WORKFLOW 绑定许可访问指定实例。
        豫言_云工_工作流暂停: async (名, 实例号) => {
          await (await 绑定(环境, 许可, 名, 'WORKFLOW').get(文字(实例号))).pause();
        },
        豫言_云工_工作流恢复: async (名, 实例号) => {
          await (await 绑定(环境, 许可, 名, 'WORKFLOW').get(文字(实例号))).resume();
        },
        豫言_云工_工作流重启: async (名, 实例号, 选项文) => {
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('工作流重启选项须为对象');
          await (await 绑定(环境, 许可, 名, 'WORKFLOW').get(文字(实例号))).restart(选项);
        },
        豫言_云工_工作流终止: async (名, 实例号, 选项文) => {
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('工作流终止选项须为对象');
          await (await 绑定(环境, 许可, 名, 'WORKFLOW').get(文字(实例号))).terminate(选项);
        },
        豫言_云工_工作流删除: async (名, 实例号) => {
          await (await 绑定(环境, 许可, 名, 'WORKFLOW').get(文字(实例号))).delete();
        },
        豫言_云工_持久状态句柄: () => 句柄.登记(取持久状态()),
        豫言_云工_持久仓句柄: () => 句柄.登记(取持久仓()),
        豫言_云工_持久对象标识: () => String(取持久状态().id),
        豫言_云工_持久对象标识信息文: () => {
          const ID = 取持久状态().id;
          return JSON.stringify({字符串: ID.toString(), 名称: ID.name ?? null, 辖区: ID.jurisdiction ?? null});
        },
        // 文言：事务回调别启豫言客器，所读写皆循 txn。汉语：在独立的豫言调用中运行事务回调，KV 操作绑定到平台 txn。
        豫言_云工_持久事务执行: async 名 => {
          if (!对象状态 || 事务仓) throw Error('仅持久对象事件可启动事务');
          const 仓 = 取持久仓();
          if (typeof 仓.transaction !== 'function') throw Error('持久对象不支持异步事务');
          return await 仓.transaction(事务 => 执行('durable-transaction', 文字(名), 环境, 上下文, 对象状态, null, 事务));
        },
        豫言_云工_持久事务名: () => {
          if (!事务仓) throw Error('当前事件不是持久对象事务');
          return 文字(载荷);
        },
        豫言_云工_持久事务输出: 结果 => {
          if (!事务仓 || 已设事务输出) throw Error('事务结果只能设置一次');
          事务输出 = 文字(结果);
          已设事务输出 = true;
        },
        豫言_云工_持久事务回滚: () => {
          if (!事务仓) throw Error('当前事件不是持久对象事务');
          事务仓.rollback();
        },
        豫言_云工_持久读取文字: async 键 => {
          const 值 = await 取键值仓().get(文字(键));
          if (值 == null) return [false, ''];
          if (typeof 值 !== 'string') throw Error('持久对象存储值不是字符串');
          return [true, 值];
        },
        豫言_云工_持久写入文字: async (键, 值) => { await 取键值仓().put(文字(键), 文字(值)); },
        豫言_云工_持久读取值: async 键 => {
          const 值 = await 取键值仓().get(文字(键));
          return 值 === undefined ? [false, ''] : [true, JSON.stringify(句柄.出(值))];
        },
        // 文言：缺键之果以单文还，免二元组于事复入时多型转渡。汉语：以单个 JSON 字符串返回存在标记和值，便于事务回调可靠处理缺失键。
        豫言_云工_持久读取值安全文: async 键 => {
          const 值 = await 取键值仓().get(文字(键));
          return 值 === undefined ? '{"存在":false}' : JSON.stringify({存在: true, 值: 句柄.出(值)});
        },
        豫言_云工_持久写入值: async (键, 值文) => {
          await 取键值仓().put(文字(键), 句柄.入(JSON.parse(文字(值文))));
        },
        豫言_云工_持久删除: async 键 => Boolean(await 取键值仓().delete(文字(键))),
        // 文言：批读还平台 Map 之有序双元列，缺键不列；批写删皆循当前事务仓。汉语：多键操作保留 Map 排序、缺失键省略和事务上下文。
        豫言_云工_持久批读值: async (键文, 选项文) => {
          const 键列 = JSON.parse(文字(键文));
          const 选项 = JSON.parse(文字(选项文));
          if (!Array.isArray(键列) || 键列.length > 128 || !键列.every(键 => typeof 键 === 'string')) throw Error('持久批读键须为至多 128 个字符串');
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) || Object.entries(选项).some(([名, 值]) => !['allowConcurrency', 'noCache'].includes(名) || typeof 值 !== 'boolean')) throw Error('持久批读选项无效');
          const 结果 = await 取键值仓().get(键列, 选项);
          return JSON.stringify(Array.from(结果, ([键, 值]) => [键, 句柄.出(值)]));
        },
        豫言_云工_持久批写值: async (各值文, 选项文) => {
          const 各值 = JSON.parse(文字(各值文));
          const 选项 = JSON.parse(文字(选项文));
          if (!各值 || typeof 各值 !== 'object' || Array.isArray(各值) || Object.keys(各值).length > 128) throw Error('持久批写值须为至多 128 项对象');
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) || Object.entries(选项).some(([名, 值]) => !['allowUnconfirmed', 'noCache'].includes(名) || typeof 值 !== 'boolean')) throw Error('持久批写选项无效');
          await 取键值仓().put(句柄.入(各值), 选项);
        },
        豫言_云工_持久批删: async (键文, 选项文) => {
          const 键列 = JSON.parse(文字(键文));
          const 选项 = JSON.parse(文字(选项文));
          if (!Array.isArray(键列) || 键列.length > 128 || !键列.every(键 => typeof 键 === 'string')) throw Error('持久批删键须为至多 128 个字符串');
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) || Object.entries(选项).some(([名, 值]) => !['allowUnconfirmed', 'noCache'].includes(名) || typeof 值 !== 'boolean')) throw Error('持久批删选项无效');
          return Number(await 取键值仓().delete(键列, 选项));
        },
        豫言_云工_持久列举值: async 选项文 => {
          const 原选项 = JSON.parse(文字(选项文));
          if (!原选项 || typeof 原选项 !== 'object' || Array.isArray(原选项)) throw Error('持久列举选项须为对象');
          const 允许 = new Set(['start', 'startAfter', 'end', 'prefix', 'reverse', 'limit', 'allowConcurrency', 'noCache']);
          for (const [名, 值] of Object.entries(原选项)) {
            if (!允许.has(名)) throw Error('持久列举选项无效：' + 名);
            if (['start', 'startAfter', 'end', 'prefix'].includes(名) && typeof 值 !== 'string') throw Error('持久列举键边界须为字符串');
            if (['reverse', 'allowConcurrency', 'noCache'].includes(名) && typeof 值 !== 'boolean') throw Error('持久列举开关须为爻');
            if (名 === 'limit' && (!Number.isSafeInteger(值) || 值 < 1)) throw Error('持久列举数量须为正整数');
          }
          if (Object.hasOwn(原选项, 'start') && Object.hasOwn(原选项, 'startAfter')) throw Error('持久列举不能同时指定 start 与 startAfter');
          const 结果 = await 取键值仓().list(原选项);
          return JSON.stringify(Array.from(结果, ([键, 值]) => [键, 句柄.出(值)]));
        },
        // 文言：清仓动 SQL、KV 与告警；确认既写之术各从平台。汉语：deleteAll 和 sync 直接转发 DurableObjectStorage，不在 KV 事务回调内调用。
        豫言_云工_持久清仓: async 选项文 => {
          if (事务仓) throw Error('事务回调内不可清空持久仓');
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) || Object.entries(选项).some(([名, 值]) => !['allowUnconfirmed', 'noCache'].includes(名) || typeof 值 !== 'boolean')) throw Error('持久清仓选项无效');
          await 取持久仓().deleteAll(选项);
        },
        豫言_云工_持久同步: async () => {
          if (事务仓) throw Error('事务回调内不可单独同步持久仓');
          await 取持久仓().sync();
        },
        // 文言：SQLite 同步 KV 与异步 KV 同表；列举于本次宿主调用取尽。汉语：同步 KV 操作直接访问 SQLite 后端，list 在同一次调用中消费迭代器。
        豫言_云工_同步键值读取: 键 => {
          const 值 = 取同步键值仓().get(文字(键));
          return 值 === undefined ? [false, ''] : [true, JSON.stringify(句柄.出(值))];
        },
        豫言_云工_同步键值写入: (键, 值文) => {
          取同步键值仓().put(文字(键), 句柄.入(JSON.parse(文字(值文))));
        },
        豫言_云工_同步键值删除: 键 => Boolean(取同步键值仓().delete(文字(键))),
        豫言_云工_同步键值列举: 选项文 => {
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('同步 KV 列举选项须为对象');
          const 允许 = new Set(['start', 'startAfter', 'end', 'prefix', 'reverse', 'limit']);
          for (const [名, 值] of Object.entries(选项)) {
            if (!允许.has(名)) throw Error('同步 KV 列举选项无效：' + 名);
            if (['start', 'startAfter', 'end', 'prefix'].includes(名) && typeof 值 !== 'string') throw Error('同步 KV 键边界须为字符串');
            if (名 === 'reverse' && typeof 值 !== 'boolean') throw Error('同步 KV 逆序须为爻');
            if (名 === 'limit' && (!Number.isSafeInteger(值) || 值 < 1)) throw Error('同步 KV 数量须为正整数');
          }
          if (Object.hasOwn(选项, 'start') && Object.hasOwn(选项, 'startAfter')) throw Error('同步 KV 列举不能同时指定 start 与 startAfter');
          return JSON.stringify(Array.from(取同步键值仓().list(选项), ([键, 值]) => [键, 句柄.出(值)]));
        },
        豫言_云工_持久SQL执行: (语句, 参数文) => {
          const 仓 = 取SQL仓();
          const 参数 = JSON.parse(文字(参数文));
          if (!Array.isArray(参数)) throw Error('SQL 参数须为癸象数组');
          const 结果 = 仓.exec(文字(语句), ...参数.map(项 => 句柄.入(项))).toArray();
          return JSON.stringify(句柄.出(结果));
        },
        // 文言：游标之序由客掌，宿主每次直行 next/raw/one；越候之视依平台。汉语：暴露平台 SqlStorageCursor 的迭代、原始行、单行和统计，跨 await 的快照限制仍按平台语义。
        豫言_云工_持久SQL游标新建: (语句, 参数文) => {
          const 参数 = JSON.parse(文字(参数文));
          if (!Array.isArray(参数)) throw Error('SQL 参数须为癸象数组');
          return 句柄.登记(取SQL仓().exec(文字(语句), ...参数.map(项 => 句柄.入(项))));
        },
        豫言_云工_持久SQL游标下一行: (号, 原始) => {
          const 游标 = 取SQL游标(号);
          const 结果 = (原始 ? 游标.raw() : 游标).next();
          return JSON.stringify(结果.done ? {已终: true} : {已终: false, 值: 句柄.出(结果.value)});
        },
        豫言_云工_持久SQL游标余行: (号, 原始) => {
          const 游标 = 取SQL游标(号);
          return JSON.stringify(句柄.出((原始 ? 游标.raw() : 游标).toArray()));
        },
        豫言_云工_持久SQL游标单行安全: 号 => {
          try { return JSON.stringify({成功: true, 值: 句柄.出(取SQL游标(号).one())}); }
          catch (错) { return JSON.stringify({成功: false, 错误: String(错?.message ?? 错)}); }
        },
        豫言_云工_持久SQL游标属性: 号 => {
          const 游标 = 取SQL游标(号);
          return JSON.stringify({列名: Array.from(游标.columnNames), 已读行: 游标.rowsRead, 已写行: 游标.rowsWritten});
        },
        豫言_云工_持久SQL游标释放: 号 => { 取SQL游标(号); 句柄.释放(文字(号)); },
        豫言_云工_持久SQL数据库大小: () => String(取SQL仓().databaseSize),
        // 文言：复时之签惟 SQLite 云上可得；本地不行则归阴与原误。汉语：PITR 方法原样调用平台，安全入口将本地未支持或参数错误返回豫言。
        豫言_云工_持久当前书签安全: async () => {
          try { return [true, String(await 取恢复仓().getCurrentBookmark())]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_持久时刻书签安全: async 时文 => {
          try {
            const 时 = Number(文字(时文));
            if (!Number.isSafeInteger(时) || 时 < 0) throw Error('恢复书签时刻无效');
            return [true, String(await 取恢复仓().getBookmarkForTime(时))];
          } catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_持久下次会话恢复书签安全: async 书签 => {
          try {
            const 文 = 文字(书签);
            if (!文) throw Error('恢复书签不可为空');
            return [true, String(await 取恢复仓().onNextSessionRestoreBookmark(文))];
          } catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        // 文言：客既备复时之签，方可命对象重启；宿主不代客决断。汉语：豫言显式决定是否调用 ctx.abort 以完成预定恢复；本地不实际执行此操作。
        豫言_云工_持久重启对象: (原因, 重试告警) => {
          if (事务仓) throw Error('持久事务回调内不可重启对象');
          取持久状态().abort(文字(原因), {retryAlarm: Boolean(重试告警)});
        },
        豫言_云工_持久SQL事务: 步骤文 => {
          const 仓 = 取持久仓();
          if (!仓.sql || typeof 仓.transactionSync !== 'function') throw Error('持久对象没有 SQLite 同步事务');
          const 步骤 = JSON.parse(文字(步骤文));
          if (!Array.isArray(步骤) || 步骤.length > 256) throw Error('SQL 事务步骤无效');
          const 结果 = 仓.transactionSync(() => 步骤.map(项 => {
            if (!Array.isArray(项) || 项.length !== 2 || typeof 项[0] !== 'string' || !Array.isArray(项[1])) {
              throw Error('SQL 事务步骤须为语句与参数数组');
            }
            return 仓.sql.exec(项[0], ...项[1].map(值 => 句柄.入(值))).toArray();
          }));
          return JSON.stringify(句柄.出(结果));
        },
        豫言_云工_持久取告警: async () => {
          const 时 = await 取持久仓().getAlarm();
          return 时 == null ? [false, ''] : [true, String(时)];
        },
        豫言_云工_持久设告警: async 时文 => {
          const 时 = Number(文字(时文));
          if (!Number.isSafeInteger(时) || 时 < 0) throw Error('持久对象告警时刻无效');
          await 取持久仓().setAlarm(时);
        },
        豫言_云工_持久延后告警: async 毫秒 => {
          const 延时 = Number(毫秒);
          if (!Number.isSafeInteger(延时) || 延时 < 0) throw Error('持久对象告警延时无效');
          const 时 = Date.now() + 延时;
          if (!Number.isSafeInteger(时)) throw Error('持久对象告警时刻溢出');
          await 取持久仓().setAlarm(时);
        },
        豫言_云工_持久删告警: async () => { await 取持久仓().deleteAlarm(); },
        豫言_云工_持久告警重试数: () => Number(取告警信息().retryCount ?? 0),
        豫言_云工_持久告警为重试: () => Boolean(取告警信息().isRetry),
        豫言_云工_定时控制器句柄: () => 句柄.登记(取定时()),
        豫言_云工_定时表达式: () => 取定时().cron,
        豫言_云工_定时时刻: () => String(取定时().scheduledTime),
        豫言_云工_定时不重试: () => {
          const 控制 = 取定时();
          if (typeof 控制.noRetry !== 'function') throw Error('定时控制器不支持 noRetry');
          控制.noRetry();
        },
        豫言_云工_邮件句柄: () => 句柄.登记(取邮件()),
        豫言_云工_邮件发件: () => 取邮件().from,
        豫言_云工_邮件收件: () => 取邮件().to,
        豫言_云工_邮件大小: () => 取邮件().rawSize,
        豫言_云工_邮件可转发: () => Boolean(取邮件().canBeForwarded),
        豫言_云工_邮件标头句柄: () => 句柄.登记(取邮件().headers),
        豫言_云工_邮件原文流句柄: () => 句柄.登记(取邮件().raw),
        豫言_云工_邮件拒绝: 原因 => { 取邮件().setReject(文字(原因)); },
        豫言_云工_邮件转发: async 收件 => JSON.stringify(句柄.出(await 取邮件().forward(文字(收件)))),
        豫言_云工_邮件带标头转发: async (收件, 标头号) => {
          const 标头 = 句柄.取得(文字(标头号));
          if (!(标头 instanceof Headers)) throw Error('邮件转发标头句柄不是 Headers');
          return JSON.stringify(句柄.出(await 取邮件().forward(文字(收件), 标头)));
        },
        豫言_云工_邮件回复原文: async 原文 => {
          const 构造 = 全局.EmailMessage ?? (await import('cloudflare:email')).EmailMessage;
          if (typeof 构造 !== 'function') throw Error('云工 EmailMessage 构造器不存在');
          const 信 = 取邮件();
          return JSON.stringify(句柄.出(await 信.reply(new 构造(信.to, 信.from, 文字(原文)))));
        },
        豫言_云工_队列批次句柄: () => 句柄.登记(取批次()),
        豫言_云工_队列名称: () => 取批次().queue,
        豫言_云工_队列消息数: () => 取批次().messages.length,
        豫言_云工_队列消息句柄: 序 => 句柄.登记(取消息(序)),
        豫言_云工_队列消息正文: 序 => JSON.stringify(句柄.出(取消息(序).body)),
        豫言_云工_队列消息标识: 序 => 取消息(序).id,
        豫言_云工_队列消息尝试数: 序 => 取消息(序).attempts,
        豫言_云工_队列消息时间: 序 => 取消息(序).timestamp.toISOString(),
        豫言_云工_队列消息确认: 序 => { 取消息(序).ack(); },
        豫言_云工_队列消息重试: 序 => { 取消息(序).retry(); },
        豫言_云工_队列消息延后重试: (序, 秒) => { 取消息(序).retry({delaySeconds: Number(秒)}); },
        豫言_云工_队列全数确认: () => { 取批次().ackAll(); },
        豫言_云工_队列全数重试: () => { 取批次().retryAll(); },
        豫言_云工_队列全数延后重试: 秒 => { 取批次().retryAll({delaySeconds: Number(秒)}); },
        豫言_云工_读取请求: 输入,
        豫言_云工_设置响应: (状态, 种类, 内容) => {
          const 码 = Number(状态);
          if (!Number.isInteger(码) || 码 < 200 || 码 > 599) throw Error('响应状态无效');
          设响应(new Response(文字(内容), {status: 码, headers: {'content-type': 文字(种类)}}));
        },
        豫言_云工_请求文字: async (方法, 网址, 内容) => {
          const 目标文 = 文字(网址);
          if (Array.isArray(许可.OUTBOUND_ORIGINS) && !已授外发网址(目标文)) throw Error('上游网址未获授权');
          const 回应 = await 网络(目标文, {method: 文字(方法), body: 文字(方法) === 'GET' ? undefined : 文字(内容), redirect: Array.isArray(许可.OUTBOUND_ORIGINS) ? 'error' : undefined});
          return JSON.stringify({状态: 回应.status, 正文: await 限文(回应)});
        },
        // 文言：外发惟许列明之 HTTPS 来源，拒凭据与异源转址。汉语：上游请求按程序许可核对 HTTPS 来源，适配器还须使用 redirect:error。
        豫言_云工_外发网址已授权: 网址 => 已授外发网址(文字(网址)),
        // 文言：先发求而归待柄，客得以断信号，后候回应。汉语：豫言先取得在途 fetch Promise，随后可中止，再领取 Response 句柄或错误。
        豫言_云工_请求发起可中断: (网址, 选项文, 信号号) => {
          const 信号 = 句柄.取得(文字(信号号));
          if (!(信号 instanceof 全局.AbortSignal)) throw Error('请求中断信号句柄无效');
          const 目标文 = 文字(网址);
          if (Array.isArray(许可.OUTBOUND_ORIGINS) && !已授外发网址(目标文)) throw Error('上游网址未获授权');
          const 选项 = 句柄.入(JSON.parse(文字(选项文)));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项)) throw Error('请求选项须为对象');
          let 待;
          try { 待 = Promise.resolve(网络(目标文, {...选项, redirect: Array.isArray(许可.OUTBOUND_ORIGINS) ? 'error' : 选项.redirect, signal: 信号})); }
          catch (错) { 待 = Promise.reject(错); }
          待.catch(() => {});
          return 句柄.登记(待);
        },
        豫言_云工_请求候回应安全: async 等号 => {
          const 名 = 文字(等号);
          let 已取 = false;
          try { const 待 = 句柄.取得(名); 已取 = true; return [true, 句柄.登记(await 待)]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
          finally { if (已取) 句柄.释放(名); }
        },
        // 文言：回应之体若空，则返阴；有流则授原生柄，客可逐块续解。汉语：把 fetch Response.body 显式交给豫言流接口，保留 null 与错误状态。
        豫言_云工_回应状态: 号 => Number(取回应对象(号).status),
        豫言_云工_回应标头句柄: 号 => 句柄.登记(取回应对象(号).headers),
        豫言_云工_回应正文流句柄: 号 => {
          const 体 = 取回应对象(号).body;
          return 体 === null ? [false, ''] : [true, 句柄.登记(体)];
        },
        豫言_云工_回应文字安全: async 号 => {
          try { return [true, await 限文(句柄.取得(文字(号)))]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_键值读取: async (名, 键) => (await 绑定(环境, 许可, 名, 'KV').get(文字(键))) ?? '',
        豫言_云工_键值写入: async (名, 键, 值) => { await 绑定(环境, 许可, 名, 'KV').put(文字(键), 文字(值)); },
        豫言_云工_对象读取: async (名, 键) => {
          const 对象 = await 绑定(环境, 许可, 名, 'R2').get(文字(键));
          return 对象 ? await 限文(对象) : '';
        },
        豫言_云工_对象写入: async (名, 键, 值) => { await 绑定(环境, 许可, 名, 'R2').put(文字(键), 文字(值)); },
        豫言_云工_数据库查询: async (名, 语句) => {
          const 结果 = await 绑定(环境, 许可, 名, 'D1').prepare(文字(语句)).all();
          return JSON.stringify(结果);
        },
        豫言_云工_服务请求文字: async (名, 方法, 网址, 内容) => {
          const 请求 = new Request(文字(网址), {method: 文字(方法), body: 文字(方法) === 'GET' ? undefined : 文字(内容)});
          const 回应 = await 绑定(环境, 许可, 名, 'SERVICE').fetch(请求);
          return JSON.stringify({状态: 回应.status, 正文: await 限文(回应)});
        },
        豫言_云工_持久对象请求文字: async (名, 对象名, 方法, 网址, 内容) => {
          const 空间 = 绑定(环境, 许可, 名, 'DO');
          const 桩 = 空间.get(空间.idFromName(文字(对象名)));
          const 请求 = new Request(文字(网址), {method: 文字(方法), body: 文字(方法) === 'GET' ? undefined : 文字(内容)});
          const 回应 = await 桩.fetch(请求);
          return JSON.stringify({状态: 回应.status, 正文: await 限文(回应)});
        },
        // 文言：对象空间、标识与桩留宿主；客以柄裁命名、辖区及定位。汉语：豫言持有命名空间、DurableObjectId 和 Stub 句柄，选项直接遵循平台规则。
        豫言_云工_持久空间句柄: 名 => 句柄.登记(绑定(环境, 许可, 名, 'DO')),
        豫言_云工_持久空间命名ID: (空间号, 名) =>
          句柄.登记(句柄.取得(文字(空间号)).idFromName(文字(名))),
        豫言_云工_持久空间唯一ID: (空间号, 选项文) => {
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) ||
              Object.entries(选项).some(([名, 值]) => 名 !== 'jurisdiction' || typeof 值 !== 'string' || !值))
            throw Error('持久对象唯一 ID 选项无效');
          return 句柄.登记(句柄.取得(文字(空间号)).newUniqueId(选项));
        },
        豫言_云工_持久空间唯一ID安全: (空间号, 选项文) => {
          try { return [true, 能力.豫言_云工_持久空间唯一ID(空间号, 选项文)]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_持久空间还原ID安全: (空间号, ID文) => {
          try { return [true, 句柄.登记(句柄.取得(文字(空间号)).idFromString(文字(ID文)))]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_持久ID字符串: 号 => 句柄.取得(文字(号)).toString(),
        豫言_云工_持久ID信息文: 号 => {
          const ID = 句柄.取得(文字(号));
          return JSON.stringify({字符串: ID.toString(), 名称: ID.name ?? null, 辖区: ID.jurisdiction ?? null});
        },
        豫言_云工_持久ID相等: (甲, 乙) => Boolean(句柄.取得(文字(甲)).equals(句柄.取得(文字(乙)))),
        豫言_云工_持久空间按ID取桩: (空间号, ID号, 选项文) => {
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) ||
              Object.entries(选项).some(([名, 值]) => 名 !== 'locationHint' || typeof 值 !== 'string' || !值))
            throw Error('持久对象取桩选项无效');
          return 句柄.登记(句柄.取得(文字(空间号)).get(句柄.取得(文字(ID号)), 选项));
        },
        豫言_云工_持久空间按名取桩: (空间号, 名, 选项文) => {
          const 选项 = JSON.parse(文字(选项文));
          if (!选项 || typeof 选项 !== 'object' || Array.isArray(选项) ||
              Object.entries(选项).some(([键, 值]) => 键 !== 'locationHint' || typeof 值 !== 'string' || !值))
            throw Error('持久对象按名取桩选项无效');
          return 句柄.登记(句柄.取得(文字(空间号)).getByName(文字(名), 选项));
        },
        豫言_云工_持久空间辖区: (空间号, 辖区) =>
          句柄.登记(句柄.取得(文字(空间号)).jurisdiction(文字(辖区))),
        豫言_云工_持久空间辖区安全: (空间号, 辖区) => {
          try { return [true, 句柄.登记(句柄.取得(文字(空间号)).jurisdiction(文字(辖区)))]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_请求句柄: () => {
          if (!请求) throw Error('当前事件没有 HTTP 请求');
          return 句柄.登记(请求);
        },
        豫言_云工_上下文句柄: () => {
          if (!上下文) throw Error('云工上下文不存在');
          return 句柄.登记(上下文);
        },
        豫言_云工_绑定句柄: (种类, 名) => 句柄.登记(绑定(环境, 许可, 名, 文字(种类))),
        // 文言：文值、密值各按名授；空串亦为实值。汉语：文本环境变量与 Secret 分别授权，空字符串也可读取。
        豫言_云工_环境文字: 名 => {
          const 值 = 取值绑定(环境, 许可, 名, 'ENV');
          if (typeof 值 !== 'string') throw Error('环境变量不是文字');
          return 值;
        },
        豫言_云工_环境值文: 名 => JSON.stringify(句柄.出(取值绑定(环境, 许可, 名, 'ENV'))),
        豫言_云工_秘密文字: 名 => {
          const 值 = 取值绑定(环境, 许可, 名, 'SECRET');
          if (typeof 值 !== 'string') throw Error('Secret 不是文字');
          return 值;
        },
        // 文言：许可仍严；缺密与空文分明，密值不书日志。汉语：可选 Secret 仍按名称授权；缺失与空字符串分开返回，不记录密钥。
        豫言_云工_秘密文字安全文: 名 => {
          const 名称 = 文字(名);
          if (!许可.SECRET?.includes(名称)) throw Error('未授权的 SECRET 绑定：' + 名称);
          if (!Object.hasOwn(环境, 名称)) return '{"存在":false}';
          const 值 = 环境[名称];
          if (值 == null) return '{"存在":false}';
          if (typeof 值 !== 'string') throw Error('Secret 不是文字');
          return JSON.stringify({存在: true, 文字: 值});
        },
        // 文言：账号密仓异步取值，未有与空文有别。汉语：Secrets Store 绑定调用 get()，缺失值与空字符串分开返回。
        豫言_云工_密仓读取: async 名 => {
          const 值 = await 绑定(环境, 许可, 名, 'SECRETS_STORE').get();
          if (值 == null) return [false, ''];
          if (typeof 值 !== 'string') throw Error('Secrets Store 返回值不是文字');
          return [true, 值];
        },
        豫言_云工_版本元数据文: 名 => {
          const 值 = 绑定(环境, 许可, 名, 'VERSION');
          return JSON.stringify(句柄.出({id: 值.id, tag: 值.tag, timestamp: 值.timestamp}));
        },
        豫言_云工_分析写点: (名, 点文) => {
          const 点 = JSON.parse(文字(点文));
          if (!点 || typeof 点 !== 'object' || Array.isArray(点)) throw Error('分析数据点必须是对象');
          const blobs = 点.blobs ?? [], doubles = 点.doubles ?? [], indexes = 点.indexes ?? [];
          if (!Array.isArray(blobs) || blobs.length > 20 || blobs.some(项 => typeof 项 !== 'string') ||
              !Array.isArray(doubles) || doubles.length > 20 || doubles.some(项 => typeof 项 !== 'number' || !Number.isFinite(项)) ||
              !Array.isArray(indexes) || indexes.length > 1 || indexes.some(项 => typeof 项 !== 'string')) {
            throw Error('分析数据点字段无效');
          }
          绑定(环境, 许可, 名, 'ANALYTICS').writeDataPoint({blobs, doubles, indexes});
        },
        // 文言：限流之键由客定，配额与计数归云工。汉语：豫言决定限流键，宿主只调用获授权绑定的 limit 方法。
        豫言_云工_限流尝试: async (名, 键) => {
          const 结果 = await 绑定(环境, 许可, 名, 'RATE_LIMIT').limit({key: 文字(键)});
          if (!结果 || typeof 结果.success !== 'boolean') throw Error('限流绑定返回值无效');
          return 结果.success;
        },
        豫言_云工_动态加载: (名, 程序字节, cpuMs, subRequests) =>
          句柄.登记(绑定(环境, 许可, 名, 'LOADER').load(造隔离客码(程序字节, cpuMs, subRequests))),
        豫言_云工_动态按号取: (名, 标识, 程序字节, cpuMs, subRequests) =>
          句柄.登记(绑定(环境, 许可, 名, 'LOADER').get(文字(标识), async () => 造隔离客码(程序字节, cpuMs, subRequests))),
        豫言_云工_动态入口句柄: 号 => 句柄.登记(句柄.取得(文字(号)).getEntrypoint()),
        豫言_云工_全局句柄: 名 => {
          const 名称 = 句柄.允名(文字(名));
          if (!(名称 in 全局)) throw Error('云工全局能力不存在：' + 名称);
          return 句柄.登记(全局[名称]);
        },
        豫言_云工_读取属性: (号, 名) => {
          const 对象 = 句柄.取得(文字(号));
          return JSON.stringify(句柄.出(对象[句柄.允名(文字(名))]));
        },
        豫言_云工_设置对象属性: (号, 名, 值文) => {
          const 对象 = 句柄.取得(文字(号));
          对象[句柄.允名(文字(名))] = 句柄.入(JSON.parse(文字(值文)));
        },
        豫言_云工_调用方法: async (号, 名, 参数文) => {
          const 对象 = 句柄.取得(文字(号));
          const 方法 = 对象[句柄.允名(文字(名))];
          if (typeof 方法 !== 'function') throw Error('宿主成员不是方法');
          return JSON.stringify(句柄.出(await Reflect.apply(方法, 对象, 句柄.参数(文字(参数文)))));
        },
        豫言_云工_调用方法原始: (号, 名, 参数文) => {
          const 对象 = 句柄.取得(文字(号));
          const 方法 = 对象[句柄.允名(文字(名))];
          if (typeof 方法 !== 'function') throw Error('宿主成员不是方法');
          return JSON.stringify(句柄.出(Reflect.apply(方法, 对象, 句柄.参数(文字(参数文)))));
        },
        豫言_云工_等待句柄: async 号 => JSON.stringify(句柄.出(await 句柄.取得(文字(号)))),
        豫言_云工_构造对象: (名, 参数文) => {
          const 构造 = 全局[句柄.允名(文字(名))];
          if (typeof 构造 !== 'function') throw Error('云工构造器不存在');
          return JSON.stringify(句柄.出(Reflect.construct(构造, 句柄.参数(文字(参数文)))));
        },
        豫言_云工_调用全局: async (名, 参数文) => {
          const 函数 = 全局[句柄.允名(文字(名))];
          if (typeof 函数 !== 'function') throw Error('云工全局函数不存在');
          return JSON.stringify(句柄.出(await Reflect.apply(函数, 全局, 句柄.参数(文字(参数文)))));
        },
        豫言_云工_调用全局安全: async (名, 参数文) => {
          try {
            const 函数 = 全局[句柄.允名(文字(名))];
            if (typeof 函数 !== 'function') throw Error('云工全局函数不存在');
            return [true, JSON.stringify(句柄.出(await Reflect.apply(函数, 全局, 句柄.参数(文字(参数文)))))];
          } catch (错) { return [false, JSON.stringify({名称: String(错?.name ?? 'Error'), 消息: String(错?.message ?? 错)})]; }
        },
        豫言_云工_网址编解码安全: (方法, 内容) => {
          const 名 = 文字(方法);
          if (!['encodeURI', 'encodeURIComponent', 'decodeURI', 'decodeURIComponent'].includes(名)) return [false, '方法不受支持'];
          try { return [true, 全局[名](文字(内容))]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_设置响应句柄: 号 => {
          const 结果 = 句柄.取得(文字(号));
          if (!(结果 instanceof Response)) throw Error('宿主句柄不是 Response');
          设响应(结果);
        },
        // 文言：频道仅属持久对象；慢客逾限即断，使其可循客法重取。汉语：广播仅供同一 Durable Object 的事件共享，每订阅读者有 1 MiB 队列上限。
        豫言_云工_广播新流: 名值 => {
          if (!对象状态) throw Error('广播须在持久对象内使用');
          const 名 = 广播名(名值);
          let 诸听 = 广播频道.get(名);
          if (!诸听) {
            if (广播频道.size >= 64) throw Error('云工广播频道达到上限');
            诸听 = new Set();
            广播频道.set(名, 诸听);
          }
          if (诸听.size >= 256) throw Error('云工广播订阅达到上限');
          const 态 = {控制: null, 队列: [], 待字: 0, 已闭: false};
          const 清理 = () => {
            诸听.delete(态);
            if (诸听.size === 0 && 广播频道.get(名) === 诸听) 广播频道.delete(名);
          };
          const 排送 = () => {
            while (态.队列.length && 态.控制.desiredSize > 0) {
              const 块 = 态.队列.shift();
              态.待字 -= 块.byteLength;
              态.控制.enqueue(块);
            }
            if (态.已闭 && 态.队列.length === 0) { 态.控制.close(); 清理(); }
          };
          const 流 = new 全局.ReadableStream({
            start(控制) { 态.控制 = 控制; 诸听.add(态); },
            pull() { 排送(); },
            cancel() { 态.已闭 = true; 态.队列.length = 0; 态.待字 = 0; 清理(); }
          });
          try { return 句柄.登记(流); }
          catch (错) { void 流.cancel(); throw 错; }
        },
        豫言_云工_广播发字节: (名值, 内容) => {
          if (!对象状态) throw Error('广播须在持久对象内使用');
          const 名 = 广播名(名值), 诸听 = 广播频道.get(名);
          if (!诸听) return 0;
          const 字 = 内容.slice();
          if (字.byteLength > 65536) throw Error('云工广播单块超过 64 KiB');
          let 已送 = 0;
          for (const 态 of Array.from(诸听)) {
            if (态.已闭) continue;
            if (态.待字 + 字.byteLength > 1048576) {
              态.已闭 = true;
              态.队列.length = 0;
              态.控制.error(Error('云工广播读者积压超过上限'));
              诸听.delete(态);
              continue;
            }
            态.队列.push(字.slice());
            态.待字 += 字.byteLength;
            while (态.队列.length && 态.控制.desiredSize > 0) {
              const 块 = 态.队列.shift();
              态.待字 -= 块.byteLength;
              态.控制.enqueue(块);
            }
            已送++;
          }
          if (诸听.size === 0) 广播频道.delete(名);
          return 已送;
        },
        豫言_云工_广播关闭: 名值 => {
          if (!对象状态) throw Error('广播须在持久对象内使用');
          const 名 = 广播名(名值), 诸听 = 广播频道.get(名);
          if (!诸听) return 0;
          广播频道.delete(名);
          for (const 态 of 诸听) {
            态.已闭 = true;
            while (态.队列.length && 态.控制.desiredSize > 0) {
              const 块 = 态.队列.shift();
              态.待字 -= 块.byteLength;
              态.控制.enqueue(块);
            }
            if (态.队列.length === 0) 态.控制.close();
          }
          return 诸听.size;
        },
        豫言_云工_释放句柄: 号 => { 句柄.释放(文字(号)); },
        豫言_云工_句柄取字节: async 号 => {
          const 值 = 句柄.取得(文字(号));
          if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
          if (ArrayBuffer.isView(值)) return new Uint8Array(值.buffer, 值.byteOffset, 值.byteLength).slice();
          if (typeof Blob !== 'undefined' && 值 instanceof Blob) return new Uint8Array(await 值.arrayBuffer());
          throw Error('句柄不是二进制对象');
        },
        豫言_云工_字节成句柄: 内容 => 句柄.登记(内容.slice()),
        // 文言：物字之部件及名类皆由客定，宿主但造 Blob、File 并归原字。汉语：豫言控制 Blob/File 组成，宿主保留原生类型、MIME 和流语义。
        豫言_云工_物字造字节: (内容, 类别) => 句柄.登记(new 全局.Blob([内容.slice()], {type: 文字(类别)})),
        豫言_云工_物字造组合: (部件文, 选项文) => 句柄.登记(new 全局.Blob(句柄.参数(文字(部件文)), 句柄.入(JSON.parse(文字(选项文))))),
        豫言_云工_文件造字节: (内容, 名称, 类别, 修改时) => 句柄.登记(new 全局.File([内容.slice()], 文字(名称), {type: 文字(类别), lastModified: Number(修改时)})),
        豫言_云工_文件造组合: (部件文, 名称, 选项文) => 句柄.登记(new 全局.File(句柄.参数(文字(部件文)), 文字(名称), 句柄.入(JSON.parse(文字(选项文))))),
        豫言_云工_物字信息文: 号 => {
          const 值 = 句柄.取得(文字(号));
          if (!(值 instanceof 全局.Blob)) throw Error('句柄不是 Blob 或 File');
          const 是文件 = typeof 全局.File === 'function' && 值 instanceof 全局.File;
          return JSON.stringify({字节数: 值.size, 类别: 值.type, 文件名: 是文件 ? 值.name : null,
            修改毫秒: 是文件 ? 值.lastModified : null, 相对路径: 是文件 ? String(值.webkitRelativePath ?? '') : null});
        },
        豫言_云工_物字切片: (号, 起, 止, 类别) => 句柄.登记(句柄.取得(文字(号)).slice(Number(起), Number(止), 文字(类别))),
        豫言_云工_物字切片至尾: (号, 起, 类别) => 句柄.登记(句柄.取得(文字(号)).slice(Number(起), undefined, 文字(类别))),
        豫言_云工_物字原字: async 号 => new Uint8Array(await 句柄.取得(文字(号)).arrayBuffer()),
        豫言_云工_物字原生字节安全: async 号 => {
          try { return [true, await 句柄.取得(文字(号)).bytes()]; }
          catch (错) { return [false, new TextEncoder().encode(String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错))]; }
        },
        豫言_云工_物字文字: async 号 => await 句柄.取得(文字(号)).text(),
        豫言_云工_物字流: 号 => 句柄.登记(句柄.取得(文字(号)).stream()),
        豫言_云工_物字文字流安全: 号 => {
          try { return [true, 句柄.登记(句柄.取得(文字(号)).textStream())]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        // 文言：表单诸值按原序归客；文件留柄，毋以象文伤原字。汉语：保留 FormData 重名项顺序，文件通过句柄交给豫言。
        豫言_云工_表单新建: () => 句柄.登记(new 全局.FormData()),
        豫言_云工_表单追加文字: (号, 名, 值) => 句柄.取得(文字(号)).append(文字(名), 文字(值)),
        豫言_云工_表单追加文件: (号, 名, 文件号, 文件名) => 句柄.取得(文字(号)).append(文字(名), 句柄.取得(文字(文件号)), 文字(文件名)),
        豫言_云工_表单追加原文件: (号, 名, 文件号) => 句柄.取得(文字(号)).append(文字(名), 句柄.取得(文字(文件号))),
        豫言_云工_表单设置文字: (号, 名, 值) => 句柄.取得(文字(号)).set(文字(名), 文字(值)),
        豫言_云工_表单设置文件: (号, 名, 文件号, 文件名) => 句柄.取得(文字(号)).set(文字(名), 句柄.取得(文字(文件号)), 文字(文件名)),
        豫言_云工_表单设置原文件: (号, 名, 文件号) => 句柄.取得(文字(号)).set(文字(名), 句柄.取得(文字(文件号))),
        豫言_云工_表单删除: (号, 名) => 句柄.取得(文字(号)).delete(文字(名)),
        豫言_云工_表单含名: (号, 名) => 句柄.取得(文字(号)).has(文字(名)),
        豫言_云工_表单首项文: (号, 名) => {
          const 值 = 句柄.取得(文字(号)).get(文字(名));
          return JSON.stringify(值 === null ? null : typeof 值 === 'string' ? {种类: '文字', 值} : {种类: '文件', 句柄: 句柄.登记(值)});
        },
        豫言_云工_表单首文件安全: (号, 名) => {
          const 值 = 句柄.取得(文字(号)).get(文字(名));
          return 值 instanceof 全局.File ? [true, 句柄.登记(值)] : [false, ''];
        },
        豫言_云工_表单同名诸项文: (号, 名) => JSON.stringify(句柄.取得(文字(号)).getAll(文字(名)).map(值 =>
          typeof 值 === 'string' ? {种类: '文字', 值} : {种类: '文件', 句柄: 句柄.登记(值)})),
        豫言_云工_表单诸项文: 号 => JSON.stringify(Array.from(句柄.取得(文字(号)).entries(), ([名, 值]) =>
          [名, typeof 值 === 'string' ? {种类: '文字', 值} : {种类: '文件', 句柄: 句柄.登记(值)}])),
        豫言_云工_表单键列文: 号 => JSON.stringify(Array.from(句柄.取得(文字(号)).keys())),
        豫言_云工_表单值列文: 号 => JSON.stringify(Array.from(句柄.取得(文字(号)).values(), 值 =>
          typeof 值 === 'string' ? {种类: '文字', 值} : {种类: '文件', 句柄: 句柄.登记(值)})),
        豫言_云工_表单解析正文: async 号 => 句柄.登记(await 句柄.取得(文字(号)).formData()),
        // 文言：文编解码归宿主标准器，客操其字与状态。汉语：豫言调用 Worker 的 TextEncoder、TextDecoder 和流式解码。
        豫言_云工_编码UTF8: 内容 => new 全局.TextEncoder().encode(文字(内容)),
        豫言_云工_解码文字: (标记, 严格, 略首, 内容) =>
          new 全局.TextDecoder(文字(标记), {fatal: Boolean(严格), ignoreBOM: Boolean(略首)}).decode(内容),
        豫言_云工_创建解码器: (标记, 严格, 略首) =>
          句柄.登记(new 全局.TextDecoder(文字(标记), {fatal: Boolean(严格), ignoreBOM: Boolean(略首)})),
        豫言_云工_续解码: (号, 内容) => 句柄.取得(文字(号)).decode(内容, {stream: true}),
        豫言_云工_终解码: 号 => 句柄.取得(文字(号)).decode(),
        豫言_云工_编码入容量: (内容, 容量) => {
          const 长度 = Number(容量);
          if (!Number.isSafeInteger(长度) || 长度 < 0 || 长度 > 2 * 1024 * 1024) throw Error('编码目标容量无效');
          const 目标 = new Uint8Array(长度);
          const 结果 = new 全局.TextEncoder().encodeInto(文字(内容), 目标);
          return [结果.read, 目标.subarray(0, 结果.written)];
        },
        // 文言：候时之约可受中断；拒则归状，不遗悬约。汉语：可取消等待立即附上拒绝处理，避免未处理的 Promise 拒绝。
        豫言_云工_调度等待: async 毫秒 => {
          const 时 = Number(文字(毫秒));
          if (!Number.isFinite(时) || 时 < 0) throw Error('等待时长无效');
          await 全局.scheduler.wait(时);
        },
        豫言_云工_调度控制器: () => 句柄.登记(new 全局.AbortController()),
        // 文言：中断理由与原生信号留柄，客可察其态而联诸信号。汉语：AbortSignal 对象和 reason 留在宿主，豫言只持句柄并查询状态。
        豫言_云工_中断控制器新建: () => 句柄.登记(new 全局.AbortController()),
        豫言_云工_中断控制器信号: 号 => 句柄.登记(句柄.取得(文字(号)).signal),
        豫言_云工_中断控制器中断: 号 => 句柄.取得(文字(号)).abort(),
        豫言_云工_中断控制器带理由: (号, 理由文) => 句柄.取得(文字(号)).abort(句柄.入(JSON.parse(文字(理由文)))),
        豫言_云工_中断信号已中断: 号 => Boolean(句柄.取得(文字(号)).aborted),
        豫言_云工_中断信号状态文: 号 => {
          const 信号 = 句柄.取得(文字(号));
          if (!(信号 instanceof 全局.AbortSignal)) throw Error('句柄不是 AbortSignal');
          const 理由 = 信号.reason;
          return JSON.stringify({已中断: 信号.aborted, 理由: 句柄.出(理由), 理由名: String(理由?.name ?? ''), 理由消息: String(理由?.message ?? '')});
        },
        豫言_云工_中断信号检查安全: 号 => {
          try { 句柄.取得(文字(号)).throwIfAborted(); return [true, '']; }
          catch (错) { return [false, JSON.stringify({名称: String(错?.name ?? 'Error'), 消息: String(错?.message ?? 错), 值: 句柄.出(错)})]; }
        },
        豫言_云工_中断信号立断: 理由文 => 句柄.登记(全局.AbortSignal.abort(句柄.入(JSON.parse(文字(理由文))))),
        豫言_云工_中断信号立断无理由: () => 句柄.登记(全局.AbortSignal.abort()),
        豫言_云工_中断信号限时安全: 毫秒 => {
          try { return [true, 句柄.登记(全局.AbortSignal.timeout(Number(毫秒)))]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_中断信号合一安全: 诸号文 => {
          try { return [true, 句柄.登记(全局.AbortSignal.any(句柄.参数(文字(诸号文))))]; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_中断信号等事文: async (号, 时限) => {
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
        豫言_云工_调度可取消等待: (毫秒, 控制号) => {
          const 时 = Number(文字(毫秒));
          if (!Number.isFinite(时) || 时 < 0) throw Error('等待时长无效');
          const 控制器 = 句柄.取得(文字(控制号));
          if (!(控制器 instanceof 全局.AbortController)) throw Error('等待控制器句柄无效');
          return 句柄.登记(全局.scheduler.wait(时, {signal: 控制器.signal}).then(
            () => [true, ''], 错 => [false, String(错?.name ?? 错)]));
        },
        豫言_云工_调度中断: 控制号 => {
          const 控制器 = 句柄.取得(文字(控制号));
          if (!(控制器 instanceof 全局.AbortController)) throw Error('等待控制器句柄无效');
          控制器.abort();
        },
        豫言_云工_调度等结果: async 等号 => await 句柄.取得(文字(等号)),
        // 文言：密钥留宿主，惟以柄用之。汉语：CryptoKey 不出宿主，豫言只持不透明句柄。
        豫言_云工_密码随机识别: () => 全局.crypto.randomUUID(),
        豫言_云工_密码随机字节: 长度 => {
          const 数 = Number(长度);
          if (!Number.isSafeInteger(数) || 数 < 0 || 数 > 65536) throw Error('随机字节长度无效');
          return 全局.crypto.getRandomValues(new Uint8Array(数));
        },
        豫言_云工_密码摘要: async (算法, 内容) => new Uint8Array(await 全局.crypto.subtle.digest(文字(算法), 内容)),
        豫言_云工_密码导入AES: async 钥字节 => 句柄.登记(await 全局.crypto.subtle.importKey('raw', 钥字节, 'AES-GCM', false, ['encrypt', 'decrypt'])),
        豫言_云工_密码生成AES: async (位数, 可导) =>
          句柄.登记(await 全局.crypto.subtle.generateKey({name: 'AES-GCM', length: Number(位数)}, Boolean(可导), ['encrypt', 'decrypt'])),
        豫言_云工_密码导出AES: async 钥号 =>
          new Uint8Array(await 全局.crypto.subtle.exportKey('raw', 句柄.取得(文字(钥号)))),
        豫言_云工_密码AES加密: async (钥号, 随机数, 附加文, 明文) =>
          new Uint8Array(await 全局.crypto.subtle.encrypt({name: 'AES-GCM', iv: 随机数, additionalData: 附加文}, 句柄.取得(文字(钥号)), 明文)),
        豫言_云工_密码AES解密: async (钥号, 随机数, 附加文, 密文) =>
          new Uint8Array(await 全局.crypto.subtle.decrypt({name: 'AES-GCM', iv: 随机数, additionalData: 附加文}, 句柄.取得(文字(钥号)), 密文)),
        豫言_云工_密码导入HMAC: async (散列, 钥字节) =>
          句柄.登记(await 全局.crypto.subtle.importKey('raw', 钥字节, {name: 'HMAC', hash: 文字(散列)}, false, ['sign', 'verify'])),
        豫言_云工_密码HMAC签: async (钥号, 内容) =>
          new Uint8Array(await 全局.crypto.subtle.sign('HMAC', 句柄.取得(文字(钥号)), 内容)),
        豫言_云工_密码HMAC验: async (钥号, 签文, 内容) =>
          全局.crypto.subtle.verify('HMAC', 句柄.取得(文字(钥号)), 签文, 内容),
        豫言_云工_密码PBKDF2派生字节: async (口令, 盐, 轮数, 散列, 位数) => {
          const 基钥 = await 全局.crypto.subtle.importKey('raw', 口令, 'PBKDF2', false, ['deriveBits']);
          return new Uint8Array(await 全局.crypto.subtle.deriveBits({name: 'PBKDF2', salt: 盐, iterations: Number(轮数), hash: 文字(散列)}, 基钥, Number(位数)));
        },
        豫言_云工_密码HKDF派生字节: async (原钥, 盐, 用途, 散列, 位数) => {
          const 基钥 = await 全局.crypto.subtle.importKey('raw', 原钥, 'HKDF', false, ['deriveBits']);
          return new Uint8Array(await 全局.crypto.subtle.deriveBits({name: 'HKDF', salt: 盐, info: 用途, hash: 文字(散列)}, 基钥, Number(位数)));
        },
        豫言_云工_密码PBKDF2派生AES: async (口令, 盐, 轮数, 散列, 位数) => {
          const 基钥 = await 全局.crypto.subtle.importKey('raw', 口令, 'PBKDF2', false, ['deriveKey']);
          return 句柄.登记(await 全局.crypto.subtle.deriveKey({name: 'PBKDF2', salt: 盐, iterations: Number(轮数), hash: 文字(散列)}, 基钥, {name: 'AES-GCM', length: Number(位数)}, false, ['encrypt', 'decrypt']));
        },
        豫言_云工_密码HKDF派生AES: async (原钥, 盐, 用途, 散列, 位数) => {
          const 基钥 = await 全局.crypto.subtle.importKey('raw', 原钥, 'HKDF', false, ['deriveKey']);
          return 句柄.登记(await 全局.crypto.subtle.deriveKey({name: 'HKDF', salt: 盐, info: 用途, hash: 文字(散列)}, 基钥, {name: 'AES-GCM', length: Number(位数)}, false, ['encrypt', 'decrypt']));
        },
        豫言_云工_打开可读流: 号 => 句柄.登记(句柄.取得(文字(号)).getReader()),
        豫言_云工_读取流块: async 号 => {
          const 结果 = await 句柄.取得(文字(号)).read();
          if (结果.done) return [true, new Uint8Array()];
          return [false, 取流字节(结果.value)];
        },
        豫言_云工_读取文字流块: async 号 => {
          const 结果 = await 句柄.取得(文字(号)).read();
          if (结果.done) return [true, ''];
          if (typeof 结果.value !== 'string') throw Error('可读流块不是文字');
          return [false, 结果.value];
        },
        // 文言：严解有失则归阴与错文，毋使 JSPI 异常越桥。汉语：安全读取文字流，将流错误变成豫言可检查的结果。
        豫言_云工_读取文字流块安全: async 号 => {
          try {
            const 结果 = await 句柄.取得(文字(号)).read();
            if (结果.done) return [true, '{"已终":true}'];
            if (typeof 结果.value !== 'string') throw Error('可读流块不是文字');
            return [true, JSON.stringify({已终: false, 文字: 结果.value})];
          } catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        // 文言：流败归阴与事故文，不令客器之请事俱败。汉语：安全读取将 Promise 拒绝转为显式错误，成功时以句柄保留原始字节。
        豫言_云工_读取流块安全: async 号 => {
          try {
            const 结果 = await 句柄.取得(文字(号)).read();
            if (结果.done) return [true, '{"已终":true}'];
            const 字节号 = 句柄.登记(取流字节(结果.value));
            return [true, JSON.stringify({已终: false, 字节句柄: 字节号})];
          } catch (错) {
            return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)];
          }
        },
        豫言_云工_释放流读取器: 号 => {
          句柄.取得(文字(号)).releaseLock();
          句柄.释放(文字(号));
        },
        豫言_云工_取消流读取器: async (号, 原因) => {
          const 名 = 文字(号);
          await 句柄.取得(名).cancel(文字(原因));
          句柄.释放(名);
        },
        // 文言：写器之待与读器可并行；异步写闭皆归柄，客可后候其果。汉语：写入与读取可并发，豫言取得 Promise 句柄后自行决定等待时机。
        豫言_云工_打开可写流: 号 => 句柄.登记(句柄.取得(文字(号)).getWriter()),
        豫言_云工_可写流已锁: 号 => Boolean(句柄.取得(文字(号)).locked),
        豫言_云工_写器容量文: 号 => JSON.stringify(句柄.取得(文字(号)).desiredSize),
        豫言_云工_写器就绪安全: async 号 => {
          try { await 句柄.取得(文字(号)).ready; return [true, '']; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_写器已闭安全: async 号 => {
          try { await 句柄.取得(文字(号)).closed; return [true, '']; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_写器发字节: (号, 内容) => {
          const 待 = 句柄.取得(文字(号)).write(内容.slice());
          待.catch(() => {});
          return 句柄.登记(待);
        },
        豫言_云工_写器发文字: (号, 内容) => {
          const 待 = 句柄.取得(文字(号)).write(文字(内容));
          待.catch(() => {});
          return 句柄.登记(待);
        },
        豫言_云工_写器发关闭: 号 => {
          const 待 = 句柄.取得(文字(号)).close();
          待.catch(() => {});
          return 句柄.登记(待);
        },
        豫言_云工_写器候操作安全: async 号 => {
          const 名 = 文字(号);
          let 已取 = false;
          try { const 待 = 句柄.取得(名); 已取 = true; await 待; return [true, '']; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
          finally { if (已取) 句柄.释放(名); }
        },
        豫言_云工_写器中断安全: async (号, 原因) => {
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
        豫言_云工_释放流写器: 号 => {
          const 名 = 文字(号);
          句柄.取得(名).releaseLock();
          句柄.释放(名);
        },
        // 文言：压缩流两端俱留宿主，客以柄逐块行之；短字亦可一次往还。汉语：暴露原生压缩流读写端，并提供短字节安全转换。
        豫言_云工_压缩流创建: 格式 => 句柄.登记(new 全局.CompressionStream(文字(格式))),
        豫言_云工_解压流创建: 格式 => 句柄.登记(new 全局.DecompressionStream(文字(格式))),
        豫言_云工_压缩流读端: 号 => 句柄.登记(句柄.取得(文字(号)).readable),
        豫言_云工_压缩流写端: 号 => 句柄.登记(句柄.取得(文字(号)).writable),
        // 文言：文转字与字转文皆守原生流义；豫言执端柄而逐块读写。汉语：TextEncoderStream/TextDecoderStream 保持跨块状态和原生背压。
        豫言_云工_文字编码流新建: () => 句柄.登记(new 全局.TextEncoderStream()),
        豫言_云工_文字解码流新建: (标记, 严格, 略首) =>
          句柄.登记(new 全局.TextDecoderStream(文字(标记), {fatal: Boolean(严格), ignoreBOM: Boolean(略首)})),
        豫言_云工_文字转换流信息文: 号 => {
          const 流 = 句柄.取得(文字(号));
          return JSON.stringify({编码: 流.encoding, 严格: 'fatal' in 流 ? 流.fatal : null, 略首: 'ignoreBOM' in 流 ? 流.ignoreBOM : null});
        },
        豫言_云工_文字转换流读端: 号 => 句柄.登记(句柄.取得(文字(号)).readable),
        豫言_云工_文字转换流写端: 号 => 句柄.登记(句柄.取得(文字(号)).writable),
        豫言_云工_压缩字节安全: async (格式, 内容) => {
          try { return [true, await 转换压缩字节('压缩', 格式, 内容)]; }
          catch (错) { return [false, new TextEncoder().encode(String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错))]; }
        },
        豫言_云工_解压字节安全: async (格式, 内容) => {
          try { return [true, await 转换压缩字节('解压', 格式, 内容)]; }
          catch (错) { return [false, new TextEncoder().encode(String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错))]; }
        },
        // 文言：外联 TCP 惟于事中开之，宿主守平台禁址；客执流柄自决读写。汉语：连接只在事件内创建，地址限制交给 Workers；豫言控制读写流。
        豫言_云工_TCP连接安全: async (主机, 端口, 安全传输, 半开) => {
          try {
            const {connect} = await import('cloudflare:sockets');
            const 埠 = Number(端口);
            const 传输 = 文字(安全传输);
            if (!Number.isSafeInteger(埠) || 埠 < 1 || 埠 > 65535) throw Error('TCP 端口无效');
            if (!['off', 'on', 'starttls'].includes(传输)) throw Error('TCP 安全传输选项无效');
            const 连接 = connect({hostname: 文字(主机), port: 埠}, {secureTransport: 传输, allowHalfOpen: Boolean(半开)});
            return [true, 句柄.登记(连接)];
          } catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_TCP已连安全: async 号 => {
          try {
            const 信息 = await 句柄.取得(文字(号)).opened;
            return [true, JSON.stringify({远端地址: 信息.remoteAddress, 本地地址: 信息.localAddress})];
          } catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_TCP读流: 号 => 句柄.登记(句柄.取得(文字(号)).readable),
        豫言_云工_TCP写流: 号 => 句柄.登记(句柄.取得(文字(号)).writable),
        豫言_云工_TCP取写器: 号 => 句柄.登记(句柄.取得(文字(号)).getWriter()),
        豫言_云工_TCP写字节: async (号, 内容) => { await 句柄.取得(文字(号)).write(内容.slice()); },
        豫言_云工_TCP终写: async 号 => {
          const 名 = 文字(号);
          await 句柄.取得(名).close();
          句柄.取得(名).releaseLock();
          句柄.释放(名);
        },
        豫言_云工_TCP释写器: 号 => {
          const 名 = 文字(号);
          句柄.取得(名).releaseLock();
          句柄.释放(名);
        },
        豫言_云工_TCP升级TLS: 号 => 句柄.登记(句柄.取得(文字(号)).startTls()),
        豫言_云工_TCP关闭: async 号 => { await 句柄.取得(文字(号)).close(); },
        豫言_云工_TCP已关闭安全: async 号 => {
          try { await 句柄.取得(文字(号)).closed; return [true, '']; }
          catch (错) { return [false, String(错?.name ?? 'Error') + ': ' + String(错?.message ?? 错)]; }
        },
        豫言_云工_创建可读流: () => {
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
        豫言_云工_写入流块: async (号, 内容) => {
          const 态 = 可写流.get(文字(号));
          if (!态) throw Error('可写流句柄无效');
          while (!态.已关闭 && 态.控制器.desiredSize <= 0) await new Promise(完成 => { 态.唤醒 = 完成; });
          if (态.已关闭) throw Error('可写流已关闭');
          态.控制器.enqueue(内容.slice());
        },
        豫言_云工_关闭可读流: 号 => {
          const 态 = 可写流.get(文字(号));
          if (!态) throw Error('可写流句柄无效');
          if (!态.已关闭) { 态.已关闭 = true; 态.控制器.close(); }
          可写流.delete(文字(号));
        }
      };
      const {运行} = 创建豫言实例(程序模块, 值桥模块, 能力, {输出});
      const 运行毕 = 运行().then(() => {
        for (const 态 of 事件源.values()) 态.来源.close();
        事件源.clear();
        if (有响应 && !响应) throw Error('豫言程序未设置响应');
        if (需工作流输出 && !已设工作流输出) throw Error('豫言工作流未设置输出');
        if (需流块输出 && !已设流块输出) throw Error('豫言流拉取回调未供块');
        if (需事务输出 && !已设事务输出) throw Error('豫言事务回调未供结果');
        return 需工作流输出 ? 工作流输出 : 需流块输出 ? 流块输出 : 需事务输出 ? 事务输出 : 响应;
      }).catch(错 => {
        for (const 态 of 事件源.values()) 态.来源.close();
        事件源.clear();
        for (const 态 of 可写流.values()) {
          if (!态.已关闭) { 态.已关闭 = true; 态.控制器.error(错); }
          if (态.唤醒) { const 完成 = 态.唤醒; 态.唤醒 = null; 完成(); }
        }
        throw 错;
      }).finally(() => {
        for (const 项 of 定时器.values()) {
          if (项.重复) 全局.clearInterval(项.原号);
          else 全局.clearTimeout(项.原号);
        }
        定时器.clear();
        定时队列.length = 0;
        定时待交.clear();
        if (定时唤醒) { const 完成 = 定时唤醒; 定时唤醒 = null; 完成({种类: 'closed'}); }
      });
      if (上下文?.waitUntil) 上下文.waitUntil(运行毕.catch(() => {}));
      return 有响应 ? Promise.race([响应已备, 运行毕]) : 需工作流输出 || 需流块输出 || 需事务输出 ? 运行毕 : 运行毕.then(() => undefined);
  };
  return {
    fetch(请求, 环境, 上下文) { return 执行('fetch', 请求, 环境, 上下文); },
    serviceFetch(请求, 环境, 上下文) { return 执行('service-fetch', 请求, 环境, 上下文); },
    queue(批次, 环境, 上下文) { return 执行('queue', 批次, 环境, 上下文); },
    scheduled(控制, 环境, 上下文) { return 执行('scheduled', 控制, 环境, 上下文); },
    email(邮件, 环境, 上下文) { return 执行('email', 邮件, 环境, 上下文); },
    durableFetch(请求, 环境, 状态) { return 执行('durable-fetch', 请求, 环境, 状态, 状态); },
    durableAlarm(告警, 环境, 状态) { return 执行('durable-alarm', 告警, 环境, 状态, 状态); },
    durableWebSocketMessage(套接字, 数据, 环境, 状态) { return 执行('durable-websocket-message', {套接字, 数据}, 环境, 状态, 状态); },
    durableWebSocketClose(套接字, 代码, 原因, 正常, 环境, 状态) { return 执行('durable-websocket-close', {套接字, 代码, 原因, 正常}, 环境, 状态, 状态); },
    durableWebSocketError(套接字, 错误, 环境, 状态) { return 执行('durable-websocket-error', {套接字, 错误: String(错误?.message ?? 错误)}, 环境, 状态, 状态); },
    workflow(事件, 步骤, 环境) { return 执行('workflow', 事件, 环境, null, null, 步骤); }
  };
}
