// 文言：此器仿持久对象之仓、输入闸与保活，供验云工宿主壳；其与真器之异，注于各法。
// 汉语：可复用的 Durable Object 模拟：ctx.storage（get/put/delete/list/deleteAll/alarm/transaction）、输入闸、
// blockConcurrencyWhile、waitUntil、id，以及把 fetch/alarm 事件经输入闸交给云工宿主的对象包装。
// 用法见同目录说明；差异（以本地 workerd 4.129.0 实测为准，云端待验）逐项写在各方法注释里。

const 编码 = new TextEncoder();
// 文言：键序依 UTF-8 字节，与平台同。汉语：键按 UTF-8 字节序排序（JavaScript 默认按 UTF-16 码元序，二者对增补平面字符不同）。
export const 比较键 = (甲, 乙) => Buffer.compare(编码.encode(甲), 编码.encode(乙));
const 字节数 = 文 => 编码.encode(文).length;

// 文言：键值皆循结构化克隆而存取，取出者与所存者不相牵连。汉语：所有存取都做 structuredClone，调用方修改返回值不影响仓内数据。
const 克隆 = 值 => structuredClone(值);

// 文言：闸闭则诸事候之；可叠闭，尽开乃行。汉语：输入闸：blockConcurrencyWhile 期间不投递新事件；可重入计数。
export class 输入闸 {
  #闭数 = 0;
  #候 = [];
  get 已闭() { return this.#闭数 > 0; }
  async 等待开门() {
    while (this.#闭数 > 0) await new Promise(完成 => this.#候.push(完成));
  }
  关() { this.#闭数++; }
  开() {
    if (--this.#闭数 === 0) for (const 完成 of this.#候.splice(0)) 完成();
  }
}

export class 模拟持久仓 {
  #数据 = new Map();
  #告警 = null;
  #事务尾 = Promise.resolve();
  #清告警 = true;
  #闸 = null;
  #活动 = null;
  // 文言：初始可为对象，键值直入；`删仓清告警` 仿 SQLite 后端新兼容日期之行为；闸者，事务期间闭之。
  // 汉语：初始数据用普通对象给出；`删仓清告警`（默认真）表示 deleteAll 同时清除告警，与 SQLite 存储在新兼容日期下一致；
  // `闸` 为可选的输入闸：事务回调运行期间关闭它（本地 workerd 实测事务回调期间其他事件排队）。
  constructor(初始 = {}, {删仓清告警 = true, 闸 = null} = {}) {
    for (const [键, 值] of Object.entries(初始)) this.#数据.set(键, 克隆(值));
    this.#清告警 = 删仓清告警;
    this.#闸 = 闸;
  }
  // 文言：测试直窥已提交之物，勿经事务。汉语：供测试断言的已提交数据 Map（值已是结构化值）。
  get 数据() { return this.#数据; }
  get #当前() { return this.#活动 ? this.#活动.数据 : this.#数据; }
  static 校验键(键) {
    if (typeof 键 !== 'string') throw new TypeError('持久仓键必须是字符串');
    if (字节数(键) > 2048) throw new RangeError('持久仓键超过 2048 字节');
  }
  static 有序键(表, 选项 = {}) {
    if (选项 === null || typeof 选项 !== 'object') throw new TypeError('list 选项必须是对象');
    const {start, startAfter, end, prefix, reverse, limit} = 选项;
    if (start !== undefined && startAfter !== undefined) throw new TypeError('list 不能同时指定 start 与 startAfter');
    if (limit !== undefined && (!Number.isSafeInteger(limit) || limit < 1)) throw new RangeError('list 的 limit 必须是正整数');
    let 诸键 = [...表.keys()].sort(比较键);
    if (prefix !== undefined) 诸键 = 诸键.filter(键 => 键.startsWith(prefix));
    if (start !== undefined) 诸键 = 诸键.filter(键 => 比较键(键, start) >= 0);
    if (startAfter !== undefined) 诸键 = 诸键.filter(键 => 比较键(键, startAfter) > 0);
    if (end !== undefined) 诸键 = 诸键.filter(键 => 比较键(键, end) < 0);
    if (reverse) 诸键.reverse();
    return limit === undefined ? 诸键 : 诸键.slice(0, limit);
  }
  static async 读(表, 键或键组) {
    if (Array.isArray(键或键组)) {
      if (键或键组.length > 128) throw new RangeError('get 至多 128 个键');
      键或键组.forEach(模拟持久仓.校验键);
      const 果 = new Map();
      for (const 键 of [...new Set(键或键组)].sort(比较键)) if (表.has(键)) 果.set(键, 克隆(表.get(键)));
      return 果;
    }
    模拟持久仓.校验键(键或键组);
    return 表.has(键或键组) ? 克隆(表.get(键或键组)) : undefined;
  }
  static async 写(表, 键或对象, 值) {
    if (typeof 键或对象 === 'string') {
      模拟持久仓.校验键(键或对象);
      表.set(键或对象, 克隆(值));
      return;
    }
    if (!键或对象 || typeof 键或对象 !== 'object') throw new TypeError('put 需要键与值，或键值对象');
    const 诸键 = Object.keys(键或对象);
    if (诸键.length > 128) throw new RangeError('put 至多 128 个键');
    诸键.forEach(模拟持久仓.校验键);
    for (const 键 of 诸键) 表.set(键, 克隆(键或对象[键]));
  }
  static async 删(表, 键或键组) {
    if (Array.isArray(键或键组)) {
      if (键或键组.length > 128) throw new RangeError('delete 至多 128 个键');
      键或键组.forEach(模拟持久仓.校验键);
      let 数 = 0;
      for (const 键 of new Set(键或键组)) if (表.delete(键)) 数++;
      return 数;
    }
    模拟持久仓.校验键(键或键组);
    return 表.delete(键或键组);
  }
  static async 列(表, 选项) {
    const 果 = new Map();
    for (const 键 of 模拟持久仓.有序键(表, 选项)) 果.set(键, 克隆(表.get(键)));
    return 果;
  }
  async get(键或键组) { return 模拟持久仓.读(this.#当前, 键或键组); }
  async put(键或对象, 值) { return 模拟持久仓.写(this.#当前, 键或对象, 值); }
  async delete(键或键组) { return 模拟持久仓.删(this.#当前, 键或键组); }
  async list(选项 = {}) { return 模拟持久仓.列(this.#当前, 选项); }
  async deleteAll() {
    this.#当前.clear();
    if (this.#清告警) this.#写告警(null);
  }
  async sync() {}
  #写告警(值) { if (this.#活动) this.#活动.告警 = 值; else this.#告警 = 值; }
  // 文言：告警仅一，取毫秒整数或空。汉语：单个对象至多一个告警，getAlarm 返回绝对毫秒或 null；setAlarm 接受数字或 Date。
  async getAlarm() { return this.#活动 ? this.#活动.告警 : this.#告警; }
  async setAlarm(时) {
    const 毫秒 = 时 instanceof Date ? 时.getTime() : 时;
    if (typeof 毫秒 !== 'number' || !Number.isFinite(毫秒)) throw new TypeError('setAlarm 需要有限数字或 Date');
    this.#写告警(Math.trunc(毫秒));
  }
  async deleteAlarm() { this.#写告警(null); }
  // 文言：事务串行，且事务期间他事皆候；回调内之读写（含经 storage 直行者与告警）皆入草稿，败或回滚则尽弃之。
  // 汉语：异步 KV 事务（本地 workerd 4.129.0 实测）：多个事务串行执行；事务回调运行期间输入闸关闭，其他事件排队；回调内经 txn 或直接经 storage
  // 发出的写入与告警调用都属于本事务，回调抛错或 rollback() 则全部丢弃；rollback 之后再用事务对象会抛出
  // “Cannot put() on rolled back transaction” 之类错误。
  async transaction(回调) {
    const 前 = this.#事务尾;
    let 放行;
    this.#事务尾 = new Promise(完成 => { 放行 = 完成; });
    await 前;
    this.#闸?.关();
    const 草稿 = {数据: new Map([...this.#数据].map(([键, 值]) => [键, 克隆(值)])), 告警: this.#告警};
    this.#活动 = 草稿;
    let 已回滚 = false;
    let 已结束 = false;
    const 校活 = 法 => {
      if (已结束) throw new Error(`Cannot ${法}() on a finished transaction`);
      if (已回滚) throw new Error(`Cannot ${法}() on rolled back transaction`);
    };
    const 事务 = {
      get: async 键 => { 校活('get'); return 模拟持久仓.读(草稿.数据, 键); },
      put: async (键, 值) => { 校活('put'); return 模拟持久仓.写(草稿.数据, 键, 值); },
      delete: async 键 => { 校活('delete'); return 模拟持久仓.删(草稿.数据, 键); },
      list: async (选项 = {}) => { 校活('list'); return 模拟持久仓.列(草稿.数据, 选项); },
      getAlarm: async () => { 校活('getAlarm'); return 草稿.告警; },
      setAlarm: async 时 => { 校活('setAlarm'); 草稿.告警 = Math.trunc(时 instanceof Date ? 时.getTime() : 时); },
      deleteAlarm: async () => { 校活('deleteAlarm'); 草稿.告警 = null; },
      rollback: () => { 校活('rollback'); 已回滚 = true; }
    };
    try {
      const 果 = await 回调(事务);
      if (!已回滚) {
        this.#数据 = 草稿.数据;
        this.#告警 = 草稿.告警;
      }
      return 果;
    } finally {
      已结束 = true;
      this.#活动 = null;
      this.#闸?.开();
      放行();
    }
  }
}

// 文言：状态者，持久对象之 ctx；闸、保活、标识皆在此。汉语：模拟 DurableObjectState：storage、id、waitUntil、blockConcurrencyWhile、abort。
export class 模拟持久状态 {
  constructor({标识 = '模拟持久对象标识', 名称 = null, 初始 = {}, 删仓清告警 = true, 块时限毫秒 = 30000} = {}) {
    this.闸 = new 输入闸();
    this.storage = new 模拟持久仓(初始, {删仓清告警, 闸: this.闸});
    this.id = {toString: () => 标识, name: 名称 ?? undefined, jurisdiction: undefined, equals: 他 => String(他) === 标识};
    this.保活 = [];
    this.重置次数 = 0;
    this.已中止 = null;
    this.块时限毫秒 = 块时限毫秒;
    this.#块尾 = Promise.resolve();
  }
  #块尾;
  // 文言：waitUntil 于对象内，平台文档谓其不延寿；此器只记之，供测试自取。汉语：记录传入的承诺，不改变对象寿命；测试用 等待保活() 等它们结束。
  waitUntil(承诺) { this.保活.push(Promise.resolve(承诺)); }
  async 等待保活() { await Promise.allSettled(this.保活); }
  // 文言：闭闸而候前区，区内可候外物，毕则开；区之败则对象重置。汉语：blockConcurrencyWhile：调用时立即关闭输入闸，多个调用依次执行；
  // 回调抛错，或超过 块时限毫秒（真器为 30 秒），记为对象重置（重置次数加一）并把错误抛给调用者。
  async blockConcurrencyWhile(回调) {
    const 前 = this.#块尾;
    let 放行;
    this.#块尾 = new Promise(完成 => { 放行 = 完成; });
    this.闸.关();
    try {
      await 前;
      let 计时;
      const 超时 = new Promise((_, 拒) => { 计时 = setTimeout(() => 拒(new Error('blockConcurrencyWhile 超过时限')), this.块时限毫秒); });
      try { return await Promise.race([Promise.resolve().then(回调), 超时]); }
      catch (错) { this.重置次数++; throw 错; }
      finally { clearTimeout(计时); }
    } finally {
      this.闸.开();
      放行();
    }
  }
  abort(原因) { this.已中止 = 原因 ?? true; this.重置次数++; }
}

// 文言：对象者，类实例；事至而闸不闭方投于宿主。汉语：把 fetch/alarm 事件交给云工宿主的包装，事件先过输入闸。
export class 模拟持久对象 {
  constructor({宿主, 环境 = {}, 状态 = new 模拟持久状态()} = {}) {
    if (!宿主) throw new TypeError('需要云工宿主');
    this.宿主 = 宿主;
    this.环境 = 环境;
    this.状态 = 状态;
    this.事件日志 = [];
  }
  get storage() { return this.状态.storage; }
  async #投递(种类, 运行) {
    const 记 = {种类, 到达: performance.now(), 投递: 0, 结束: 0, 失败: null};
    this.事件日志.push(记);
    await this.状态.闸.等待开门();
    记.投递 = performance.now();
    try { return await 运行(); }
    catch (错) { 记.失败 = 错; throw 错; }
    finally { 记.结束 = performance.now(); }
  }
  fetch(请求) { return this.#投递('fetch', () => this.宿主.durableFetch(请求, this.环境, this.状态)); }
  alarm(信息) { return this.#投递('alarm', () => this.宿主.durableAlarm(信息, this.环境, this.状态)); }
  webSocketMessage(套接字, 消息) { return this.#投递('websocket-message', () => this.宿主.durableWebSocketMessage(套接字, 消息, this.环境, this.状态)); }
  // 文言：平台触发告警：先清告警再入处理；败则告警复设以待重试。汉语：模拟平台送出告警——处理开始时告警视为已清除（处理中 getAlarm 为 null，
  // 除非处理入口自己重设）；处理抛错时平台把告警排到退避之后（此处设为当前时刻起 2 秒 × 2^重试数），并把错误抛给调用者以便断言。
  async 触发告警({重试数 = 0} = {}) {
    const 时 = await this.storage.getAlarm();
    if (时 === null) throw new Error('当前没有告警可触发');
    await this.storage.deleteAlarm();
    try { return await this.alarm({retryCount: 重试数, isRetry: 重试数 > 0}); }
    catch (错) {
      if ((await this.storage.getAlarm()) === null) await this.storage.setAlarm(Date.now() + 2000 * 2 ** 重试数);
      throw 错;
    }
  }
}

export function 创建模拟持久对象({宿主, 环境 = {}, 初始 = {}, 标识, 名称, 块时限毫秒} = {}) {
  return new 模拟持久对象({宿主, 环境, 状态: new 模拟持久状态({标识, 名称, 初始, 块时限毫秒})});
}
