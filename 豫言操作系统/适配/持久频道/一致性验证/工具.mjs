// 文言：网页事件流、持久频道一致性验证之共用器（Node，实 Wasm）。汉语：网页事件流与持久频道一致性验证共用的测试工具（Node，真实 Wasm）。
// 用法：环境变量 产物目录 指向应用的 dist/<输出名>（先用私有暂存构建该应用），如 env 产物目录=<暂存>/dist/事件流一致性 node --test 事件流一致性.test.mjs。
import {readFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {scheduler as 节点调度} from 'node:timers/promises';
import {DatabaseSync} from 'node:sqlite';

const 产物根 = (() => {
  const 目录 = process.env.产物目录;
  if (!目录) throw new Error('请设置环境变量 产物目录 为应用的 dist/<输出名> 目录（先用私有暂存构建该应用）');
  return pathToFileURL(resolve(目录) + '/');
})();

// Workers 全局有 scheduler.wait，Node 没有：用 timers/promises 垫片；一小时以上的计时不占住事件循环。
export function 造全局() {
  const 调度 = {wait: (毫秒, 选项) => 节点调度.wait(Number(毫秒), {...选项, ref: Number(毫秒) < 3600000})};
  return new Proxy(globalThis, {
    get: (目标, 键) => 键 === 'scheduler' ? 调度 : Reflect.get(目标, 键),
    has: (目标, 键) => 键 === 'scheduler' || Reflect.has(目标, 键)
  });
}

let 已载入 = null;
export async function 载入应用() {
  if (!已载入) {
    const 模块 = await import(new URL('宿主.mjs', 产物根));
    const 程序模块 = await WebAssembly.compile(await readFile(new URL('程序.wasm', 产物根)));
    const 值桥模块 = await WebAssembly.compile(await readFile(new URL('值桥.wasm', 产物根)));
    已载入 = {创建云工宿主: 模块.创建云工宿主, 程序模块, 值桥模块};
  }
  const {创建云工宿主, 程序模块, 值桥模块} = 已载入;
  // 每次得到新的宿主对象：等价于一个新的持久对象实例（频道注册表随宿主对象存续）。
  return (许可, 额外 = {}) => 创建云工宿主({程序模块, 值桥模块, 许可, 全局: 造全局(), ...额外});
}

// node:sqlite 模拟 D1：prepare() 返回类实例，run()/all()/first()/batch() 与 D1 公开面一致。
export function 创建模拟D1(建表) {
  const sql = new DatabaseSync(':memory:');
  if (建表) sql.exec(建表);
  const 转参 = 值 => 值 === undefined ? null : typeof 值 === 'boolean' ? (值 ? 1 : 0) : 值;
  class 语句 {
    constructor(文) { this.文 = 文; this.值 = []; }
    bind(...值) { this.值 = 值.map(转参); return this; }
    async first() { return sql.prepare(this.文).get(...this.值) ?? null; }
    async all() { const 行 = sql.prepare(this.文).all(...this.值); return {results: 行.map(项 => ({...项})), success: true, meta: {changes: 0, last_row_id: 0}}; }
    async run() { const 结 = sql.prepare(this.文).run(...this.值); return {results: [], success: true, meta: {changes: Number(结.changes), last_row_id: Number(结.lastInsertRowid)}}; }
    同步执行() {
      const 读 = /^\s*(select|with|pragma)/i.test(this.文) || /\breturning\b/i.test(this.文);
      if (读) { const 行 = sql.prepare(this.文).all(...this.值); return {results: 行.map(项 => ({...项})), success: true, meta: {changes: 0, last_row_id: 0}}; }
      const 结 = sql.prepare(this.文).run(...this.值);
      return {results: [], success: true, meta: {changes: Number(结.changes), last_row_id: Number(结.lastInsertRowid)}};
    }
  }
  const DB = {
    prepare(文) { return new 语句(文); },
    async batch(语句们) {
      sql.exec('BEGIN');
      try { const 果 = 语句们.map(项 => 项.同步执行()); sql.exec('COMMIT'); return 果; }
      catch (错) { sql.exec('ROLLBACK'); throw 错; }
    }
  };
  return {sql, DB};
}

export const 日志表 = 'CREATE TABLE 日志(序 INTEGER PRIMARY KEY AUTOINCREMENT, 键 TEXT, 值 TEXT);';
export const 读日志 = (sql, 键) => sql.prepare('SELECT 值 FROM 日志 WHERE 键 = ? ORDER BY 序').all(键).map(行 => 行.值);

export const 睡 = 毫秒 => new Promise(完成 => setTimeout(完成, 毫秒));
export async function 等到(条件, {超时 = 5000, 间隔 = 10, 说明 = '条件'} = {}) {
  const 起 = Date.now();
  for (;;) {
    const 果 = await 条件();
    if (果) return 果;
    if (Date.now() - 起 > 超时) throw new Error('等待超时：' + 说明);
    await 睡(间隔);
  }
}

// 事件流响应的读取器：按块、按帧读取，带超时。
export class 流读取器 {
  constructor(响应) {
    this.读 = 响应.body.getReader();
    this.解 = new TextDecoder();
    this.缓 = '';
    this.已终 = false;
    this.块数 = 0;
    this.字节数 = 0;
  }
  async 下一块(超时毫秒 = 5000) {
    let 计时;
    const 超 = new Promise(完成 => { 计时 = setTimeout(() => 完成({超时: true}), 超时毫秒); });
    try {
      const 果 = await Promise.race([this.读.read().then(结 => ({结}), 错 => ({错})), 超]);
      if (果.超时) return {类型: '超时'};
      if (果.错 !== undefined) { this.已终 = true; return {类型: '错', 错: 果.错}; }
      if (果.结.done) { this.已终 = true; return {类型: '终'}; }
      this.块数++;
      this.字节数 += 果.结.value.byteLength;
      return {类型: '块', 字节: 果.结.value};
    } finally { clearTimeout(计时); }
  }
  // 读到下一帧（以空行结尾）的文本，不含结尾空行；流终则返回 null，超时或出错抛错。
  async 下一帧(超时毫秒 = 5000) {
    for (;;) {
      const 位 = this.缓.indexOf('\n\n');
      if (位 >= 0) { const 帧 = this.缓.slice(0, 位); this.缓 = this.缓.slice(位 + 2); return 帧; }
      if (this.已终) return null;
      const 块 = await this.下一块(超时毫秒);
      if (块.类型 === '超时') throw new Error('读取事件帧超时');
      if (块.类型 === '错') throw 块.错;
      if (块.类型 === '块') this.缓 += this.解.decode(块.字节, {stream: true});
    }
  }
  async 读完(超时毫秒 = 20000) {
    const 帧们 = [];
    for (;;) { const 帧 = await this.下一帧(超时毫秒); if (帧 === null) return 帧们; 帧们.push(帧); }
  }
  // 读到流终，返回全部文本与块数（不按帧切分）。
  async 全文(超时毫秒 = 20000) {
    let 文 = this.缓; this.缓 = '';
    for (;;) {
      if (this.已终) return 文;
      const 块 = await this.下一块(超时毫秒);
      if (块.类型 === '超时') throw new Error('读取事件流超时');
      if (块.类型 === '错') throw 块.错;
      if (块.类型 === '块') 文 += this.解.decode(块.字节, {stream: true});
    }
  }
  async 取消(原因) { await this.读.cancel(原因); this.已终 = true; }
}

export const 请求 = (路径, 选项 = {}) => new Request('https://x.test' + 路径, 选项);
