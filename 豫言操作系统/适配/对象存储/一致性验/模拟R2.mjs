// 文言：仿 workerd R2 绑定之类实例，供真 Wasm 之节点试验；汉语：Cloudflare R2 绑定的类实例模拟。
// 形状取自真实 workerd（miniflare）的探测结果：
//   - head/put 返回 HeadResult 类实例，get 返回 GetResult 类实例（非纯对象，故宿主桥发句柄），属性为自有可枚举；
//   - uploaded 是 Date；httpMetadata、customMetadata 是纯对象；checksums 有 toJSON；
//   - list 返回纯对象 {objects, truncated, cursor, delimitedPrefixes}，未截断时 cursor 键存在而值为 undefined；
//   - put 的 onlyIf 只有真正的 Headers 生效，纯对象被静默忽略（这是适配必须传 Headers 的原因）；
//   - put 的 sha256 不符抛 Error，消息与平台原文同形（含错误码 10037）；键超过 1024 字节抛 10020；list 的 limit 超界抛 10022。
import {createHash} from 'node:crypto';

const 编码 = new TextEncoder();
const 字节长 = 文 => 编码.encode(文).length;
const 复制字节 = 视图 => new Uint8Array(视图.buffer, 视图.byteOffset, 视图.byteLength).slice();
const 十六进制 = 字节 => Buffer.from(字节).toString('hex');
const 摘要 = (算法, 字节) => createHash(算法).update(字节).digest('hex');

async function 读全部(值) {
  if (值 === null || 值 === undefined) return new Uint8Array();
  if (typeof 值 === 'string') return 编码.encode(值);
  if (值 instanceof ArrayBuffer) return new Uint8Array(值).slice();
  if (ArrayBuffer.isView(值)) return 复制字节(值);
  if (typeof Blob !== 'undefined' && 值 instanceof Blob) return new Uint8Array(await 值.arrayBuffer());
  if (typeof ReadableStream !== 'undefined' && 值 instanceof ReadableStream) return new Uint8Array(await new Response(值).arrayBuffer());
  throw new TypeError('put: Unsupported value type');
}

class 校验和 {
  constructor(字节) { this.md5 = Uint8Array.from(Buffer.from(摘要('md5', 字节), 'hex')).buffer; this.sha256 = Uint8Array.from(Buffer.from(摘要('sha256', 字节), 'hex')).buffer; }
  toJSON() { return {md5: 十六进制(new Uint8Array(this.md5)), sha256: 十六进制(new Uint8Array(this.sha256))}; }
}

export class 头结果 {
  constructor(记录) {
    this.key = 记录.key;
    this.version = 记录.版本;
    this.size = 记录.字节.length;
    this.etag = 记录.etag;
    this.httpEtag = `"${记录.etag}"`;
    this.checksums = new 校验和(记录.字节);
    this.uploaded = new Date(记录.上传毫秒);
    this.httpMetadata = {...记录.httpMetadata};
    this.customMetadata = {...记录.customMetadata};
    this.range = undefined;
    this.storageClass = 'Standard';
    this.ssecKeyMd5 = undefined;
  }
  writeHttpMetadata(标头) { for (const [名, 值] of Object.entries(this.httpMetadata)) if (typeof 值 === 'string') 标头.set(名, 值); }
}

export class 取结果 extends 头结果 {
  constructor(记录, 桶) {
    super(记录);
    const 字节 = 记录.字节;
    let 偏 = 0;
    this.bodyUsed = false;
    this.body = new ReadableStream({
      pull: 控制 => {
        桶.记录.push({方法: 'body.pull', 键: 记录.key});
        if (偏 >= 字节.length) { 控制.close(); return; }
        const 块 = 字节.subarray(偏, Math.min(偏 + 65536, 字节.length));
        偏 += 块.length;
        控制.enqueue(块.slice());
      },
      cancel: () => { 桶.记录.push({方法: 'body.cancel', 键: 记录.key}); }
    }, {highWaterMark: 0});   // 无人读取则不拉取，便于断言“未读取正文”
    // 每个整读方法都记入桶，供“直通不进 Wasm 内存”之类的断言使用。
    for (const 名 of ['arrayBuffer', 'bytes', 'text', 'json', 'blob']) {
      this[名] = async () => {
        桶.记录.push({方法: 'GetResult.' + 名, 键: 记录.key});
        this.bodyUsed = true;
        if (名 === 'arrayBuffer') return 字节.slice().buffer;
        if (名 === 'bytes') return 字节.slice();
        if (名 === 'blob') return new Blob([字节]);
        const 文 = new TextDecoder().decode(字节);
        return 名 === 'json' ? JSON.parse(文) : 文;
      };
    }
  }
}

const 属性字段 = ['contentType', 'contentLanguage', 'contentDisposition', 'contentEncoding', 'cacheControl'];

export class 模拟R2 {
  #对象 = new Map();
  #故障 = new Map();
  #版本 = 0;
  记录 = [];
  时钟;
  constructor({时钟 = () => Date.now()} = {}) { this.时钟 = 时钟; }

  // 试验用：让某方法的下一次调用抛出给定错误。
  注入故障(方法, 错误) { this.#故障.set(方法, 错误); }
  #进入(方法, 摘要文) {
    this.记录.push({方法, ...摘要文});
    const 错 = this.#故障.get(方法);
    if (错) { this.#故障.delete(方法); throw 错; }
  }
  #验键(方法, 键) {
    if (typeof 键 !== 'string') throw new TypeError(`${方法}: key must be a string`);
    if (字节长(键) > 1024) throw new Error(`${方法}: The specified object name is not valid. (10020)`);
  }
  获取记录(键) { return this.#对象.get(键); }
  当前对象数() { return this.#对象.size; }

  async head(键) {
    this.#进入('head', {键});
    this.#验键('head', 键);
    const 记录 = this.#对象.get(键);
    return 记录 ? new 头结果(记录) : null;
  }
  async get(键, 选项) {
    this.#进入('get', {键, 选项});
    this.#验键('get', 键);
    const 记录 = this.#对象.get(键);
    return 记录 ? new 取结果(记录, this) : null;
  }
  #条件通过(标头, 记录) {
    // 只实现 If-None-Match: * 与 If-Match: "etag"；其余标头忽略。
    const 不匹配 = 标头.get('If-None-Match'), 匹配 = 标头.get('If-Match');
    if (不匹配 !== null) {
      if (不匹配 === '*') { if (记录) return false; }
      else if (记录 && 不匹配.split(',').map(项 => 项.trim()).includes(`"${记录.etag}"`)) return false;
    }
    if (匹配 !== null) {
      if (!记录) return false;
      if (!匹配.split(',').map(项 => 项.trim()).includes(`"${记录.etag}"`)) return false;
    }
    return true;
  }
  async put(键, 值, 选项 = {}) {
    this.#进入('put', {键, 选项键: Object.keys(选项), 有Headers条件: 选项.onlyIf instanceof Headers, 值类型: 值 === null ? 'null' : typeof 值 === 'string' ? 'string' : Object.prototype.toString.call(值)});
    this.#验键('put', 键);
    const 字节 = await 读全部(值);
    const 旧 = this.#对象.get(键);
    // 真实 R2（本地 workerd 探测）：先校验摘要，后判前置条件——摘要不符时即使条件也不满足仍抛错。
    for (const 算法 of ['md5', 'sha1', 'sha256', 'sha384', 'sha512']) {
      if (选项[算法] === undefined) continue;
      const 给 = typeof 选项[算法] === 'string' ? 选项[算法] : 十六进制(new Uint8Array(选项[算法]));
      const 实 = 摘要(算法, 字节);
      if (给 !== 实) {
        const 名 = 算法.toUpperCase().replace(/^(SHA)(\d+)$/, '$1-$2');
        throw new Error(`put: The ${名} checksum you specified did not match what we received.\nYou provided a ${名} checksum with value: ${给}\nActual ${名} was: ${实} (10037)`);
      }
    }
    // onlyIf 只认 Headers 与 R2Conditional 的已知字段；其他形状（如纯对象）被静默忽略。
    if (选项.onlyIf instanceof Headers && !this.#条件通过(选项.onlyIf, 旧)) return null;
    const httpMetadata = {};
    const 源 = 选项.httpMetadata instanceof Headers ? Object.fromEntries(选项.httpMetadata) : (选项.httpMetadata ?? {});
    for (const 名 of 属性字段) if (typeof 源[名] === 'string') httpMetadata[名] = 源[名];
    if (源.cacheExpiry instanceof Date) httpMetadata.cacheExpiry = 源.cacheExpiry;
    const 自定 = 选项.customMetadata ?? {};
    for (const [名, 值2] of Object.entries(自定)) if (typeof 值2 !== 'string') throw new TypeError('put: Type error with argument 3 (customMetadata values must be strings)');
    const 记录 = {key: 键, 字节, etag: 摘要('md5', 字节), 版本: String(++this.#版本), 上传毫秒: this.时钟(), httpMetadata, customMetadata: {...自定}};
    this.#对象.set(键, 记录);
    return new 头结果(记录);
  }
  async delete(键们) {
    this.#进入('delete', {键们: Array.isArray(键们) ? 键们.length : 1});
    const 诸 = Array.isArray(键们) ? 键们 : [键们];
    if (诸.length > 1000) throw new Error('delete: The maximum number of keys allowed for delete is 1000 (10008)');
    for (const 键 of 诸) { this.#验键('delete', 键); this.#对象.delete(键); }
  }
  async list(选项 = {}) {
    this.#进入('list', {选项});
    const 限 = 选项.limit ?? 1000;
    if (!Number.isInteger(限) || 限 < 1 || 限 > 1000) throw new Error('list: MaxKeys params must be positive integer <= 1000. (10022)');
    const 前缀 = 选项.prefix ?? '';
    let 诸键 = [...this.#对象.keys()].filter(键 => 键.startsWith(前缀)).sort((甲, 乙) => Buffer.compare(Buffer.from(甲), Buffer.from(乙)));
    if (选项.cursor !== undefined) {
      const 起 = Buffer.from(选项.cursor, 'base64').toString('utf8');
      诸键 = 诸键.filter(键 => Buffer.compare(Buffer.from(键), Buffer.from(起)) > 0);
    }
    const 含元 = new Set(选项.include ?? []);
    const 物们 = [], 前缀们 = [];
    let 已满 = false, 末键 = null;
    for (const 键 of 诸键) {
      let 项 = 键;
      if (选项.delimiter) {
        const 位 = 键.indexOf(选项.delimiter, 前缀.length);
        if (位 >= 0) {
          const 前 = 键.slice(0, 位 + 选项.delimiter.length);
          if (前缀们.includes(前)) { 末键 = 键; continue; }
          if (物们.length + 前缀们.length >= 限) { 已满 = true; break; }
          前缀们.push(前); 末键 = 键; continue;
        }
      }
      if (物们.length + 前缀们.length >= 限) { 已满 = true; break; }
      const 记录 = this.#对象.get(项);
      const 结果 = new 头结果(记录);
      if (!含元.has('customMetadata')) 结果.customMetadata = {};
      if (!含元.has('httpMetadata')) 结果.httpMetadata = {};
      物们.push(结果); 末键 = 键;
    }
    const 果 = {objects: 物们, truncated: 已满, cursor: 已满 ? Buffer.from(末键).toString('base64') : undefined, delimitedPrefixes: 前缀们};
    return 果;
  }
}
