// 文言：构建之文件藏于主线程之内存：文以径为键，目录记其子名；诸客经问答用之。
// 汉语：构建宿主的内存文件系统，放在主线程：文件按规范绝对路径存（内容与修改时间，时间以秒计），目录另记子项名；
//   各客体工作线程经共享内存问答使用它（见 进程.mjs）。只有普通文件与目录，没有符号链接与权限位。
const 编码器 = new TextEncoder(), 解码器 = new TextDecoder();
export const 字节 = 值 => typeof 值 === 'string' ? 编码器.encode(值) : 值 instanceof Uint8Array ? 值 : new Uint8Array(值);
export const 文 = 值 => 值 instanceof Uint8Array ? 解码器.decode(值) : String(值);

// 文言：径从基而解，去点与双点。汉语：相对路径以基目录为起点解析，去掉“.”与“..”段，得规范绝对路径。
export function 规范路径(径, 基 = '/') {
  径 = 文(径);
  const 段们 = [];
  for (const 段 of (径.startsWith('/') ? 径 : 文(基) + '/' + 径).split('/')) {
    if (!段 || 段 === '.') continue;
    if (段 === '..') 段们.pop(); else 段们.push(段);
  }
  return '/' + 段们.join('/');
}
const 父径 = 径 => { const 位 = 径.lastIndexOf('/'); return 位 <= 0 ? '/' : 径.slice(0, 位); };
const 末名 = 径 => 径.slice(径.lastIndexOf('/') + 1);
const 现秒 = () => Math.floor(Date.now() / 1000);

export class 文件不存在 extends Error {
  constructor(径) { super('文件或目录不存在：' + 径); this.code = 'ENOENT'; }
}

export class 构建文件系统 {
  constructor({上限字节 = 4 * 1024 * 1024 * 1024} = {}) {
    this.文件 = new Map();
    this.目录 = new Map([['/', new Set()]]);
    this.总字节 = 0;
    this.上限字节 = 上限字节;
  }
  // 文言：建目录及其所缺之上级。汉语：建目录，连同缺少的上级目录；路径上已有同名文件时报错。
  建目录(径) {
    径 = 规范路径(径);
    if (this.目录.has(径)) return;
    if (this.文件.has(径)) throw Error('已有同名文件，不能建目录：' + 径);
    this.建目录(父径(径));
    this.目录.set(径, new Set());
    this.目录.get(父径(径)).add(末名(径));
  }
  写(径, 内容, 时间 = 现秒()) {
    径 = 规范路径(径);
    if (this.目录.has(径)) throw Error('是目录，不能写入：' + 径);
    内容 = 字节(内容);
    const 旧长 = this.文件.get(径)?.内容.length ?? 0;
    if (this.总字节 - 旧长 + 内容.length > this.上限字节) throw Error('构建文件系统超过上限');
    this.建目录(父径(径));
    this.文件.set(径, {内容, 时间});
    this.目录.get(父径(径)).add(末名(径));
    this.总字节 += 内容.length - 旧长;
  }
  读(径) {
    const 项 = this.文件.get(规范路径(径));
    if (!项) throw new 文件不存在(规范路径(径));
    return 项.内容;
  }
  删文件(径) {
    径 = 规范路径(径);
    const 项 = this.文件.get(径);
    if (!项) throw new 文件不存在(径);
    this.文件.delete(径);
    this.目录.get(父径(径))?.delete(末名(径));
    this.总字节 -= 项.内容.length;
  }
  // 文言：删径，目录则并其所含。汉语：删除路径；是目录时连同其中全部内容（rm -rf 用），不存在时不报错。
  删全部(径) {
    径 = 规范路径(径);
    if (this.文件.has(径)) return this.删文件(径);
    if (!this.目录.has(径) || 径 === '/') return;
    for (const 名 of [...this.目录.get(径)]) this.删全部(径 + '/' + 名);
    this.目录.delete(径);
    this.目录.get(父径(径))?.delete(末名(径));
  }
  列(径) {
    径 = 规范路径(径);
    const 子 = this.目录.get(径);
    if (!子) throw new 文件不存在(径);
    return [...子];
  }
  是文件(径) { return this.文件.has(规范路径(径)); }
  是目录(径) { return this.目录.has(规范路径(径)); }
  存在(径) { 径 = 规范路径(径); return this.文件.has(径) || this.目录.has(径); }
  时间(径) {
    径 = 规范路径(径);
    if (this.文件.has(径)) return this.文件.get(径).时间;
    if (this.目录.has(径)) return 1;
    throw new 文件不存在(径);
  }
  // 文言：复制文或目录。汉语：复制文件或整个目录（cp -r 的语义；保留修改时间）。
  复制(源, 的) {
    源 = 规范路径(源); 的 = 规范路径(的);
    if (this.文件.has(源)) {
      const 目标 = this.目录.has(的) ? 的 + '/' + 末名(源) : 的;
      const 项 = this.文件.get(源);
      return this.写(目标, 项.内容.slice(), 项.时间);
    }
    if (!this.目录.has(源)) throw new 文件不存在(源);
    const 目标 = this.目录.has(的) ? 的 + '/' + 末名(源) : 的;
    this.建目录(目标);
    for (const 名 of this.列(源)) this.复制(源 + '/' + 名, 目标 + '/' + 名);
  }
  移动(源, 的) {
    this.复制(源, 的);
    this.删全部(源);
  }
}

// 文言：解 tar 之归档入文件系统：识 ustar、pax 之长名与 GNU 长名；符号链接与特殊文件略之。
// 汉语：把 tar 归档（字节）展开进文件系统的某个目录：认 ustar 头、pax 扩展头（path）与 GNU 长名（L）；
//   剥离层数为 n 时去掉每个路径开头的 n 段（GitHub 的 tarball 外面包一层“所有者-仓库-提交”目录）；符号链接、设备等特殊项略过。
//   返回 {文件数, 全局注释}；全局注释是 pax 全局头里的 comment（GitHub 与 git archive 写的是提交号）。
export function 展开归档(文件系统, 归档, {目录 = '/', 剥离层数 = 0, 时间} = {}) {
  let 位 = 0, 长名 = null, 扩展 = {}, 文件数 = 0, 全局注释 = '';
  // 文言：八进制之栏，前或有空，后止于空或零。汉语：八进制栏：开头可有空格或零字节，遇到非数字即止。
  const 读八进制 = (起, 长) => {
    let 值 = 0, 序 = 起;
    while (序 < 起 + 长 && (归档[序] === 32 || 归档[序] === 0)) 序++;
    for (; 序 < 起 + 长; 序++) { const 码 = 归档[序]; if (码 < 48 || 码 > 55) break; 值 = 值 * 8 + (码 - 48); }
    return 值;
  };
  const 读串 = (起, 长) => { let 终 = 起; while (终 < 起 + 长 && 归档[终] !== 0) 终++; return 解码器.decode(归档.subarray(起, 终)); };
  const 解扩展 = 数据 => {
    const 果 = {};
    for (let 序 = 0; 序 < 数据.length;) {
      let 空格 = 序; while (空格 < 数据.length && 数据[空格] !== 32) 空格++;
      const 长 = Number(解码器.decode(数据.subarray(序, 空格)));
      if (!长) break;
      const 记录 = 解码器.decode(数据.subarray(空格 + 1, 序 + 长 - 1)), 等 = 记录.indexOf('=');
      果[记录.slice(0, 等)] = 记录.slice(等 + 1);
      序 += 长;
    }
    return 果;
  };
  while (位 + 512 <= 归档.length) {
    const 头位 = 位;
    if (归档.subarray(头位, 头位 + 512).every(码 => 码 === 0)) break;
    const 大小 = 读八进制(头位 + 124, 12), 种 = String.fromCharCode(归档[头位 + 156] || 48);
    const 数据 = 归档.subarray(头位 + 512, 头位 + 512 + 大小);
    位 = 头位 + 512 + Math.ceil(大小 / 512) * 512;
    if (种 === 'x') { 扩展 = 解扩展(数据); continue; }
    if (种 === 'g') { 全局注释 = 解扩展(数据).comment ?? 全局注释; continue; }
    if (种 === 'L') { 长名 = 读串(头位 + 512, 大小); continue; }
    const 是标准 = 读串(头位 + 257, 5) === 'ustar', 前缀 = 是标准 ? 读串(头位 + 345, 155) : '', 本名 = 读串(头位, 100);
    const 名 = 扩展.path ?? 长名 ?? (前缀 ? 前缀 + '/' + 本名 : 本名);
    const 修改时间 = 时间 ?? (Math.floor(Number(扩展.mtime)) || 读八进制(头位 + 136, 12) || 1);
    扩展 = {}; 长名 = null;
    const 段们 = 名.split('/').filter(Boolean).slice(剥离层数);
    if (!段们.length || 段们.some(段 => 段 === '..')) continue;
    const 目标 = 规范路径(段们.join('/'), 目录);
    if (种 === '5') { 文件系统.建目录(目标); continue; }
    if (种 !== '0' && 种 !== '7') continue;
    文件系统.写(目标, 数据.slice(), 修改时间);
    文件数++;
  }
  return {文件数, 全局注释};
}

// 文言：取网址之 gzip 之 tar 而展之；可传缓存之名，同址再取则用浏览器之存。
// 汉语：取来 gzip 压缩的 tar 并解压成字节：用浏览器的 DecompressionStream；给了缓存名时先查 Cache Storage（网址里带提交号等不变标识时才缓存），没有再取并存入。
export async function 取压缩归档(网址, {缓存名 = '', 信号, 报传输 = () => {}} = {}) {
  let 回答 = null;
  const 起 = performance.now();
  let 下载字节 = 0;
  const 缓存 = 缓存名 && typeof caches !== 'undefined' ? await caches.open(缓存名).catch(() => null) : null;
  if (缓存) 回答 = await 缓存.match(网址).catch(() => null);
  const 来自缓存 = Boolean(回答);
  const 报 = 连接状态 => 报传输({下载字节: 来自缓存 ? 0 : 下载字节, 经过毫秒: Math.max(1, Math.round(performance.now() - 起)), 连接状态, 来自缓存});
  报('连接中');
  if (!回答) {
    回答 = await fetch(网址, {credentials: 'same-origin', signal: 信号});
    if (!回答.ok) throw Error('取归档失败：' + 回答.status + ' ' + 网址);
    // 汉语：缓存与正文读取并行，下载进度随网络流到达报告。文言：缓存与正文并读，进度随流至而报之。
    if (缓存) 缓存.put(网址, 回答.clone()).catch(() => {});
  }
  const 计量 = new TransformStream({
    transform(块, 管) { 下载字节 += 块.byteLength; 报(来自缓存 ? '读取缓存' : '下载中'); 管.enqueue(块); },
    flush() { 报('下载完成'); }
  });
  const 流 = 回答.body.pipeThrough(计量).pipeThrough(new DecompressionStream('gzip'));
  return new Uint8Array(await new Response(流).arrayBuffer());
}
