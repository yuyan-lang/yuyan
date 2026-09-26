// 真实 workerd（wrangler dev --local）复验的 HTTP 驱动工具。
// 环境变量 远端地址：本地 workerd（wrangler dev --local）的地址，如 http://localhost:18811。
export const 基址 = process.env.远端地址;
if (!基址) throw new Error('请设置环境变量 远端地址 为已启动的 wrangler dev 地址（见一致性验证说明）');
export const 睡 = 毫秒 => new Promise(完成 => setTimeout(完成, 毫秒));
export async function 等到(条件, {超时 = 15000, 间隔 = 50, 说明 = '条件'} = {}) {
  const 起 = Date.now();
  for (;;) { const 果 = await 条件(); if (果) return 果; if (Date.now() - 起 > 超时) throw new Error('等待超时：' + 说明); await 睡(间隔); }
}
export const 取 = (路径, 选项) => fetch(基址 + 路径, 选项);
export const 读日志 = async 键 => (await (await 取('/日志?键=' + encodeURIComponent(键))).json()).map(行 => 行.值);
export class 流读取器 {
  constructor(响应) { this.响应 = 响应; this.读 = 响应.body.getReader(); this.解 = new TextDecoder(); this.缓 = ''; this.已终 = false; this.块数 = 0; this.字节数 = 0; }
  async 下一块(超时 = 10000) {
    let 计时;
    const 超 = new Promise(完成 => { 计时 = setTimeout(() => 完成({超时: true}), 超时); });
    try {
      const 果 = await Promise.race([this.读.read().then(结 => ({结}), 错 => ({错})), 超]);
      if (果.超时) return {类型: '超时'};
      if (果.错 !== undefined) { this.已终 = true; return {类型: '错', 错: 果.错}; }
      if (果.结.done) { this.已终 = true; return {类型: '终'}; }
      this.块数++; this.字节数 += 果.结.value.byteLength;
      return {类型: '块', 字节: 果.结.value};
    } finally { clearTimeout(计时); }
  }
  async 下一帧(超时 = 10000) {
    for (;;) {
      const 位 = this.缓.indexOf('\n\n');
      if (位 >= 0) { const 帧 = this.缓.slice(0, 位); this.缓 = this.缓.slice(位 + 2); return 帧; }
      if (this.已终) return null;
      const 块 = await this.下一块(超时);
      if (块.类型 === '超时') throw new Error('读取事件帧超时');
      if (块.类型 === '错') throw 块.错;
      if (块.类型 === '块') this.缓 += this.解.decode(块.字节, {stream: true});
    }
  }
  async 读完() { const 帧们 = []; for (;;) { const 帧 = await this.下一帧(); if (帧 === null) return 帧们; 帧们.push(帧); } }
  async 全文(超时 = 30000) {
    let 文 = this.缓; this.缓 = '';
    for (;;) {
      if (this.已终) return 文;
      const 块 = await this.下一块(超时);
      if (块.类型 === '超时') throw new Error('读取事件流超时');
      if (块.类型 === '错') throw 块.错;
      if (块.类型 === '块') 文 += this.解.decode(块.字节, {stream: true});
    }
  }
  async 取消(原因) { try { await this.读.cancel(原因); } catch {} this.已终 = true; }
}
