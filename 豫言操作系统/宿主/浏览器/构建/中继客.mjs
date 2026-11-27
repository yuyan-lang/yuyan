// 汉语：浏览器 TCP 中继客户端。主线程每条连接持有一个 WebSocket；握手、字节队列、半关闭与等待共用同一状态。路径与参数名、帧文字、关闭码与状态码的说明都取自中继约.mjs（由津梁的中继约籍册生成，#1465），这里不另写。
// 文言：一门一舟，主工掌之；凭验乃通，帛至则藏，口缄犹听，舟返乃终。径名、帧文、闭码与状说皆取诸中继约，不复别书。
// 赋曰：凭书先渡，候信开津；来帛依序，去札循因。成连授符，败途陈码；静门候事，满限还声。半掩言户，全收客音；役终撤柄，期尽回询。字流存本，帧序留真。
import {中继问名, 中继帧文, 中继闭码, 中继状们} from './中继约.mjs';
const 解码器 = new TextDecoder();
const 发片长 = 1 << 20;
// 汉语：状态码到说明的表、各状态的负码，都照中继约的「中继状们」生成。文言：状码之说与诸状之负码，皆依中继约之诸状而成。
const 原因 = Object.fromEntries(Object.values(中继状们).map(({码, 说明}) => [码, 说明]));
const 负码 = 状 => -BigInt(状.码);
const 帧不合 = 中继状们.帧不合.码, 断连 = 中继状们.断连.码;
const 柄误负 = 负码(中继状们.柄误), 参误负 = 负码(中继状们.帧不合), 不支持负 = 负码(中继状们.不支持), 写已闭负 = 负码(中继状们.写已闭);

export class 中继客 {
  // 汉语：取号 是进程表给的号源（连接号与子进程号同出一计，群候据号分得清），有事 在连接状态变了时调（叫醒群候里等它的线程）；单独用时连接号从一起。文言：取号为程表所授之号源，有事于连态易时呼之；独用则自一起。
  constructor(配置, {取号 = null, 有事 = null} = {}) { this.配置 = 配置; this.连接们 = new Map(); this.下号 = 1; this.取号 = 取号; this.有事 = 有事; }
  误由(码) { return 原因[Math.abs(Number(码))] ?? ('网络错误（码 ' + 码 + '）'); }
  叩门(主机, 端口) {
    if (!this.配置?.地址) return [不支持负, 0n];
    主机 = 主机 instanceof Uint8Array ? 解码器.decode(主机) : String(主机);
    let 舟;
    try {
      const 址 = new URL(this.配置.地址);
      if (!['ws:', 'wss:'].includes(址.protocol)) return [参误负, 0n];
      址.pathname = 中继问名.径; 址.search = ''; 址.hash = '';
      址.searchParams.set(中继问名.主机, 主机); 址.searchParams.set(中继问名.端口, String(端口));
      舟 = new WebSocket(址);
    } catch { return [参误负, 0n]; }
    const 柄 = this.取号 ? this.取号() : this.下号++, 连 = {舟, 已连: false, 已闭: false, 缄: false, 误: 0, 块们: [], 偏: 0, 候们: new Set()};
    this.连接们.set(柄, 连);
    舟.binaryType = 'arraybuffer';
    const 败 = 码 => { 连.误 ||= 码; 连.已闭 = true; this.唤(连); };
    舟.onopen = () => { if (!连.已闭) 舟.send(中继帧文.凭 + (this.配置.令牌 ?? '')); };
    舟.onmessage = ({data}) => {
      if (连.已闭) return;
      if (!连.已连 && data === 中继帧文.连上) 连.已连 = true;
      else if (连.已连 && data instanceof ArrayBuffer) { if (data.byteLength) 连.块们.push(new Uint8Array(data)); }
      else { 败(帧不合); 舟.close(中继闭码.基 + 帧不合, '协议消息无效'); }
      this.唤(连);
    };
    // 汉语：error 后等待 close 的明确关闭码，保留四千加底码的原因。文言：误至犹候闭信，存其本码。
    舟.onerror = () => {};
    舟.onclose = ({code}) => {
      连.已闭 = true;
      if (code !== 中继闭码.常) 连.误 ||= code > 中继闭码.基 && code < 中继闭码.顶 ? code - 中继闭码.基 : 断连;
      else if (!连.已连) 连.误 ||= 断连;
      this.唤(连);
    };
    return [1n, BigInt(柄)];
  }
  唤(连) { for (const 候 of [...连.候们]) 候(); this.有事?.(); }
  事件(连, 关注) {
    return ((连.块们.length || 连.已闭 ? 1 : 0) & 关注) |
      ((连.已连 && !连.已闭 && 连.舟.readyState === WebSocket.OPEN ? 2 : 0) & 关注) | (连.误 ? 4 : 0) | (连.已闭 ? 8 : 0);
  }
  守候(连, 关注, 时限) {
    if (!Number.isInteger(关注) || 关注 < 1 || 关注 > 3 || !Number.isSafeInteger(时限)) return [参误负, 0n];
    const 事 = this.事件(连, 关注);
    if (事 || 时限 === 0) return [0n, BigInt(事)];
    return new Promise(成 => {
      let 钟;
      const 还 = 事 => { clearTimeout(钟); 连.候们.delete(候); 成([0n, BigInt(事)]); };
      const 候 = () => { const 事 = this.事件(连, 关注); if (事) 还(事); };
      连.候们.add(候);
      if (时限 >= 0) 钟 = setTimeout(() => 还(this.事件(连, 关注)), 时限);
    });
  }
  听(连, 限) {
    if (!Number.isSafeInteger(限)) return [参误负, new Uint8Array()];
    if (限 <= 0) return [0n, new Uint8Array()];
    if (连.块们.length) {
      const 首 = 连.块们[0], 止 = Math.min(首.length, 连.偏 + 限), 字节 = 首.slice(连.偏, 止);
      连.偏 = 止;
      if (止 === 首.length) { 连.块们.shift(); 连.偏 = 0; }
      return [0n, 字节];
    }
    return [连.误 ? -BigInt(连.误) : 连.已闭 ? 2n : 1n, new Uint8Array()];
  }
  告(连, 内容, 起) {
    if (!Number.isSafeInteger(起) || 起 < 0 || 起 > 内容.length) return [参误负, 0n];
    if (起 === 内容.length) return [0n, 0n];
    if (连.误) return [-BigInt(连.误), 0n];
    if (连.已闭 || 连.缄) return [写已闭负, 0n];
    if (!连.已连) return [1n, 0n];
    if (连.舟.readyState !== WebSocket.OPEN) return [写已闭负, 0n];
    // 汉语：单次至多发送一兆字节，沿用原生中转段的部分写语义。文言：一次至多一兆，循原生分书之义。
    const 段 = 内容.subarray(起, Math.min(内容.length, 起 + 发片长));
    try { 连.舟.send(段); return [0n, BigInt(段.length)]; }
    catch { return [写已闭负, 0n]; }
  }
  调(名, ...参) {
    if (名 === '误由') return this.误由(参[0]);
    if (名 === '叩门') return this.叩门(参[0], 参[1]);
    const 空 = 名 === '听' ? '' : 名 === '听帛' ? new Uint8Array() : ['勿滞', '缄口', '别'].includes(名) ? undefined : 0n;
    if (!this.配置?.地址) return [不支持负, 空];
    const 柄 = Number(参[0]), 连 = this.连接们.get(柄);
    if (!连) return [柄误负, 空];
    switch (名) {
      case '入门': return [连.误 ? -BigInt(连.误) : 连.已连 ? 0n : 1n, BigInt(柄)];
      case '守候': return this.守候(连, Number(参[1]), Number(参[2]));
      case '听帛': return this.听(连, Number(参[1]));
      // 汉语：文字边界也接收原始字节，保留分片里的半个字符。文言：文界亦受本帛，半字犹全其真。
      case '听': return this.听(连, Number(参[1]));
      // 汉语：文与帛在宿主边界都给原始字节，共用发送路径。文言：文帛出界皆本字，共循一途而致之。
      case '续告':
      case '续告帛': return this.告(连, 参[1], Number(参[2]));
      // 汉语：WebSocket 发送由浏览器安排，勿滞接受设置并返回成功。文言：舟行由东道掌之，受其勿滞而报成。
      case '勿滞': return [0n, undefined];
      case '缄口':
        if (连.误) return [-BigInt(连.误), undefined];
        if (连.已闭) return [写已闭负, undefined];
        if (!连.已连) return [1n, undefined];
        if (连.舟.readyState !== WebSocket.OPEN) return [写已闭负, undefined];
        if (!连.缄) { 连.舟.send(中继帧文.缄口); 连.缄 = true; }
        return [0n, undefined];
      case '别':
        连.已闭 = true; this.唤(连); 连.舟.close(中继闭码.常); this.连接们.delete(柄); return [0n, undefined];
      default: throw Error('未知中继操作：' + 名);
    }
  }
  停止() { for (const 柄 of [...this.连接们.keys()]) this.调('别', 柄); }
}
