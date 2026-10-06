// 文言：客线程与主线程以共享之存相问答，此书其值之编解：标一字节，次以其载。
// 汉语：构建宿主里客体工作线程与主线程经共享内存问答，这里是答复值的编码与解码：每个值先写一个类型字节，再写载荷（小端）。
//   类型：0 无（undefined、null），1 假，2 真，3 小数（f64），4 大整数（十进制文字），5 文字（UTF-8），6 字节，7 数组。
const 编码器 = new TextEncoder(), 解码器 = new TextDecoder();

export function 编码值(值) {
  const 块们 = [];
  let 总长 = 0;
  const 推 = 块 => { 块们.push(块); 总长 += 块.length; };
  const 长度头 = (种, 长) => { const 头 = new Uint8Array(5); 头[0] = 种; new DataView(头.buffer).setUint32(1, 长, true); return 头; };
  const 写 = 项 => {
    if (项 === undefined || 项 === null) return 推(Uint8Array.of(0));
    if (项 === false) return 推(Uint8Array.of(1));
    if (项 === true) return 推(Uint8Array.of(2));
    if (typeof 项 === 'number') { const 块 = new Uint8Array(9); 块[0] = 3; new DataView(块.buffer).setFloat64(1, 项, true); return 推(块); }
    if (typeof 项 === 'bigint') { const 文 = 编码器.encode(String(项)); 推(长度头(4, 文.length)); return 推(文); }
    if (typeof 项 === 'string') { const 文 = 编码器.encode(项); 推(长度头(5, 文.length)); return 推(文); }
    if (项 instanceof Uint8Array) { 推(长度头(6, 项.length)); return 推(项); }
    if (Array.isArray(项)) { 推(长度头(7, 项.length)); for (const 子 of 项) 写(子); return; }
    throw TypeError('构建宿主答复不能编码此值：' + Object.prototype.toString.call(项));
  };
  写(值);
  const 果 = new Uint8Array(总长);
  let 位 = 0;
  for (const 块 of 块们) { 果.set(块, 位); 位 += 块.length; }
  return 果;
}

export function 解码值(字节) {
  const 视 = new DataView(字节.buffer, 字节.byteOffset, 字节.byteLength);
  let 位 = 0;
  const 读 = () => {
    const 种 = 字节[位++];
    switch (种) {
      case 0: return undefined;
      case 1: return false;
      case 2: return true;
      case 3: { const 数 = 视.getFloat64(位, true); 位 += 8; return 数; }
      case 4: case 5: case 6: {
        const 长 = 视.getUint32(位, true); 位 += 4;
        const 段 = 字节.slice(位, 位 + 长); 位 += 长;
        return 种 === 4 ? BigInt(解码器.decode(段)) : 种 === 5 ? 解码器.decode(段) : 段;
      }
      case 7: { const 长 = 视.getUint32(位, true); 位 += 4; const 列 = new Array(长); for (let 序 = 0; 序 < 长; 序++) 列[序] = 读(); return 列; }
      default: throw Error('构建宿主答复编码有误：类型 ' + 种);
    }
  };
  const 果 = 读();
  if (位 !== 字节.length) throw Error('构建宿主答复编码末尾有多余字节');
  return 果;
}
