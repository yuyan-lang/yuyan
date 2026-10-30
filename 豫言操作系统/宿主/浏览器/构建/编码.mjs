// 文言：客线程与主线程以共享之存相问答，此书其值之编解，兼续行与悬栈之边界：标一字节，次以其载。
// 汉语：构建宿主的答复值编解码，以及宿主步骤续行和 JSPI 边界适配。每个值先写一个类型字节，再写载荷（小端）。
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

import {造边界导入 as 原造边界导入, 读边界段, 包装实现} from './边界.mjs';

// 汉语：按字节缓存模块。先按长度与散列分桶，再逐字节核对，编译函数可返回模块或编译中的 Promise。
// 文言：以长与散分籍，以字与节核同；模成则藏，诺至亦存。
// 赋曰：字长分籍各成行，散值寻仓待核章。同帛共模承旧器，异文别铸立新梁。
export function 取编模(缓存, 字, 编) {
  let 散 = 5381;
  for (const 项 of 字) 散 = (Math.imul(散, 33) ^ 项) >>> 0;
  const 键 = 字.length + '/' + 散, 桶 = 缓存.get(键) ?? [];
  const 旧 = 桶.find(项 => 项.字.every((值, 序) => 值 === 字[序]));
  if (旧) return 旧.模;
  const 模 = 编();
  桶.push({字: 字.slice(), 模}); 缓存.set(键, 桶);
  return 模;
}

// 汉语：逐步执行宿主操作；同步答复直接续行，异步答复兑现后续行，异常送回原处的捕捉块。
// 文言：循步行事，既答则进，未答则候；有祸还告，令本处承之。
// 赋曰：同步循阶承旧约，异程候诺续前章。成辞逐次归原处，败信依途入旧防。
export const 续行 = 函 => {
  const 实 = (...参) => {
    const 步 = 函(...参);
    const 进 = (法, 值) => {
      let 项 = 步[法](值);
      while (!项.done) {
        if (项.value instanceof Promise) return 项.value.then(果 => 进('next', 果), 错 => 进('throw', 错));
        项 = 步.next(项.value);
      }
      return 项.value;
    };
    return 进('next');
  };
  实.异步 = typeof WebAssembly.Suspending === 'function' && typeof WebAssembly.promising === 'function';
  return 实;
};

// 汉语：浏览器的系统调用在宿主内挂起，客体接口仍保留原签名；转换在答复兑现后执行。
// 文言：客签如故，宿务可悬；答至乃化，还其所期。
// 赋曰：来参依式过桥梁，去果循签入旧仓。宿栈悬时留客影，回书至处复程光。
export function 造边界导入(模块, 桥, 实现表, {异步 = false, 入 = () => {}, 出 = () => {}, 察 = () => {}} = {}) {
  if (!异步) return 原造边界导入(模块, 桥, 实现表);
  const 导入 = 原造边界导入(模块, 桥, 实现表), 签们 = 读边界段(模块) ?? new Map();
  for (const [键, 签] of 签们) {
    const [模, 名] = 键.split('\t'), 实 = 实现表[模]?.[名];
    if (typeof 实 !== 'function') {
      const 桩 = 导入[模]?.[名];
      if (typeof 桩 === 'function') 导入[模][名] = (...参) => { 察(); return 桩(...参); };
      continue;
    }
    const 护 = (...参) => {
      察(); 入();
      let 果;
      try { 果 = 实(...参); } finally { 出(); }
      if (果 instanceof Promise) return 果.then(值 => { 察(); return 值; }, 错 => { 察(); throw 错; });
      察(); return 果;
    };
    护.异步 = 实.异步;
    导入[模][名] = 包装实现(实.异步 ? {...签, 异: true} : 签, 护, 桥, 模 + '.' + 名);
  }
  return 导入;
}
