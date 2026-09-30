// 文言：中央张量之宿主术：一块 Wasm 内存藏诸张量，中央张量内核（底层豫言所书之 Wasm）于其上行诸算。适配以内核表打包参数、定组数，此篇惟管存之分合、参数与绑定之安放、内核之调用，及数项内核所无之辅（抽样、整数与块量化八位之转、跨步之抄、数学之函）。浏览器与云工共用此篇，不写 import。
// 汉语：中央处理器张量后端的网页宿主部分（浏览器与云工共用，本文件不写 import）：全部张量存储放在一块 WebAssembly.Memory 里，由中央张量内核（库/底层库/中央张量内核，底层豫言写成的 Wasm 模块）在其上计算。
//       适配（豫言操作系统/适配/张量计算）用内核表打包参数区、求组数，本文件只管：存储的分配与释放（首次适配、相邻合并，按上下文记账）、参数区与绑定表的安放、调用内核入口，以及内核没有的几项辅助（按概率抽样、转成八位与三十二位整数及块量化八位、非三十二位类型的跨步复制、三角函数与乘方）。
//       页面跨源隔离（crossOriginIsolated）时用共享内存与 Web Worker 多线程：各工作线程用同一模块、同一块共享内存实例化后在工作板上等工作，主线程照工作板的布局写好字段、唤醒它们、自己也领组，再用 Atomics.waitAsync 异步等完成数（浏览器主线程不能 Atomics.wait）；否则单线程。
// 汉语：内存布局：[0, 64 KiB) 不用（地址 0 表示失败）；64 KiB 起 256 字节为工作板，其后 768 字节为绑定表，再后到 192 KiB 为参数区（更长的参数区临时从堆里分配）；192 KiB 起是堆。
// 汉语：待办事项：内存超过 4 GiB（memory64）；页面关闭前不结束工作线程；工作线程中途出错时主线程会一直等候；单个上下文的存储上限与整个页面共用同一块内存，多个上下文合计超出时分配失败。

const 页 = 65536;
const 板址 = 65536;
const 表址 = 板址 + 256;
const 参区址 = 板址 + 1024;
const 堆起 = 3 * 页;
const 参区长 = 堆起 - 参区址;
const 对齐 = 64;

// 文言：随手所用之暂存。汉语：位型换算用的暂存。
const 暂单 = new Float32Array(1), 暂单位 = new Uint32Array(暂单.buffer);
const 数值 = 值 => Number(值?.小数 ?? 值);

// 文言：单精而半精，就近取偶。汉语：单精度转 binary16 的位型（就近取偶；NaN 成安静 NaN，超出范围成无穷，过小成次正规数或零）。
const 转半位 = 值 => {
  暂单[0] = 值;
  const 位 = 暂单位[0], 符 = (位 >>> 16) & 0x8000, 阶 = (位 >>> 23) & 0xff;
  let 尾 = 位 & 0x7fffff;
  if (阶 === 0xff) return 符 | 0x7c00 | (尾 ? 0x200 : 0);
  const 新阶 = 阶 - 127 + 15;
  if (新阶 >= 0x1f) return 符 | 0x7c00;
  if (新阶 <= 0) {
    if (新阶 < -10) return 符;
    尾 |= 0x800000;
    const 移 = 14 - 新阶, 余 = 尾 & ((1 << 移) - 1), 中 = 1 << (移 - 1);
    let 半 = 尾 >>> 移;
    if (余 > 中 || (余 === 中 && (半 & 1))) 半++;
    return 符 | 半;
  }
  let 半 = (新阶 << 10) | (尾 >>> 13);
  const 余 = 尾 & 0x1fff;
  if (余 > 0x1000 || (余 === 0x1000 && (半 & 1))) 半++;
  return 符 | 半;
};

// 文言：化为整数：先就近取偶，后饱和于其域；NaN 为零（同诺节宿主）。汉语：转为整数类型：先就近取偶，再饱和到类型范围；NaN 转为 0（与诺节宿主 张量.mjs 相同）。
const 取偶 = 值 => {
  const 近 = Math.round(值);
  return 近 - 值 === 0.5 && 近 % 2 !== 0 ? 近 - 1 : 近;
};
const 饱和 = (值, 下, 上) => (Number.isNaN(值) ? 0 : Math.min(上, Math.max(下, 取偶(值))));

const 解数列 = 文 => {
  const 字 = typeof 文 === 'string' ? 文 : new TextDecoder().decode(文);
  return 字 === '' ? [] : 字.split(',').map(Number);
};

// 文言：造中央张量之能。汉语：创建中央张量能力，返回以原语名为键的函数表，由浏览器与云工宿主并进各自的能力表。
//   取内核模块(多线程) → Promise<WebAssembly.Module>：单线程版（中央张量内核.wasm）或多线程版（中央张量内核多线程.wasm）；
//   多线程：是否可以多线程（浏览器为 crossOriginIsolated 且有 SharedArrayBuffer、Worker、Atomics.waitAsync；云工为否）；
//   工作线程网址：工作线程脚本（中央张量工作线程.mjs）的网址；最大页：内存页数上限（每页 64 KiB）；线程数：工作线程个数上限。
export function 创建中央张量能力({取内核模块, 多线程 = false, 工作线程网址 = null, 最大页 = 65536, 线程数 = 0, 全局 = globalThis}) {
  let 启用中 = null, 态 = null;
  const 已分 = new Map(), 境用 = new Map();
  let 空闲 = [], 下个境号 = 1;

  const 字节视 = () => new Uint8Array(态.内存.buffer);
  const 单精视 = () => new Float32Array(态.内存.buffer);

  // 文言：首次适配而分，不足则扩内存。汉语：首次适配分配（按 64 字节对齐），空闲块不够时扩内存；扩不动时返回 0。
  const 分配内部 = (长, 境号) => {
    const 需 = Math.ceil(Math.max(长, 1) / 对齐) * 对齐;
    for (let 序 = 0; 序 < 空闲.length; 序++) {
      const [起, 段] = 空闲[序];
      if (段 < 需) continue;
      if (段 === 需) 空闲.splice(序, 1);
      else 空闲[序] = [起 + 需, 段 - 需];
      已分.set(起, {长: 需, 境号});
      return 起;
    }
    const 旧末 = 态.内存.buffer.byteLength, 末块 = 空闲.at(-1);
    const 尾 = 末块 && 末块[0] + 末块[1] === 旧末 ? 空闲.pop() : [旧末, 0];
    const 增页 = Math.ceil((需 - 尾[1]) / 页);
    try { 态.内存.grow(增页); }
    catch {
      if (尾[1] > 0) 空闲.push(尾);
      return 0;
    }
    const 共 = 尾[1] + 增页 * 页;
    已分.set(尾[0], {长: 需, 境号});
    if (共 > 需) 空闲.push([尾[0] + 需, 共 - 需]);
    return 尾[0];
  };
  // 文言：释而并其邻。汉语：释放并与相邻空闲块合并（空闲表按起点有序）。
  const 释放内部 = 址 => {
    const 项 = 已分.get(址);
    if (!项) return;
    已分.delete(址);
    if (项.境号 !== 0) 境用.set(项.境号, (境用.get(项.境号) ?? 0) - 项.长);
    let 低 = 0, 高 = 空闲.length;
    while (低 < 高) {
      const 中 = (低 + 高) >> 1;
      if (空闲[中][0] < 址) 低 = 中 + 1;
      else 高 = 中;
    }
    空闲.splice(低, 0, [址, 项.长]);
    if (低 + 1 < 空闲.length && 空闲[低][0] + 空闲[低][1] === 空闲[低 + 1][0]) {
      空闲[低][1] += 空闲[低 + 1][1];
      空闲.splice(低 + 1, 1);
    }
    if (低 > 0 && 空闲[低 - 1][0] + 空闲[低 - 1][1] === 空闲[低][0]) {
      空闲[低 - 1][1] += 空闲[低][1];
      空闲.splice(低, 1);
    }
  };
  const 须址 = (址, 长) => {
    if (!(Number.isSafeInteger(址) && 址 >= 堆起 && Number.isSafeInteger(长) && 长 >= 0 && 址 + 长 <= 态.内存.buffer.byteLength)) {
      throw Error('中央张量：地址越界');
    }
  };

  // 文言：起工作线程，候其皆就绪。汉语：创建工作线程并等全部实例化完毕；任何一个失败都退回单线程。
  const 起工作线程 = async (模块, 数) => {
    const 工们 = [];
    try {
      await Promise.all(Array.from({length: 数}, (_, 序) => new Promise((成, 败) => {
        const 工 = new 全局.Worker(工作线程网址, {type: 'module', name: '中央张量工作线程' + (序 + 1)});
        工们.push(工);
        工.addEventListener('message', 事 => (事.data?.就绪 ? 成() : 败(Error(String(事.data?.错 ?? '工作线程失败')))), {once: true});
        工.addEventListener('error', 事 => 败(Error(String(事.message ?? '工作线程失败'))), {once: true});
        工.postMessage({模块, 内存: 态.内存, 板址, 序号: 序 + 1});
      })));
      return 工们;
    } catch (错) {
      for (const 工 of 工们) 工.terminate();
      全局.console?.warn?.('中央张量：工作线程不可用，改用单线程：' + String(错?.message ?? 错));
      return [];
    }
  };

  // 文言：启之：取模、建存、实例、起工。汉语：启用：取内核模块、建内存、实例化，多线程时起工作线程；只做一次，返回（码，存储上限，线程数，消息）。
  const 启用 = async () => {
    try {
      const 多 = 多线程 && 线程数 > 0;
      const 模块 = await 取内核模块(多);
      const 内存 = new 全局.WebAssembly.Memory({initial: 4, maximum: 最大页, ...(多 ? {shared: true} : {})});
      const 实例 = new 全局.WebAssembly.Instance(模块, {环境: {内存}});
      态 = {模块, 内存, 实例, 工们: [], 线程: 0};
      空闲 = [[堆起, 4 * 页 - 堆起]];
      if (多) {
        态.工们 = await 起工作线程(模块, 线程数);
        态.线程 = 态.工们.length;
      }
      return [0, 最大页 * 页 - 堆起, 态.线程, ''];
    } catch (错) {
      态 = null;
      return [2, 0, 0, '中央张量内核不可用：' + String(错?.message ?? 错)];
    }
  };

  // 文言：派于役板：书其字、醒诸工、主线亦领，异步候其毕。汉语：经工作板派发（布局见中央张量内核 派发。豫）：先清已完成组数、把领取字换成新代号，再写其余字段与代号并唤醒全部工作线程；
  //       主线程也调用「板领取」领组执行，最后用 Atomics.waitAsync 等已完成组数到齐。
  const 板派发 = async (号, 参址, 组数, 块) => {
    const 字 = new Int32Array(态.内存.buffer), 基 = 板址 >> 2;
    const 代 = (Atomics.load(字, 基) + 1) | 0;
    Atomics.store(字, 基 + 7, 0);
    Atomics.store(new BigInt64Array(态.内存.buffer, 板址 + 32, 1), 0, BigInt.asIntN(64, BigInt(代 >>> 0) << 32n));
    字[基 + 2] = 号;
    字[基 + 3] = 参址;
    字[基 + 4] = 表址;
    字[基 + 5] = 组数;
    字[基 + 6] = 块;
    Atomics.store(字, 基, 代);
    Atomics.notify(字, 基);
    态.实例.exports.板领取(板址, 代, 0);
    for (;;) {
      const 已 = Atomics.load(字, 基 + 7);
      if ((已 >>> 0) >= 组数) return;
      const 候 = Atomics.waitAsync(字, 基 + 7, 已);
      if (候.async) await 候.value;
    }
  };

  // 文言：行一入口：安参数与绑定，单线径调，多线付役板。汉语：运行一个内核入口：参数区字节放进参数区（过长时临时分配），绑定表字节放进绑定表，组数够多且有工作线程时经工作板派发（返回 Promise），否则单线程直接调用。
  const 运行 = (号值, 参数, 绑定, 组数值) => {
    const 号 = Number(号值), 组数 = Number(组数值);
    if (!态) throw Error('中央张量：尚未启用');
    if (!(绑定.length <= 参区址 - 表址)) throw Error('中央张量：绑定过多');
    const 临时 = 参数.length > 参区长 ? 分配内部(参数.length, 0) : 0;
    if (参数.length > 参区长 && 临时 === 0) throw Error('中央张量：参数区分配失败');
    const 参址 = 临时 || 参区址;
    const 视 = 字节视();
    视.set(参数, 参址);
    视.set(绑定, 表址);
    const 完 = () => { if (临时) 释放内部(临时); };
    if (态.线程 === 0 || 组数 < 2) {
      try { 态.实例.exports.运行入口(号, 参址, 表址, 0, 组数); }
      finally { 完(); }
      return null;
    }
    const 块 = Math.max(1, Math.floor(组数 / ((态.线程 + 1) * 8)));
    return 板派发(号, 参址, 组数, 块).finally(完).then(() => null);
  };

  // 文言：诺节同法之抽样。汉语：按概率抽样，算法与诺节宿主 张量.mjs 逐步相同（同一随机整数得同一结果）：温度不大于零时取最大值序号（NaN 小于一切数，并列取小序号）。
  const 抽样 = (址值, 数值个, 温度值, 前k值, 累积p值, 随机数) => {
    const 址 = Number(址值), 总 = Number(数值个), 温度 = 数值(温度值), 前k = Number(前k值), 累积p = 数值(累积p值);
    须址(址, 总 * 4);
    const 值们 = new Float32Array(态.内存.buffer, 址, 总);
    if (!(温度 > 0)) {
      let 序 = 0, 最大 = NaN;
      for (let 位 = 0; 位 < 总; 位++) {
        const 值 = 值们[位];
        if (Number.isNaN(值)) continue;
        if (Number.isNaN(最大) || 值 > 最大) { 最大 = 值; 序 = 位; }
      }
      return 序;
    }
    const 分 = new Float64Array(总);
    for (let 位 = 0; 位 < 总; 位++) 分[位] = Number.isNaN(值们[位]) ? -Infinity : 值们[位] / 温度;
    const 候选 = Array.from({length: 总}, (_, 位) => 位).sort((左, 右) => (分[右] - 分[左]) || 左 - 右);
    const 留数 = 前k >= 1 ? Math.min(前k, 总) : 总, 率 = new Float64Array(留数);
    let 和 = 0;
    for (let 位 = 0; 位 < 留数; 位++) { 率[位] = Math.exp(分[候选[位]] - 分[候选[0]]); 和 += 率[位]; }
    for (let 位 = 0; 位 < 留数; 位++) 率[位] /= 和;
    let 终 = 留数;
    if (累积p > 0 && 累积p < 1) {
      let 累 = 0;
      for (终 = 0; 终 < 留数;) { 累 += 率[终++]; if (累 >= 累积p) break; }
      let 新和 = 0;
      for (let 位 = 0; 位 < 终; 位++) 新和 += 率[位];
      for (let 位 = 0; 位 < 终; 位++) 率[位] /= 新和;
    }
    const 模 = 16777216n, 随 = Number(((BigInt(随机数) % 模) + 模) % 模) / 16777216;
    let 累 = 0;
    for (let 位 = 0; 位 < 终; 位++) { 累 += 率[位]; if (累 > 随) return 候选[位]; }
    return 候选[终 - 1];
  };

  // 文言：单精转他类：八位、三十二位整数取偶而饱和；块量化八位依 GGML 之法（同诺节宿主）。汉语：连续单精度转成八位整数（3）、三十二位整数（4）或块量化八位（5，照 GGML quantize_row_q8_0_ref，与诺节宿主相同）。
  const 单精转 = (源址值, 目址值, 数值个, 目类值) => {
    const 源址 = Number(源址值), 目址 = Number(目址值), 数 = Number(数值个), 类 = Number(目类值);
    须址(源址, 数 * 4);
    const 值们 = new Float32Array(态.内存.buffer, 源址, 数);
    if (类 === 3) {
      须址(目址, 数);
      const 出 = new Int8Array(态.内存.buffer, 目址, 数);
      for (let 序 = 0; 序 < 数; 序++) 出[序] = 饱和(值们[序], -128, 127);
    } else if (类 === 4) {
      须址(目址, 数 * 4);
      const 出 = new Int32Array(态.内存.buffer, 目址, 数);
      for (let 序 = 0; 序 < 数; 序++) 出[序] = 饱和(值们[序], -2147483648, 2147483647);
    } else if (类 === 5) {
      if (数 % 32 !== 0) throw Error('中央张量：块量化八位的元素数须是 32 的倍数');
      须址(目址, (数 / 32) * 34);
      const 字 = new Uint8Array(态.内存.buffer), 符字 = new Int8Array(态.内存.buffer);
      for (let 块 = 0, 位 = 目址; 块 < 数 / 32; 块++, 位 += 34) {
        let 最大 = 0;
        for (let 序 = 0; 序 < 32; 序++) {
          const 绝 = Math.abs(值们[块 * 32 + 序]);
          最大 = 最大 > 绝 ? 最大 : 绝;
        }
        const 缩 = Math.fround(最大 / 127), 倒 = 缩 ? Math.fround(1 / 缩) : 0, 半 = 转半位(缩);
        字[位] = 半 & 255;
        字[位 + 1] = 半 >> 8;
        for (let 序 = 0; 序 < 32; 序++) {
          const 值 = Math.fround(值们[块 * 32 + 序] * 倒);
          符字[位 + 2 + 序] = Number.isNaN(值) ? 0 : 值 < 0 ? -Math.floor(-值 + 0.5) : Math.floor(值 + 0.5);
        }
      }
    } else throw Error('中央张量：不支持的目标类型 ' + 类);
    return null;
  };

  // 文言：跨步之抄：依逻辑之序，以同宽之位型自源视抄入目视；先尽读源，故共存亦无碍。汉语：跨步复制：按形状的逻辑次序（行优先）把源视图的元素原样（宽 1、2 或 4 字节的位型）抄进目标视图；起点与步长以元素计，步长可为零或负；先把源完整读出再写，所以源与目标共享存储也无妨。
  const 跨步抄 = (宽值, 源址值, 源起值, 源步文, 目址值, 目起值, 目步文, 形文) => {
    const 宽 = Number(宽值), 源址 = Number(源址值), 目址 = Number(目址值), 源起 = Number(源起值), 目起 = Number(目起值);
    const 源步 = 解数列(源步文), 目步 = 解数列(目步文), 形 = 解数列(形文), 维数 = 形.length;
    const 类 = 宽 === 1 ? Uint8Array : 宽 === 2 ? Uint16Array : 宽 === 4 ? Uint32Array : null;
    if (!类 || 源址 % 宽 || 目址 % 宽 || 源步.length !== 维数 || 目步.length !== 维数) throw Error('中央张量：跨步复制参数无效');
    const 总 = 形.reduce((积, 项) => 积 * 项, 1), 源 = new 类(态.内存.buffer), 目 = new 类(态.内存.buffer);
    const 源基 = 源址 / 宽 + 源起, 目基 = 目址 / 宽 + 目起, 暂 = new 类(总), 标 = new Array(维数).fill(0);
    const 界 = 态.内存.buffer.byteLength / 宽;
    let 源偏 = 源基;
    for (let 序 = 0; 序 < 总; 序++) {
      if (!(源偏 >= 0 && 源偏 < 界)) throw Error('中央张量：跨步复制越界');
      暂[序] = 源[源偏];
      for (let 维 = 维数 - 1; 维 >= 0; 维--) {
        标[维]++;
        源偏 += 源步[维];
        if (标[维] < 形[维]) break;
        源偏 -= 源步[维] * 形[维];
        标[维] = 0;
      }
    }
    标.fill(0);
    let 目偏 = 目基;
    for (let 序 = 0; 序 < 总; 序++) {
      if (!(目偏 >= 0 && 目偏 < 界)) throw Error('中央张量：跨步复制越界');
      目[目偏] = 暂[序];
      for (let 维 = 维数 - 1; 维 >= 0; 维--) {
        标[维]++;
        目偏 += 目步[维];
        if (标[维] < 形[维]) break;
        目偏 -= 目步[维] * 形[维];
        标[维] = 0;
      }
    }
    return null;
  };

  // 文言：数学之函：零余弦、一正弦、二乘方、三上取整、四大于（一或零）。汉语：数学函数（双精度）：0 余弦、1 正弦、2 乘方、3 向上取整、4 甲大于乙时得 1 否则 0。
  const 数学 = (码值, 甲值, 乙值) => {
    const 甲 = 数值(甲值), 乙 = 数值(乙值);
    const 果 = [Math.cos(甲), Math.sin(甲), Math.pow(甲, 乙), Math.ceil(甲), 甲 > 乙 ? 1 : 0][Number(码值)];
    if (果 === undefined) throw Error('中央张量：数学函数码无效');
    return {小数: 果};
  };

  const 须启 = () => { if (!态) throw Error('中央张量：尚未启用'); };
  return {
    豫言_中央张量_启用: () => (态 ? [0, 最大页 * 页 - 堆起, 态.线程, ''] : (启用中 ??= 启用().finally(() => { 启用中 = null; }))),
    豫言_中央张量_新境: () => { 须启(); const 号 = 下个境号++; 境用.set(号, 0); return 号; },
    // 文言：释一境之诸存。汉语：释放一个上下文的全部存储。
    豫言_中央张量_释放境: 号值 => {
      须启();
      const 号 = Number(号值);
      for (const [址, 项] of [...已分]) if (项.境号 === 号) 释放内部(址);
      境用.delete(号);
      return null;
    },
    // 文言：分存：逾上限或内存不足则零。汉语：为上下文「境号」分配存储（境号 0 为宿主内部，不计入上限）；超出上限或内存扩不动时返回 0；清零为 1 时先清零。
    豫言_中央张量_分配: (号值, 长值, 清零, 上限值) => {
      须启();
      const 号 = Number(号值), 长 = Number(长值), 上限 = Number(上限值);
      if (!(Number.isSafeInteger(长) && 长 >= 0)) return 0;
      const 需 = Math.ceil(Math.max(长, 1) / 对齐) * 对齐;
      if (号 !== 0 && (境用.get(号) ?? 0) + 需 > 上限) return 0;
      if (需 > 4294967296 - 堆起) return 0;
      const 址 = 分配内部(长, 号);
      if (址 === 0) return 0;
      if (号 !== 0) 境用.set(号, (境用.get(号) ?? 0) + 需);
      if (Number(清零)) 字节视().fill(0, 址, 址 + 需);
      return 址;
    },
    豫言_中央张量_释放: 址 => { 须启(); 释放内部(Number(址)); return null; },
    豫言_中央张量_写字节: (址值, 字节) => {
      须启();
      const 址 = Number(址值);
      须址(址, 字节.length);
      字节视().set(字节, 址);
      return null;
    },
    豫言_中央张量_读字节: (址值, 长值) => {
      须启();
      const 址 = Number(址值), 长 = Number(长值);
      须址(址, 长);
      return 字节视().slice(址, 址 + 长);
    },
    豫言_中央张量_读单精: 址值 => {
      须启();
      const 址 = Number(址值);
      须址(址, 4);
      return {小数: 单精视()[址 >> 2]};
    },
    豫言_中央张量_写单精: (址值, 值) => {
      须启();
      const 址 = Number(址值);
      须址(址, 4);
      单精视()[址 >> 2] = 数值(值);
      return null;
    },
    豫言_中央张量_运行: (号, 参数, 绑定, 组数) => { 须启(); return 运行(号, 参数, 绑定, 组数); },
    豫言_中央张量_单精位型: 值 => { 暂单[0] = 数值(值); return 暂单位[0]; },
    豫言_中央张量_数学: 数学,
    豫言_中央张量_单精转: (...参) => { 须启(); return 单精转(...参); },
    豫言_中央张量_跨步抄: (...参) => { 须启(); return 跨步抄(...参); },
    豫言_中央张量_抽样: (...参) => { 须启(); return 抽样(...参); }
  };
}
