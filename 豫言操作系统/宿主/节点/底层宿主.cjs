// 文言：底层之模（无回收者）于诺节之宿：导入内存者，依其导入之限建之（可共享、可六十四位址）；导入函数者，供「数学」「诺节」二模之函；出「工作线程」者，起工作线程若干，各以同一模块、同一共享内存实例化而调之，主线乃调 _start。
// 汉语：诺节宿主运行底层（无垃圾回收）模块：模块导入内存时，按导入段里的限制建内存（可共享、可六十四位地址）；导入函数时，提供「数学」与「诺节」两个导入模块里的函数；模块导出「工作线程」时，先起若干工作线程，每个用同一模块、同一块共享内存实例化并调用 工作线程(序号)，主线（宿主的客体线程）再调用 _start。工作线程数取环境变量 YY_底层线程数，缺省为可用处理器数减一。
// 汉语：待办事项：数据段在每次实例化时都会重写导入的共享内存（工作线程晚于主线实例化，会覆盖主线已改的数据区）；底层模块应避免在导入共享内存时使用静态串，或先由主线等各工作线程就绪。表、全局变量与标签的导入不支持。
'use strict';
const {Worker, isMainThread, workerData} = require('node:worker_threads');
const 系统 = require('node:os'), 文件 = require('node:fs');

// 文言：读无号 LEB128。汉语：读无符号 LEB128，返回 [值（BigInt）, 新位置]。
function 读无号(字节, 位) {
  let 值 = 0n, 移 = 0n;
  for (;;) {
    const 字 = 字节[位++];
    值 |= BigInt(字 & 0x7f) << 移;
    移 += 7n;
    if ((字 & 0x80) === 0) return [值, 位];
  }
}
function 读名(字节, 位) {
  const [长, 后] = 读无号(字节, 位);
  const 止 = 后 + Number(长);
  return [Buffer.from(字节.subarray(后, 止)).toString('utf8'), 止];
}
// 文言：读限：标志之位零有最大、位一共享、位二六十四位址。汉语：读限制：标志第 0 位有最大页数、第 1 位共享、第 2 位六十四位地址。
function 读限(字节, 位) {
  const 标 = 字节[位++];
  let 最小, 最大 = null;
  [最小, 位] = 读无号(字节, 位);
  if (标 & 1) [最大, 位] = 读无号(字节, 位);
  return [{最小, 最大, 共享: (标 & 2) !== 0, 长址: (标 & 4) !== 0}, 位];
}
// 文言：值型之读：引用型或带堆型。汉语：跳过一个值类型（带堆类型的引用类型多一个有符号 LEB128）。
function 跳值型(字节, 位) {
  const 型 = 字节[位++];
  if (型 === 0x63 || 型 === 0x64) { while (字节[位++] & 0x80); }
  return 位;
}

// 文言：自二进制之导入段取所导之内存。汉语：从模块二进制的导入段取出全部内存导入：[{模块, 字段, 最小, 最大, 共享, 长址}]。
function 读内存导入(字节) {
  const 果 = [];
  let 位 = 8;
  while (位 < 字节.length) {
    const 号 = 字节[位++];
    let 长;
    [长, 位] = 读无号(字节, 位);
    const 止 = 位 + Number(长);
    if (号 === 2) {
      let 数;
      [数, 位] = 读无号(字节, 位);
      for (let 序 = 0n; 序 < 数; 序++) {
        let 模块, 字段;
        [模块, 位] = 读名(字节, 位);
        [字段, 位] = 读名(字节, 位);
        const 种 = 字节[位++];
        if (种 === 0) [, 位] = 读无号(字节, 位);
        else if (种 === 1) { 位 = 跳值型(字节, 位); [, 位] = 读限(字节, 位); }
        else if (种 === 2) { let 限; [限, 位] = 读限(字节, 位); 果.push({模块, 字段, ...限}); }
        else if (种 === 3) { 位 = 跳值型(字节, 位) + 1; }
        else if (种 === 4) { 位++; [, 位] = 读无号(字节, 位); }
        else throw Error('底层宿主：不认识的导入种类 ' + 种);
      }
      return 果;
    }
    位 = 止;
  }
  return 果;
}

// 文言：依所导之限建内存。汉语：按导入的限制建内存；六十四位地址的页数用 BigInt。
function 建内存(限) {
  if (限.长址) return new WebAssembly.Memory({address: 'i64', initial: 限.最小, ...(限.最大 === null ? {} : {maximum: 限.最大}), shared: 限.共享});
  return new WebAssembly.Memory({initial: Number(限.最小), ...(限.最大 === null ? {} : {maximum: Number(限.最大)}), shared: 限.共享});
}

// 文言：宿主所供之函：「数学」以双精度求超越之函（测试之参照用之）；「诺节」书字节、整数、小数，取毫秒与工作线程之数。
// 汉语：宿主提供的函数：「数学」按双精度求超越函数（测试的参照值用）；「诺节」写出内存里的字节、写整数、写小数，取毫秒时间与工作线程数。
function 宿主函数(取内存, 写出, 线程数) {
  const 文 = 值 => 写出(Buffer.from(值));
  return {
    数学: {
      指数: Math.exp, 对数: Math.log, 正弦: Math.sin, 余弦: Math.cos, 正切: Math.tan,
      双曲正切: Math.tanh, 反正切: Math.atan, 幂: Math.pow, 以二为底对数: Math.log2
    },
    诺节: {
      写字节: (址, 长) => 写出(Buffer.from(new Uint8Array(取内存().buffer, 址 >>> 0, 长 >>> 0))),
      写整数: 值 => 文(String(值)),
      写长整数: 值 => 文(String(值)),
      写小数: 值 => 文(String(值)),
      毫秒: () => performance.now(),
      线程数: () => 线程数
    }
  };
}

// 文言：成导入之对象。汉语：组装导入对象：内存按（模块，字段）放入，函数按宿主函数表查找，缺的报错。
function 建导入(模块, 内存导入们, 内存们, 取内存, 写出, 线程数) {
  const 函们 = 宿主函数(取内存, 写出, 线程数), 导入 = {};
  for (const 项 of WebAssembly.Module.imports(模块)) {
    (导入[项.module] ??= {});
    if (项.kind === 'memory') {
      const 序 = 内存导入们.findIndex(内 => 内.模块 === 项.module && 内.字段 === 项.name);
      导入[项.module][项.name] = 内存们[序];
    } else if (项.kind === 'function') {
      const 函 = 函们[项.module]?.[项.name];
      if (!函) throw Error('底层宿主：未实现的导入函数 ' + 项.module + '.' + 项.name);
      导入[项.module][项.name] = 函;
    } else throw Error('底层宿主：不支持的导入 ' + 项.module + '.' + 项.name + '（' + 项.kind + '）');
  }
  return 导入;
}

// 文言：导出之内存，取其首。汉语：取模块用的内存（导入的或导出的第一块），供写字节用。
function 模块内存(实例, 内存们) {
  if (内存们.length > 0) return 内存们[0];
  return Object.values(实例.exports).find(值 => 值 instanceof WebAssembly.Memory);
}

// 文言：此模须由宿主之底层之法行之否：GC 之模（出元组之原型，或有边界之段）不在此列；余者导入非回收宿主之物，或出工作线程者是。汉语：判断模块是否要由本文件运行：WasmGC 程序（导出元组原型 yy_tuple，或带「豫言边界」段）不归这里，它们可以有带类型的导入；其余模块导入了 yuyan:gc-host/v1 以外的东西，或导出了「工作线程」，就是底层模块。
function 是底层模块(模块) {
  const 导出们 = WebAssembly.Module.exports(模块);
  if (导出们.some(项 => 项.name === 'yy_tuple') || WebAssembly.Module.customSections(模块, '豫言边界').length > 0) return false;
  return WebAssembly.Module.imports(模块).some(项 => 项.module !== 'yuyan:gc-host/v1') ||
    导出们.some(项 => 项.name === '工作线程');
}

// 文言：主线行之：建内存、起工作线程、调 _start；工作线程中错则书其栈而令进程终之，免主线永候。
// 汉语：在宿主的客体线程里运行：建内存、起工作线程、调用 _start，返回退出码（陷阱照常抛出）。工作线程出错时写出错误栈并向本进程发 SIGTERM，免得主线在原子等待里永远等下去。
function 运行底层模块(模块, 字节, 写出) {
  const 内存导入们 = 读内存导入(字节);
  const 内存们 = 内存导入们.map(建内存);
  const 要线程 = WebAssembly.Module.exports(模块).some(项 => 项.name === '工作线程');
  const 设数 = Number(process.env.YY_底层线程数 ?? NaN);
  const 线程数 = !要线程 ? 0 : Number.isInteger(设数) && 设数 >= 0 ? 设数 : Math.max(0, 系统.availableParallelism() - 1);
  let 实例 = null;
  const 取内存 = () => 模块内存(实例, 内存们);
  实例 = new WebAssembly.Instance(模块, 建导入(模块, 内存导入们, 内存们, 取内存, 写出, 线程数));
  const 工们 = [];
  if (线程数 > 0) {
    if (!内存们.some(内 => 内.buffer instanceof SharedArrayBuffer)) throw Error('底层宿主：导出「工作线程」的模块须导入共享内存');
    for (let 序 = 0; 序 < 线程数; 序++) {
      const 工 = new Worker(__filename, {workerData: {底层工作线程: true, 模块, 内存导入们, 内存们, 序号: 序 + 1, 线程数}});
      工.unref();
      工们.push(工);
    }
  }
  let 退出码 = 0;
  try {
    const 入口 = 实例.exports._start ?? 实例.exports.启动;
    if (typeof 入口 !== 'function') throw Error('底层宿主：模块没有导出 _start 或 启动');
    入口();
  } catch (错) {
    if (错.退出码 === undefined) throw 错;
    退出码 = 错.退出码;
  } finally {
    for (const 工 of 工们) 工.terminate();
  }
  return {模块, 退出码};
}

// 文言：工作线程：以同一模块同一内存实例化，调「工作线程」。汉语：工作线程：用同一模块与同一块内存实例化，调用导出的 工作线程(序号)，返回即结束。
if (!isMainThread && workerData?.底层工作线程) {
  const {模块, 内存导入们, 内存们, 序号, 线程数} = workerData;
  try {
    const 写出 = 值 => process.stdout.write(值);
    const 实例 = new WebAssembly.Instance(模块, 建导入(模块, 内存导入们, 内存们, () => 内存们[0], 写出, 线程数));
    实例.exports.工作线程(序号);
  } catch (错) {
    文件.writeSync(2, '底层工作线程 ' + 序号 + ' 出错：' + (错?.stack ?? 错) + '\n');
    process.kill(process.pid, 'SIGTERM');
  }
}

module.exports = {是底层模块, 运行底层模块, 读内存导入};
