// 文言：系统库调用之原语：以诺节之 node:ffi 开动态之库、取符号之址、依签名而调、读写原生之存；惟供适配与系统库之用，不入公共之接口。
// 汉语：路一诺节宿主的系统库调用原语（外调名以 豫言_节点_外部库 开头）：基于诺节的 node:ffi 打开动态库、取符号地址、按签名调用、读写原生内存。只供适配与系统库（如 库/系统库调用、库/金属）使用，不进公共接口。
// 文言：此模无所引，发行时内联于启动文件；node:ffi 初用乃载，须诺节以 --experimental-ffi 起（宿主.cjs 为客之工作线程加之；应用宿主.mjs 见程序用此原语则带之自重启）。
// 汉语：本模块不写 import：发行启动文件把它内联进一个函数作用域。node:ffi 在第一次用到时经 process.getBuiltinModule 载入，要求诺节带 --experimental-ffi 启动：宿主.cjs 给客体工作线程加上此旗；应用宿主.mjs 发现程序用到这些原语时带此旗重启自身。
// 文言：签名之书：返型一字，全角冒号，次以参型，一参一字；无：无返，爻：布尔，字：八位无符号，整：三十二位有符号，长：六十四位有符号，址：指针，单：单精，双：双精。
// 汉语：签名文字：先写一个返回类型字，再写全角冒号“：”，后面每个参数一个类型字。类型字：无（无返回，只用于返回）、爻（布尔）、字（八位无符号）、整（三十二位有符号）、长（六十四位有符号）、址（指针）、单（单精度浮点）、双（双精度浮点）。例：『址：址址长』。
// 汉语：待办事项：结构体按值传递与返回（arm64 上大于 16 字节的结构体由调用者传指向副本的指针，可用“址”；小结构体按值、x86-64 上的结构体均未支持）；回调（node:ffi 的回调只能在创建它的线程上被调用，金属的完成回调在其内部线程上，故一律用同步等待）；可变参数函数；任意函数指针（只认经“取符号”登记过的地址，可改用 dladdr 反查或 libffi 的 ffi_call）；视窗（libc 不同，分配原语未适配）。

const 类型字表 = {无: 'void', 爻: 'bool', 字: 'uint8', 整: 'int32', 长: 'int64', 址: 'pointer', 单: 'float', 双: 'double'};

// 文言：C 库之名：苹果与 Linux 皆可由主程之象（dlopen 空）得之。汉语：C 库函数（posix_memalign、free、memmove、memset）从进程主映像（dlopen(NULL)）取得，macOS 与 Linux 都可用。
const 主映像 = null;

// 文言：允许：工具宿主恒许；应用宿主惟 --允许系统库调用 乃许，不许则如不可用。汉语：参数 允许：工具宿主总是允许；应用宿主只在给了 --允许系统库调用 时允许，不允许时各原语如同 node:ffi 不可用。
export function 创建外部库能力({文字, 取张量存储 = null, 允许 = true}) {
  // 文言：未试为 undefined，不可用为 null。汉语：node:ffi 模块：undefined 表示尚未尝试载入，null 表示不可用。
  let 模块 = undefined;
  const 载入 = () => {
    if (!允许) return null;
    if (模块 !== undefined) return 模块;
    // 文言：载时发“实验之能”之警一次，惟拦此条，免乱工具之标准错误。汉语：node:ffi 载入时发出一次“实验特性”警告；只拦这一条，免得扰乱工具的标准错误输出。
    const 原发 = process.emitWarning;
    process.emitWarning = function (警告, ...余) {
      if (/\bFFI\b/u.test(String(警告?.message ?? 警告))) return undefined;
      return 原发.call(this, 警告, ...余);
    };
    try { 模块 = process.getBuiltinModule?.('node:ffi') ?? null; } catch { 模块 = null; } finally { process.emitWarning = 原发; }
    return 模块;
  };
  const 须 = () => 载入() ?? (() => {
    throw Error(允许 ? '系统库调用不可用：诺节须支持 node:ffi 并以 --experimental-ffi 启动' : '系统库调用未获授权：应用宿主须给 --允许系统库调用');
  })();
  const 数 = 值 => (typeof 值 === 'object' && 值 !== null ? Number(值.小数) : Number(值));
  const 整 = 值 => (typeof 值 === 'bigint' ? 值 : BigInt(Math.trunc(数(值))));
  const 无号 = 值 => BigInt.asUintN(64, 整(值));
  const 有号 = 值 => BigInt.asIntN(64, BigInt(值));

  // 文言：库号自一始；同径同号。汉语：库号从 1 起顺序分配，同一路径复用同一个号；库对象留在表里，不被回收。
  const 库表 = new Map();
  const 路径号 = new Map();
  let 下库号 = 1n;
  // 文言：址之来历：取符号时记其库径与名，调时据以造函。汉语：地址来历表：取符号时记下（库路径，符号名），调用时据此创建函数对象。
  const 符号表 = new Map();
  // 文言：一库之象，同名惟许一签，故每签各开一象。汉语：node:ffi 的同一个库对象里同一符号名只能取一种签名，所以每种（库路径，签名）各开一个库对象。
  const 签名库 = new Map();
  // 文言：按“址＋签名”存函。汉语：按“地址＋签名”缓存函数对象与参数、返回类型。
  const 函数表 = new Map();

  const 解签名 = 签名 => {
    const 段 = 签名.split('：');
    const 转 = 字 => 类型字表[字] ?? (() => { throw Error('系统库调用：签名里有未知类型字「' + 字 + '」：' + 签名); })();
    if (段.length !== 2 || [...段[0]].length !== 1) throw Error('系统库调用：签名须为“返回类型字：参数类型字…”：' + 签名);
    const 参型 = [...段[1]].map(转);
    if (参型.includes('void')) throw Error('系统库调用：参数类型不能是“无”：' + 签名);
    return {返型: 转(段[0]), 参型};
  };
  const 造函数 = (址, 签名, 键) => {
    const 块 = 须();
    const 源 = 符号表.get(址) ?? (() => { throw Error('系统库调用：地址未经取符号登记：' + 址); })();
    const {返型, 参型} = 解签名(签名);
    const 库键 = (源.路径 ?? '') + '\0' + 签名;
    let 库 = 签名库.get(库键);
    if (!库) { 库 = new 块.DynamicLibrary(源.路径); 签名库.set(库键, 库); }
    const 项 = {函: 库.getFunction(源.名, {arguments: 参型, return: 返型}), 参型, 返型};
    函数表.set(键, 项);
    return 项;
  };
  const 转参 = (型, 值) => {
    switch (型) {
      case 'pointer': return 无号(值);
      case 'int64': return BigInt.asIntN(64, 整(值));
      case 'int32': return Number(BigInt.asIntN(32, 整(值)));
      case 'uint8': case 'bool': return Number(BigInt.asUintN(8, 整(值)));
      default: return 数(值);
    }
  };
  const 转果 = (型, 果) => {
    switch (型) {
      case 'void': return null;
      case 'float': case 'double': return {小数: 果};
      default: return 有号(果);
    }
  };

  // 文言：C 库诸函，初用乃取。汉语：内存原语所用的 C 库函数，第一次用到时取得。
  let C库 = null;
  const C = () => {
    if (C库) return C库;
    const 库 = new (须().DynamicLibrary)(主映像);
    C库 = {
      对齐分配: 库.getFunction('posix_memalign', {arguments: ['pointer', 'uint64', 'uint64'], return: 'int32'}),
      释放: 库.getFunction('free', {arguments: ['pointer'], return: 'void'}),
      移动: 库.getFunction('memmove', {arguments: ['pointer', 'pointer', 'uint64'], return: 'pointer'}),
      填: 库.getFunction('memset', {arguments: ['pointer', 'int32', 'uint64'], return: 'pointer'})
    };
    return C库;
  };
  const 槽 = new BigUint64Array(1);

  // 文言：读写之类字：字、整、长、址归整数，单、双归小数。汉语：读写原语的类型字：字、整、长、址对应整数，单、双对应小数。
  const 读整 = {字: (块, 址) => BigInt(块.getUint8(址, 0)), 整: (块, 址) => BigInt(块.getInt32(址, 0)),
    长: (块, 址) => 块.getInt64(址, 0), 址: (块, 址) => 有号(块.getUint64(址, 0))};
  const 写整 = {字: (块, 址, 值) => 块.setUint8(址, 0, Number(BigInt.asUintN(8, 值))),
    整: (块, 址, 值) => 块.setInt32(址, 0, Number(BigInt.asIntN(32, 值))),
    长: (块, 址, 值) => 块.setInt64(址, 0, BigInt.asIntN(64, 值)), 址: (块, 址, 值) => 块.setUint64(址, 0, BigInt.asUintN(64, 值))};
  const 取类字 = (表, 值) => 表[文字(值)] ?? (() => { throw Error('系统库调用：读写类型字无效：' + 文字(值)); })();

  return {
    // 文言：可用否：诺节能载 node:ffi 则阳。汉语：能载入 node:ffi 时为真；诺节未带 --experimental-ffi、版本过旧或构建不含外部函数接口时为假。
    豫言_节点_外部库可用: () => 载入() !== null,
    // 文言：开库：返（码，库号，消息）；码零为成，一为未许，二为不可用，四为库不存。汉语：打开动态库，返回（状态码，库号，消息）：0 成功；1 未获授权（应用宿主没给 --允许系统库调用）；2 node:ffi 不可用；4 打开失败（消息是 dlopen 的错误）。路径为空串时打开进程主映像（可见已载入的全部全局符号）。
    豫言_节点_外部库打开: 路径值 => {
      const 块 = 载入();
      if (!块) return 允许 ? [2n, 0n, 'node:ffi 不可用'] : [1n, 0n, '未获授权：应用宿主须给 --允许系统库调用'];
      const 路径 = 文字(路径值) || 主映像;
      const 已有 = 路径号.get(路径);
      if (已有 !== undefined) return [0n, 已有, ''];
      try {
        const 号 = 下库号++;
        库表.set(号, {路径, 库: new 块.DynamicLibrary(路径)});
        路径号.set(路径, 号);
        return [0n, 号, ''];
      } catch (错) { return [4n, 0n, String(错?.message ?? 错)]; }
    },
    // 文言：取符号之址，不得则零。汉语：取符号地址，找不到返回 0；库号无效时抛出异常。
    豫言_节点_外部库取符号: (号, 名值) => {
      const 项 = 库表.get(BigInt(号)) ?? (() => { throw Error('系统库调用：库号无效：' + 号); })();
      const 名 = 文字(名值);
      let 址;
      try { 址 = 项.库.getSymbol(名); } catch { return 0n; }
      if (!址) return 0n;
      符号表.set(址, {路径: 项.路径, 名});
      return 有号(址);
    },
    // 文言：依签名调址所指之函；参组之数须合签名。汉语：按签名调用地址处的函数：参数组是数组（整数或小数，按签名转换），个数须与签名一致；按“地址＋签名”缓存函数对象。返回整数（指针也作整数，高位为一时是负数）、小数或元。
    豫言_节点_外部库调用: (址值, 签名值, 参数组) => {
      const 址 = 无号(址值);
      const 签名 = 文字(签名值);
      const 键 = 址 + '\0' + 签名;
      const 项 = 函数表.get(键) ?? 造函数(址, 签名, 键);
      const 参们 = Array.isArray(参数组) ? 参数组 : [];
      if (参们.length !== 项.参型.length) throw Error('系统库调用：参数个数不合签名（' + 签名 + '）：' + 参们.length);
      const 实参 = new Array(参们.length);
      for (let 序 = 0; 序 < 参们.length; 序++) 实参[序] = 转参(项.参型[序], 参们[序]);
      return 转果(项.返型, 项.函(...实参));
    },
    // 文言：分原生之存，零之；败则返零。汉语：分配原生内存（posix_memalign，按给定对齐，至少 8 字节对齐）并清零；失败返回 0。须以释放原语归还。
    豫言_节点_外部库分配: (字节值, 对齐值) => {
      const 字节 = 无号(字节值), 对齐 = 无号(对齐值) < 8n ? 8n : 无号(对齐值);
      const 块 = 须();
      if (C().对齐分配(块.getRawPointer(槽), 对齐, 字节 === 0n ? 1n : 字节) !== 0) return 0n;
      const 址 = 槽[0];
      C().填(址, 0, 字节);
      return 有号(址);
    },
    豫言_节点_外部库释放: 址值 => { 须(); C().释放(无号(址值)); return null; },
    // 文言：读写一值于址；类型字见上。汉语：在地址处读写一个值，类型字：字、整、长、址（整数），单、双（小数）。地址为零时抛出异常。
    豫言_节点_外部库读整数: (址值, 类值) => 取类字(读整, 类值)(须(), 无号(址值)),
    豫言_节点_外部库写整数: (址值, 类值, 值) => { 取类字(写整, 类值)(须(), 无号(址值), 整(值)); return null; },
    豫言_节点_外部库读小数: (址值, 类值) => {
      const 类 = 文字(类值), 块 = 须();
      if (类 === '单') return {小数: 块.getFloat32(无号(址值), 0)};
      if (类 === '双') return {小数: 块.getFloat64(无号(址值), 0)};
      throw Error('系统库调用：读小数的类型字须为 单 或 双：' + 类);
    },
    豫言_节点_外部库写小数: (址值, 类值, 值) => {
      const 类 = 文字(类值), 块 = 须();
      if (类 === '单') 块.setFloat32(无号(址值), 0, 数(值));
      else if (类 === '双') 块.setFloat64(无号(址值), 0, 数(值));
      else throw Error('系统库调用：写小数的类型字须为 单 或 双：' + 类);
      return null;
    },
    // 文言：原生之存与字节串相转，各复制之。汉语：原生内存与字节串互相复制（单次不超过宿主值桥上限 16 MiB）；复制原语在原生内存之间搬移（允许重叠）。
    豫言_节点_外部库读字节: (址值, 数值) => {
      const 长 = Number(数值);
      if (长 === 0) return Buffer.alloc(0);
      return Buffer.from(须().toArrayBuffer(无号(址值), 长, true));
    },
    豫言_节点_外部库写字节: (址值, 字节) => {
      if (字节.length > 0) 须().exportArrayBufferView(字节, 无号(址值), 字节.length);
      return null;
    },
    豫言_节点_外部库复制: (目标值, 源值, 数值) => { 须(); C().移动(无号(目标值), 无号(源值), 无号(数值)); return null; },
    // 文言：读 C 之文（以零终，UTF-8）。汉语：读以零结尾的 UTF-8 C 字符串；地址为零时返回空串。
    豫言_节点_外部库读文字: 址值 => (无号(址值) === 0n ? '' : 须().toString(无号(址值))),
    // 文言：张量之存之址：返（码，址，字节数）；张量不存则码三。惟应用宿主有张量。汉语：取张量存储（SharedArrayBuffer）的原生地址与字节数，返回（状态码，地址，字节数）：0 成功；3 张量无效或已释放，或宿主没有张量（工具宿主）。地址只在张量存活期间有效：调用者须先让原生一方（如金属缓冲）放手，再释放张量。
    豫言_节点_外部库张量存储: 号 => {
      const 缓 = 取张量存储?.(号) ?? null;
      if (!缓) return [3n, 0n, 0n];
      return [0n, 有号(须().getRawPointer(缓)), BigInt(缓.byteLength)];
    }
  };
}
