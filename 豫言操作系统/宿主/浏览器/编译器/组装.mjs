// 文言：只启既用之能。汉语：只启用用到的特性；禁止 All 意外启用实验性描述符等浏览器未支持的扩展。
// 文言：所入皆编器所书之二进制。汉语：输入是编译器直接写出的 Wasm 二进制（用户程序，以及编译器本身）。
// 文言：逐节察之，有名为「豫言边界」之自定段否。汉语：逐节扫描 Wasm 字节，看有没有名为「豫言边界」的自定义段（Binaryen.js 读不到自定义段，故直接读字节）。
const 边界段名字节 = new TextEncoder().encode("豫言边界");
function 有边界段(字节) {
  const 读数 = 位 => { let 值 = 0, 移 = 0, 字; do { 字 = 字节[位.值++]; 值 += (字 & 127) * 2 ** 移; 移 += 7; } while (字 & 128 && 移 < 35); return 值; };
  const 位 = { 值: 8 };
  while (位.值 < 字节.length) {
    const 节 = 字节[位.值++], 长 = 读数(位), 末 = 位.值 + 长;
    if (节 === 0) {
      const 名长 = 读数(位);
      if (名长 === 边界段名字节.length && 边界段名字节.every((字, 序) => 字节[位.值 + 序] === 字)) return true;
    }
    位.值 = 末;
  }
  return false;
}
export function 创建组装器(binaryen) {
  const 型 = binaryen.Features;
  const 特性 = 型.MutableGlobals | 型.NontrappingFPToInt | 型.BulkMemory | 型.SignExt |
    型.ExceptionHandling | 型.TailCall | 型.ReferenceTypes | 型.Multivalue | 型.GC | 型.BulkMemoryOpt;
  // 文言：豫言之客，有边界段者是。汉语：认豫言程序：看有没有自定义段「豫言边界」（编译器给每个 WasmGC 模块都写）。
  const 是豫言客体 = 字节 => 有边界段(字节);
  // 文言：未有承异者，则报其本辞。表增一格以容承异之函，起始先立之为当前承异者；其闭包为唯含表位之元组。
  // 汉语：浏览器宿主安装顶层字符串异常处理器，避免默认空处理器触发 illegal cast、掩盖编译诊断。函数表加一格放处理器，
  // _start 开头把它设为当前异常处理器（闭包是只含表位的 $tuple 元组）。依赖编译器写出的名字段找到 _start 与 $exception。
  function 装顶层承异(模块) {
    const 表 = 模块.getTableByIndex(0), 表息 = binaryen.getTableInfo(表), 位 = 表息.initial;
    if (模块.getNumElementSegments() !== 1) throw Error("不支持的编译器模块布局：元素段数目");
    const 段息 = binaryen.getElementSegmentInfo(模块.getElementSegmentByIndex(0));
    const 始 = 模块.getFunction("_start");
    if (段息.data.length !== 位 || !始 || !模块.getGlobal("exception")) throw Error("不支持的编译器模块布局：缺少函数表、_start 或 exception");
    const 等 = binaryen.eqref;
    binaryen.Table.setInitial(表, 位 + 1);
    if (表息.max !== undefined) binaryen.Table.setMax(表, 位 + 1);
    模块.addFunctionImport("browser_failure", "yuyan:browser/v1", "fail", binaryen.createType([等]), binaryen.none);
    模块.addFunction("browser_unhandled", binaryen.createType([等, 等]), 等, [], 模块.block(null, [
      模块.call("browser_failure", [模块.local.get(1, 等)], binaryen.none), 模块.unreachable()
    ], binaryen.unreachable));
    模块.removeElementSegment(段息.name);
    模块.addActiveElementSegment(表息.name, 段息.name, [...段息.data, "browser_unhandled"], 模块.i32.const(0));
    // 文言：同构之型即同型，故另造 (array (mut eqref)) 即 $tuple。汉语：同构的独立递归组类型相同，重新构造的数组类型就是运行时的 $tuple。
    const 建 = new binaryen.TypeBuilder(1);
    建.setArrayType(0, 等, binaryen.notPacked, true);
    const 元组 = binaryen.getTypeFromHeapType(建.buildAndDispose()[0], false);
    const 始息 = binaryen.getFunctionInfo(始);
    binaryen.Function.setBody(始, 模块.block(null, [
      模块.global.set("exception", 模块.array.new_fixed(元组, [模块.ref.i31(模块.i32.const(位))])), 始息.body
    ], 始息.results));
  }
  return function 组装(输入, 优化 = false) {
    let 模块, 阶段;
    try {
      阶段 = "Wasm 二进制读取";
      // 文言：读时即启诸能，否则引用之型或误作 anyref。汉语：读入时就启用这些特性；否则 Binaryen 按默认特性读，会把 eqref 等引用类型误读成 anyref，写回后不合法。
      模块 = binaryen.readBinary(输入, 特性);
      const 豫言客体 = 是豫言客体(输入);
      if (豫言客体) { 阶段 = "安装顶层异常处理器"; 装顶层承异(模块); }
      阶段 = "Binaryen 优化与函数枚举";
      if (优化) { binaryen.setOptimizeLevel(2); binaryen.setShrinkLevel(1); binaryen.setDebugInfo(true); 模块.optimize(); }
      const 函数们 = Array.from({ length: 模块.getNumFunctions() }, (_, 位) => 模块.getFunctionByIndex(位));
      if (豫言客体) {
        // 文言：千步一问时，死循环亦可止。汉语：V8 的纯尾调用循环可能延迟线程终止；定期进入 JS 宿主检查期限，同时提供引擎中断机会。
        const 检查 = "浏览器时限检查", 计数 = "浏览器步数", 步进 = "浏览器步进";
        模块.addFunctionImport(检查, "yuyan:browser/v1", "check", binaryen.none, binaryen.none);
        模块.addGlobal(计数, binaryen.i32, true, 模块.i32.const(1024));
        模块.addFunction(步进, binaryen.none, binaryen.none, [], 模块.block(null, [
          模块.global.set(计数, 模块.i32.sub(模块.global.get(计数, binaryen.i32), 模块.i32.const(1))),
          模块.if(模块.i32.eqz(模块.global.get(计数, binaryen.i32)), 模块.block(null, [
            模块.global.set(计数, 模块.i32.const(1024)), 模块.call(检查, [], binaryen.none)
          ]))
        ]));
        for (const 函数 of 函数们) {
          const 信息 = binaryen.getFunctionInfo(函数);
          if (!信息.module) binaryen.Function.setBody(函数, 模块.block(null, [模块.call(步进, [], binaryen.none), 信息.body], 信息.results));
        }
      }
      阶段 = "Wasm 二进制生成";
      const 结果 = 模块.emitBinary();
      if (!WebAssembly.validate(结果)) throw Error("当前浏览器不支持生成程序使用的 WasmGC、尾调用或异常指令");
      return 结果;
    } catch (错误) {
      // 文言：附其所处之段，原错仍存为因。汉语：让 Safari 的栈溢出能定位到组装步骤。
      throw new Error("浏览器组装失败（" + 阶段 + "）：" + 错误.message, { cause: 错误 });
    } finally { 模块?.dispose(); }
  };
}
