// 文言：只启既用之能，中文字先化字节。汉语：Binaryen 文本入口接收字节字符串；禁止 All 意外启用实验性描述符等浏览器未支持的扩展。
export function 创建组装器(binaryen) {
  const 型 = binaryen.Features;
  const 特性 = 型.MutableGlobals | 型.NontrappingFPToInt | 型.BulkMemory | 型.SignExt |
    型.ExceptionHandling | 型.TailCall | 型.ReferenceTypes | 型.Multivalue | 型.GC | 型.BulkMemoryOpt;
  return function 组装(文, 优化 = false) {
    if (文.includes('(import "yuyan:gc-host/v1" "call"')) {
      // 文言：未有承异者，则报其本辞。汉语：浏览器宿主安装顶层字符串异常处理器，避免默认空处理器触发 illegal cast、掩盖编译诊断。
      const 表 = 文.match(/\(table (\d+) funcref\)/);
      if (!表 || !文.includes('(func (export "_start")')) throw Error("不支持的编译器模块布局");
      const 位 = Number(表[1]);
      文 = 文.replace("(module", '(module\n(import "yuyan:browser/v1" "fail" (func $browser_failure (param (ref null eq))))')
        .replace(表[0], `(table ${位 + 1} funcref)`)
        .replace(/\(elem \(i32.const 0\) ([^)]*)\)/, '(elem (i32.const 0) $1 $browser_unhandled)')
        .replace('(func (export "_start")', `(func $browser_unhandled (type $t2) (param (ref null eq)) (param (ref null eq)) (result (ref null eq)) local.get 1 call $browser_failure unreachable)\n(func (export "_start") i32.const ${位} ref.i31 array.new_fixed $tuple 1 global.set $exception`);
    }
    const 字节 = new TextEncoder().encode(文), 片段 = [];
    for (let 位 = 0; 位 < 字节.length; 位 += 8192) 片段.push(String.fromCharCode(...字节.subarray(位, 位 + 8192)));
    const 模块 = binaryen.parseText(片段.join(""), 特性);
    try {
      if (优化) { binaryen.setOptimizeLevel(2); binaryen.setShrinkLevel(1); binaryen.setDebugInfo(true); 模块.optimize(); }
      const 函数们 = Array.from({ length: 模块.getNumFunctions() }, (_, 位) => 模块.getFunctionByIndex(位));
      if (函数们.some(函数 => binaryen.getFunctionInfo(函数).module === "yuyan:gc-host/v1")) {
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
      const 结果 = 模块.emitBinary();
      if (!WebAssembly.validate(结果)) throw Error("当前浏览器不支持生成程序使用的 WasmGC、尾调用或异常指令");
      return 结果;
    } finally { 模块.dispose(); }
  };
}
