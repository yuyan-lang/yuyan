import { 内存文件系统, 执行模块 } from "./宿主.mjs";

// 文言：编与行各有其室，旧稿不入新室。汉语：每次请求创建编译文件系统；运行程序只能访问自己的空文件系统。
export function 创建编译器({ 编译模块, 桥模块, 资料, 组装, 收取标准缓存 = () => {} }) {
  let 标准缓存 = {};
  return async function 编译运行(code, 仅编译 = false, 报告 = () => {}) {
    const 项目=code&&typeof code==='object'?code.files:null;
    if(!项目&&(typeof code!=="string"||new TextEncoder().encode(code).length>24000||!code.trim()))return {ok:false,phase:'compile',error:'源码须为 1 至 24000 字节'};
    if(项目&&(Array.isArray(项目)||!Object.hasOwn(项目,'入口。豫')||Object.keys(项目).length>64||Object.entries(项目).some(([p,t])=>typeof t!=='string'||p.startsWith('/')||p.split('/').some(x=>!x||x==='.'||x==='..')||p.includes('\\'))||new TextEncoder().encode(JSON.stringify(项目)).length>524288))return {ok:false,phase:'compile',error:'云项目文件无效'};
    const 文件 = new 内存文件系统({ ...资料, ...标准缓存 });
    if(项目){
      文件.删('/用户程序/用户程序。包。豫');
      文件.写('/包上下文','豫构包上下文二\n豫言\t标准库\t/库/标准库/标准库。包。豫\n访客\t云项目\t/用户程序/云项目。包。豫\t豫言\t标准库');
      for(const [p,t] of Object.entries(项目))文件.写('/用户程序/'+p,t);
    }else 文件.写('/用户程序/入口。豫',code);
    报告({ type: "stage", phase: "compile", label: "正在浏览器内编译" });
    const compilation = await 执行模块(编译模块, 桥模块, 文件, [
      "/用户程序/入口。豫", "--package-context", "/包上下文", "--target=wasmgc", "-o", "/程序.wasm",
      // 文言：验编亦须成器。汉语：compile_yuyan 执行完整代码生成和组装，只略过运行。
    ], { 编译: true, 组装, 报告 });
    if (!compilation.ok) return { ...compilation, phase: "compile" };
    // 文言：官书之缓存可复用，客稿与输出皆弃。汉语：只保留标准库派生文件及原时间戳，每个请求仍使用独立文件系统。
    标准缓存 = Object.fromEntries([...文件.文件].filter(([名]) => 名.startsWith("/.yybuild/") && 名.includes("/库/标准库/")));
    收取标准缓存(标准缓存);
    if (仅编译){const bytes=文件.读('/程序.wasm');const sha256=Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256',bytes)),x=>x.toString(16).padStart(2,'0')).join('');return {...compilation,phase:'compile',artifact:{bytes:bytes.length,sha256}};}
    报告({ type: "stage", phase: "run", label: "编译通过，正在浏览器内运行" });
    try {
      const 模块 = new WebAssembly.Module(文件.读("/程序.wasm"));
      return { ...await 执行模块(模块, 桥模块, new 内存文件系统(), [], { 报告 }), phase: "run", compilation };
    } catch (错) { return { ok: false, phase: "run", error: 错.message, compilation }; }
  };
}
