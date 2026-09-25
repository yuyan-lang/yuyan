import { 创建组装器 } from "./组装.mjs";
import { 创建编译器 } from "./编译核心.mjs";

// 文言：器常驻，客稿各编；主窗可随时止工。汉语：复用引擎模块；主线程负责超时终止，避免同步 Wasm 阻塞时无法响应停止消息。
async function 读取(名) {
  const 回 = await fetch(new URL("./资源/" + 名, import.meta.url), { cache: "no-cache" });
  if (!回.ok) throw Error("编译资源加载失败：" + 名);
  return 回;
}
const 就绪 = (async () => {
  if (typeof WebAssembly.promising !== "function") throw Error("此浏览器缺少 WebAssembly JSPI，请更新浏览器后重试");
  // 文言：先受客信，后候器成。汉语：Binaryen 含顶层 await，静态导入会延迟消息监听器安装并丢失浏览器发来的首个任务；改为动态加载。
  const { default: binaryen } = await import("./资源/组装器.mjs");
  const 组装 = 创建组装器(binaryen);
  const 清单 = await (await 读取("清单.json")).json();
  async function 取资源(名) {
    const 内容 = await (await 读取(名)).arrayBuffer();
    const 摘要 = await crypto.subtle.digest("SHA-256", 内容);
    const 串 = Array.from(new Uint8Array(摘要), 值 => 值.toString(16).padStart(2, "0")).join("");
    if (内容.byteLength !== 清单[名]?.bytes || 串 !== 清单[名]?.sha256) throw Error("编译资源版本不一致，请刷新页面");
    return 内容;
  }
  const [编译模块, 桥模块, 资料] = await Promise.all([
    取资源("编译器.wasm").then(字节 => WebAssembly.compile(字节)),
    取资源("值桥接.wasm").then(字节 => WebAssembly.compile(字节)),
    取资源("标准库.json.gz").then(字节 => new Response(new Response(字节).body.pipeThrough(new DecompressionStream("gzip"))).json())
  ]);
  return 创建编译器({ 编译模块, 桥模块, 资料, 组装 });
})();
// 文言：先记其败，待客来乃报。汉语：初始化可在任务到达前失败，先接住拒绝，实际请求仍能收到原始错误。
就绪.catch(() => {});
let 忙 = false;
self.onmessage = async ({ data }) => {
  if (忙) return;
  忙 = true;
  const { id, code, compileOnly } = data;
  const 报告 = 事件 => self.postMessage({ id, event: 事件 });
  try {
    报告({ type: "stage", phase: "load", label: "正在加载浏览器编译器与标准库" });
    const 编译 = await 就绪;
    self.postMessage({ id, result: await 编译(code, compileOnly, 报告) });
  } catch (错) { self.postMessage({ id, result: { ok: false, phase: "compile", error: 错.message } }); }
  finally { 忙 = false; }
};
self.postMessage({ ready: true });
