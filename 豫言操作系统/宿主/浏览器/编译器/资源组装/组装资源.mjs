import { readFile, writeFile, mkdir, rm } from "node:fs/promises";
import { gzipSync } from "node:zlib";
import { createHash } from "node:crypto";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";
import { 创建编译器 } from "../编译核心.mjs";

// 文言：只接成器之资，不代编译之术；编器与值桥皆豫言之器径书，原样发之。汉语：由 浏览器编译资源 调用，固定参数位置；输入编译器 Wasm（构建时带编译器参数 --浏览器检查点，编译器直接写出顶层异常处理器与时限检查点）、标准库资料与值桥 Wasm（浏览器编译资源 按编译器核心的结构化表示写出），输出发布资源和校验清单；编译器与值桥都原样发布。
// 文言：出目恒取末参；旧器多传一径于其前，亦不误书。汉语：输出目录总取最后一个参数：各处还没重建的旧版 浏览器编译资源 会在它前面多传一个 Binaryen 目录（且不检查本脚本成败），这样也不会把资源写错地方。
const 参数 = process.argv.slice(2);
const [输入, 资料路径, 值桥路径] = 参数, 输出目录 = 参数.at(-1);
if (参数.length < 4) throw Error("请通过 浏览器编译资源 调用");
const 目录 = pathToFileURL(resolve(输出目录) + "/");
await mkdir(目录, { recursive: true });
const 编译器字节 = await readFile(输入);
// 文言：编器无检点者，警之。汉语：编译器没带浏览器检查点时只警告：浏览器与云端编译服务都没法按时限打断编译，但仍可用。
if (!WebAssembly.Module.imports(await WebAssembly.compile(编译器字节)).some(项 => 项.module === "yuyan:browser/v1" && 项.name === "check"))
  console.error("警告：编译器 Wasm 没有浏览器检查点；构建编译器时请加编译器参数 --浏览器检查点");
const 产物 = {
  "编译器.wasm": 编译器字节,
  "值桥接.wasm": await readFile(值桥路径),
  "标准库.json.gz": gzipSync(await readFile(资料路径), { level: 9 })
};
// 文言：官书先编，客端惟编新稿。汉语：发布时预编译可信标准库，避免浏览器较小的栈在首次解析大型库源码时溢出。
const 资料 = JSON.parse(await readFile(资料路径, "utf8"));
const 预编译 = 创建编译器({
  编译模块: await WebAssembly.compile(产物["编译器.wasm"]),
  桥模块: await WebAssembly.compile(产物["值桥接.wasm"]), 资料,
  收取标准缓存: 缓存 => {
    for (const [名, 项] of Object.entries(缓存)) 资料[名] = { 内容: new TextDecoder().decode(项.内容), 时间: 项.时间 };
  }
});
const 预热结果 = await 预编译("寻观「豫章」之书。「书」于『标准库预编译』。", true);
if (!预热结果.ok) throw Error("标准库预编译失败：" + JSON.stringify(预热结果));
产物["标准库.json.gz"] = gzipSync(JSON.stringify(资料), { level: 9 });
const 清单 = {};
for (const [名, 字节] of Object.entries(产物)) {
  await writeFile(new URL(名, 目录), 字节);
  清单[名] = { bytes: 字节.length, sha256: createHash("sha256").update(字节).digest("hex") };
}
await writeFile(new URL("清单.json", 目录), JSON.stringify(清单, null, 2) + "\n");
// 文言：旧版所出而今不用者，删之。汉语：删掉旧版产出、现已不用的文件（编译器精简版与 Binaryen 胶水及其许可）。
for (const 旧 of ["编译器精简.wasm", "组装器.mjs", "组装器许可.txt"]) await rm(new URL(旧, 目录), { force: true });
console.log(JSON.stringify(清单));
