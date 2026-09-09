import { readFile, writeFile, mkdir, rm } from "node:fs/promises";
import { gzipSync } from "node:zlib";
import { createHash } from "node:crypto";
import binaryen from "binaryen";
import { 创建组装器 } from "../网页/编译/组装.mjs";
import { 创建编译器 } from "../网页/编译/编译核心.mjs";

// 文言：只接组装之器，不代编译之术。汉语：由豫言资源工具调用，固定输出位置，所有发布二进制均由 WAT 重建。
const [输入, 资料路径] = process.argv.slice(2);
if (!输入 || !资料路径) throw Error("请通过 yy浏览器编译资源 调用");
const 目录 = new URL("../网页/编译/资源/", import.meta.url);
await mkdir(目录, { recursive: true });
const 组装 = 创建组装器(binaryen);
const 产物 = {
  "编译器.wasm": 组装(await readFile(输入, "utf8"), true),
  "值桥接.wasm": 组装(await readFile(new URL("../网页/编译/值桥接.wat", import.meta.url), "utf8")),
  "标准库.json.gz": gzipSync(await readFile(资料路径), { level: 9 }),
  "组装器.mjs": await readFile(new URL("../node_modules/binaryen/index.js", import.meta.url)),
  "组装器许可.txt": await readFile(new URL("../node_modules/binaryen/LICENSE", import.meta.url))
};
// 文言：官书先编，客端惟编新稿。汉语：发布时预编译可信标准库，避免浏览器较小的栈在首次解析大型库源码时溢出。
const 资料 = JSON.parse(await readFile(资料路径, "utf8"));
const 预编译 = 创建编译器({
  编译模块: await WebAssembly.compile(产物["编译器.wasm"]),
  桥模块: await WebAssembly.compile(产物["值桥接.wasm"]), 资料, 组装,
  收取标准缓存: 缓存 => {
    for (const [名, 项] of Object.entries(缓存)) 资料[名] = { 内容: new TextDecoder().decode(项.内容), 时间: 项.时间 };
  }
});
const 预热结果 = await 预编译("寻观「标准库」之书。「打印行」于『标准库预编译』。", true);
if (!预热结果.ok) throw Error("标准库预编译失败：" + JSON.stringify(预热结果));
产物["标准库.json.gz"] = gzipSync(JSON.stringify(资料), { level: 9 });
const 清单 = {};
for (const [名, 字节] of Object.entries(产物)) {
  await writeFile(new URL(名, 目录), 字节);
  清单[名] = { bytes: 字节.length, sha256: createHash("sha256").update(字节).digest("hex") };
}
await writeFile(new URL("清单.json", 目录), JSON.stringify(清单, null, 2) + "\n");
await rm(new URL("编译器精简.wasm", 目录), { force: true });
console.log(JSON.stringify(清单));
