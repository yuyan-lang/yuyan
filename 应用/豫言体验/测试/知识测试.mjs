import { test } from "node:test";
import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";
import { 构建知识 } from "../源码/知识.mjs";

// 古曰：书有增删，册须相应。今释：真实目录必须覆盖所有标准库源码模块，测试文件不作为库模块。
test("签名目录覆盖标准库所有模块且分页不丢失声明", async () => {
  const 根 = new URL("../../../库/", import.meta.url);
  const 遍历 = async 相对 => {
    const 项们 = await readdir(new URL(相对, 根), { withFileTypes: true });
    return (await Promise.all(项们.filter(项 => !项.name.endsWith("_v0")).map(async 项 => {
      const 路径 = 相对 + 项.name;
      return 项.isDirectory() ? 遍历(路径 + "/") : 路径.endsWith("。豫") && !路径.endsWith("。测试。豫") && !路径.endsWith("。包。豫") ? [路径] : [];
    }))).flat();
  };
  const 目录 = JSON.parse(await readFile(new URL("../知识/标准库签名.json", import.meta.url), "utf8"));
  assert.deepEqual(目录.模块.map(项 => 项.路径).sort(), (await 遍历("标准库/")).sort());
  const 知识 = 构建知识("指南", 目录, {});
  let offset = 0; const 全部 = [];
  do { const 页 = 知识.查询({ offset, limit: 80 }); 全部.push(...页.entries); offset = 页.nextOffset; } while (offset !== null);
  assert.equal(全部.length, 知识.签名数);
  assert.ok(全部.some(项 => 项.name === "整数表示" && 项.signature.includes("字符串")));
  assert.ok(全部.every(项 => !项.signature.includes("未知类型") && !项.signature.includes("/Users/")));
  for (const 项 of 全部) assert.ok(知识.提示.includes(`「${项.name}」：${项.signature}`));
});
