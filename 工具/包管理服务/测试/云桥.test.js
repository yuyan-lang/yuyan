import assert from "node:assert/strict";
import test from "node:test";

globalThis.DurableObject ??= class {};

const {
  是包下载路径,
  是包数据上传路径,
  处理数据库,
  处理待发布对象,
  处理上传入口,
  处理固化,
  上传待发布包,
} = await import("../源码/桥接操作.js");

test("只把根目录下的 zip 当作匿名下载", () => {
  assert.equal(是包下载路径(new Request("https://example.test/%E8%B1%AB%E8%A8%80-2.0.zip")), "豫言-2.0.zip");
  assert.equal(是包下载路径(new Request("https://example.test/upload/prepare/豫言-2.0.zip", { method: "POST" })), null);
  assert.equal(是包下载路径(new Request("https://example.test/目录/豫言-2.0.zip")), null);
  assert.equal(是包下载路径(new Request("https://example.test/豫言-2.0.txt")), null);
});

test("D1 标量桥只返回第一列", async () => {
  const 环境 = {
    DB: {
      prepare(问辞) {
        assert.equal(问辞, "SELECT 1 AS 数;");
        return { first: async () => ({ 数: 1, 旁列: 2 }) };
      },
    },
  };
  const 回应 = await 处理数据库(new Request("http://database.internal/scalar", {
    method: "POST",
    body: "SELECT 1 AS 数;",
  }), 环境);
  assert.equal(回应.status, 200);
  assert.equal(await 回应.text(), "1");
});

test("D1 批量桥分拆后交给原子批次", async () => {
  const 已准备 = [];
  let 已批量 = null;
  const 环境 = {
    DB: {
      prepare(问辞) {
        已准备.push(问辞);
        return { 问辞 };
      },
      async batch(语句们) {
        已批量 = 语句们;
      },
    },
  };
  const 回应 = await 处理数据库(new Request("http://database.internal/batch", {
    method: "POST",
    body: "INSERT A;\n-- yy-next --\nDELETE B;",
  }), 环境);
  assert.equal(回应.status, 204);
  assert.deepEqual(已准备, ["INSERT A;", "DELETE B;"]);
  assert.deepEqual(已批量, [{ 问辞: "INSERT A;" }, { 问辞: "DELETE B;" }]);
});

test("上传入口返回同域数据路径，不需要 R2 凭据", async () => {
  const 摘要 = "a".repeat(64);
  const 回应 = await 处理上传入口(new Request("http://upload.internal/", {
    method: "POST",
    headers: {
      "X-Object-Key": `pending/${摘要}.zip`,
      "X-Object-SHA256": 摘要,
      "X-Object-Size": "12",
    },
  }), { MAX_PACKAGE_BYTES: "100" });
  assert.equal(回应.status, 200);
  assert.equal(await 回应.text(), `/upload/data/${摘要}.zip`);
});

test("只识别带六十四位摘要的 PUT 数据路径", () => {
  const 摘要 = "b".repeat(64);
  assert.equal(是包数据上传路径(new Request(`https://example.test/upload/data/${摘要}.zip`, { method: "PUT", body: "x", duplex: "half" })), 摘要);
  assert.equal(是包数据上传路径(new Request(`https://example.test/upload/data/${摘要}.zip`)), null);
  assert.equal(是包数据上传路径(new Request("https://example.test/upload/data/not-a-digest.zip", { method: "PUT", body: "x", duplex: "half" })), null);
});

test("同域上传先由豫言服务授权，再流式写入 R2 binding", async () => {
  const 摘要 = "d".repeat(64);
  const 数据 = new TextEncoder().encode("zip-data");
  let 已写入 = null;
  const 环境 = {
    MAX_PACKAGE_BYTES: "100",
    PACKAGE_CONTAINER: {
      getByName(名称) {
        assert.equal(名称, "豫言包管理");
        return {
          async fetch(请求) {
            assert.equal(new URL(请求.url).pathname, `/upload/authorize/${摘要}`);
            assert.equal(请求.headers.get("Authorization"), "Bearer package-token");
            assert.equal(请求.headers.get("X-Package-Size"), String(数据.byteLength));
            return new Response("");
          },
        };
      },
    },
    PACKAGES: {
      async put(键, 正文, 选项) {
        已写入 = { 键, 数据: new Uint8Array(await new Response(正文).arrayBuffer()), 选项 };
      },
      async head() {
        return { size: 数据.byteLength };
      },
    },
  };
  const 回应 = await 上传待发布包(new Request(`https://example.test/upload/data/${摘要}.zip`, {
    method: "PUT",
    headers: {
      Authorization: "Bearer package-token",
      "Content-Type": "application/zip",
      "Content-Length": String(数据.byteLength),
      "X-Package-SHA256": 摘要,
    },
    body: 数据,
    duplex: "half",
  }), 环境, 摘要);
  assert.equal(回应.status, 201);
  assert.equal(已写入.键, `pending/${摘要}.zip`);
  assert.deepEqual(已写入.数据, 数据);
  assert.equal(已写入.选项.customMetadata.sha256, 摘要);
});

test("production 中待发布对象以文本包封传给豫言", async () => {
  const 摘要 = "c".repeat(64);
  const 数据 = new TextEncoder().encode("zip\0bytes");
  const 回应 = await 处理待发布对象(new Request("http://packages.internal/", {
    headers: { "X-Object-Key": `pending/${摘要}.zip` },
  }), {
    PACKAGES: {
      async get() {
        return {
          size: 数据.byteLength,
          customMetadata: { sha256: 摘要 },
          async arrayBuffer() {
            return 数据.buffer;
          },
        };
      },
    },
  });
  assert.equal(回应.status, 200);
  assert.equal(await 回应.text(), `${数据.byteLength}\n${Buffer.from(数据).toString("base64")}`);
});

test("同一 production bucket 内把待发布对象固化为正式对象", async () => {
  const 数据 = new TextEncoder().encode("zip-data");
  const 摘要 = Array.from(new Uint8Array(await crypto.subtle.digest("SHA-256", 数据)), (值) => 值.toString(16).padStart(2, "0")).join("");
  let 已写入 = null;
  const 环境 = {
    PACKAGES: {
      async get() {
        return { size: 数据.byteLength, customMetadata: { sha256: 摘要 }, arrayBuffer: async () => 数据.buffer };
      },
      async put(键, 正文, 选项) {
        已写入 = { 键, 正文, 选项 };
      },
    },
  };
  const 回应 = await 处理固化(new Request("http://publish.internal/", {
    method: "POST",
    headers: {
      "X-Source-Key": `pending/${摘要}.zip`,
      "X-Destination-Key": `sha256/${摘要}.zip`,
      "X-Object-SHA256": 摘要,
    },
  }), 环境);
  assert.equal(回应.status, 204);
  assert.equal(已写入.键, `sha256/${摘要}.zip`);
  assert.deepEqual(new Uint8Array(已写入.正文), 数据);
});
