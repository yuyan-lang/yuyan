// 古曰：内桥不问包法，惟以受限之辞驱云器。
// 今释：这些处理器是可单测的能力桥；命名、版本、认证和发布规则全部留在豫言服务。

const 最大查询字节数 = 65536;
const 上传键格式 = /^pending\/[0-9a-f]{64}\.zip$/;
const 正式键格式 = /^sha256\/[0-9a-f]{64}\.zip$/;

export function 回应文字(文字, 状态 = 200, 附加标头 = {}) {
  const 正文 = new TextEncoder().encode(文字);
  return new Response(正文, {
    status: 状态,
    headers: {
      "Content-Type": "text/plain; charset=utf-8",
      "Content-Length": String(正文.byteLength),
      "X-Content-Type-Options": "nosniff",
      ...附加标头,
    },
  });
}

export function 回应错误(状态, 消息) {
  return 回应文字(消息, 状态, { "Cache-Control": "no-store" });
}

async function 读取有限文字(请求, 上限 = 最大查询字节数) {
  const 长度 = Number(请求.headers.get("Content-Length") ?? "0");
  if (Number.isFinite(长度) && 长度 > 上限) throw new Error("请求正文过大");
  const 文字 = await 请求.text();
  if (new TextEncoder().encode(文字).byteLength > 上限) throw new Error("请求正文过大");
  return 文字;
}

function 取标量值(记录) {
  if (!记录) return null;
  const 值们 = Object.values(记录);
  return 值们.length === 0 || 值们[0] === null ? "" : String(值们[0]);
}

export async function 处理数据库(请求, 环境) {
  if (请求.method !== "POST") return 回应错误(405, "数据库桥只接受 POST");
  const 路径 = new URL(请求.url).pathname;
  try {
    const 查询 = await 读取有限文字(请求);
    if (路径 === "/execute") {
      await 环境.DB.exec(查询);
      return new Response(null, { status: 204 });
    }
    if (路径 === "/scalar") {
      const 记录 = await 环境.DB.prepare(查询).first();
      const 值 = 取标量值(记录);
      return 值 === null ? new Response(null, { status: 404 }) : 回应文字(值);
    }
    if (路径 === "/batch") {
      const 分隔符 = "\n-- yy-next --\n";
      const 问辞们 = 查询.split(分隔符).map((问辞) => 问辞.trim()).filter(Boolean);
      if (问辞们.length < 1 || 问辞们.length > 16) return 回应错误(400, "批量问辞数无效");
      await 环境.DB.batch(问辞们.map((问辞) => 环境.DB.prepare(问辞)));
      return new Response(null, { status: 204 });
    }
    return 回应错误(404, "数据库桥路由不存在");
  } catch (错误) {
    return 回应错误(500, 错误 instanceof Error ? 错误.message : String(错误));
  }
}

export async function 处理摘要(请求) {
  if (请求.method !== "POST") return 回应错误(405, "摘要桥只接受 POST");
  const 数据 = await 请求.arrayBuffer();
  if (数据.byteLength > 4096) return 回应错误(413, "摘要输入过大");
  const 摘要 = new Uint8Array(await crypto.subtle.digest("SHA-256", 数据));
  return 回应文字(Array.from(摘要, (值) => 值.toString(16).padStart(2, "0")).join(""));
}

function 取安全对象键(请求, 格式) {
  const 对象键 = 请求.headers.get("X-Object-Key") ?? "";
  return 格式.test(对象键) ? 对象键 : null;
}

function 字节转六十四进制(数据) {
  const 字节们 = new Uint8Array(数据);
  const 分块们 = [];
  for (let 起点 = 0; 起点 < 字节们.length; 起点 += 32768) {
    分块们.push(String.fromCharCode(...字节们.subarray(起点, 起点 + 32768)));
  }
  return btoa(分块们.join(""));
}

export async function 处理待发布对象(请求, 环境) {
  const 对象键 = 取安全对象键(请求, 上传键格式);
  if (!对象键) return 回应错误(400, "待发布对象键无效");
  if (请求.method !== "GET" && 请求.method !== "HEAD") return 回应错误(405, "待发布对象桥只接受 GET 或 HEAD");
  const 对象 = 请求.method === "HEAD" ? await 环境.PACKAGES.head(对象键) : await 环境.PACKAGES.get(对象键);
  if (!对象) return new Response(null, { status: 404 });
  const 最大字节数 = Number(环境.MAX_PACKAGE_BYTES ?? "10485760");
  if (对象.size > 最大字节数) return 回应错误(413, "待发布对象超过包大小上限");
  if (请求.method === "HEAD") return new Response(null, {
    status: 200,
    headers: { "Content-Length": String(对象.size), "X-Object-SHA256": 对象.customMetadata?.sha256 ?? "" },
  });
  const 编码 = 字节转六十四进制(await 对象.arrayBuffer());
  return 回应文字(`${对象.size}\n${编码}`);
}

export async function 处理上传入口(请求, 环境) {
  if (请求.method !== "POST") return 回应错误(405, "上传入口桥只接受 POST");
  const 对象键 = 取安全对象键(请求, 上传键格式);
  const 摘要 = 请求.headers.get("X-Object-SHA256") ?? "";
  const 字节数 = Number(请求.headers.get("X-Object-Size") ?? "0");
  const 最大字节数 = Number(环境.MAX_PACKAGE_BYTES ?? "10485760");
  if (!对象键 || !/^[0-9a-f]{64}$/.test(摘要) || !Number.isSafeInteger(字节数) || 字节数 < 1 || 字节数 > 最大字节数) {
    return 回应错误(400, "上传签名参数无效");
  }
  if (!对象键.includes(摘要)) return 回应错误(400, "对象键与摘要不一致");
  return 回应文字(`/upload/data/${摘要}.zip`);
}

export function 是包数据上传路径(请求) {
  if (请求.method !== "PUT") return null;
  const 匹配 = new URL(请求.url).pathname.match(/^\/upload\/data\/([0-9a-f]{64})\.zip$/);
  return 匹配?.[1] ?? null;
}

export async function 上传待发布包(请求, 环境, 摘要) {
  const 最大字节数 = Number(环境.MAX_PACKAGE_BYTES ?? "10485760");
  const 字节数 = Number(请求.headers.get("Content-Length") ?? "0");
  const 声明摘要 = 请求.headers.get("X-Package-SHA256");
  const 种类 = (请求.headers.get("Content-Type") ?? "").split(";", 1)[0].trim().toLowerCase();
  if (!Number.isSafeInteger(字节数) || 字节数 < 1 || 字节数 > 最大字节数) {
    return 回应错误(400, "Content-Length 须为包大小上限内的正整数");
  }
  if (种类 !== "application/zip") return 回应错误(415, "上传数据须为 application/zip");
  if (请求.headers.has("Content-Encoding")) return 回应错误(400, "上传数据不得使用 Content-Encoding");
  if (声明摘要 !== null && 声明摘要 !== 摘要) return 回应错误(400, "路径摘要与声明摘要不一致");
  if (请求.body === null) return 回应错误(400, "上传数据不得为空");

  const 授权标头 = new Headers({
    "X-Yuyan-Path": `/upload/authorize/${摘要}`,
    "X-Package-SHA256": 摘要,
    "X-Package-Size": String(字节数),
  });
  const 持有令牌 = 请求.headers.get("Authorization");
  if (持有令牌 !== null) 授权标头.set("Authorization", 持有令牌);
  const 容器 = 环境.PACKAGE_CONTAINER.getByName("豫言包管理");
  const 授权回应 = await 容器.fetch(new Request(`http://container.internal/upload/authorize/${摘要}`, {
    method: "POST",
    headers: 授权标头,
  }));
  if (!授权回应.ok) return 授权回应;

  const 对象键 = `pending/${摘要}.zip`;
  await 环境.PACKAGES.put(对象键, 请求.body, {
    httpMetadata: { contentType: "application/zip" },
    customMetadata: { sha256: 摘要 },
  });
  const 对象 = await 环境.PACKAGES.head(对象键);
  if (!对象 || 对象.size !== 字节数) {
    await 环境.PACKAGES.delete(对象键);
    return 回应错误(409, "R2 实际收到的字节数与 Content-Length 不一致");
  }
  return 回应文字("待发布包已上传", 201);
}

export async function 处理固化(请求, 环境) {
  if (请求.method !== "POST") return 回应错误(405, "固化桥只接受 POST");
  const 源键 = 请求.headers.get("X-Source-Key") ?? "";
  const 目标键 = 请求.headers.get("X-Destination-Key") ?? "";
  const 摘要 = 请求.headers.get("X-Object-SHA256") ?? "";
  if (!上传键格式.test(源键) || !正式键格式.test(目标键) || !/^[0-9a-f]{64}$/.test(摘要)) return 回应错误(400, "固化参数无效");
  const 对象 = await 环境.PACKAGES.get(源键);
  if (!对象) return 回应错误(404, "待发布对象不存在");
  const 最大字节数 = Number(环境.MAX_PACKAGE_BYTES ?? "10485760");
  if (对象.size > 最大字节数) return 回应错误(413, "待发布对象超过包大小上限");
  if (对象.customMetadata?.sha256 !== 摘要) return 回应错误(409, "待发布对象摘要元数据不一致");
  const 数据 = await 对象.arrayBuffer();
  const 实际摘要字节 = new Uint8Array(await crypto.subtle.digest("SHA-256", 数据));
  const 实际摘要 = Array.from(实际摘要字节, (值) => 值.toString(16).padStart(2, "0")).join("");
  if (实际摘要 !== 摘要) return 回应错误(409, "待发布对象实际摘要不一致");
  await 环境.PACKAGES.put(目标键, 数据, {
    httpMetadata: { contentType: "application/zip" },
    customMetadata: { sha256: 摘要 },
  });
  return new Response(null, { status: 204 });
}

export async function 下载已发布包(请求, 环境, 文件名) {
  const 记录 = await 环境.DB.prepare(`SELECT "对象键", "SHA256", "压缩字节数" FROM "包版本" WHERE "文件名" = ?`).bind(文件名).first();
  if (!记录) return 回应错误(404, "未找到指定包版本");
  const 标签 = `"${记录.SHA256}"`;
  if (请求.headers.get("If-None-Match") === 标签) return new Response(null, { status: 304, headers: { ETag: 标签, "Cache-Control": "public, max-age=31536000, immutable" } });
  const 对象 = await 环境.PACKAGES.get(记录.对象键);
  if (!对象) return 回应错误(503, "版本已登记，但包对象暂不可用");
  return new Response(请求.method === "HEAD" ? null : 对象.body, {
    status: 200,
    headers: {
      "Content-Type": "application/zip",
      "Content-Length": String(记录.压缩字节数),
      "Content-Disposition": `attachment; filename*=UTF-8''${encodeURIComponent(文件名)}`,
      "Cache-Control": "public, max-age=31536000, immutable",
      "X-Content-Type-Options": "nosniff",
      "X-Package-SHA256": 记录.SHA256,
      ETag: 标签,
    },
  });
}

export function 是包下载路径(请求) {
  const 网址 = new URL(请求.url);
  if (请求.method !== "GET" && 请求.method !== "HEAD") return null;
  if (!/^\/[^/]+\.zip$/.test(网址.pathname)) return null;
  try {
    return decodeURIComponent(网址.pathname.slice(1));
  } catch {
    return null;
  }
}
