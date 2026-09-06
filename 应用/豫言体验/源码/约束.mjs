// 古曰：所求必有所限，非豫言之事则辞之。
// 今释：固定输入结构与服务端模型协议；拒绝请求不能由客户端改变角色或工具。
export const 源码上限 = 24000;
export const 请求上限 = 40000;
export const 拒绝文字 = "这里只接受编写或修改豫言程序的请求。";
export function 验请求(值, 路径) {
  if (!值 || typeof 值 !== "object" || Array.isArray(值)) throw new Error("请求格式无效");
  const 允许键 = 路径 === "/api/run" ? ["code"] : ["code", "prompt"];
  if (Object.keys(值).some(键 => !允许键.includes(键))) throw new Error("请求含有不支持的字段");
  if (typeof 值.code !== "string" || new TextEncoder().encode(值.code).length > 源码上限) throw new Error("源码不得超过 24 KB");
  if (路径 === "/api/run" && !值.code.trim()) throw new Error("请先写入豫言源码");
  if (路径 === "/api/assist" && (typeof 值.prompt !== "string" || !值.prompt.trim() || 值.prompt.length > 2000)) throw new Error("需求须为 1 至 2000 字");
  return 值;
}
export function 验模型源码(值) {
  if (!值 || 值.refuse !== false || typeof 值.code !== "string" || Object.keys(值).some(键 => !["refuse", "code"].includes(键))) throw new Error(拒绝文字);
  验请求({ code: 值.code }, "/api/run");
  if (值.code.includes("```") || !/[「『]/u.test(值.code)) throw new Error("模型未提交豫言源码");
  return 值.code;
}
export async function 读限量正文(请求, 上限 = 请求上限) {
  if (!请求.body) throw new Error("请求正文为空");
  const 读取器 = 请求.body.getReader();
  const 块们 = []; let 总长 = 0;
  try {
    while (true) {
      const { done, value } = await 读取器.read();
      if (done) break;
      总长 += value.byteLength;
      if (总长 > 上限) { await 读取器.cancel(); throw new Error("请求过大"); }
      块们.push(value);
    }
  } finally { 读取器.releaseLock(); }
  const 字节 = new Uint8Array(总长); let 偏移 = 0;
  for (const 块 of 块们) { 字节.set(块, 偏移); 偏移 += 块.byteLength; }
  return new TextDecoder("utf-8", { fatal: true }).decode(字节);
}
