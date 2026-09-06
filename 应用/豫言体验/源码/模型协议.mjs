// 古曰：授器各有所司，型号与用度皆有定制。
// 今释：模型请求参数和工具定义由可信服务决定，客户端不能更换模型、增加工具或放大预算。
const 工具 = (name, description, properties, required = []) => ({ type: "function", function: {
  name, description, parameters: { type: "object", properties, required, additionalProperties: false }
} });
export const 工具定义 = [
  工具("query_library", "查询完整标准库公开签名与模块。支持关键词、精确模块路径与分页；空查询枚举全部签名。", {
    query: { type: "string", description: "函数名或类型关键词，空串表示全部" },
    module: { type: "string", description: "目录中的精确模块路径，空串表示全部模块" },
    offset: { type: "integer", minimum: 0 }, limit: { type: "integer", minimum: 1, maximum: 80 }
  }),
  工具("read_example", "读取经过编译运行验证的豫言示例。空名称列出所有可用示例。", { name: { type: "string" } }),
  工具("compile_yuyan", "编译完整豫言源码，返回退出状态与关键诊断；根据错误修改后可再次调用。", { code: { type: "string" } }, ["code"]),
  工具("run_yuyan", "隔离编译并运行完整豫言源码，检查实际输出；没有网络、交互输入或任意终端。", { code: { type: "string" } }, ["code"])
];
export const 最大轮数 = 6;
export const 最大执行次数 = 6;
export const 助写时限 = 480000;

export function 组模型请求(内容, 型号) {
  if (!内容 || !Array.isArray(内容.messages) || 内容.messages.length < 1 || 内容.messages.length > 64) throw new Error("模型消息数量无效");
  const 模式 = 内容.mode ?? "legacy";
  if (!["scope", "agent", "legacy"].includes(模式)) throw new Error("模型请求模式无效");
  const 参数 = { model: 型号, messages: 内容.messages, stream: false };
  if (模式 === "agent") return { ...参数, tools: 工具定义, max_tokens: 12000, thinking: { type: "enabled" }, reasoning_effort: "high" };
  return { ...参数, max_tokens: 模式 === "scope" ? 256 : 4096, thinking: { type: "disabled" }, response_format: { type: "json_object" } };
}

export function 取关键诊断(结果, 上限 = 7000) {
  const 原文 = [结果.error, 结果.stderr, 结果.stdout].filter(Boolean).join("\n");
  // 古曰：取其病处，勿以炉声蔽之。今释：过滤进度与初始化噪声，长诊断同时保留开头和末尾。
  const 位置 = 原文.search(/未捕捉|无法解析|在检查|error:|fatal error:|Error:/);
  const 净文 = (位置 >= 0 ? 原文.slice(位置) : 原文).split("\n")
    .filter(行 => !/^\[\d{4}-|^豫言(?:初始|最大)/.test(行)).join("\n").trim();
  return 净文.length <= 上限 ? 净文 : 净文.slice(0, 2000) + "\n[中间诊断省略；完整日志已回传浏览器]\n" + 净文.slice(-(上限 - 2050));
}

export function 验工具参数(名称, 参数) {
  const 定义 = 工具定义.find(项 => 项.function.name === 名称)?.function.parameters;
  if (!定义) throw new Error("工具不存在；只能查询豫言资料或编译运行豫言源码");
  if (!参数 || typeof 参数 !== "object" || Array.isArray(参数)) throw new Error("工具参数必须是对象");
  if (Object.keys(参数).some(键 => !Object.hasOwn(定义.properties, 键))) throw new Error("工具包含不支持的参数");
  if (定义.required.some(键 => !Object.hasOwn(参数, 键))) throw new Error("缺少必要工具参数");
  for (const [键, 值] of Object.entries(参数)) {
    const 规则 = 定义.properties[键];
    if (规则.type === "string" && (typeof 值 !== "string" || (键 !== "code" && 值.length > 200))) throw new Error("查询参数无效");
    if (规则.type === "integer" && (!Number.isSafeInteger(值) || 值 < 规则.minimum || (规则.maximum && 值 > 规则.maximum))) throw new Error("分页参数无效");
  }
  return 参数;
}
