// 古曰：官书之名实悉列，查询止于其册。今释：全部公开签名进入上下文，也可分页查询；工具不接受文件系统路径。
export function 构建知识(指南, 目录, 示例) {
  if (!Array.isArray(目录.模块) || !目录.模块.length) throw new Error("标准库签名目录为空");
  const 声明 = 目录.模块.flatMap(模块 => 模块.声明.map(项 => ({ module: 模块.路径, name: 项.名称, signature: 项.签名 })));
  const 签名文字 = 目录.模块.map(模块 => `模块 ${模块.路径}\n` + 模块.声明.map(项 => `「${项.名称}」：${项.签名}`).join("\n")).join("\n\n");
  const 提示 = `${指南}\n\n# 已验证完整示例\n${Object.entries(示例).map(([名, 文]) => `## ${名}\n${文}`).join("\n\n")}\n\n# 标准库全部公开签名\n${签名文字}`;
  return {
    提示, 模块数: 目录.模块.length, 签名数: 声明.length,
    查询({ query = "", module = "", offset = 0, limit = 40 }) {
      if (module && !目录.模块.some(项 => 项.路径 === module)) return { error: "模块不存在", modules: 目录.模块.map(项 => 项.路径) };
      const 结果 = 声明.filter(项 => (!module || 项.module === module) && (!query || [项.name, 项.signature, 项.module].some(文 => 文.includes(query))));
      return { total: 结果.length, offset, nextOffset: offset + limit < 结果.length ? offset + limit : null, entries: 结果.slice(offset, offset + limit) };
    },
    示例({ name = "" }) {
      if (!name) return { names: Object.keys(示例) };
      return Object.hasOwn(示例, name) ? { name, code: 示例[name] } : { error: "示例不存在", names: Object.keys(示例) };
    }
  };
}
