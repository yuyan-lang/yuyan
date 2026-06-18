---
name: fixity-migration
description: Migration from component-based operators to fixity-based operators in the Yuyan self-hosting compiler
---

# Fixity Migration

## Done
- `操作符数据结构。豫`: 新 `Fixity` 类型 (`FxNone | FxOp(Int) | FxComp(String)`)
- `构造操作符` 简化为 keyword + left_fixity + right_fixity + elaborator + name
- 删除了 `组件`、`优先级关系`、`绑定组件`、`特殊字符串组件`、`字符串组件`、`元素组件`

## 待更新（引用旧类型的文件）
- `操作符操作。豫` — `获取标号`、`组件表示`、`获取所有组件`、`操作符是开放的` 等用到了旧字段
- `操作符集术。豫` — `插入闭合操作符`、`插入开放操作符`、优先级计算
- `内建操作符对象定义。豫` — 所有操作符定义要转成新 fixity 格式
- `表达式解析。豫` — `匹配组件获取参数` 改为基于 fixity 的 operand pop
- `表达式解析输入分析。豫` — 操作符匹配和归约逻辑
- `符列环境术。豫` — 操作符集读写
- `抽象语法分析。豫` — 操作符声明处理（`术` 关键字）
- `粗语法分析。豫` — 结构名称操作

## 编译
- 用 `yy_bs_stable` 编译，不用 `yy`
- 每次改动后验证
