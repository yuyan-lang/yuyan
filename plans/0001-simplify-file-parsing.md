# Simplify File Parsing

## Goal

Remove the old rough parsing layer and make file parsing a thin pass over top-level statements.

The target pipeline is:

```text
source file
 -> lexer / concrete syntax: list of top-level token lists
 -> top-level expression parsing for each statement
 -> file definition list
 -> file structure checking / type checking
```

The following concepts should disappear:

```text
粗分析语句
粗声明
粗分析编译单元
具体连续语句节点
具体连续表达式节点
处理连续语句
```

## Top-Level Syntax

Keep only the minimal top-level forms:

```text
寻〇之书
寻观〇之书
寻诵〇之书
寻观诵〇之书
观〇之书
观诵〇之书
诵〇之书

〇立〇也
〇乃〇也
〇者〇也
〇即〇也
```

Remove the compatibility forms:

```text
导入 / 打开 / 导出
是一种
的类型是
的类型可以是
的定义是
其实就是
号
```

## Expression Parsing First

Work on `表达式解析` first.

Add a top-level operator set separate from the ordinary expression operator set:

```text
默认操作符
顶层操作符
```

Ordinary expressions use `默认操作符`.
Top-level file statements use `默认操作符 + 顶层操作符`.

Top-level parsing should not introduce a new serialized AST layer such as `顶层文件操作节点` or `顶层定义声明节点`.
If temporary helper values are needed, keep them local to parsing / abstract syntax analysis and do not serialize them.

## File Definition Generation

`抽象语法分析` should become a thin file-level organizer. It should take the current file name and a list of top-level token lists, call top-level expression parsing for each statement, and produce a `文件定义列`.

Definition rules:

```text
A 立 T 也
 -> A maps to 构造声明近似类型节点 with 唯一构造器节点(current file, A, T)

A 乃 T 也
 followed immediately by:
    A 者 body 也
    or A 即 body 也
 -> A maps to 定义声明近似类型节点
    transparency comes from 者 / 即
    type and body use the existing 按需自动递归近似类 / 按需自动递归定义 logic

A 者 body 也
 -> A maps to 定义声明近似类型节点(定义不透明, 近似类型占位式, body)

A 即 body 也
 -> A maps to 定义声明近似类型节点(定义透明, 近似类型占位式, body)
```

`乃` does not produce a file definition by itself. It consumes the immediately following same-name definition.

## File Operations

File operations should not create first-class module values.

```text
寻〇之书
 -> dependency only; no file definition item

观〇之书
 -> anonymous item: 文件打开近似类型节点

诵〇之书
 -> anonymous item: 文件导出近似类型节点

寻观〇之书
 -> dependency + 文件打开近似类型节点

寻诵〇之书
 -> dependency + 文件导出近似类型节点

寻观诵〇之书
 -> dependency + 文件打开近似类型节点 + 文件导出近似类型节点
```

`寻` must not bind a module name as a first-class module.

## Implementation Order

1. Refactor `表达式解析` to support the minimal top-level operator set.
2. Change concrete file parsing so the file entry returns `列（列 表达式）`, not a continuous-statement expression node.
3. Rewrite `抽象语法分析` so its input is `列（列 表达式）` and its output is `文件定义列`.
4. Rewrite `导入预测` to scan top-level statements directly and collect only dependency file names.
5. Remove `粗语法树`, `粗语法分析*`, and `粗分析编译单元引用`.
6. Remove `具体连续语句节点`, `具体连续表达式节点`, and `处理连续语句`.
7. Run type-checking validation.
8. Run full bootstrapping validation.

## Desired End State

```text
表达式解析:
  parses ordinary expressions and top-level declaration operators

抽象语法分析:
  turns top-level statements into 文件定义列

文件结构检查:
  handles open/export/import markers

类型检查:
  resolves names, checks definitions, and produces checked file definitions
```

The main design constraint is that `抽象语法分析` should not remain a second parser. It should only do file-level organization.
