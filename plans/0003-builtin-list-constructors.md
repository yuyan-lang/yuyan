# Builtin List Constructors

## Goal

Make list constructors builtin to reduce parsing/type-checking overhead and improve source readability.

The intended builtin list surface is:

```text
列
罄
缀
：：
【】
，
```

`多` and `空` are historical names. They should be replaced by:

```text
多 -> 缀
空 -> 罄
```

## Builtin Semantics

Introduce builtin list constants:

```text
内建类型列
内建列罄
内建列缀
```

Their types are:

```text
列 : 元类型 -> 元类型
罄 : {甲 : 元类型} -> 列 甲
缀 : {甲 : 元类型} -> 甲 -> 列 甲 -> 列 甲
```

`缀` and `罄` should behave as constructors, not ordinary functions.

Use the existing constructor path:

```text
唯一构造器节点
展开后唯一构造器节点
```

The compiler may assign fixed unique constructor ids for the builtin list constructors, for example:

```text
内建列罄构造器序数 = -1001
内建列缀构造器序数 = -1002
```

Before using negative ids, verify that later stages do not assume constructor ids are positive. If they do, reserve a high positive range instead.

## Operators

Add these operators:

```text
【】
〇：：〇
〇，〇
```

`：：` is the user-facing cons operator.

```text
甲 ：： 乙
```

expands to:

```text
缀 甲 乙
```

`：：` should be right-associative:

```text
甲 ：： 乙 ：： 罄
```

means:

```text
甲 ：： (乙 ：： 罄)
```

The list literal syntax:

```text
【】
【甲】
【甲，乙，丙】
```

expands to:

```text
罄
缀 甲 罄
缀 甲 (缀 乙 (缀 丙 罄))
```

`，` should only be used as a parser-local sequence operator for list literals at first. It must not survive into type checking.

## Parsing Shape

The preferred implementation is:

1. `，` builds a parser-local sequence representation inside list brackets.
2. `【...】` consumes that sequence and right-folds it into `缀 ... 罄`.
3. The final AST contains only ordinary applications of `缀` and `罄`, or direct builtin constructor nodes if that is cleaner.

Do not introduce a serialized list-literal AST node unless absolutely necessary.

## YY Compiler Changes

Update the 豫言 compiler:

1. Add builtin constants for `列`, `罄`, and `缀`.
2. Add string representations for those builtins.
3. Resolve source names directly during parsing/desugaring:

```text
列 -> 内建类型列
罄 -> 内建列罄
缀 -> 内建列缀
```

4. Keep temporary compatibility while migrating:

```text
多 -> 内建列缀
空 -> 内建列罄
```

5. Make `内建物类型` return the correct dependent function types.
6. Make `罄/缀` lower through the existing constructor machinery with fixed constructor ids.
7. Ensure pattern matching, type erasure, CPS, and code generation keep working through the existing constructor path.

## SML Compiler Changes

Update the SML compiler in parallel:

1. Add builtin list type / constructor representation in the SML AST layer.
2. Update expression construction so these names parse as builtins:

```text
列
罄
缀
多
空
```

3. Add fixed constructor ids for `罄` and `缀`.
4. Reuse the existing `CConsInfoTypeConstructor` / `CConsInfoElementConstructor` path.
5. Avoid introducing a separate list runtime unless the existing constructor path cannot support it.

## Migration Order

1. Mechanically replace complete identifiers:

```text
「多」 -> 「缀」
「空」 -> 「罄」
```

Only replace complete quoted identifiers. Do not touch compound names such as:

```text
多态列
空缺
空值节点
空串典
```

2. Add builtin support for `列/罄/缀`.
3. Add `：：`.
4. Add list literal syntax using `【】` and `，`.
5. Remove ordinary stdlib declarations for `列/罄/缀` after bootstrap succeeds.
6. Optionally migrate nested cons expressions to list literals for readability.

## Validation

Run type-checking validation after the compiler parses again.

Run full bootstrapping validation once both the YY compiler and SML compiler agree on builtin list constructors and fixed constructor ids.
