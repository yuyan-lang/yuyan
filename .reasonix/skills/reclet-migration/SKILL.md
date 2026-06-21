---
name: reclet-migration
description: Project-specific Yuyan migration rules for replacing local structure blocks such as `虑（...）` / structure-recursive declarations with expression-level `递归虑...其...者...而...` and `虑...者...而...`. Use when editing yylib or compiler `.豫`/`.yuyan` code to exercise the structure recursion operator / reclet syntax, especially when converting old structure declarations, local recursive helper functions, or direct expressions separated by `。`.
---

# Reclet Migration

Use this skill when migrating local structure blocks to expression-level let / recursive-let syntax in this repository.

## Non-negotiable Rules

- Do not edit `*_v0` directories unless the user explicitly asks.
- Do not run compiler checks, tests, lint, formatting, build, smoke, or other verification commands unless the user explicitly asks.
- Make small hand-written patches. Do not regex-rewrite whole files.
- Read the surrounding function before editing; structure blocks often mix declarations, direct expressions, comments, and effectful calls.

## Target Syntax

Use ordinary let for non-recursive local values:

```yuyan
虑「名」者「定义式」而
    「后式」
```

Use recursive let for one local recursive value:

```yuyan
递归虑「名」其「类型」者
    「定义式」
而
    「后式」
```

When the recursive value is a function, the definition normally starts with `会...而...`:

```yuyan
递归虑「处理」其化「字符串」列而「字符串」列者
    会「剩余」而
        ...
而
    「处理」于「输入」
```

## Converting Old Structure Declarations

Old local structure form:

```yuyan
虑（
    「处理」乃化「字符串」列而「字符串」列也。
    「处理」者会「剩余」而
        鉴「剩余」而
            有「空」则「空」
            或有「多」于「头」于「尾」则 ...
        也。
    「结果」者「处理」于「列」也。
    「串组合并」于「结果」。
）
```

Preferred reclet form:

```yuyan
递归虑「处理」其化「字符串」列而「字符串」列者
    会「剩余」而
        鉴「剩余」而
            有「空」则「空」
            或有「多」于「头」于「尾」则 ...
而
    虑「结果」者「处理」于「列」而
    「串组合并」于「结果」
```

Important details:

- Move the `乃 ... 也。` type into `其 ...`.
- Remove `` when the recursive binding itself supplies the self name.
- Remove helper aliases like `虑「递归」者「处理」而`; call `「处理」` directly.
- Remove the declaration-tail `也。` from `「名」者 ... 也。`. In reclet, the `而` after the definition is the separator.
- Do not leave `也` immediately before the reclet separator `而`. An error like `不期待也字后有其他输入，却得到了[『而』...]` usually means the old declaration-tail `也` survived.

## Direct Expressions In Structure Blocks

In a structure block, direct expressions can be separated by `。`:

```yuyan
「处理数组」于「物」。
「写混合数组引用」于「返回」于「零」于「数组」。
「写混合数组引用」于「返回」于「一」于「物长」。
「返回」
```

After migration, the reclet body is a single expression. Convert direct expression sequences to one of these forms.

Use `；` sequence expression when the operations are direct effects followed by a value:

```yuyan
（「处理数组」于「物」；
「写混合数组引用」于「返回」于「零」于「数组」；
「写混合数组引用」于「返回」于「一」于「物长」；
「返回」）
```

Use `虑「无」者 ... 而 ...` when it reads better as explicit sequencing or when a step has to bind a unit result:

```yuyan
虑「无」者「写数组引用」于「数组」于「序数」于「头」而
虑「无」者「写引用」于「序数」于（「序数」加「一」）而
「处理数组」于「尾」
```

Never leave multiple `。`-separated direct expressions after `而`.

## Non-Recursive Declarations Around Reclet

Convert local values before the recursive helper into nested `虑`:

```yuyan
虑「返回」者「新混合数组引用」于「二」而
虑「物长」者「长度」于「物」而
递归虑「处理数组」其化「甲」列而「有」者
    ...
而
    ...
```

For trivial aliases that exist only to support the old structure style, inline them if that simplifies the expression and does not change behavior.

## Parentheses

- Preserve required parentheses when the whole reclet is embedded as an argument or list element.
- Do not add parentheses around the recursive definition body just because it used to be a structure declaration.
- Do not delete meaningful parentheses inside pattern branches, `若...则...否则...`, type assertions, or application arguments.
- After editing, read the immediate expression boundary: every `递归虑...者 <definition> 而 <body>` has exactly one definition expression and one body expression.

## Checklist Before Finishing

- Each migrated recursive helper has no remaining `乃 ... 也。` declaration line.
- Each migrated recursive helper has no `` unless there is a deliberate reason.
- The old declaration-tail `也。` after the recursive helper is gone.
- Any old direct expression sequence using `。` is now either a `；` sequence expression or a `虑「无」者...而...` chain.
- `递归虑` body is a single expression.
- No verification commands were run unless the user explicitly requested them.
