---
name: yylib-operator-removal
description: Manually remove yylib custom operator declarations and their supporting definitions from the Yuyan codebase, without Python or automated rewrites.
---

# Yylib Operator Removal

Use this skill when removing first-class/custom operators from `yylib` and replacing their uses with ordinary function calls, constructors, or compiler builtins.

## Hard Rules

- Work manually. Use `rg`, `sed`, `nl`, and file reads to inspect; use `apply_patch` for edits.
- Do not use Python. Do not write Python scripts, one-off Python snippets, or Python-based rewrites.
- Do not use broad automated rewrites. This migration is syntax-sensitive and must be reviewed case by case.
- Ignore `*_v0` directories unless the user explicitly asks.
- Follow repository instructions about verification. In this repo, do not run compiler checks, tests, lint, formatting, build commands, or smoke tests unless the user explicitly asks.
- Always review changes afterwards with `git diff` and targeted `rg` searches.

## Exact Operator Shapes

An operator declaration in `yylib` has this shape:

```text
术<operator-form>交<assoc>序<precedence>也。
```

Where:

- `<operator-form>` contains one or more operand holes written as `〇`, or may be a no-hole form such as `【】`.
- `<assoc>` is `左`, `右`, or `无`.
- `<precedence>` is written in Chinese digits, for example `七零零`, `六八九`, `七一零`.

Examples:

```text
术〇+〇交左序七零零也。
术〇衔〇交右序七零零也。
术【】交无序七一零也。
术以〇合并〇交无序七零零也。
```

The declaration is usually paired with one or more supporting forms nearby:

```text
〇+〇乃化「整数」而化「整数」而「整数」也。
「〇+〇」者「加」也。
术〇+〇交左序七零零也。
```

Other common supporting forms:

```text
「〇衔〇」即「多」也。
【〇】乃（承「元类型」者「甲」而化「甲」而「甲」列）也。
「【〇】」者会「甲」而「多」于「甲」于「空」也。
可有〇即「可有」也。
```

When removing an operator, remove or rewrite the whole operator surface, not only the `术...` line.

## Current yylib Operator Set

At the time this skill was written, the active `yylib` operators are:

- `〇其〇`
- `〇大于〇`
- `〇=〇`
- `〇等于〇`
- `〇不等于〇`
- `〇小于〇`
- `〇+〇`
- `〇加〇`
- `〇-〇`
- `〇减〇`
- `〇且〇`
- `或可有〇`
- `可有〇`
- `〇列`
- `列`
- `【】`
- `〇衔〇`
- `【〇】`
- `【〇，〇】`
- `【〇，〇，〇】`
- `【〇，〇，〇，〇】`
- `【〇，〇，〇，〇，〇】`
- `〇@〇`
- `〇接〇`
- `以〇合并〇`
- `〇为〇子字符串`
- `〇是〇的结尾`
- `〇是〇的开头`
- `以〇合并列〇`

Before editing, relist the current set with:

```bash
rg -n --glob '!**/*_v0/**' '^\s*术.*交.*序.*也。' yylib
```

## Manual Removal Workflow

1. Identify the operator declaration:

```bash
rg -n --glob '!**/*_v0/**' '^\s*术.*交.*序.*也。' yylib
```

2. Inspect the surrounding code manually:

```bash
sed -n '<start>,<end>p' <file>
```

3. Find all uses of the operator form before removing it. Search both quoted and unquoted forms when applicable:

```bash
rg -n --glob '!**/*_v0/**' '<operator text>|「<operator text>」|<plain alias>' .
```

4. Rewrite uses by hand. Prefer existing named functions or constructors:

- `甲 + 乙` -> `「加」于「甲」于「乙」` or the local named equivalent.
- `甲 减 乙` / `甲 - 乙` -> `「减」于「甲」于「乙」`.
- `甲 衔 乙` -> `「多」于「甲」于「乙」` when this is cons, not append.
- `甲 @ 乙` / `甲 接 乙` -> `「附加」于「甲」于「乙」`.
- `以「分隔符」合并「列」` -> `「以〇合并〇」于「分隔符」于「列」` only as an interim step if the operator binding still exists; otherwise rename the function first.
- `〇附〇` should not be reintroduced in `yylib`; string concatenation is a compiler builtin.

5. Remove the operator declaration and its operator-facing support:

- Remove `术...交...序...也。`.
- Remove the operator type judgment such as `〇+〇乃...也。` if it exists only for the operator.
- Remove the quoted operator definition such as `「〇+〇」者...也。` if it exists only for the operator.
- Remove alias declarations such as `「〇衔〇」即...也。` if they are only there to expose the operator.
- Keep the ordinary named function or constructor if other code still uses it.

6. Review manually:

```bash
git diff -- <changed files>
rg -n --glob '!**/*_v0/**' '^\s*术.*交.*序.*也。' yylib
rg -n --glob '!**/*_v0/**' '<removed operator text>|「<removed operator text>」' .
```

The goal of review is to catch:

- dangling operator declarations
- dangling operator type judgments
- dangling quoted operator definitions
- remaining uses of removed syntax
- accidental edits inside string literals or field labels
- accidental changes in `*_v0` directories

## Things Not To Do

- Do not replace every `〇` pattern mechanically. Some are active builtins, parser syntax, or documentation.
- Do not remove ordinary named functions just because an operator wrapper used them.
- Do not edit generated build artifacts such as `.yybuild*`, binaries, or logs.
- Do not run verification commands unless explicitly asked.
