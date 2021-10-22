# 豫言 
一款中文编程语言

## 版本
非常早期的预览版（0.1.0alpha)

## Features

This language is a simple functional programming languages with 
- function types
- sum types
- product types
- isorecursive types
- universal types
- existential types

## Features to be implemented

I find the following feature to be usually desirable 
- Nested expression with declarations,
(I tried to implement this, but the parser needs a lot of rework, so pending for now)

- Custom Exceptions

- Letcc

- Mutable references


## Installation

You can compile with either smlnj or mlton.

With mlton, use the following command: 
```
mlton  -output yy -default-ann 'allowExtendedTextConsts true' -verbose 1 src/development.mlb
```

To run
```
./yy r filename.yuyan
```

To run with more informative debug output 
```
./yy rv filename.yuyan
./yy rvv filename.yuyan
```
(r stands for run and v stands for verbose)

## Examples

See the `examples` directory.

## Syntax Sheet (Informal)
The key words (that cannot appear as a name and has a special meaning no matter where it is) are
  `"「", "」", "『", "』", "。"`. The single quotes are used for 
  + identifiers (if 
  no keywords appear inside a pair of `"「", "」"`) 
  + OR brackets (if there 
  are keywords (`"「", "」", "『", "』", "。"`) appearing inside a pair of `"「", "」"`).

| 豫言           | Standard Language | Precedence |
| ------------- |-------------| ------|
| 〇者〇也 |  type t = T | - |
| 以〇为〇 |  e : T |  - |
| 设〇为〇 |  #define e = E | - |
| 施〇乃为〇 | e = E | - |
| 术〇交[左右无]序[零一二三四五六七八九]+也 | infix[lr ] op [0123456789]+ |  - |
| ------------- |-------------|  ---------- |
| 有 | 1 | 420 |
| 无 | 0 |  420 |
  |夫〇表〇| l : t | 400 |
  |〇合〇 | *  |  380 |
  |〇亦〇 | +  | 360 |
  |夫〇表〇合夫〇表〇| *{l : t, l2 : t2} | - |
  |夫〇表〇亦夫〇表〇| +{l : t, l2 : t2} | - |
  |化〇而〇 | t1 -> t2 |  350 | 
|承〇而〇 | ∀t. T | 340 |
|有〇则〇 | ∃t. T | 320 |
 |复〇为〇 | ρt. T | 300 |
| ------------- |-------------|  ------ |
|元| () | 720 | 
|〇中〇| e1.l | 700 |
 |〇于〇| (e1 e2) | 690 |
 |〇与〇| ⟨e1, e2, e3⟩ | 680 |
 |〇临〇| l.e | 670 |
   |卷〇| fold e| 660 |
   |舒〇| unfold e|  650 |
   |鉴〇而「曰〇则有〇而〇[或曰〇则有〇而〇]+」| case e of {l.x => e1 \| l2.y => e2} | 640 |
   |授〇以〇| e [T] | 600 |
   |入〇合〇| pack t in e | 560 |
   |开〇则有〇者〇而〇| open e as t, x in e2 | 540 |
|会〇而〇| λ x. e | 520 |
|遇〇者〇而〇| λ x: T. e| 520 |
|循〇以〇| fix f. e| 510 |
|受〇而〇| Λ t. e| 500 |


## Bugs

Bugs are to be expected. If you encounter an exception on an otherwise ok program, it is probably a bug. 

## Contribution
Language features should be thoroughly discussed before they are merged as PRs. 
