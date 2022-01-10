# 豫言 
一款函数式中文编程语言

## 版本
非常早期的预览版（0.1.0alpha)

Everything is subject to change

## Features

This language is a simple functional programming language with 
- function types
- sum types
- product types
- isorecursive types
- universal types
- existential types

## Features to be implemented

I find the following features to be usually desirable 

- Custom Exceptions

- Letcc

- Mutable references


## Installation

You can compile with either smlnj or mlton.

With mlton, use the following command: 
```
mlton -output yy src/development.mlb
```

## Usage
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

Projects are file lists with extension `。豫库`. They can be runned by the command, e.g. 
```
./yy r examples/helloworld/行。豫库
```

<!-- To run with a faster runtime (in practice `k` seems to be faster)
```
./yy rk filename.yuyan
./yy rkv filename.yuyan
./yy rkvv filename.yuyan
``` -->

## Examples

See the [`examples`](https://github.com/yuyan-lang/yuyan/tree/master/examples) directory.

Examples include definition of booleans, unary integers, lists and the quicksort algorithm.

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
| 注〇| // comment | - |
|有书〇曰〇| public struct { } | - |
|吾书〇曰〇| private struct { } | - |
 |观〇之书|  open S | - |
| ------------- |-------------|  ---------- |
| 《《字符串》》 | string (builtin) | 420 |
| 有 | 1 | 420 |
| 无 | 0 |  420 |
  |夫〇表〇| l : t | 400 |
  |〇合〇 | *  |  380 |
  |〇亦〇 | +  | 360 |
  |夫〇表〇合夫〇表〇| *{l : t, l2 : t2} | - |
  |夫〇表〇亦夫〇表〇| +{l : t, l2 : t2} | - |
|〇启以〇 | ∀t. T启以T2 = [T2/t]T | 355 |
  |化〇而〇 | t1 -> t2 |  350 | 
   |复〇为〇 | ρt. T | 345 |
|承〇而〇 | ∀t. T | 340 |
|有〇则〇 | ∃t. T | 320 |
| ------------- |-------------|  ------ |
|元| () | 720 | 
|〇之〇| (struct S).e/t | 710 | 
|〇中〇| e1.l | 700 |
 |〇于〇| (e1 e2) | 690 |
 |〇与〇| ⟨e1,e2⟩ | 680 |
 |〇与〇与〇| ⟨e1,e2,e3⟩ | 680 |
 |〇临〇| l.e | 670 |
   |卷〇| fold e| 660 |
   |舒〇| unfold e|  650 |
   |鉴〇而「曰〇则有〇而〇[或曰〇则有〇而〇]+」| case e of {l.x => e1 \| l2.y => e2} | 640 |
   |〇授以〇| e [T] | 600 |
   |入〇合〇| pack t in e | 560 |
   |开〇则有〇者〇而〇| open e as t, x in e2 | 540 |
|《《C调用》》名〇传〇 | ffi_c_call ... args ⟨e1,e2,e3⟩ | 530 |
|会〇而〇| λ x. e | 520 |
|遇〇者〇而〇| λ x: T. e| 520 |
|循〇以〇| fix f. e| 510 |
|受〇而〇| Λ t. e| 500 |
|虑〇以成〇之道| let ... in ... end | 490 |

## Bugs

Bugs are to be expected. If you encounter an exception on an otherwise ok program, it is probably a bug. 

## Contribution
Language features should be thoroughly discussed before they are merged as PRs. 


## Tips for debugging

Some annoying issue I've encountered:

+ UTF8 sometimes have control characters that mess with parsing, can use vim to chase them out. The issue is likely due to vim plugin for vscode intermixing with  the pinyin input method.


## Performance 

Sadly with only an interpreted enviroment, 
the performance is somewhat not as good as i expected, despite the fact that  the theoretical performance is good (since type checking is optimized away and RT is using "byte code").



See `performance-investigation` for testing scripts (you can run quicksort test to 
compare the performance with other languages).
<!-- 
My somewhat naive implementation of k machine is not optimal in the sense that substitutions are better implemented as functions and not as values. Add "k" to the first argument runs the K machine (which appears to be 
faster) than the pK machine. -->

The performance should improve once I figure out how to compile everything to assembly.
