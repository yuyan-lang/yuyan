# 豫言 
一款函数式中文编程语言

一款中文函数式编程语言

## 版本
非常早期的预览版（0.1.0alpha)

所有的语言功能都可能在后期改变。
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
