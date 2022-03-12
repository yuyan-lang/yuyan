# 豫言 
一款函数式中文编程语言

一款中文函数式编程语言

[English Version](README_eng.md)（可能有些信息已过期）
## 版本
非常早期的预览版（0.1.0alpha)

本语言仍在早期的开发阶段，所有的语言功能都可能在后期改变。

## 语言特征

本语言是简单类型的函数式编程豫言。支持的类型包括以下：
- 函数类型(function types)
- 总和类型(sum types)
- 乘积类型(product types)
- 递归类型(isorecursive types)
- 通用类型(universal types)
- 存在类型(existential types)


## 安装

详见[`安装指南`](InstallationInstruction.md)

## 运行

运行一个程序：
```
yy filename.yuyan
```

编译一个程序
```
yy filename.yuyan -c -o output.out
```

显示帮助
```
yy -h
```

## 帮助文档
### 语言规范

[语言规范](LanguageSpecification.md)
[语法表](SyntaxSheet.md)

### 标准库及工具库文档 (API Docs)

详见[API文档](autogen/docs/目录.html)

## 语言插件

[VSCode插件](https://marketplace.visualstudio.com/items?itemName=yuyan-lang.yuyan-vscode)

## 样例

可以查看[`标准库`](https://github.com/yuyan-lang/yuyan/tree/master/yylib)，以及[`performance-investigation`](https://github.com/yuyan-lang/yuyan/tree/master/performance-investigation)下的实现文件。


## 漏洞(Bugs)

由于处于开发的早期阶段，目前有很多bug，我们正在逐步修复，以及采用新的设计来避免这些bug的发生。

如果你遇到了bug，请在`Issues`界面汇报。

## 贡献

出于语言设计的早期阶段，我们会认真听取语言功能方面的建议。

## 关于调试技巧 

vscode的vim插件有时候会和输入法冲突导致输入一些看不见的字符导致编译失败，可以使用vim，debug。



## 语言性能

详见[语言性能](LanguagePerformance.md)
