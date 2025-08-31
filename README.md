# 豫言 (Yuyan) 编程语言

[![VS Code Extension](https://img.shields.io/visual-studio-marketplace/v/yuyan-lang.yuyan-vscode.svg)](https://marketplace.visualstudio.com/items?itemName=yuyan-lang.yuyan-vscode)
[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/yuyan-lang/yuyan)

豫言是一个现代化的函数式编程语言，使用中文语法，旨在让中文使用者能够用自然的方式编写代码。豫言使用 OCaml 作为后端编译器，未来计划实现与 OCaml 的双向集成。

## 语言特性

- **中文语法**：完全使用中文关键字和语法结构
- **函数式编程**：支持高阶函数、不可变数据结构等函数式编程特性
- **类型安全**：静态类型系统确保程序的正确性
- **模块化**：丰富的标准库，包含数据结构、数学运算、输入输出等模块
- **自定义操作符**：支持定义前缀、中缀、后缀和环缀操作符

## 语法特点

### 注释
使用「：：」作为注释标记：
```
「：这是一行注释：」
```

### 自定义操作符
豫言支持灵活的操作符定义：
```
术「399」「左」加「右」「400」盖谓「加」于「左」于「右」也。
```
这定义了一个左结合的中缀操作符 `加`，左优先级为399，右优先级为400。

### 函数定义
```
「矩阵乘法」乃化「小数」列列而化「小数」列列而「小数」列列也。
「矩阵乘法」者会「矩阵A」而会「矩阵B」而
    虑「长宽」者「长度」于「矩阵A」而
    ...
```

## 开发环境设置

### VSCode 扩展支持

为了获得更好的开发体验，推荐安装豫言语言的 VSCode 扩展：

- **从 Marketplace 安装**：[豫言语言扩展](https://marketplace.visualstudio.com/items?itemName=yuyan-lang.yuyan)
- **本地开发版本**：项目中包含了 VSCode 扩展源码，位于 `./yuyan-vscode` 目录下，可用于开发和调试扩展功能

### 依赖安装

1. **安装 OPAM**（OCaml 包管理器）：
   ```bash
   # macOS
   brew install opam
   
   # Ubuntu/Debian
   sudo apt-get install opam
   
   # 其他系统请参考：https://opam.ocaml.org/doc/Install.html
   ```

2. **初始化 OPAM 环境**：
   ```bash
   opam init
   eval $(opam env)
   ```

3. **安装 OCaml 和 Dune**：
   ```bash
   opam install ocaml dune
   ```

### 编译项目
```bash
dune build
```

### 测试标准库
```bash
dune exec -- yyocamlc 藏书阁/标准库。豫
```

### 运行示例程序
运行矩阵乘法性能测试：
```bash
dune exec -- yyocamlc performance-investigation/matrix-multiply/矩阵乘法。豫
```

## 在线体验

你可以在 GitHub.dev 上直接体验豫言编程语言：

[![在 GitHub.dev 中打开](https://github.com/codespaces/badge.svg)](https://github.dev/yuyan-lang/yuyan/tree/demo)

默认会打开矩阵乘法示例文件，展示豫言语言的基本语法和功能。

## 标准库

豫言提供了丰富的标准库，位于 `藏书阁/标准库/` 目录下：

- **语言核心**：内建类型、引用、异常处理
- **数据结构**：多态列表、字符串、字典、可选值等
- **数学运算**：基本数学操作和函数
- **输入输出**：文件和控制台 I/O 操作
- **操作系统**：系统调用和环境交互
- **控制结构**：惰性求值等控制流功能

## 示例代码

以下是一个矩阵乘法的完整示例：

```
观「藏书阁」之「标准库」之书。

「矩阵长宽」乃「整数」也。
「矩阵长宽」者一零零也。

「生成随机矩阵」乃化「整数」而「小数」列列也。
「生成随机矩阵」者会「长宽」而
    「打表」于「长宽」于会「行」而
        「打表」于「长宽」于会「列」而
            「随机」之「获取随机小数」于「元」也。

「矩阵乘法」乃化「小数」列列而化「小数」列列而「小数」列列也。
「矩阵乘法」者会「矩阵A」而会「矩阵B」而
    虑「长宽」者「长度」于「矩阵A」而
    「打表」于「长宽」于会「行」而
        「打表」于「长宽」于会「列」而
            「具体列」之「小数列求和」于
                （「打表」于「长宽」于（会「K」而
                    「小数操作」之「乘」
                        于（「第N个」于「K」于（「第N个」于「行」于「矩阵A」））
                        于（「第N个」于「列」于（「第N个」于「K」于「矩阵B」））））也。
```

## 技术架构

豫言编程语言采用 OCaml 作为编译后端，这使得豫言能够：

- **高性能**：利用 OCaml 的优化编译器生成高效的原生代码
- **类型安全**：继承 OCaml 强大的类型系统
- **函数式特性**：完全支持高阶函数、模式匹配等函数式编程特性
- **互操作性**：未来计划实现与 OCaml 生态的双向集成，允许豫言代码调用 OCaml 库，反之亦然

## 贡献

欢迎为豫言编程语言的发展贡献力量！如有问题或建议，请提交 Issue 或 Pull Request。

## 许可证

本项目采用开源许可证，详情请查看 LICENSE 文件。