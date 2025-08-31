# Yuyan Programming Language Guide

## Introduction

Yuyan (豫言) is a functional programming language with Chinese syntax that compiles to OCaml. It features a sophisticated type system with dependent types, custom operators, and a rich standard library implemented in Chinese characters.

## Basic Syntax

### Comments
Comments are enclosed in `「：` and `：」` markers:
```yuyan
「： This is a comment ：」
```

### Numbers
Yuyan uses Chinese numerals:
- `零` = 0, `一` = 1, `二` = 2, `三` = 3, `四` = 4, `五` = 5, `六` = 6, `七` = 7, `八` = 8, `九` = 9
- Decimal point: `点` (e.g., `三点五` = 3.5)

### Basic Data Types

#### Built-in Types
- `字符串` - String type
- `整数` - Integer type  
- `小数` - Float type
- `有` - Unit type
- `爻` - Boolean type
  - `阳` - True
  - `阴` - False
- `元` - Unit value

#### Identifiers
Identifiers are enclosed in `「」` brackets:
```yuyan
「变量名」
「函数」
```

### Variables and Functions

#### Function Definition
Functions are defined using `乃` (is) and `也` (end):
```yuyan
「函数名」乃化「参数类型」而「返回类型」也。
「函数名」者会「参数名」而表达式也。
```

Example:
```yuyan
「加法」乃化「整数」而化「整数」而「整数」也。
「加法」者会「甲」而会「乙」而「加」于「甲」于「乙」也。
```

#### Variable Binding
Use `虑` to bind variables:
```yuyan
虑「变量名」者「值」而
「表达式」
```

### Custom Operators

Yuyan supports defining custom operators with precedence:

#### Syntax
```yuyan
术「左优先级」「左参数名」操作符「右参数名」「右优先级」盖谓「表达式」也。
```

#### Examples
```yuyan
「： Define addition operator ：」
术「399」「左」加「右」「400」盖谓「加」于「左」于「右」也。

「： Define comparison operators ：」
术「390」「左」等于「右」「390」盖谓「等」于「左」于「右」也。
术「390」「左」大于「右」「390」盖谓「大于」于「左」于「右」也。
```

#### Operator Types
- **Infix**: `「左」操作符「右」` - binary operators
- **Prefix**: `一操作符「右」` - unary prefix
- **Postfix**: `「左」操作符一` - unary postfix  
- **Circumfix**: `一操作符「中」操作符一` - surrounding operators

### Control Flow

#### Conditional Expressions
```yuyan
若「条件」则「真分支」否则「假分支」
```

#### Pattern Matching
```yuyan
鉴「表达式」而
或有「模式1」则「结果1」
或有「模式2」则「结果2」
```

### Lists and Data Structures

#### List Type
```yuyan
「列」作化元类型而元类型也。
```

#### List Construction
- Empty list: `空` or `【】`
- Cons operator: `衔` (e.g., `「头」衔「尾」`)
- List literal: `【「元素1」、「元素2」】`

#### List Operations
- Map: `态射`
- Filter: `滤`  
- Fold left: `从左折叠`
- Fold right: `从右折叠`
- Append: `附加` or `接`
- Length: `长度`
- Reverse: `反转`

### Type System

#### Type Annotations
```yuyan
「变量」乃「类型」也。
```

#### Function Types  
```yuyan
化「输入类型」而「输出类型」
```

#### Dependent Types
Yuyan supports dependent types with Pi types:
```yuyan
承「变量」而「类型表达式」
```

### Modules and Imports

#### Module Declaration
```yuyan
观「模块路径」之书。
```

#### Module Structure
Files are organized in the `藏书阁` (library) directory structure.

### Standard Library

The standard library is organized into several modules:

#### `标准库/语言核心`
- Core language features
- Built-in types
- Exception handling
- References and continuations

#### `标准库/数据结构`  
- Lists (`多态列`)
- Integers (`整数操作`)
- Strings (`字符串术`)
- Optional values (`可选值`)
- Boolean operations (`爻术`)

#### `标准库/输入输出`
- I/O operations (`笔`)

#### `标准库/数学运算`
- Mathematical operations
- Random numbers
- Conversions

### Built-in Functions

#### Integer Operations
- `加` - Addition
- `减` - Subtraction  
- `乘` - Multiplication
- `除以` - Division
- `等` - Equality
- `大于` - Greater than
- `小于` - Less than

#### Boolean Operations
- `且` - Logical AND
- `或` - Logical OR
- `非` - Logical NOT

#### String Operations
- `表示` - Convert to string representation

### Error Handling

#### Exceptions
```yuyan
「发生事故」于『错误消息』
```

#### Exception Types
Exceptions are created and thrown using the exception system defined in the standard library.

### External Interface

#### External Function Calls
```yuyan
《《外部调用『函数名』》》
```

This allows calling OCaml functions from Yuyan code.

### Compilation

#### Build Commands
- `dune build` - Build the project
- `dune exec -- yyocamlc 文件名.豫` - Compile a specific file

### Example Programs

#### Hello World Equivalent
```yuyan
观「藏书阁」之「标准库」之书。

「问候」乃化「字符串」而「字符串」也。
「问候」者会「名字」而「你好，」接「名字」也。
```

#### List Processing
```yuyan
观「标准库」之「数据结构」之书。

「数字列表」乃【「一」、「二」、「三」】也。
「平方」乃化「整数」而「整数」也。
「平方」者会「数」而「乘」于「数」于「数」也。

「平方列表」乃「态射」于「平方」于「数字列表」也。
```

#### Custom Data Types
```yuyan
「颜色」作元类型也。
「红」立「颜色」也。
「绿」立「颜色」也。
「蓝」立「颜色」也。

「描述颜色」乃化「颜色」而「字符串」也。
「描述颜色」者会「颜色」而鉴「颜色」而
或有「红」则『红色』
或有「绿」则『绿色』
或有「蓝」则『蓝色』
也。
```

## Language Design Philosophy

Yuyan embraces Chinese linguistic patterns and cultural concepts:

- **Semantic Clarity**: Keywords are chosen for their semantic meaning rather than brevity
- **Cultural Integration**: Uses traditional Chinese concepts and terminology
- **Functional Programming**: Emphasizes immutability and functional composition
- **Type Safety**: Strong static typing with dependent type support
- **Modularity**: Well-organized standard library with clear hierarchical structure

## Development Status

The language is actively being developed with:
- Type checking system implementation
- Standard library expansion  
- Custom operator system
- OCaml backend compilation
- Comprehensive error reporting

Note: The language is currently in development and may have compilation issues with complex programs as the type system continues to be refined.