# LieScript 语言参考手册

## 概述

LieScript 是一种静态类型的编程语言，语法设计参考 Rust，但简化了生命周期和宏等高级功能。语言提供运行时支持，包括垃圾回收和无栈异步支持。主要支持 Linux 和 Windows 平台，使用 Cranelift 进行代码生成，llvm-lld 作为链接器。

## 词法结构

### 标识符
- 格式：`[a-zA-Z_][a-zA-Z0-9_]*`
- 示例：`x`, `my_variable`, `MyStruct`

### 关键字

#### 严格关键字
- `as`, `break`, `const`, `continue`, `crate`, `else`, `enum`, `extern`
- `false`, `fn`, `for`, `if`, `impl`, `let`, `loop`, `match`, `mod`
- `move`, `mut`, `pub`, `ref`, `return`, `self`, `Self`, `static`
- `struct`, `super`, `trait`, `true`, `type`, `unsafe`, `use`
- `where`, `while`, `in`

#### 保留关键字
- `abstract`, `become`, `box`, `do`, `final`, `macro`, `override`
- `priv`, `try`, `typeof`, `unsized`, `virtual`, `yield`

#### 弱关键字
- `async`, `await`, `dyn`

#### 类型关键字
- `any`, `bool`, `byte`, `char`, `string`, `int`, `float`

### 字面量

#### 布尔字面量
- `true`, `false`

#### 数值字面量
- 整数：`42`, `-10`, `0xFF`, `0o77`
- 浮点数：`3.14`, `-2.5`, `1e10`

#### 字符字面量
- 单引号：`'a'`, `'\n'`, `'\u{1F600}'`

#### 字符串字面量
- 双引号：`"hello"`, `"world\n"`

### 标点符号

#### 单字符标点
- `=`, `<`, `>`, `!`, `~`, `+`, `-`, `*`, `/`, `%`, `^`, `&`, `|`
- `@`, `.`, `,`, `;`, `:`, `#`, `$`, `?`, `_`, `{`, `}`, `[`, `]`, `(`, `)`

#### 多字符标点
- 比较：`<=`, `==`, `!=`, `>=`
- 逻辑：`&&`, `||`
- 位移：`<<`, `>>`
- 复合赋值：`+=`, `-=`, `*=`, `/=`, `%=`, `^=`, `&=`, `|=`, `<<=`, `>>=`
- 范围：`..`, `...`, `..=`
- 路径：`::`
- 箭头：`->`, `<-`, `=>`

## 语法结构

### 表达式 (Expression)

LieScript 的表达式系统严格遵循 Rust 规范，支持丰富的表达式类型：

#### 基本表达式
- **字面量表达式**：`42`, `"hello"`, `true`
- **路径表达式**：`std::io::stdout`, `my_function`
- **运算符表达式**：`a + b`, `x && y`

#### 复合表达式
- **块表达式**：`{ let x = 42; x + 1 }`
- **控制流表达式**：
  - `if condition { ... } else { ... }`
  - `while condition { ... }`
  - `for item in collection { ... }`
  - `match value { pattern => expr, ... }`
- **函数调用表达式**：`function_name(arg1, arg2)`
- **结构体表达式**：`Point { x: 10, y: 20 }`
- **闭包表达式**：`|x| x + 1`
- **异步表达式**：`async { ... }`, `future.await`

### 语句 (Statement)

#### 声明语句
- **变量声明**：`let x = 42;`, `let mut y = 10;`
- **常量声明**：`const MAX_VALUE: i32 = 100;`
- **静态变量声明**：`static COUNTER: i32 = 0;`

#### 控制流语句
- `return value;`
- `break;`, `break label;`
- `continue;`, `continue label;`

### 项声明 (Item)

模块级别的声明，包括：

#### 函数定义
```liescript
fn function_name(param1: Type1, param2: Type2) -> ReturnType {
    // 函数体
}

// 泛型函数
fn generic_function<T>(value: T) -> T {
    value
}
```

#### 结构体定义
```liescript
// 命名字段结构体
struct Point {
    x: i32,
    y: i32,
}

// 元组结构体
struct Color(u8, u8, u8);

// 单元结构体
struct Unit;
```

#### 枚举定义
```liescript
enum Option<T> {
    Some(T),
    None,
}

enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}
```

#### 类型别名
```liescript
type MyInt = i32;
type Result<T> = std::result::Result<T, Error>;
```

#### 模块定义
```liescript
mod my_module {
    // 模块内容
}
```

#### 导入声明
```liescript
use std::io;
use std::collections::{HashMap, HashSet};
```

### 模式匹配 (Pattern)

#### 基本模式
- **字面量模式**：`0`, `true`, `"hello"`
- **变量模式**：`x`
- **通配符模式**：`_`

#### 结构模式
- **结构体模式**：`Point { x, y }`
- **元组模式**：`(0, y)`, `(x, 0)`
- **引用模式**：`&x`

### 类型系统 (Type)

#### 原生类型
- `any` - 任意类型
- `bool` - 布尔值
- `byte` - 字节
- `int` - 整数
- `float` - 浮点数
- `char` - 字符
- `string` - 字符串

#### 复合类型
- **引用类型**：`&i32`, `&mut String`
- **数组类型**：`[i32; 5]`
- **切片类型**：`[i32]`
- **元组类型**：`(i32, String)`, `()`
- **函数指针类型**：`fn(i32) -> i32`
- **Never 类型**：`!`
- **推断类型**：`_`

### 名称解析 (Name)

#### 路径系统
- **简单路径**：`std::io`
- **类型路径**：`Vec<String>`
- **表达式路径**：`collection.iter()`

#### 可见性
- `pub` - 公开可见性
- `pub(crate)` - 模块级可见性

## 运算符系统

### 一元运算符 (UnOp)
- `-` (负号)
- `!` (逻辑非)
- `*` (解引用)

### 二元运算符 (BinOp)

#### 算术运算符
- `+`, `-`, `*`, `/`, `%`

#### 比较运算符
- `==`, `!=`, `<`, `>`, `<=`, `>=`

#### 逻辑运算符
- `&&`, `||`

#### 位运算符
- `&`, `|`, `^`, `<<`, `>>`

#### 赋值运算符
- `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`

### 后缀运算符 (PostfixOp)
- `.` (成员访问)
- `[]` (索引)
- `()` (函数调用)
- `?` (错误传播)

## 解析器架构

### 解析上下文 (ParseContext)
解析器使用 `ParseContext` 结构来管理解析状态：

```rust
pub struct ParseContext<'i> {
    iter: Peekable<Iter<'i, TokenSpan>>,      // Token 迭代器
    config: &'i ParserConfig,                  // 解析器配置
    diagnostics: &'i mut Diagnostics,          // 诊断信息收集器
    recovery_stack: Vec<RecoveryPoint<'i>>,   // 错误恢复栈
    lsp_state: Option<LspState>,              // LSP 状态信息
}
```

### 错误恢复机制
解析器支持智能错误恢复，包括：
- **恢复点保存**：保存迭代器状态用于错误恢复
- **诊断信息收集**：记录解析错误和警告
- **LSP 集成**：支持语言服务器协议功能

### 模块化解析
解析器按语法组件分类组织：
- `expression.rs` - 表达式解析
- `item.rs` - 项声明解析  
- `name.rs` - 名称解析
- `pattern.rs` - 模式匹配解析
- `statement.rs` - 语句解析
- `type.rs` - 类型解析

## 编译流程

### 词法分析
- 输入：源代码文本
- 输出：Token 流
- 功能：识别关键字、标识符、字面量、标点符号

### 语法分析
- 输入：Token 流
- 输出：抽象语法树 (AST)
- 功能：构建完整的语法结构

### 语义分析
- 输入：AST
- 输出：语义信息
- 功能：类型检查、名称解析、错误检测

### 代码生成
- 输入：语义分析结果
- 输出：目标代码
- 工具：Cranelift (AOT) + llvm-lld (链接)

## 语言特性

### 静态类型系统
- 强类型检查
- 类型推断支持
- 泛型编程

### 内存安全
- 引用和借用系统
- 移动语义
- 自动内存管理 (GC)

### 异步编程
- 无栈协程支持
- async/await 语法
- 异步 I/O 操作

### 错误处理
- Result 类型
- ? 操作符错误传播
- 可恢复错误机制

## 标准库预览

### 基本类型
- 原生类型：`any`, `bool`, `byte`, `int`, `float`, `char`, `string`
- 容器类型：`Vec<T>`, `HashMap<K, V>`, `Option<T>`, `Result<T, E>`

### IO 操作
```liescript
use std::io;

let mut input = String::new();
io::stdin().read_line(&mut input)?;
println!("{}", input);
```

### 并发编程
```liescript
use std::thread;

let handle = thread::spawn(|| {
    // 并发任务
});
handle.join().unwrap();
```

## 开发工具

### 编译器命令
```bash
liescript build main.ls    # 编译项目
liescript run main.ls      # 运行程序
liescript check main.ls    # 语法检查
```

### 项目结构
```
my_project/
├── Cargo.toml      # 项目配置
├── src/
│   ├── main.ls     # 主入口文件
│   └── lib.ls      # 库文件
└── target/         # 编译输出
```

## 语法速查表

### 变量声明
```liescript
let x = value;           // 不可变变量
let mut x = value;       // 可变变量
const NAME: Type = value; // 常量
static NAME: Type = value; // 静态变量
```

### 函数定义
```liescript
fn name(param: Type) -> ReturnType { body }
```

### 控制流
```liescript
if condition { } else { }
while condition { }
for item in iter { }
loop { }
match value { patterns }
```

### 错误处理
```liescript
Result<T, E>
value?  // 错误传播
```

### 异步编程
```liescript
async fn name() -> Type { }
value.await
async { }
```

---

*本文档基于 LieScript 的实际代码实现，反映了语言的当前设计状态。随着语言的发展，内容将持续更新。*