# LieScript Diagnostic System

现代化的诊断系统，为LieScript编程语言提供全面的错误报告和诊断功能。

## 功能特性

- ✅ **多阶段错误报告** - 支持词法分析、语法分析、语义分析等不同阶段的错误诊断
- ✅ **丰富的错误信息** - 包含错误位置、错误类型、建议修复方案等详细信息
- ✅ **源代码高亮显示** - 在错误报告中高亮显示相关源代码片段
- ✅ **LSP集成支持** - 为Language Server Protocol提供诊断信息格式支持
- ✅ **错误分级系统** - 支持错误、警告、信息、提示等不同级别的诊断信息
- ✅ **多文件支持** - 能够处理跨文件的错误诊断和报告
- ✅ **错误链支持** - 支持相关错误的链式显示
- ✅ **终端彩色输出** - 提供美观的终端彩色诊断报告

## 快速开始

### 基本使用

```rust
use liescript_diagnostic::{
    diagnostic::{DiagnosticBuilder, DiagnosticKind, Phase, Severity},
    reporter::{ConsoleReporter, DiagnosticCollector},
    source::{FileId, SourceMap, Span},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 创建源代码管理器
    let mut source_map = SourceMap::new();
    
    // 添加源代码文件
    let file_id = source_map.add_file(
        "example.lie".to_string(),
        "let x = 42;".to_string(),
    );

    // 创建诊断收集器
    let mut collector = DiagnosticCollector::new(source_map);

    // 创建诊断信息
    let span = Span::new(file_id, 0, 3);
    let diagnostic = DiagnosticBuilder::new(
        Severity::Error,
        Phase::Parse,
        "E001",
        "语法错误",
        span,
    )
    .description("详细错误描述")
    .suggestion("修复建议")
    .build();

    collector.add(diagnostic);

    // 报告到控制台
    collector.report_to_console(true)?;

    Ok(())
}
```

### 使用预定义的错误类型

```rust
use liescript_diagnostic::diagnostic::DiagnosticKind;

let diagnostic = DiagnosticBuilder::new(
    DiagnosticKind::TypeMismatch.severity(),
    DiagnosticKind::TypeMismatch.phase(),
    DiagnosticKind::TypeMismatch.code(),
    "类型不匹配",
    span,
)
.build();
```

### LSP集成

```rust
use liescript_diagnostic::lsp::{LspDiagnosticManager, LspDiagnosticConfig};

let config = LspDiagnosticConfig::default();
let mut lsp_manager = LspDiagnosticManager::new(source_map, config);

lsp_manager.add_diagnostic(diagnostic);
lsp_manager.publish_all();

let lsp_diagnostics = lsp_manager.publisher().get_diagnostics();
```

## 核心组件

### Diagnostic（诊断信息）

诊断信息的基本结构，包含：
- 严重级别（Error/Warning/Info/Hint）
- 错误阶段（Lexical/Parse/TypeCheck等）
- 错误代码和消息
- 源代码位置（Span）
- 详细描述和修复建议
- 相关错误链

### SourceMap（源代码管理器）

管理多个源代码文件，提供：
- 文件ID管理
- 源代码位置计算
- 行号/列号转换
- 源代码片段提取

### Reporter（报告器）

提供多种输出格式：
- **ConsoleReporter** - 终端彩色输出
- **LspReporter** - LSP协议格式
- **DiagnosticCollector** - 诊断信息收集和管理

### LSP集成

完整的LSP协议支持：
- LspDiagnosticAdapter - 诊断信息转换
- LspDiagnosticPublisher - 诊断信息发布
- LspDiagnosticManager - 诊断管理

## 错误类型

### 预定义错误类型

| 错误类型 | 代码 | 严重级别 | 阶段 | 描述 |
|---------|------|----------|------|------|
| SyntaxError | E001 | Error | Parse | 语法错误 |
| TypeError | E002 | Error | TypeCheck | 类型错误 |
| UndefinedVariable | E003 | Error | Semantic | 未定义变量 |
| UndefinedFunction | E004 | Error | Semantic | 未定义函数 |
| TypeMismatch | E005 | Error | TypeCheck | 类型不匹配 |
| ArgumentCountMismatch | E006 | Error | Semantic | 参数数量不匹配 |
| DuplicateDefinition | E007 | Error | Semantic | 重复定义 |
| DivisionByZero | E008 | Error | Runtime | 除零错误 |
| MemoryError | E009 | Error | Runtime | 内存错误 |
| UnreachableCode | W001 | Warning | Semantic | 不可达代码 |
| UnusedVariable | W002 | Warning | Semantic | 未使用变量 |

## 示例

运行示例代码：

```bash
# 基本使用示例
cargo run --example basic_usage

# 高级功能示例
cargo run --example advanced_usage
```

## 集成到编译器

要将诊断系统集成到LieScript编译器中，需要：

1. 在编译器各阶段创建相应的诊断信息
2. 使用DiagnosticCollector收集所有诊断信息
3. 在编译完成后报告诊断结果
4. 根据诊断结果决定是否继续编译过程

## 配置

### LSP配置

```rust
let config = LspDiagnosticConfig {
    enabled: true,
    max_diagnostics: 100,
    include_info: true,
    include_hints: false,
    delay_ms: 100,
};
```

### 控制台输出配置

```rust
let reporter = ConsoleReporter::new(true); // true表示详细模式
```

## 许可证

本项目采用与LieScript项目相同的许可证。

## 贡献

欢迎提交Issue和Pull Request来改进诊断系统。