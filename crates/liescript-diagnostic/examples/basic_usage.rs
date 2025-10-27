//! 基本使用示例
//!
//! 展示如何使用LieScript诊断系统的基本功能。

use liescript_diagnostic::{
    diagnostic::{DiagnosticBuilder, DiagnosticKind, Phase, Severity},
    reporter::{ConsoleReporter, DiagnosticCollector},
    source::SourceMap,
};
use liescript_lexical::{FileId, Span};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 创建源代码管理器
    let mut source_map = SourceMap::new();

    // 添加源代码文件
    let file_id = source_map.add_file(
        "example.lie".to_string(),
        r#"fn main() {
    let x = 42;
    let y = x + "hello"; // 类型错误
}
"#
        .to_string(),
    );

    // 创建诊断收集器
    let mut collector = DiagnosticCollector::new(source_map);

    // 创建类型错误诊断
    let type_error_span = Span::new(file_id, 40, 50);
    let type_error = DiagnosticBuilder::new(
        DiagnosticKind::TypeMismatch.severity(),
        DiagnosticKind::TypeMismatch.phase(),
        DiagnosticKind::TypeMismatch.code(),
        "类型不匹配",
        type_error_span,
    )
    .description("无法将字符串类型与整数类型相加")
    .suggestion("确保操作数类型匹配，或者使用类型转换")
    .help_url("https://docs.liescript.dev/type-system")
    .build();

    collector.add(type_error);

    // 创建未使用变量警告
    let unused_span = Span::new(file_id, 15, 16);
    let unused_warning = DiagnosticBuilder::new(
        DiagnosticKind::UnusedVariable.severity(),
        DiagnosticKind::UnusedVariable.phase(),
        DiagnosticKind::UnusedVariable.code(),
        "未使用的变量",
        unused_span,
    )
    .description("变量'x'被声明但从未使用")
    .suggestion("考虑删除此变量或使用它")
    .label("unused")
    .build();

    collector.add(unused_warning);

    // 报告到控制台
    println!("=== 基本诊断报告 ===");
    collector.report_to_console(false)?;

    println!("\n=== 详细诊断报告 ===");
    collector.report_to_console(true)?;

    // 转换为LSP诊断信息
    let lsp_diagnostics = collector.to_lsp_diagnostics()?;
    println!("\n=== LSP诊断信息 (JSON格式) ===");
    for diag in lsp_diagnostics {
        println!("{}", serde_json::to_string_pretty(&diag)?);
    }

    // 检查是否有错误
    if collector.has_errors() {
        println!("\n编译失败: 发现错误");
    } else {
        println!("\n编译成功: 只有警告");
    }

    Ok(())
}
