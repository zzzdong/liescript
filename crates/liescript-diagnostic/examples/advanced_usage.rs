//! 高级使用示例
//! 
//! 展示LieScript诊断系统的高级功能，包括错误链、多文件支持等。

use liescript_diagnostic::{
    diagnostic::{DiagnosticBuilder, DiagnosticKind, Phase, Severity},
    lsp::{LspDiagnosticManager, LspDiagnosticConfig},
    reporter::DiagnosticCollector,
    source::{ SourceMap},
};
use liescript_lexical::{FileId, Span};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 创建源代码管理器（支持多文件）
    let mut source_map = SourceMap::new();
    
    // 添加多个源代码文件
    let main_file_id = source_map.add_file(
        "main.lie".to_string(),
        r#"use math;

fn main() {
    let result = math::add(5, "hello"); // 类型错误
    println(result);
}
"#
        .to_string(),
    );

    let math_file_id = source_map.add_file(
        "math.lie".to_string(),
        r#"pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
"#
        .to_string(),
    );

    // 创建诊断收集器
    let mut collector = DiagnosticCollector::new(source_map.clone());

    // 创建复杂的错误链
    let main_error_span = Span::new(main_file_id, 40, 60);
    let math_error_span = Span::new(math_file_id, 20, 30);

    // 相关错误（在math模块中）
    let related_error = DiagnosticBuilder::new(
        Severity::Info,
        Phase::Semantic,
        "I001",
        "函数定义",
        math_error_span,
    )
    .description("函数'add'期望两个i32参数")
    .build();

    // 主要错误（在main模块中）
    let main_error = DiagnosticBuilder::new(
        DiagnosticKind::TypeMismatch.severity(),
        DiagnosticKind::TypeMismatch.phase(),
        DiagnosticKind::TypeMismatch.code(),
        "参数类型不匹配",
        main_error_span,
    )
    .description("调用math::add时，第二个参数类型不匹配")
    .suggestion("将字符串转换为整数，或者使用正确的参数类型")
    .related(related_error)
    .build();

    collector.add(main_error);

    // 创建未定义函数错误
    let undefined_func_span = Span::new(main_file_id, 70, 80);
    let undefined_error = DiagnosticBuilder::new(
        DiagnosticKind::UndefinedFunction.severity(),
        DiagnosticKind::UndefinedFunction.phase(),
        DiagnosticKind::UndefinedFunction.code(),
        "未定义的函数",
        undefined_func_span,
    )
    .description("函数'println'未定义")
    .suggestion("可能是'print'或'println!'，或者需要导入相应模块")
    .help_url("https://docs.liescript.dev/stdlib")
    .build();

    collector.add(undefined_error);

    // 使用LSP诊断管理器
    let lsp_config = LspDiagnosticConfig {
        enabled: true,
        max_diagnostics: 50,
        include_info: true,
        include_hints: true,
        delay_ms: 200,
    };

    let mut lsp_manager = LspDiagnosticManager::new(source_map, lsp_config);

    // 添加诊断到LSP管理器
    for diagnostic in collector.diagnostics() {
        lsp_manager.add_diagnostic(diagnostic.clone());
    }

    // 发布所有诊断
    lsp_manager.publish_all();

    // 输出结果
    println!("=== 高级诊断报告 ===");
    collector.report_to_console(true)?;

    println!("\n=== LSP诊断管理器状态 ===");
    println!("配置: {:?}", lsp_manager.config());
    println!("已发布诊断数量: {}", lsp_manager.publisher().get_diagnostics().len());

    // 演示错误链处理
    println!("\n=== 错误链处理演示 ===");
    for diagnostic in collector.diagnostics() {
        if !diagnostic.related.is_empty() {
            println!("主要错误: {}", diagnostic.message);
            for related in &diagnostic.related {
                println!("  相关错误: {} (在文件: {})", 
                    related.message, 
                    collector.source_map().get_file_path(related.file_id()).unwrap_or("unknown")
                );
            }
        }
    }

    // 演示多文件支持
    println!("\n=== 多文件支持演示 ===");
    for file_id in collector.source_map().file_ids() {
        if let Some(path) = collector.source_map().get_file_path(file_id) {
            println!("文件: {} (ID: {})", path, file_id.id());
        }
    }

    Ok(())
}