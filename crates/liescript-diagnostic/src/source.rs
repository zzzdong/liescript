//! 源代码位置管理
//! 
//! 管理源代码文件、位置信息和跨文件引用。

use std::collections::HashMap;
use std::fmt;
use serde::{Serialize, Deserialize};
use liescript_lexical::{FileId, Span};


/// 源代码文件信息
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// 文件ID
    pub id: FileId,
    /// 文件路径
    pub path: String,
    /// 文件内容
    pub content: String,
    /// 行号映射（行号 -> 字节偏移）
    line_offsets: Vec<u32>,
}

impl SourceFile {
    /// 创建新的源代码文件
    pub fn new(id: FileId, path: String, content: String) -> Self {
        let line_offsets = Self::build_line_offsets(&content);
        SourceFile {
            id,
            path,
            content,
            line_offsets,
        }
    }

    /// 构建行号偏移映射
    fn build_line_offsets(content: &str) -> Vec<u32> {
        let mut offsets = vec![0];
        let mut current_offset = 0;

        for c in content.chars() {
            current_offset += c.len_utf8() as u32;
            if c == '\n' {
                offsets.push(current_offset);
            }
        }

        offsets
    }

    /// 获取行号对应的字节偏移范围
    pub fn line_range(&self, line: usize) -> Option<(u32, u32)> {
        if line >= self.line_offsets.len() {
            return None;
        }

        let start = self.line_offsets[line];
        let end = if line + 1 < self.line_offsets.len() {
            self.line_offsets[line + 1]
        } else {
            self.content.len() as u32
        };

        Some((start, end))
    }

    /// 获取位置对应的行号和列号
    pub fn position(&self, offset: u32) -> Option<(usize, usize)> {
        if offset > self.content.len() as u32 {
            return None;
        }

        let line = self.line_offsets
            .binary_search(&offset)
            .unwrap_or_else(|line| line - 1);

        let line_start = self.line_offsets[line];
        let column = (offset - line_start) as usize;

        Some((line, column))
    }

    /// 获取指定行的内容
    pub fn line_content(&self, line: usize) -> Option<&str> {
        if let Some((start, end)) = self.line_range(line) {
            Some(&self.content[start as usize..end as usize])
        } else {
            None
        }
    }

    /// 获取Span对应的源代码片段
    pub fn snippet(&self, span: Span) -> Option<&str> {
        if span.file != self.id {
            return None;
        }

        let range = span.to_range();
        if range.end <= self.content.len() {
            Some(&self.content[range])
        } else {
            None
        }
    }
}

/// 源代码管理器
#[derive(Debug, Default, Clone)]
pub struct SourceMap {
    /// 文件映射
    files: HashMap<FileId, SourceFile>,
    /// 下一个文件ID
    next_file_id: u32,
}

impl SourceMap {
    /// 创建新的源代码管理器
    pub fn new() -> Self {
        SourceMap {
            files: HashMap::new(),
            next_file_id: 1, // 0保留给虚拟文件
        }
    }

    /// 添加源代码文件
    pub fn add_file(&mut self, path: String, content: String) -> FileId {
        let id = FileId::new(self.next_file_id);
        self.next_file_id += 1;

        let file = SourceFile::new(id, path, content);
        self.files.insert(id, file);
        id
    }

    /// 获取文件信息
    pub fn get_file(&self, id: FileId) -> Option<&SourceFile> {
        self.files.get(&id)
    }

    /// 获取文件路径
    pub fn get_file_path(&self, id: FileId) -> Option<&str> {
        self.files.get(&id).map(|f| f.path.as_str())
    }

    /// 获取所有文件ID
    pub fn file_ids(&self) -> Vec<FileId> {
        self.files.keys().copied().collect()
    }

    /// 获取位置对应的行号和列号
    pub fn position(&self, span: Span) -> Option<(String, usize, usize)> {
        self.get_file(span.file)
            .and_then(|file| {
                file.position(span.start)
                    .map(|(line, col)| (file.path.clone(), line, col))
            })
    }

    /// 获取Span对应的源代码片段
    pub fn snippet(&self, span: Span) -> Option<String> {
        self.get_file(span.file)
            .and_then(|file| file.snippet(span).map(|s| s.to_string()))
    }

    /// 获取指定行的内容
    pub fn line_content(&self, file_id: FileId, line: usize) -> Option<String> {
        self.get_file(file_id)
            .and_then(|file| file.line_content(line).map(|s| s.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_operations() {
        let file_id = FileId::new(1);
        let span = Span::new(file_id, 10, 20);

        assert_eq!(span.len(), 10);
        assert!(!span.is_empty());
        assert!(span.contains(15));
        assert!(!span.contains(25));

        let (left, right) = span.split(5);
        assert_eq!(left.end, 15);
        assert_eq!(right.start, 15);
    }

    #[test]
    fn test_source_file() {
        let content = "line1\nline2\nline3".to_string();
        let file = SourceFile::new(FileId::new(1), "test.lie".to_string(), content);

        assert_eq!(file.line_content(0), Some("line1\n"));
        assert_eq!(file.line_content(1), Some("line2\n"));
        assert_eq!(file.line_content(2), Some("line3"));

        let pos = file.position(8); // 'line2'的第二个字符
        assert_eq!(pos, Some((1, 1)));
    }

    #[test]
    fn test_source_map() {
        let mut source_map = SourceMap::new();
        let file_id = source_map.add_file("test.lie".to_string(), "content".to_string());

        assert!(source_map.get_file(file_id).is_some());
        assert_eq!(source_map.get_file_path(file_id), Some("test.lie"));
    }
}