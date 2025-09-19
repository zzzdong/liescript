use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl FileId {
    pub fn new(id: u32) -> Self {
        FileId(id)
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }
}

impl Default for FileId {
    fn default() -> Self {
        FileId(0)
    }
}

#[derive(Debug)]
pub struct SourceFile {
    pub path: PathBuf,
    pub src: String,
}

impl SourceFile {
    pub fn new(path: impl Into<PathBuf>, src: String) -> SourceFile {
        SourceFile {
            path: path.into(),
            src,
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn source(&self) -> &str {
        &self.src
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }

    pub fn is_empty(&self) -> bool {
        self.src.is_empty()
    }
}

#[derive(Debug)]
pub struct SourceMap {
    files: BTreeMap<FileId, SourceFile>,
    next_file_id: AtomicU32,
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            files: BTreeMap::new(),
            next_file_id: AtomicU32::new(1), // 0 保留给虚拟文件
        }
    }

    /// 添加新源文件并返回其 FileId
    pub fn add_file(&mut self, path: impl Into<PathBuf>, src: String) -> FileId {
        let file_id = FileId::new(self.next_file_id.fetch_add(1, Ordering::Relaxed));
        let source_file = SourceFile::new(path, src);
        self.files.insert(file_id, source_file);
        file_id
    }

    /// 获取文件数量
    pub fn len(&self) -> usize {
        self.files.len()
    }

    /// 检查是否为空
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    /// 根据 FileId 获取源文件
    pub fn get_file(&self, file_id: FileId) -> Option<&SourceFile> {
        self.files.get(&file_id)
    }

    /// 根据路径获取源文件
    pub fn get_file_by_path(&self, path: &Path) -> Option<&SourceFile> {
        self.files.values().find(|file| file.path == path)
    }

    /// 获取源代码内容
    pub fn source(&self, file_id: FileId) -> Option<&str> {
        self.get_file(file_id).map(|file| file.source())
    }

    /// 获取文件路径
    pub fn path(&self, file_id: FileId) -> Option<&Path> {
        self.get_file(file_id).map(|file| file.path())
    }

    /// 获取行号和列号（基于字节偏移量）
    pub fn line_col(&self, file_id: FileId, offset: usize) -> Option<(usize, usize)> {
        self.get_file(file_id).and_then(|file| {
            if offset > file.src.len() {
                return None;
            }

            let mut line = 1;
            let mut col = 1;
            let mut current_offset = 0;

            for c in file.src.chars() {
                if current_offset >= offset {
                    break;
                }

                if c == '\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }

                current_offset += c.len_utf8();
            }

            Some((line, col))
        })
    }

    /// 获取指定行的内容
    pub fn get_line(&self, file_id: FileId, line_num: usize) -> Option<&str> {
        self.get_file(file_id).and_then(|file| {
            if line_num == 0 {
                return None;
            }

            let mut current_line = 1;
            let mut line_start = 0;

            for (i, c) in file.src.char_indices() {
                if current_line == line_num {
                    // 找到行尾
                    let line_end = file.src[i..]
                        .find('\n')
                        .map(|pos| i + pos)
                        .unwrap_or(file.src.len());
                    return Some(&file.src[i..line_end]);
                }

                if c == '\n' {
                    current_line += 1;
                    line_start = i + 1;
                }
            }

            None
        })
    }

    /// 获取指定位置的字符
    pub fn get_char(&self, file_id: FileId, offset: usize) -> Option<char> {
        self.get_file(file_id)
            .and_then(|file| file.src[offset..].chars().next())
    }

    /// 获取文件大小
    pub fn file_size(&self, file_id: FileId) -> Option<usize> {
        self.get_file(file_id).map(|file| file.len())
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_and_get_file() {
        let mut source_map = SourceMap::new();
        let file_id = source_map.add_file("test.ls", "fn main() {}".to_string());

        assert!(source_map.get_file(file_id).is_some());
        assert_eq!(source_map.source(file_id), Some("fn main() {}"));
        assert_eq!(source_map.path(file_id).unwrap().to_str(), Some("test.ls"));
    }

    #[test]
    fn test_line_col_calculation() {
        let mut source_map = SourceMap::new();
        let source = "fn main() {\nprintln!(\"hello\");\n}".to_string();
        let file_id = source_map.add_file("test.ls", source);

        // 测试第一行
        assert_eq!(source_map.line_col(file_id, 0), Some((1, 1))); // 'f'
        assert_eq!(source_map.line_col(file_id, 10), Some((1, 11))); // ')'

        // 测试第二行
        assert_eq!(source_map.line_col(file_id, 13), Some((2, 2))); // 'p'
        assert_eq!(source_map.line_col(file_id, 30), Some((2, 19))); // ')'

        // 测试第三行
        assert_eq!(source_map.line_col(file_id, 31), Some((3, 1))); // '}'
    }

    #[test]
    fn test_get_line() {
        let mut source_map = SourceMap::new();
        let source = "fn main() {\n    println!(\"hello\");\n}".to_string();
        let file_id = source_map.add_file("test.ls", source);

        assert_eq!(source_map.get_line(file_id, 1), Some("fn main() {"));
        assert_eq!(
            source_map.get_line(file_id, 2),
            Some("    println!(\"hello\");")
        );
        assert_eq!(source_map.get_line(file_id, 3), Some("}"));
        assert_eq!(source_map.get_line(file_id, 4), None);
    }

    #[test]
    fn test_file_operations() {
        let mut source_map = SourceMap::new();
        assert!(source_map.is_empty());

        let file_id1 = source_map.add_file("file1.ls", "content1".to_string());
        let file_id2 = source_map.add_file("file2.ls", "content2".to_string());

        assert_eq!(source_map.len(), 2);
        assert!(!source_map.is_empty());

        assert_eq!(source_map.source(file_id1), Some("content1"));
        assert_eq!(source_map.source(file_id2), Some("content2"));
        assert_eq!(source_map.source(FileId::new(999)), None);
    }
}
