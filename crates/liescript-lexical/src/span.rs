use std::fmt;

use serde::{Deserialize, Serialize};

/// 文件标识符
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FileId(u32);

impl FileId {
    /// 创建新的文件ID
    pub fn new(id: u32) -> Self {
        FileId(id)
    }

    /// 获取内部ID值
    pub fn id(&self) -> u32 {
        self.0
    }
}

impl fmt::Display for FileId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "file://{}", self.0)
    }
}

// 默认文件ID为0，only use for test
impl Default for FileId {
    fn default() -> Self {
        FileId(0)
    }
}

/// 源代码位置范围
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    /// 文件标识符
    pub file: FileId,
    /// 起始位置（字节偏移）
    pub start: u32,
    /// 结束位置（字节偏移）
    pub end: u32,
}

impl Span {
    /// 创建新Span
    pub fn new(file: FileId, start: u32, end: u32) -> Self {
        assert!(start <= end, "起始位置不能大于结束位置");
        Span { file, start, end }
    }

    /// 创建虚拟Span（用于没有具体位置的信息）
    pub fn dummy() -> Self {
        Span {
            file: FileId::new(0),
            start: 0,
            end: 0,
        }
    }

    /// 分割Span
    pub fn split(&self, offset: u32) -> (Span, Span) {
        assert!(offset <= self.end - self.start, "分割位置超出范围");
        (
            Span {
                file: self.file,
                start: self.start,
                end: self.start + offset,
            },
            Span {
                file: self.file,
                start: self.start + offset,
                end: self.end,
            },
        )
    }

    /// 转换为Range<usize>
    pub fn to_range(&self) -> std::ops::Range<usize> {
        self.start as usize..self.end as usize
    }

    /// 合并两个Span
    pub fn merge(&self, other: Span) -> Span {
        assert!(self.file == other.file, "只能合并同一文件的Span");
        Span {
            file: self.file,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// 检查Span是否包含位置
    pub fn contains(&self, pos: u32) -> bool {
        self.start <= pos && pos < self.end
    }

    /// 获取Span长度
    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    /// 检查Span是否为空
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}-{}", self.file, self.start, self.end)
    }
}

/// 带位置信息的类型
#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    /// 值
    pub value: T,
    /// 位置信息
    pub span: Span,
}

impl<T> Spanned<T> {
    /// 创建新的Spanned值
    pub fn new(value: T, span: Span) -> Self {
        Spanned { value, span }
    }

    /// 获取位置信息
    pub fn span(&self) -> Span {
        self.span
    }

    /// 获取值引用
    pub fn value(&self) -> &T {
        &self.value
    }

    /// 获取文件ID
    pub fn file(&self) -> FileId {
        self.span.file
    }

    /// 映射值
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }
}

impl<T: PartialEq> PartialEq<Spanned<T>> for Spanned<T> {
    fn eq(&self, other: &Spanned<T>) -> bool {
        self.value == other.value
    }
}

impl<T: PartialEq> PartialEq<T> for Spanned<T> {
    fn eq(&self, other: &T) -> bool {
        &self.value == other
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> From<T> for Spanned<T> {
    fn from(value: T) -> Self {
        Spanned::new(value, Span::dummy())
    }
}

/// 位置信息接口
pub trait HasSpan {
    /// 获取位置信息
    fn span(&self) -> Span;
}

impl<T> HasSpan for Spanned<T> {
    fn span(&self) -> Span {
        self.span
    }
}
