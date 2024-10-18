/// A span represents a range of text in the source code.
/// It is used to provide more detailed error messages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32, // Byte offset from the start of the file
    pub end: u32,   // Byte offset from the start of the file
    pub line: u32,  // Line number (starting from 1)
    pub col: u32,   // Column number (starting from 1)
}

impl Span {
    pub fn new(start: u32, end: u32, line: u32, col: u32) -> Self {
        Span {
            start,
            end,
            line,
            col,
        }
    }
}
