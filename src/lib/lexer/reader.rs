use std::iter::Peekable;

use utf8_chars::BufReadCharsExt;

pub(crate) struct CharReader<'r, R>
where
    R: std::io::BufRead,
{
    chars: Peekable<utf8_chars::Chars<'r, R>>,
    offset: usize,
}

impl<'r, R> CharReader<'r, R>
where
    R: std::io::BufRead,
{
    pub fn new(src: &'r mut R) -> Self {
        Self {
            chars: src.chars().peekable(),
            offset: 0,
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Note: this ignores errors and returns `None` instead.
    pub fn peek(&mut self) -> Option<&char> {
        match self.chars.peek() {
            Some(Ok(c)) => Some(c),
            Some(Err(_)) => None,
            None => None,
        }
    }

    pub fn skip_read_while(&mut self, predicate: impl Fn(&char) -> bool) {
        while let Some(c) = self.peek() {
            if predicate(c) {
                self.next();
            } else {
                break;
            }
        }
    }
}

/// A character reader for a `std::io::BufRead` which iterates over one character at
/// a time.
///
/// Note: this ignores errors and returns `None` (which ends the iterator) instead.
impl<R> Iterator for CharReader<'_, R>
where
    R: std::io::BufRead,
{
    type Item = char;

    /// Reads the chars buffer and returns the next char.
    ///
    /// Note: this ignores errors and returns `None` (which ends the iterator) instead.
    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next() {
            Some(Ok(c)) => {
                self.offset += c.len_utf8();
                Some(c)
            }
            Some(Err(_)) => None,
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn peek() {
        let code = "0123";
        let mut src = Cursor::new(code);
        let mut reader = CharReader::new(&mut src);

        assert_eq!(reader.peek(), Some(&'0'));
        assert_eq!(reader.peek(), Some(&'0'));
        assert_eq!(reader.next(), Some('0'));
        assert_eq!(reader.peek(), Some(&'1'));
        assert_eq!(reader.peek(), Some(&'1'));
        assert_eq!(reader.next(), Some('1'));
        assert_eq!(reader.peek(), Some(&'2'));
        assert_eq!(reader.peek(), Some(&'2'));
        assert_eq!(reader.next(), Some('2'));
        assert_eq!(reader.peek(), Some(&'3'));
        assert_eq!(reader.peek(), Some(&'3'));
        assert_eq!(reader.next(), Some('3'));
        assert_eq!(reader.peek(), None);
        assert_eq!(reader.peek(), None);
        assert_eq!(reader.next(), None);
    }
}
