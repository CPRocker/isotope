use std::iter::Peekable;

use crate::source::Source;

pub(crate) struct CharReader<'r> {
    chars: Peekable<std::str::Chars<'r>>,
    offset: usize,
}

impl<'r> CharReader<'r> {
    /// Returns a `Result<CharRead>` because this will fail if the source is not valid UTF-8
    pub fn new(src: &'r Source) -> Result<Self, std::str::Utf8Error> {
        Ok(Self {
            chars: src.chars()?.peekable(),
            offset: 0,
        })
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
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

impl Iterator for CharReader<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next() {
            Some(c) => {
                self.offset += c.len_utf8();
                Some(c)
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn peek() {
        let code = "0123";
        let src = Source::from_str(code).expect("code is valid UTF-8");
        let mut reader = CharReader::new(&src).expect("code is valid UTF-8");

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
