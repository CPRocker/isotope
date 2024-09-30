#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub orig: String,
    pub offset: usize,
}

impl Token {
    pub fn span(&self) -> std::ops::Range<usize> {
        self.offset..self.offset + self.orig.len()
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Bang
            | TokenKind::BangEqual
            | TokenKind::Break
            | TokenKind::Caret
            | TokenKind::Comma
            | TokenKind::Dot
            | TokenKind::Equal
            | TokenKind::EqualEqual
            | TokenKind::Else
            | TokenKind::False
            | TokenKind::Function
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::If
            | TokenKind::LeftBracket
            | TokenKind::LeftCurly
            | TokenKind::LeftParen
            | TokenKind::Let
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Loop
            | TokenKind::Minus
            | TokenKind::Percent
            | TokenKind::Plus
            | TokenKind::Return
            | TokenKind::RightBracket
            | TokenKind::RightCurly
            | TokenKind::RightParen
            | TokenKind::Semicolon
            | TokenKind::Slash
            | TokenKind::Star
            | TokenKind::True => write!(f, "{}", self.kind),
            TokenKind::Identifier | TokenKind::Number | TokenKind::String => {
                write!(f, "{}", self.orig)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Bang,
    BangEqual,
    Break,
    Caret,
    Comma,
    Dot,
    Else,
    Equal,
    EqualEqual,
    False,
    Function,
    Greater,
    GreaterEqual,
    Identifier,
    If,
    LeftBracket,
    LeftCurly,
    LeftParen,
    Less,
    LessEqual,
    Let,
    Loop,
    Minus,
    Number,
    Percent,
    Plus,
    Return,
    RightBracket,
    RightCurly,
    RightParen,
    Semicolon,
    Slash,
    Star,
    String,
    True,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Else => write!(f, "else"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::If => write!(f, "if"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::LeftCurly => write!(f, "{{"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Loop => write!(f, "loop"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::RightCurly => write!(f, "}}"),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::String => write!(f, "string"),
            TokenKind::True => write!(f, "true"),
        }
    }
}
