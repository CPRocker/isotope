use crate::error::TokenizationError;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // compiler-time
    Eof,
    // single character
    Equals,
    LeftParen,
    RightParen,
    Minus,
    Plus,
    Slash,
    Star,
    Semi,
    // keywords
    Let,
    Return,
    // terminals
    Identifier(String),
    Literal(String),
}

pub fn tokenize(content: String) -> Result<Vec<Token>, TokenizationError> {
    let mut tokens = Vec::new();

    let mut current_idx = 0;
    while current_idx < content.len() {
        let mut character = content.chars().nth(current_idx).unwrap();

        match character {
            '=' => tokens.push(Token::Equals),
            '(' => tokens.push(Token::LeftParen),
            ')' => tokens.push(Token::RightParen),
            '-' => tokens.push(Token::Minus),
            '+' => tokens.push(Token::Plus),
            ';' => tokens.push(Token::Semi),
            '/' => {
                current_idx += 1;

                character = content.chars().nth(current_idx).unwrap_or_default();
                match character {
                    '/' => {
                        while character != '\n' && character != '\x00' {
                            current_idx += 1;
                            character = content.chars().nth(current_idx).unwrap_or_default();
                        }
                    }
                    '*' => {
                        current_idx += 2;
                        if current_idx >= content.len() {
                            return Err(TokenizationError::UnclosedBlockComment);
                        }

                        let mut prev_character =
                            content.chars().nth(current_idx - 1).unwrap_or_default();
                        character = content.chars().nth(current_idx).unwrap_or_default();

                        while prev_character != '*' || character != '/' {
                            if character == '\x00' {
                                return Err(TokenizationError::UnclosedBlockComment);
                            }

                            current_idx += 1;
                            prev_character = character;
                            character = content.chars().nth(current_idx).unwrap_or_default();
                        }
                    }
                    _ => {
                        current_idx -= 1;
                        tokens.push(Token::Slash);
                    }
                };
            }
            '*' => tokens.push(Token::Star),
            _ => {
                if character.is_whitespace() {
                    // pass
                } else if character.is_numeric() {
                    let mut word = String::new();
                    while character.is_numeric() {
                        word = format!("{word}{character}");

                        current_idx += 1;
                        if current_idx > content.len() {
                            return Err(TokenizationError::InvalidNumber);
                        }

                        character = content.chars().nth(current_idx).unwrap_or_default();
                    }
                    current_idx -= 1;

                    tokens.push(Token::Literal(word));
                } else if character.is_alphabetic() {
                    let mut word = String::new();
                    while character.is_alphanumeric() {
                        word = format!("{word}{character}");

                        current_idx += 1;
                        if current_idx > content.len() {
                            return Err(TokenizationError::InvalidIdentifier);
                        }

                        character = content.chars().nth(current_idx).unwrap();
                    }
                    current_idx -= 1;

                    let token = match word.as_str() {
                        "let" => Token::Let,
                        "return" => Token::Return,
                        _ => Token::Identifier(word),
                    };
                    tokens.push(token);
                } else {
                    return Err(TokenizationError::UnrecognizedCharacter(character));
                }
            }
        }

        current_idx += 1;
    }

    tokens.push(Token::Eof);

    Ok(tokens)
}
