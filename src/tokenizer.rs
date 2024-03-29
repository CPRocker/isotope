use crate::error;

pub mod tokens;

pub fn tokenize(content: String) -> Result<Vec<tokens::Token>, error::TokenizationError> {
    let mut tokens = Vec::new();

    let mut current_idx = 0;
    while current_idx < content.len() {
        let mut character = content.chars().nth(current_idx).unwrap();

        match character {
            '=' => tokens.push(tokens::Token::Equals),
            '(' => tokens.push(tokens::Token::LeftParen),
            ')' => tokens.push(tokens::Token::RightParen),
            '-' => tokens.push(tokens::Token::Minus),
            '+' => tokens.push(tokens::Token::Plus),
            ';' => tokens.push(tokens::Token::Semi),
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
                            return Err(error::TokenizationError::UnclosedBlockComment);
                        }

                        let mut prev_character =
                            content.chars().nth(current_idx - 1).unwrap_or_default();
                        character = content.chars().nth(current_idx).unwrap_or_default();

                        while prev_character != '*' || character != '/' {
                            if character == '\x00' {
                                return Err(error::TokenizationError::UnclosedBlockComment);
                            }

                            current_idx += 1;
                            prev_character = character;
                            character = content.chars().nth(current_idx).unwrap_or_default();
                        }
                    }
                    _ => {
                        current_idx -= 1;
                        tokens.push(tokens::Token::Slash);
                    }
                };
            }
            '*' => tokens.push(tokens::Token::Star),
            _ => {
                if character.is_whitespace() {
                    // pass
                } else if character.is_numeric() {
                    let mut word = String::new();
                    while character.is_numeric() {
                        word = format!("{word}{character}");

                        current_idx += 1;
                        if current_idx > content.len() {
                            return Err(error::TokenizationError::InvalidNumber);
                        }

                        character = content.chars().nth(current_idx).unwrap_or_default();
                    }
                    current_idx -= 1;

                    tokens.push(tokens::Token::Literal(word));
                } else if character.is_alphabetic() {
                    let mut word = String::new();
                    while character.is_alphanumeric() {
                        word = format!("{word}{character}");

                        current_idx += 1;
                        if current_idx > content.len() {
                            return Err(error::TokenizationError::InvalidIdentifier);
                        }

                        character = content.chars().nth(current_idx).unwrap();
                    }
                    current_idx -= 1;

                    let token = match word.as_str() {
                        "let" => tokens::Token::Let,
                        "return" => tokens::Token::Return,
                        _ => tokens::Token::Identifier(word),
                    };
                    tokens.push(token);
                } else {
                    return Err(error::TokenizationError::UnrecognizedCharacter(character));
                }
            }
        }

        current_idx += 1;
    }

    tokens.push(tokens::Token::Eof);

    Ok(tokens)
}
