use crate::error;

pub mod tokens;

pub fn tokenize(content: String) -> Result<Vec<tokens::Token>, error::TokenizationError> {
    let mut tokens = Vec::new();

    let mut current_idx = 0;
    while current_idx < content.len() {
        let mut character = content.chars().nth(current_idx).unwrap();

        if character.is_whitespace() {
            // pass
        } else if character == ';' {
            tokens.push(tokens::Token::Semi);
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
                "ret" => tokens::Token::Return,
                _ => tokens::Token::Identifier(word),
            };
            tokens.push(token);
        }

        current_idx += 1;
    }

    tokens.push(tokens::Token::EOF);

    Ok(tokens)
}
