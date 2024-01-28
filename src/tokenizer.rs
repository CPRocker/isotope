pub mod tokens;

pub fn tokenize(content: String) -> Vec<tokens::Token> {
    return vec!(tokens::Token { content: String::new() });
}