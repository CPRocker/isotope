#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    name: String,
    kind: SymbolKind,
    r#type: Option<Type>,
    scope: usize,
    offset: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Function,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Unknown,
    Boolean,
    Number,
    String,
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    symbols: std::collections::HashMap<String, Symbol>,
    current_scope: usize,
}

impl SymbolTable {
    pub fn add(&mut self, name: String, kind: SymbolKind, r#type: Option<Type>) {
        let symbol = Symbol {
            name: name.clone(),
            kind,
            r#type,
            scope: self.current_scope,
            offset: None,
        };
        self.symbols.insert(name, symbol);
    }

    pub fn exists(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn update_offset(&mut self, name: &str, offset: usize) {
        if let Some(symbol) = self.symbols.get_mut(name) {
            symbol.offset = Some(offset);
        }
    }

    pub fn enter_scope(&mut self) {
        self.current_scope += 1;
    }

    pub fn exit_scope(&mut self) {
        self.symbols
            .retain(|_, symbol| symbol.scope < self.current_scope);
        self.current_scope -= 1;
    }
}
