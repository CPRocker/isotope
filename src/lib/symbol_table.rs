#[derive(Debug, Clone, PartialEq)]
pub struct Symbol<'iso> {
    name: &'iso str,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Boolean,
    Number,
    String,
}

#[derive(Debug, Default)]
pub struct SymbolTable<'iso> {
    symbols: std::collections::HashMap<&'iso str, Symbol<'iso>>,
    current_scope: usize,
}

impl<'iso> SymbolTable<'iso> {
    pub fn add(&mut self, name: &'iso str, kind: SymbolKind, r#type: Option<Type>) {
        let symbol = Symbol {
            name,
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

    pub fn get(&self, name: &str) -> Option<&Symbol<'iso>> {
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
