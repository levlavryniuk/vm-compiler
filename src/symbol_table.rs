use std::collections::HashMap;

#[derive(Debug)]
pub enum SymbolKind {
    Variable,
    Function,
    Type,
    Param,
}

#[derive(Debug)]
pub struct Symbol {
    pub kind: SymbolKind,
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.symbols.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.symbols.pop();
    }
    pub fn insert(&mut self, name: &str, symbol: Symbol) {
        self.symbols
            .last_mut()
            .unwrap()
            .insert(name.to_string(), symbol);
    }
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.symbols.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}
