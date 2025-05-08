use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::frontend::mut_visitor::{walk_block, walk_let_statement};

use super::{
    ast::{FunctionDeclarationStmt, Identifier, LetStmt, Span, Stmt},
    mut_visitor::{MutVisitor, walk_ast, walk_function_declaration, walk_identifier},
};

#[derive(Debug, Clone)]
pub enum ResolutionError {
    UndefinedSymbol(UndefinedSymbolError),
}

#[derive(Debug, Clone)]
pub struct UndefinedSymbolError {
    pub name: String,
    pub span: Span,
    pub suggestions: Vec<String>, // optional similar symbols
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ScopeId(pub u64);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SymbolId(pub u64);

#[derive(Debug)]
pub struct SymbolTable {
    pub names: HashMap<String, SymbolId>,
    pub functions: HashMap<String, SymbolId>,
    pub types: HashMap<String, SymbolId>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub symbols: SymbolTable,

    pub parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn new(id: ScopeId, parent: Option<Rc<RefCell<Scope>>>) -> Self {
        Self {
            id,
            symbols: SymbolTable::new(),
            parent,
        }
    }

    pub fn register_name(&mut self, name: &str, id: SymbolId) {
        println!("Declared symbol `{}` -> {:?}", name, id);
        self.symbols.names.insert(name.into(), id);
    }

    pub fn register_function(&mut self, name: &str, id: SymbolId) {
        println!("Declared function `{}` -> {:?}", name, id);
        self.symbols.functions.insert(name.into(), id);
    }

    pub fn lookup_symbol(&mut self, name: &str) -> Option<SymbolId> {
        if let Some(symbol) = self.symbols.names.get(name) {
            return Some(symbol.clone());
        }

        if let Some(symbol) = self.symbols.functions.get(name) {
            return Some(symbol.clone());
        }

        if let Some(symbol) = self.symbols.types.get(name) {
            return Some(symbol.clone());
        }

        if let Some(parent) = &self.parent {
            parent.borrow_mut().lookup_symbol(name)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct NameResolver {
    scope_stack: Vec<Rc<RefCell<Scope>>>,
    pub scopes: HashMap<ScopeId, Rc<RefCell<Scope>>>,

    scope_id_last: u64,
    symbol_id_last: u64,

    errors: Vec<ResolutionError>,
}

impl NameResolver {
    pub fn new() -> Self {
        let mut nr = NameResolver {
            scope_stack: vec![],
            scopes: HashMap::new(),
            scope_id_last: 0,
            symbol_id_last: 0,
            errors: vec![],
        };

        nr.push_scope(); // global
        nr
    }

    fn next_symbol_id(&mut self) -> SymbolId {
        let id = SymbolId(self.symbol_id_last);
        self.symbol_id_last += 1;
        id
    }

    pub fn solve(&mut self, ast: &mut [Stmt]) -> Result<(), Vec<ResolutionError>> {
        walk_ast(self, ast);

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn push_scope(&mut self) -> ScopeId {
        let id = ScopeId(self.scope_id_last);
        self.scope_id_last += 1;
        let scope = Rc::new(RefCell::new(Scope::new(
            id.clone(),
            self.scope_stack.last().cloned(),
        )));
        self.scope_stack.push(scope.clone());

        self.scopes.insert(id.clone(), scope.clone());

        id
    }

    fn pop_scope(&mut self) -> ScopeId {
        let scope = self.scope_stack.pop();
        scope.unwrap().borrow().id.clone()
    }

    pub fn scope(&mut self) -> Rc<RefCell<Scope>> {
        self.scope_stack.last().cloned().unwrap()
    }

    pub fn register_name(&mut self, name: &str) {
        self.scope()
            .borrow_mut()
            .register_name(name, self.next_symbol_id());
    }

    pub fn register_function(&mut self, name: &str) {
        self.scope()
            .borrow_mut()
            .register_function(name, self.next_symbol_id());
    }

    pub fn symbol_lookup(&mut self, name: &str) -> Option<SymbolId> {
        self.scope().borrow_mut().lookup_symbol(name)
    }
}

impl MutVisitor for NameResolver {
    fn visit_identifier(&mut self, identifier: &mut Identifier) {
        if let Some(symbol) = self.symbol_lookup(&identifier.name) {
            println!("Referenced symbol {:?} (`{}`)", symbol, identifier.name);
            identifier.resolved = Some(symbol.clone());
        } else {
            println!("Referenced non-existant symbol `{}`", identifier.name);
            self.errors
                .push(ResolutionError::UndefinedSymbol(UndefinedSymbolError {
                    name: identifier.name.clone(),
                    span: identifier.span.clone(),
                    suggestions: vec![],
                }));
        }

        walk_identifier(self, identifier);
    }

    fn visit_let_statement(&mut self, stmt: &mut LetStmt) {
        self.register_name(&stmt.name);

        walk_let_statement(self, stmt);
    }

    fn visit_function_declaration(&mut self, stmt: &mut FunctionDeclarationStmt) {
        self.register_function(&stmt.name);

        // register params
        for param in &stmt.parameters {
            self.register_name(&param.name.name);
        }

        walk_function_declaration(self, stmt);
    }

    fn visit_block(&mut self, block: &mut super::ast::BlockExpr) {
        let id = self.push_scope();
        block.scope_id = id.clone();

        println!("Entering scope: {:?}", block.scope_id);

        walk_block(self, block);
        let id = self.pop_scope();
        println!("Leaving scope: {:?}", id);
    }
}
