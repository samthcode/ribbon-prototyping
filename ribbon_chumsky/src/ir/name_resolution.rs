use std::collections::HashMap;

use super::{visit::Visitor, *};

#[derive(Debug, Default)]
pub struct DefScope {
    functions: HashMap<Symbol, DefId>,
    types: HashMap<Symbol, DefId>,
    parent: Option<usize>,
}

#[derive(Debug)]
pub struct DefScopeTable {
    scopes: Vec<DefScope>,
    curr_scope_idx: usize,
}

impl DefScopeTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![DefScope::default()],
            curr_scope_idx: 0,
        }
    }

    pub fn curr_scope_mut(&mut self) -> &mut DefScope {
        &mut self.scopes[self.curr_scope_idx]
    }

    pub fn enter_scope(&mut self) {
        let parent = self.curr_scope_idx;
        self.curr_scope_idx = self.scopes.len();
        self.scopes.push(DefScope {
            parent: Some(parent),
            ..Default::default()
        });
    }

    pub fn leave_scope(&mut self) {
        assert!(self.scopes.len() > 1, "popped root node of `DefScopeTable`");
        let scope = &self.scopes[self.curr_scope_idx];
        self.curr_scope_idx = scope.parent.unwrap()
    }

    // TODO: Proper error type
    pub fn add_function(&mut self, name: Symbol, def_id: DefId) -> Result<(), ()> {
        let curr = self.curr_scope_mut();
        if curr.functions.contains_key(&name) {
            // Error: cannot redefine function
            return Err(());
        }
        curr.functions.insert(name, def_id);
        Ok(())
    }

    // TODO: Proper error type
    pub fn add_type(&mut self, name: Symbol, def_id: DefId) -> Result<(), ()> {
        let curr = self.curr_scope_mut();
        if curr.types.contains_key(&name) {
            // Error: cannot redefine type
            return Err(());
        }
        curr.types.insert(name, def_id);
        Ok(())
    }

    pub fn lookup_function_from(&self, name: Symbol, scope_idx: usize) -> Option<DefId> {
        let mut curr_scope_idx = scope_idx;
        loop {
            let scope = &self.scopes[curr_scope_idx];
            if let Some(def_id) = scope.functions.get(&name) {
                return Some(def_id.clone());
            }
            curr_scope_idx = scope.parent?;
        }
    }

    pub fn lookup_type_from(&self, name: Symbol, scope_idx: usize) -> Option<DefId> {
        let mut curr_scope_idx = scope_idx;
        loop {
            let scope = &self.scopes[curr_scope_idx];
            if let Some(def_id) = scope.types.get(&name) {
                return Some(def_id.clone());
            }
            curr_scope_idx = scope.parent?;
        }
    }
}

/// This is responsible for collecting the definitions of functions and types by scope such that
/// they can be referred to anywhere where they are in scope regardless of definition order
pub struct DefCollector<'ir> {
    ir: &'ir Ir,
    def_scopes: DefScopeTable,
    // TODO: Proper errors
    errors: Vec<()>,
}

impl<'ir> DefCollector<'ir> {
    pub fn collect_defs(ir: &'ir Ir) -> DefScopeTable {
        let mut collector = DefCollector {
            ir,
            def_scopes: DefScopeTable::new(),
            errors: Vec::new(),
        };
        for item in &collector.ir.root_items {
            collector.visit_item(item);
        }
        collector.def_scopes
    }
}

impl<'ir> Visitor<'ir> for DefCollector<'ir> {
    fn ir(&self) -> &'ir Ir {
        self.ir
    }

    fn visit_item(&mut self, item_id: &ItemId) {
        let item = &self.ir().items[item_id];
        match item {
            Item::Binding(binding) => self.visit_binding(binding),
            Item::FuncDef(def_id) => {
                let func = &self.ir().defs[def_id];
                match func {
                    Def::Func { name, body, .. } => {
                        if let Err(e) = self.def_scopes.add_function(*name, def_id.clone()) {
                            self.errors.push(e)
                        }
                        self.visit_expr(body);
                    }
                    Def::Type(_) | Def::Module => {
                        panic!("expected DefId to be tied to function def")
                    }
                }
            }
            Item::Expr(id) => self.visit_expr(id),
            Item::TypeDef => todo!(),
        }
    }

    fn visit_expr(&mut self, expr_id: &ExprId) {
        let expr = &self.ir().exprs[expr_id];
        match &expr.kind {
            ExprKind::Var(_)
            | ExprKind::Path(_)
            | ExprKind::Num(_)
            | ExprKind::String(_)
            | ExprKind::Char(_)
            | ExprKind::Bool(_) => (),
            ExprKind::List(ids) => {
                for id in ids {
                    self.visit_expr(id);
                }
            }
            ExprKind::Binding(binding) => self.visit_binding(binding),
            ExprKind::Block(id) => {
                let block = &self.ir().blocks[id];
                self.def_scopes.enter_scope();
                for item in &block.items {
                    self.visit_item(item);
                }
                block.ret.as_ref().map(|i| self.visit_item(i));
                self.def_scopes.leave_scope();
            }
            ExprKind::AnonFunc { body, .. } => self.visit_expr(body),
            ExprKind::If {
                cond,
                then,
                else_,
            } => todo!(),
            ExprKind::Bin { lhs, rhs, .. } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Unary { rhs, .. } => self.visit_expr(rhs),
        }
    }
}
