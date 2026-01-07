//! This module is responsible for the resolution of symbols in the Ribbon IR
//! (Intermediate Representation). The first pass of this resolution is the
//! `ScopewideDefCollector`, which traverses the IR and collects items which are
//! visible to all elements in their scope, regardless of definition order,
//! namely function and type definitions. The second pass is the
//! `SymbolResolver`, which modifies the IR in-place, replacing paths and
//! identifiers with variants which correspond to the identifiers of the items
//! to which they refer

use std::collections::HashMap;

use crate::ir::visit::Visitor;

use super::*;

pub fn resolve<'ir>(ir: &'ir mut Ir, debug: bool) -> Result<(), Vec<&'static str>> {
    let scope_graph = IntemporalCollector::new(ir).collect();
    if debug {
        println!("{scope_graph:#?}")
    }
    if scope_graph.errors.is_empty() {
        Ok(())
    } else {
        Err(scope_graph.errors)
    }
}

struct TraversableScopeGraph {
    inner: ScopeGraph,
    visited: Vec<scope::ScopeIdx>,
}

impl TraversableScopeGraph {
    fn new(mut scope_graph: ScopeGraph) -> Self {
        scope_graph.curr_scope_idx = 0;
        Self {
            inner: scope_graph,
            visited: Vec::new(),
        }
    }
}

impl scope::ScopeGraph for TraversableScopeGraph {
    fn push_scope(&mut self, scope_kind: scope::ScopeKind) -> scope::ScopeIdx {
        let mut candidate = self.inner.curr_scope_idx + 1;
        loop {
            if candidate == self.inner.scopes.len() {
                panic!("scope graph differs from what is expected")
            }
            if self.visited.contains(&candidate) {
                candidate += 1;
            } else {
                if self.inner.scopes[candidate].kind != scope_kind {
                    panic!("scope graph differs from what is expected")
                }
                return candidate;
            }
        }
    }

    fn pop_scope(&mut self) {
        self.visited.push(self.inner.curr_scope_idx);
        self.inner.curr_scope_idx = self
            .inner
            .curr_scope_mut()
            .parent
            .expect("tried to pop global scope in scope graph traversal")
    }
}

struct SymbolResolver<'ir> {
    scope_graph: TraversableScopeGraph,
    ir: &'ir mut Ir,
}

impl<'ir> SymbolResolver<'ir> {
    pub fn new(ir: &'ir mut Ir, scope_graph: ScopeGraph) -> Self {
        Self {
            scope_graph: TraversableScopeGraph::new(scope_graph),
            ir,
        }
    }

    pub fn resolve(&mut self) -> Result<(), Vec<&'static str>> {
        todo!()
    }
}

#[derive(Debug, Default)]
struct Scope {
    kind: scope::ScopeKind,
    parent: Option<scope::ScopeIdx>,
    local_defs: HashMap<Symbol, DefId>,
    // Intemporal definitions: these are available anywhere in their scope
    //                         regardless of definition order
    function_defs: HashMap<Symbol, DefId>,
    ty_defs: HashMap<Symbol, DefId>,
}

#[derive(Debug)]
struct ScopeGraph {
    scopes: Vec<Scope>,
    curr_scope_idx: scope::ScopeIdx,
    errors: Vec<&'static str>,
}

impl ScopeGraph {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            curr_scope_idx: 0,
            errors: Vec::new(),
        }
    }

    #[inline]
    fn curr_scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[self.curr_scope_idx]
    }

    fn add_function(&mut self, name: Symbol, def_id: DefId) {
        let curr = self.curr_scope_mut();
        if curr.function_defs.contains_key(&name) {
            self.errors
                .push("cannot redefine function within the same scope");
            return;
        }
        curr.function_defs.insert(name, def_id);
    }

    fn add_ty(&mut self, name: Symbol, def_id: DefId) {
        let curr = self.curr_scope_mut();
        if curr.ty_defs.contains_key(&name) {
            self.errors
                .push("cannot redefine type within the same scope");
            return;
        }
        curr.ty_defs.insert(name, def_id);
    }
}

impl scope::ScopeGraph for ScopeGraph {
    fn push_scope(&mut self, scope_kind: scope::ScopeKind) -> scope::ScopeIdx {
        self.scopes.push(Scope {
            kind: scope_kind,
            parent: Some(self.curr_scope_idx),
            ..Default::default()
        });
        self.curr_scope_idx = self.scopes.len() - 1;
        self.curr_scope_idx
    }

    fn pop_scope(&mut self) {
        self.curr_scope_idx = self.scopes[self.curr_scope_idx]
            .parent
            .expect("attempted to pop global scope")
    }
}

#[derive(Debug)]
struct IntemporalCollector<'ir> {
    ir: &'ir Ir,
    scope_graph: ScopeGraph,
}

impl<'ir> IntemporalCollector<'ir> {
    fn new(ir: &'ir Ir) -> Self {
        Self {
            ir,
            scope_graph: ScopeGraph::new(),
        }
    }

    fn collect(mut self) -> ScopeGraph {
        for item in &self.ir.root_items {
            self.visit_item(item)
        }
        self.scope_graph
    }
}

impl<'ir> scope::ScopedVisitor<'ir> for IntemporalCollector<'ir> {
    fn ir(&self) -> &'ir Ir {
        self.ir
    }

    fn scope_graph(&mut self) -> &mut impl scope::ScopeGraph {
        &mut self.scope_graph
    }

    fn collect_def(&mut self, def_id: &DefId) {
        let def = &self.ir.defs[def_id];
        match def {
            Def::Local { .. } => (),
            Def::Func { name, .. } => self.scope_graph.add_function(*name, def_id.clone()),
            Def::Type { name, .. } => self.scope_graph.add_ty(*name, def_id.clone()),
            Def::Module => todo!(),
        }
    }
}
