use super::{visit::*, *};

pub type ScopeIdx = usize;

#[derive(Debug, Default, PartialEq, Eq)]
pub enum ScopeKind {
    /// An anonymous scope such as the body of a function or a block
    #[default]
    Anonymous,
    /// A named scope i.e. a module
    Named(Symbol),
    /// The scope of a type definition, which contains associated functions, methods, etc.
    Type(DefId),
    /// A special scope specifically for callsites which will allow an upcoming feature (TODO)
    /// object.method(arg1, arg2, arg3)
    ///               ^^^^^^^^^^^^^^^^ callsite
    Callsite(DefId),
}

pub trait ScopeGraph {
    fn push_scope(&mut self, scope_kind: ScopeKind) -> ScopeIdx;
    fn pop_scope(&mut self);
}

// /// The purpose of this is to define a constant shape of scope graph for each
// /// program. There are multiple visitors which need to traverse the IR while
// /// storing data in scopes, and this ensures that they enter and leave scopes
// /// under the same conditions, such that their scope graphs are identical in
// /// shape
// #[derive(Debug)]
// pub struct ScopedVisitor<'ir, S: ScopeGraph> {
//     ir: &'ir Ir,
//     pub scope_graph: S,
// }

pub trait ScopedVisitor<'ir> {
    fn ir(&self) -> &'ir Ir;

    fn scope_graph(&mut self) -> &mut impl ScopeGraph;
    
    fn collect_def(&mut self, _def_id: &DefId) {}
}

impl<'ir, V: ScopedVisitor<'ir>> Visitor<'ir> for V {
    fn ir(&self) -> &'ir Ir {
        ScopedVisitor::ir(self)
    }

    fn visit_block(&mut self, block: &BlockId) {
        let block = &self.ir().blocks[block];
        self.scope_graph().push_scope(ScopeKind::Anonymous);
        for item in &block.items {
            self.visit_item(item);
        }
        block.ret.as_ref().map(|r| self.visit_item(r));
        self.scope_graph().pop_scope();
    }

    fn visit_item(&mut self, item_id: &ItemId) {
        let item = &self.ir().items[item_id];
        match item {
            Item::Binding(binding) => self.visit_binding(binding),
            Item::FuncDef(id) => {
                let func_def = &self.ir().defs[id];
                match func_def {
                    Def::Func {
                        name: _,
                        params,
                        ret_ty,
                        body,
                    } => {
                        self.collect_def(id);
                        self.scope_graph().push_scope(ScopeKind::Anonymous);
                        for param in params {
                            self.visit_param(param);
                        }
                        self.visit_expr(body);
                        self.scope_graph().pop_scope();
                        ret_ty.as_ref().map(|ty| self.visit_ty(ty));
                    }
                    _ => panic!("func def item not tied to func def"),
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
            | ExprKind::Num(_)
            | ExprKind::String(_)
            | ExprKind::Char(_)
            | ExprKind::Bool(_)
            | ExprKind::List(_) => (),
            ExprKind::Path(path) => self.visit_path(path),
            ExprKind::Binding(binding) => self.visit_binding(binding),
            ExprKind::Block(id) => self.visit_block(id),
            ExprKind::AnonFunc {
                params,
                ret_ty,
                body,
            } => {
                self.scope_graph().push_scope(ScopeKind::Anonymous);
                for param in params {
                    self.visit_param(param);
                }
                self.visit_expr(body);
                self.scope_graph().pop_scope();
                ret_ty.as_ref().map(|ty| self.visit_ty(ty));
            }
            ExprKind::If { cond, then, else_ } => {
                self.scope_graph().push_scope(ScopeKind::Anonymous);
                self.visit_expr(cond);
                self.visit_block(then);
                self.scope_graph().pop_scope();

                self.scope_graph().push_scope(ScopeKind::Anonymous);
                else_.as_ref().map(|e| self.visit_block(e));
                self.scope_graph().pop_scope();
            }
            ExprKind::Bin { lhs, rhs, .. } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Unary { rhs, .. } => self.visit_expr(rhs),
        }
    }

    fn visit_pat(&mut self, pat_id: &PatId) {
        let pat = &self.ir().pats[pat_id];
        visit::walk_pat(self, pat);
    }

    fn visit_ty(&mut self, ty_id: &TyId) {
        let ty = &self.ir().tys[ty_id];
        visit::walk_ty(self, ty);
    }

    fn visit_binding(&mut self, binding: &Binding) {
        visit::walk_binding(self, binding);
    }

    fn visit_path(&mut self, path: &Path) {
        visit::walk_path(self, path);
    }

    fn visit_param(&mut self, param: &Param) {
        visit::walk_param(self, param);
    }

    fn collect_def(&mut self, _def_id: &DefId) {
        ScopedVisitor::collect_def(self, _def_id);
    }
}

// impl<'ir, S: ScopeGraph> ScopedVisitor<'ir, S> {
//     pub fn new(ir: &'ir Ir, scope_graph: S) -> Self {
//         Self {
//             ir,
//             scope_graph,
//         }
//     }

//     pub fn scope_graph(&mut self) -> &mut S {
//         &mut self.scope_graph
//     }
// }

// impl<'ir, S: ScopeGraph> Visitor<'ir> for ScopedVisitor<'ir, S> {
//     fn ir(&self) -> &'ir Ir {
//         self.ir
//     }

//     fn visit_block(&mut self, block: &BlockId) {
//         let block = &self.ir().blocks[block];
//         self.scope_graph().push_scope(ScopeKind::Anonymous);
//         for item in &block.items {
//             self.visit_item(item);
//         }
//         block.ret.as_ref().map(|r| self.visit_item(r));
//         self.scope_graph().pop_scope();
//     }

//     fn visit_item(&mut self, item_id: &ItemId) {
//         let item = &self.ir().items[item_id];
//         match item {
//             Item::Binding(binding) => self.visit_binding(binding),
//             Item::FuncDef(id) => {
//                 let func_def = &self.ir().defs[id];
//                 match func_def {
//                     Def::Func {
//                         name: _,
//                         params,
//                         ret_ty,
//                         body,
//                     } => {
//                         self.collect_def(id);
//                         self.scope_graph().push_scope(ScopeKind::Anonymous);
//                         for param in params {
//                             self.visit_param(param);
//                         }
//                         self.visit_expr(body);
//                         self.scope_graph().pop_scope();
//                         ret_ty.as_ref().map(|ty| self.visit_ty(ty));
//                     }
//                     _ => panic!("func def item not tied to func def"),
//                 }
//             }
//             Item::Expr(id) => self.visit_expr(id),
//             Item::TypeDef => todo!(),
//         }
//     }
//     fn visit_expr(&mut self, expr_id: &ExprId) {
//         let expr = &self.ir().exprs[expr_id];
//         match &expr.kind {
//             ExprKind::Var(_)
//             | ExprKind::Num(_)
//             | ExprKind::String(_)
//             | ExprKind::Char(_)
//             | ExprKind::Bool(_)
//             | ExprKind::List(_) => (),
//             ExprKind::Path(path) => self.visit_path(path),
//             ExprKind::Binding(binding) => self.visit_binding(binding),
//             ExprKind::Block(id) => self.visit_block(id),
//             ExprKind::AnonFunc {
//                 params,
//                 ret_ty,
//                 body,
//             } => {
//                 self.scope_graph().push_scope(ScopeKind::Anonymous);
//                 for param in params {
//                     self.visit_param(param);
//                 }
//                 self.visit_expr(body);
//                 self.scope_graph().pop_scope();
//                 ret_ty.as_ref().map(|ty| self.visit_ty(ty));
//             }
//             ExprKind::If { cond, then, else_ } => {
//                 self.scope_graph().push_scope(ScopeKind::Anonymous);
//                 self.visit_expr(cond);
//                 self.visit_block(then);
//                 self.scope_graph().pop_scope();

//                 self.scope_graph().push_scope(ScopeKind::Anonymous);
//                 else_.as_ref().map(|e| self.visit_block(e));
//                 self.scope_graph().pop_scope();
//             }
//             ExprKind::Bin { lhs, rhs, .. } => {
//                 self.visit_expr(lhs);
//                 self.visit_expr(rhs);
//             }
//             ExprKind::Unary { rhs, .. } => self.visit_expr(rhs),
//         }
//     }

//     fn visit_pat(&mut self, pat_id: &PatId) {
//         let pat = &self.ir().pats[pat_id];
//         visit::walk_pat(self, pat);
//     }

//     fn visit_ty(&mut self, ty_id: &TyId) {
//         let ty = &self.ir().tys[ty_id];
//         visit::walk_ty(self, ty);
//     }

//     fn visit_binding(&mut self, binding: &Binding) {
//         visit::walk_binding(self, binding);
//     }

//     fn visit_path(&mut self, path: &Path) {
//         visit::walk_path(self, path);
//     }

//     fn visit_param(&mut self, param: &Param) {
//         visit::walk_param(self, param);
//     }
// }
