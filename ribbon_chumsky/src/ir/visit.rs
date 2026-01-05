use super::*;

pub trait Visitor<'ir>: Sized {
    fn ir(&self) -> &'ir Ir;
    fn visit_item(&mut self, item_id: &ItemId) {
        let item = &self.ir().items[item_id];
        walk_item(self, item);
    }
    fn visit_expr(&mut self, expr_id: &ExprId) {
        let expr = &self.ir().exprs[expr_id];
        walk_expr(self, expr);
    }
    fn visit_pat(&mut self, pat_id: &PatId) {
        let pat = &self.ir().pats[pat_id];
        walk_pat(self, pat);
    }
    fn visit_ty(&mut self, ty_id: &TyId) {
        let ty = &self.ir().tys[ty_id];
        walk_ty(self, ty);
    }
    fn visit_block(&mut self, block_id: &BlockId) {
        let block = &self.ir().blocks[block_id];
        walk_block(self, block);
    }

    fn visit_binding(&mut self, binding: &Binding) {
        walk_binding(self, binding)
    }
    fn visit_path(&mut self, path: &Path) {
        walk_path(self, path)
    }
    fn visit_param(&mut self, param: &Param) {
        walk_param(self, param)
    }
}

pub fn walk_item<'ir, V: Visitor<'ir>>(v: &mut V, item: &Item) {
    match item {
        Item::Binding(binding) => v.visit_binding(binding),
        Item::Expr(_) => (),
        Item::TypeDef => todo!(),
        Item::FuncDef(_) => todo!(),
    }
}

pub fn walk_expr<'ir, V: Visitor<'ir>>(v: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Var(_)
        | ExprKind::String(_)
        | ExprKind::Char(_)
        | ExprKind::Bool(_)
        | ExprKind::Num(_) => (),
        ExprKind::Path(path) => v.visit_path(path),
        ExprKind::List(ids) => ids.iter().for_each(|expr_id| v.visit_expr(expr_id)),
        ExprKind::Binding(binding) => v.visit_binding(binding),
        ExprKind::Block(id) => v.visit_block(id),
        // TODO: Unsure of what the default behaviour should be here
        ExprKind::AnonFunc { .. } => (),
        ExprKind::If { cond, then, else_ } => {
            v.visit_expr(cond);
            v.visit_block(then);
            else_.as_ref().inspect(|&e| v.visit_block(e));
        }
        ExprKind::Bin { lhs, rhs, .. } => {
            v.visit_expr(lhs);
            v.visit_expr(rhs);
        }
        ExprKind::Unary { rhs, .. } => v.visit_expr(rhs),
    }
}

pub fn walk_pat<'ir, V: Visitor<'ir>>(v: &mut V, pat: &Pat) {
    match &pat.kind {
        PatKind::Ident { .. } => (),
        PatKind::Tuple(ids) => todo!(),
    }
}

pub fn walk_ty<'ir, V: Visitor<'ir>>(v: &mut V, ty: &Ty) {
    todo!()
}

pub fn walk_block<'ir, V: Visitor<'ir>>(v: &mut V, block: &Block) {
    for item in &block.items {
        v.visit_item(item);
    }
    block.ret.as_ref().map(|r| v.visit_item(r));
}

pub fn walk_binding<'ir, V: Visitor<'ir>>(v: &mut V, binding: &Binding) {
    v.visit_pat(&binding.pat);
    v.visit_expr(&binding.val);
}

pub fn walk_path<'ir, V: Visitor<'ir>>(v: &mut V, path: &Path) {
    for segment in &path.segments {
        segment.generics.as_ref().map(|generics| {
            for ty_id in generics {
                v.visit_ty(ty_id);
            }
        });
    }
}

pub fn walk_param<'ir, V: Visitor<'ir>>(v: &mut V, param: &Param) {
    v.visit_pat(&param.pat);
    v.visit_ty(&param.ty);
}

type ScopeIdx = usize;

pub trait ScopeGraph {
    type Scope;
    fn push_scope(&mut self) -> ScopeIdx;
    fn pop_scope(&mut self);
    fn curr_scope(&self) -> &Self::Scope;
    fn curr_scope_mut(&mut self) -> &mut Self::Scope;
}

/// The purpose of this is to define a constant shape of scope graph for each
/// program. There are multiple visitors which need to traverse the IR while
/// storing data in scopes, and this ensures that they enter and leave scopes
/// under the same conditions, such that their scope graphs are identical in
/// shape
pub trait ScopedVisitor<'ir, 's, S: ScopeGraph + 's>: Visitor<'ir>
where
    'ir: 's,
{
    fn scope_graph(&mut self) -> &'s mut S;

    /// Note that this is intentionally named differently to indicate that, with
    /// the default implementations of this trait, it is not intended to be
    /// solely responsible for a def, it is more of a supplementary function
    /// which can perform tasks such as collecting the definition for future use
    /// in a separate data structure
    fn walk_def(&mut self, def_id: &DefId);

    fn visit_block(&mut self, block: &BlockId) {
        let block = &self.ir().blocks[block];
        self.scope_graph().push_scope();
        walk_block(self, block);
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
                        self.walk_def(id);
                        self.scope_graph().push_scope();
                        for param in params {
                            self.visit_param(param);
                        }
                        ScopedVisitor::visit_expr(self, body);
                        self.scope_graph().pop_scope();
                        ret_ty.as_ref().map(|ty| self.visit_ty(ty));
                    }
                    _ => panic!("func def item not tied to func def"),
                }
            }
            Item::Expr(id) => ScopedVisitor::visit_expr(self, id),
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
            ExprKind::Block(id) => ScopedVisitor::visit_block(self, id),
            ExprKind::AnonFunc {
                params,
                ret_ty,
                body,
            } => {
                self.scope_graph().push_scope();
                for param in params {
                    self.visit_param(param);
                }
                ScopedVisitor::visit_expr(self, body);
                self.scope_graph().pop_scope();
                ret_ty.as_ref().map(|ty| self.visit_ty(ty));
            }
            ExprKind::If { cond, then, else_ } => {
                self.scope_graph().push_scope();
                ScopedVisitor::visit_expr(self, cond);
                ScopedVisitor::visit_block(self, then);
                self.scope_graph().pop_scope();

                self.scope_graph().push_scope();
                else_.as_ref().map(|e| ScopedVisitor::visit_block(self, e));
                self.scope_graph().pop_scope();
            }
            ExprKind::Bin { lhs, rhs, .. } => {
                ScopedVisitor::visit_expr(self, lhs);
                ScopedVisitor::visit_expr(self, rhs);
            }
            ExprKind::Unary { rhs, .. } => ScopedVisitor::visit_expr(self, rhs),
        }
    }
}
