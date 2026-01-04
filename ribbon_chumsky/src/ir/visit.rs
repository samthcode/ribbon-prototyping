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
        ExprKind::If {
            cond,
            then,
            else_ifs,
            else_,
        } => {
            v.visit_expr(cond);
            v.visit_block(then);
            else_ifs.as_ref().inspect(|elifs| {
                elifs.iter().for_each(|(c, b)| {
                    v.visit_expr(c);
                    v.visit_block(b);
                })
            });
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
