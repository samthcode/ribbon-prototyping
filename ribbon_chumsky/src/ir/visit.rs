use super::*;

pub trait Visitor<'ir>: Sized {
    fn ir(&self) -> &'ir Ir;
    fn visit_item(&mut self, item_id: &ItemId);
    fn visit_expr(&mut self, expr_id: &ExprId);
    fn visit_pat(&mut self, pat_id: &PatId);
    fn visit_ty(&mut self, ty_id: &TyId);
    fn visit_block(&mut self, block_id: &BlockId);

    fn visit_binding(&mut self, binding: &Binding);
    fn visit_path(&mut self, path: &Path);
    fn visit_param(&mut self, param: &Param);

    /// This method is optional as it is not meant to perform any recursive
    /// actions but is only to be used in collecting definitions etc. for
    /// use-cases where that is needed e.g. during name resolution
    fn collect_def(&mut self, _def_id: &DefId) {}
}

#[allow(unused)]
pub fn walk_item<'ir, V: Visitor<'ir>>(v: &mut V, item: &Item) {
    match item {
        Item::Binding(binding) => v.visit_binding(binding),
        Item::Expr(_) => (),
        Item::TypeDef => todo!(),
        Item::FuncDef(id) => {
            let Def::Func {
                name: _,
                params,
                ret_ty,
                body,
            } = &v.ir().defs[id]
            else {
                panic!("expected def to be function")
            };
            for param in params {
                v.visit_param(param);
            }
            ret_ty.as_ref().map(|ty| v.visit_ty(ty));
            v.visit_expr(body);
        }
    }
}

#[allow(unused)]
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

#[allow(unused)]
pub fn walk_pat<'ir, V: Visitor<'ir>>(_v: &mut V, pat: &Pat) {
    match &pat.kind {
        PatKind::Ident { .. } => (),
        PatKind::Tuple(_id) => todo!(),
    }
}

#[allow(unused)]
pub fn walk_ty<'ir, V: Visitor<'ir>>(_v: &mut V, _ty: &Ty) {
    todo!()
}

#[allow(unused)]
pub fn walk_block<'ir, V: Visitor<'ir>>(v: &mut V, block: &Block) {
    for item in &block.items {
        v.visit_item(item);
    }
    block.ret.as_ref().map(|r| v.visit_item(r));
}

#[allow(unused)]
pub fn walk_binding<'ir, V: Visitor<'ir>>(v: &mut V, binding: &Binding) {
    v.visit_pat(&binding.pat);
    v.visit_expr(&binding.val);
}

#[allow(unused)]
pub fn walk_path<'ir, V: Visitor<'ir>>(v: &mut V, path: &Path) {
    for segment in &path.segments {
        segment.generics.as_ref().map(|generics| {
            for ty_id in generics {
                v.visit_ty(ty_id);
            }
        });
    }
}

#[allow(unused)]
pub fn walk_param<'ir, V: Visitor<'ir>>(v: &mut V, param: &Param) {
    v.visit_pat(&param.pat);
    v.visit_ty(&param.ty);
}
