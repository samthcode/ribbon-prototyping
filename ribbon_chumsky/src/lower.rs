//! This module contains the `Lowerer`, which is responsible for lowering the Ribbon AST to its Intermediate Representation (IR)

use chumsky::span::{SpanWrap, Spanned};
use string_interner::DefaultStringInterner;

use crate::arena::Arena;
use crate::ast;
use crate::ir::{self, BlockId, DefId, ExprId, ItemId, LocalDefId, PatId, TyId};
use crate::span::Span;

pub struct Lowerer {
    interner: DefaultStringInterner,

    items: Arena<ir::Item>,
    exprs: Arena<ir::Expr>,
    local_defs: Arena<ir::LocalDef>,
    defs: Arena<ir::Def>,
    pats: Arena<ir::Pat>,
    tys: Arena<ir::Ty>,
    blocks: Arena<ir::Block>,
}

impl Lowerer {
    fn new() -> Self {
        Self {
            interner: DefaultStringInterner::new(),
            items: Arena::new(),
            exprs: Arena::new(),
            local_defs: Arena::new(),
            defs: Arena::new(),
            pats: Arena::new(),
            tys: Arena::new(),
            blocks: Arena::new(),
        }
    }

    pub fn lower(items: Vec<Spanned<ast::Item>>) -> ir::Ir {
        let mut lowerer = Lowerer::new();
        let root_items = items.into_iter().map(|i| lowerer.lower_item(i)).collect();

        ir::Ir {
            root_items,
            interner: lowerer.interner,
            items: lowerer.items,
            exprs: lowerer.exprs,
            local_defs: lowerer.local_defs,
            defs: lowerer.defs,
            pats: lowerer.pats,
            tys: lowerer.tys,
            blocks: lowerer.blocks,
        }
    }

    fn lower_item(&mut self, item: Spanned<ast::Item>) -> ItemId {
        let item = match item.inner {
            ast::Item::Binding(binding) => {
                ir::Item::Binding(self.lower_binding(binding, item.span.into()))
            }
            ast::Item::Expr(expr) => ir::Item::Expr(self.lower_expr(expr.with_span(item.span))),
            ast::Item::TypeDef(type_def) => todo!(),
            ast::Item::FuncDef(func_def) => ir::Item::FuncDef(self.lower_func(func_def)),
        };
        self.items.alloc(item)
    }

    fn lower_expr(&mut self, expr: Spanned<ast::Expr>) -> ExprId {
        let span = expr.span.into();
        use ir::ExprKind::*;
        let kind = match expr.inner {
            ast::Expr::Path(path) => Path(self.lower_path(path, span)),
            ast::Expr::Num(n) => Num(self.interner.get_or_intern(n)),
            ast::Expr::String(s) => String(self.interner.get_or_intern(s)),
            ast::Expr::Char(c) => Char(c),
            ast::Expr::Bool(b) => Bool(b),
            ast::Expr::List(exprs) => List(exprs.into_iter().map(|e| self.lower_expr(e)).collect()),
            ast::Expr::ListIndex(spanned, spanned1) => todo!(),
            ast::Expr::Bin(lhs, op, rhs) => Bin {
                lhs: self.lower_expr(*lhs),
                op: op.inner,
                op_span: op.span.into(),
                rhs: self.lower_expr(*rhs),
            },
            ast::Expr::Unary(op, rhs) => Unary {
                op: op.inner,
                op_span: op.span.into(),
                rhs: self.lower_expr(*rhs),
            },
            ast::Expr::Block(block) => Block(self.lower_block(*block, span)),
            ast::Expr::AnonFunc(func) => todo!(),
            ast::Expr::Binding(binding) => Binding(self.lower_binding(*binding, span)),
            ast::Expr::FunctionCall(spanned, spanneds) => todo!(),
            ast::Expr::FieldAccess(spanned, spanned1) => todo!(),
            ast::Expr::MethodCall(method_style_call) => todo!(),
            ast::Expr::ChainedFunctionCall(method_style_call) => todo!(),
        };
        self.exprs.alloc(ir::Expr { span, kind })
    }

    fn lower_binding(&mut self, binding: ast::Binding, span: Span) -> ir::Binding {
        let ast::Binding { pat, ty, val } = binding;
        let (pat, defs) = self.lower_pat(pat);
        let ty = ty.map(|t| self.lower_ty(t));
        let val = self.lower_expr(val);
        ir::Binding { pat, defs, ty, val }
    }

    fn lower_block(&mut self, block: ast::Block, span: Span) -> BlockId {
        let ast::Block { items, ret } = block;
        let items = items.into_iter().map(|i| self.lower_item(i)).collect();
        let ret = ret.map(|r| self.lower_item(r));
        self.blocks.alloc(ir::Block { span, items, ret })
    }

    fn lower_func(&mut self, func: ast::FuncDef) -> DefId {
        let ast::FuncDef {
            name,
            params,
            ret_ty,
            body,
        } = func;
        let name = self.interner.get_or_intern(name.inner.0);
        let params = params.into_iter().map(|p| self.lower_param(p)).collect();
        let ret_ty = ret_ty.map(|t| self.lower_ty(t));
        let body = self.lower_expr(body);
        self.defs.alloc(ir::Def::Func {
            name,
            params,
            ret_ty,
            body,
        })
    }

    fn lower_param(&mut self, param: Spanned<ast::Param>) -> ir::Param {
        let ast::Param { pat, ty, default } = param.inner;
        let span: Span = param.span.into();
        let (pat, defs) = self.lower_pat(pat);
        let ty = self.lower_ty(ty);
        let default = default.map(|d| self.lower_expr(d));
        ir::Param {
            span,
            pat,
            defs,
            ty,
            default,
        }
    }

    fn lower_pat(&mut self, pat: Spanned<ast::Pat>) -> (PatId, Vec<LocalDefId>) {
        let span: Span = pat.span.into();
        match pat.inner {
            ast::Pat::Ident(ident) => {
                let name = self.interner.get_or_intern(ident.0);
                let local_def_id = self.local_defs.alloc(ir::LocalDef {
                    name,
                    pat_id: PatId::placeholder(),
                    ty: None,
                    // TODO
                    mutable: ir::Mutability::Not,
                });
                let pat_id = self.pats.alloc(ir::Pat {
                    span,
                    kind: ir::PatKind::Ident {
                        name,
                        local_def_id: local_def_id.clone(),
                    },
                });
                self.local_defs[local_def_id.clone()].pat_id = pat_id.clone();

                (pat_id, vec![local_def_id])
            }
        }
    }

    fn lower_ty(&mut self, ty: Spanned<ast::Ty>) -> TyId {
        let span: Span = ty.span.into();
        match ty.inner {
            ast::Ty::Path(path) => {
                let lowered_ty = ir::Ty {
                    span,
                    kind: ir::TyKind::Unresolved(ir::UnresolvedTyKind::Path(
                        self.lower_path(path, span),
                    )),
                };
                self.tys.alloc(lowered_ty)
            }
            ast::Ty::Func(spanneds, spanned) => todo!(),
        }
    }

    fn lower_path(&mut self, path: ast::Path, span: Span) -> ir::Path {
        let ast::Path { segments } = path;
        let segments = segments
            .into_iter()
            .map(|s| self.lower_path_segment(s))
            .collect();
        ir::Path {
            segments,
            // n.b. This will be filled in during name resolution
            def_id: None,
        }
    }

    fn lower_path_segment(&mut self, segment: Spanned<ast::PathSegment>) -> ir::PathSegment {
        let span = segment.span.into();
        let ast::PathSegment { ident, generics } = segment.inner;
        let sym = self.interner.get_or_intern(ident.inner.0);
        let sym_span = ident.span.into();
        let generics = generics.map(|g| g.into_iter().map(|t| self.lower_ty(t)).collect());
        ir::PathSegment {
            span,
            sym,
            sym_span,
            generics,
        }
    }
}
