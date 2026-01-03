use string_interner::{DefaultStringInterner, DefaultSymbol as Symbol};

use crate::{
    arena::{Arena, Id},
    ast,
    span::Span,
};

pub mod visit;

pub type ItemId = Id<Item>;
pub type ExprId = Id<Expr>;
pub type LocalDefId = Id<LocalDef>;
pub type DefId = Id<Def>;
pub type PatId = Id<Pat>;
pub type TyId = Id<Ty>;
pub type BlockId = Id<Block>;

#[derive(Debug, Clone)]
pub struct Ir {
    pub root_items: Vec<ItemId>,

    pub interner: DefaultStringInterner,

    pub items: Arena<Item>,
    pub exprs: Arena<Expr>,
    pub local_defs: Arena<LocalDef>,
    pub defs: Arena<Def>,
    pub pats: Arena<Pat>,
    pub tys: Arena<Ty>,
    pub blocks: Arena<Block>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Binding(Binding),
    FuncDef(DefId),
    Expr(ExprId),
    TypeDef,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Var(LocalDefId),
    Path(Path),

    // Literals
    Num(Symbol),
    String(Symbol),
    Char(char),
    Bool(bool),
    List(Vec<ExprId>),

    Binding(Binding),
    Block(BlockId),
    AnonFunc {
        params: Vec<Param>,
        /// Filled during type-checking
        ret_ty: Option<TyId>,
        body: ExprId,
    },
    If {
        cond: ExprId,
        then: BlockId,
        else_ifs: Option<Vec<(ExprId, BlockId)>>,
        else_: Option<BlockId>,
    },
    Bin {
        lhs: ExprId,
        op: ast::BinOp,
        op_span: Span,
        rhs: ExprId,
    },
    Unary {
        op: ast::UnaryOp,
        op_span: Span,
        rhs: ExprId,
    },
}

#[derive(Debug, Clone)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

#[derive(Debug, Clone)]
pub enum TyKind {
    /// Used as a placeholder before types are checked
    Unresolved(UnresolvedTyKind),
}

#[derive(Debug, Clone)]
pub enum UnresolvedTyKind {
    Path(Path),
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    /// Filled during name resolution
    pub def_id: Option<DefId>,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub pat: PatId,
    /// Variables defined within the pattern
    pub defs: Vec<LocalDefId>,
    pub ty: Option<TyId>,
    pub val: ExprId,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub span: Span,
    pub sym: Symbol,
    pub sym_span: Span,
    /// During name resolution the paths should resolve to `Def::Type`
    pub generics: Option<Vec<TyId>>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub span: Span,
    pub pat: PatId,
    pub defs: Vec<LocalDefId>,
    pub ty: TyId,
    pub default: Option<ExprId>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub span: Span,
    /// Expressions/statements which are followed by a semicolon
    pub items: Vec<ItemId>,
    /// A final optional expression which is returned if present
    pub ret: Option<ItemId>,
}

#[derive(Debug, Clone)]
pub struct Pat {
    pub span: Span,
    pub kind: PatKind,
}

#[derive(Debug, Clone)]
pub enum PatKind {
    Ident {
        name: Symbol,
        local_def_id: LocalDefId,
    },
    Tuple(Vec<PatId>),
}

/// Definition of a local variable, which comes from a binding
#[derive(Debug, Clone)]
pub struct LocalDef {
    pub name: Symbol,
    pub pat_id: PatId,
    /// Filled during type-checking
    pub ty: Option<TyId>,
    pub mutable: Mutability,
}

#[derive(Debug, Clone)]
pub enum Def {
    Func {
        name: Symbol,
        params: Vec<Param>,
        /// Filled during type-checking
        ret_ty: Option<TyId>,
        body: ExprId,
    },
    /// e.g. `type Point = struct {x:i32, y: i32};`
    Type(TyId),
    Module,
}

#[derive(Debug, Clone)]
pub enum Mutability {
    Not,
    Mutable,
}
