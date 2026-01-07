use string_interner::{DefaultStringInterner, DefaultSymbol as Symbol};

use crate::{
    arena::{Arena, Id},
    ast,
    span::Span,
};

pub mod resolve;
pub mod scope;
pub mod visit;

pub type ItemId = Id<Item>;
pub type ExprId = Id<Expr>;
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
    pub defs: Arena<Def>,
    pub pats: Arena<Pat>,
    pub tys: Arena<Ty>,
    pub blocks: Arena<Block>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Item {
    Binding(Binding),
    FuncDef(DefId),
    Expr(ExprId),
    TypeDef,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ExprKind {
    Var(DefId),
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TyKind {
    /// Used as a placeholder before types are checked
    Unresolved(UnresolvedTyKind),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum UnresolvedTyKind {
    Path(Path),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    /// Filled during name resolution
    pub def_id: Option<DefId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Binding {
    pub pat: PatId,
    /// Variables defined within the pattern
    pub defs: Vec<DefId>,
    pub ty: Option<TyId>,
    pub val: ExprId,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PathSegment {
    pub span: Span,
    pub sym: Symbol,
    pub sym_span: Span,
    /// During name resolution the paths should resolve to `Def::Type`
    pub generics: Option<Vec<TyId>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Param {
    pub span: Span,
    pub pat: PatId,
    pub defs: Vec<DefId>,
    pub ty: TyId,
    pub default: Option<ExprId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Block {
    pub span: Span,
    /// Expressions/statements which are followed by a semicolon
    pub items: Vec<ItemId>,
    /// A final optional expression which is returned if present
    pub ret: Option<ItemId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Pat {
    pub span: Span,
    pub kind: PatKind,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PatKind {
    Ident { name: Symbol, def_id: DefId },
    Tuple(Vec<PatId>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Def {
    Local {
        name: Symbol,
        pat_id: PatId,
        /// Filled during type-checking
        ty: Option<TyId>,
        mutable: Mutability,
    },
    Func {
        name: Symbol,
        params: Vec<Param>,
        /// Filled during type-checking
        ret_ty: Option<TyId>,
        body: ExprId,
    },
    /// e.g. `type Point = struct {x:i32, y: i32};`
    Type {
        name: Symbol,
        ty_id: TyId,
    },
    Module,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Mutability {
    Not,
    Mutable,
}
