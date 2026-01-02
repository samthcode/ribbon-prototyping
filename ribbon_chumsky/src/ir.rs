use string_interner::DefaultSymbol as Symbol;

use crate::{arena::Id, ast, span::Span};

type ExprId = Id<Expr>;
type BindingId = Id<Binding>;
type LocalDefId = Id<LocalDef>;
type DefId = Id<Def>;
type PatId = Id<Pat>;
type TyId = Id<Ty>;
type BlockId = Id<Block>;

pub enum Item {
    Binding(BindingId),
    Expr(ExprId),
    TypeDef,
}

pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

pub enum ExprKind {
    Var(LocalDefId),
    Path {
        segments: Vec<PathSegment>,
        def_id: DefId,
    },

    // Literals
    Num(Symbol),
    String(Symbol),
    Char(char),
    Bool(bool),
    List(Vec<ExprId>),

    Binding(BindingId),
    Block(BlockId),
    Func(DefId),
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
        rhs: ExprId,
    },
}

pub struct Ty {}

pub struct Binding {
    pat: PatId,
    /// Variables defined within the pattern
    defs: Vec<LocalDefId>,
    val: ExprId,
}

pub struct PathSegment {
    pub span: Span,
    pub sym: Symbol,
    pub sym_span: Span,
    pub generics: Option<Vec<TyId>>,
}

pub struct Param {
    pub span: Span,
    pub name: Symbol,
    pub pat: PatId,
    pub defs: Vec<LocalDefId>,
    pub default: Option<ExprId>,
}

pub struct Block {
    pub span: Span,
    /// Expressions/statements which are followed by a semicolon
    pub stmts: Vec<ExprId>,
    /// A final optional expression which is returned if present
    pub ret: Option<ExprId>,
}

pub struct Pat {
    pub span: Span,
    pub kind: PatKind,
}

pub enum PatKind {
    Ident { name: Symbol, def_id: LocalDefId },
    Tuple(Vec<PatId>),
}

/// Definition of a local variable, which comes from a binding
pub struct LocalDef {
    pub name: Symbol,
    pub ty: Option<TyId>,
    pub mutable: Mutability,
}

pub enum Def {
    Local(LocalDefId),
    Func {
        params: Vec<Param>,
        ret_ty: TyId,
        body: BlockId,
    },
    /// e.g. `type Point = struct {x:i32, y: i32};`
    Type,
    Module,
}

pub enum Mutability {
    Not,
    Mutable,
}
