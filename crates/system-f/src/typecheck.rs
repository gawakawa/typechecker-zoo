use std::fmt;

use crate::{
    ast::{Expr, Type},
    errors::{TypeError, TypeResult},
};

pub type TmVar = String;
pub type TyVar = String;

#[derive(Debug, Clone)]
pub enum Entry {
    VarBnd(TmVar, Type),    // x: A
    TVarBnd(TyVar),         // α
    ETVarBnd(TyVar),        // ^α
    SETVarBnd(TyVar, Type), // ^α = τ
    Mark(TyVar),            // $α
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Entry::VarBnd(x, ty) => write!(f, "{}: {}", x, ty),
            Entry::TVarBnd(a) => write!(f, "{}", a),
            Entry::ETVarBnd(a) => write!(f, "^{}", a),
            Entry::SETVarBnd(a, ty) => write!(f, "^{} = {}", a, ty),
            Entry::Mark(a) => write!(f, "${}", a),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context(Vec<Entry>);

impl Context {
    pub fn find<F>(&self, predicate: F) -> Option<&Entry>
    where
        F: Fn(&Entry) -> bool,
    {
        self.0.iter().find(|entry| predicate(entry))
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let entries: Vec<String> = self.0.iter().rev().map(|e| e.to_string()).collect();
        write!(f, "{}", entries.join(", "))
    }
}

#[derive(Debug)]
pub struct InferenceTree {
    pub rule: String,
    pub input: String,
    pub output: String,
    pub children: Vec<InferenceTree>,
}

impl InferenceTree {
    fn new(rule: &str, input: &str, output: &str, children: Vec<InferenceTree>) -> Self {
        Self {
            rule: rule.to_string(),
            input: input.to_string(),
            output: output.to_string(),
            children,
        }
    }
}

#[derive(Default)]
pub struct BiDirectional {
    _counter: usize,
}

impl BiDirectional {
    pub fn infer(ctx: &Context, expr: &Expr) -> TypeResult<(Type, Context, InferenceTree)> {
        let input = format!("{} ⊢ {:?}", ctx, expr);

        match expr {
            Expr::Var(x) => Self::infer_var(ctx, x, &input),
            Expr::Ann(_, _) => unimplemented!(),
            Expr::LitInt(n) => Self::infer_lit_int(ctx, *n, &input),
            Expr::LitBool(b) => Self::infer_lit_bool(ctx, *b, &input),
            Expr::Abs(_, _, _) => unimplemented!(),
            Expr::App(_, _) => unimplemented!(),
            Expr::TAbs(_, _) => unimplemented!(),
            Expr::TApp(_, _) => unimplemented!(),
            Expr::Let(_, _, _) => unimplemented!(),
            Expr::IfThenElse(_, _, _) => unimplemented!(),
            Expr::BinOp(_, _, _) => unimplemented!(),
        }
    }

    ///    x : A ∈ Γ
    /// ---------------- (T-Var)
    /// Γ, x : A ⊢ x ⇒ A
    fn infer_var(
        ctx: &Context,
        x: &str,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        if let Some(Entry::VarBnd(_, ty)) =
            ctx.find(|entry| matches!(entry, Entry::VarBnd(name, _) if name == x))
        {
            let output = format!("{} ⇒ {} ⊣ {}", input, ty, ctx);
            Ok((
                ty.clone(),
                ctx.clone(),
                InferenceTree::new("InfVar", input, &output, vec![]),
            ))
        } else {
            Err(TypeError::UnboundVariable {
                name: x.to_string(),
                expr: None,
            })
        }
    }

    /// ----------- (T-LitInt)
    /// Γ ⊢ n ⇒ Int
    fn infer_lit_int(
        ctx: &Context,
        _n: i64,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let output = format!("{} ⇒ Int ⊣ {}", input, ctx);
        Ok((
            Type::Int,
            ctx.clone(),
            InferenceTree::new("InfLitInt", input, &output, vec![]),
        ))
    }

    /// ------------ (T-LitBool)
    /// Γ ⊢ b ⇒ Bool
    fn infer_lit_bool(
        ctx: &Context,
        _b: bool,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let output = format!("{} ⇒ Bool ⊣ {}", input, ctx);
        Ok((
            Type::Bool,
            ctx.clone(),
            InferenceTree::new("InfLitBool", input, &output, vec![]),
        ))
    }

    fn _subst_type(var: &TyVar, replacement: &Type, ty: &Type) -> Type {
        match ty {
            Type::Var(name) if name == var => replacement.clone(),
            Type::ETVar(name) if name == var => replacement.clone(),
            Type::Var(_) | Type::ETVar(_) | Type::Int | Type::Bool => ty.clone(),
            Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(Self::_subst_type(var, replacement, t1)),
                Box::new(Self::_subst_type(var, replacement, t2)),
            ),
            Type::Forall(bound_var, body) => {
                if bound_var == var {
                    ty.clone()
                } else {
                    Type::Forall(
                        bound_var.clone(),
                        Box::new(Self::_subst_type(var, replacement, body)),
                    )
                }
            }
        }
    }

    pub fn apply_ctx_type(ctx: &Context, ty: &Type) -> Type {
        let mut current = ty.clone();
        let mut changed = true;

        while changed {
            changed = false;
            let new_type = Self::apply_ctx_type_once(ctx, &current);
            if new_type != current {
                changed = true;
                current = new_type;
            }
        }

        current
    }

    fn apply_ctx_type_once(ctx: &Context, ty: &Type) -> Type {
        match ty {
            Type::ETVar(a) => {
                if let Some(Entry::SETVarBnd(_, replacement)) =
                    ctx.find(|entry| matches!(entry, Entry::SETVarBnd(name, _) if name == a))
                {
                    Self::apply_ctx_type_once(ctx, replacement)
                } else {
                    ty.clone()
                }
            }
            Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(Self::apply_ctx_type_once(ctx, t1)),
                Box::new(Self::apply_ctx_type_once(ctx, t2)),
            ),
            Type::Forall(var, body) => {
                Type::Forall(var.clone(), Box::new(Self::apply_ctx_type_once(ctx, body)))
            }
            _ => ty.clone(),
        }
    }
}
