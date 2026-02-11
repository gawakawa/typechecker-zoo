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
    pub fn push(&mut self, entry: Entry) {
        self.0.push(entry);
    }

    pub fn find<F>(&self, predicate: F) -> Option<&Entry>
    where
        F: Fn(&Entry) -> bool,
    {
        self.0.iter().find(|entry| predicate(entry))
    }

    pub fn break3<F>(&self, predicate: F) -> (Vec<Entry>, Option<Entry>, Vec<Entry>)
    where
        F: Fn(&Entry) -> bool,
    {
        if let Some(pos) = self.0.iter().position(predicate) {
            let left = self.0[..pos].to_vec();
            let middle = self.0[pos].clone();
            let right = self.0[pos + 1..].to_vec();
            (left, Some(middle), right)
        } else {
            (self.0.clone(), None, Vec::new())
        }
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
    counter: usize,
}

impl BiDirectional {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    pub fn fresh_tyvar(&mut self) -> TyVar {
        let var = format!("α{}", self.counter);
        self.counter += 1;
        var
    }

    pub fn infer(
        &mut self,
        ctx: &Context,
        expr: &Expr,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let input = format!("{} ⊢ {:?}", ctx, expr);

        match expr {
            Expr::Var(x) => Self::infer_var(ctx, x, &input),
            Expr::Ann(_, _) => unimplemented!(),
            Expr::LitInt(n) => Self::infer_lit_int(ctx, *n, &input),
            Expr::LitBool(b) => Self::infer_lit_bool(ctx, *b, &input),
            Expr::Abs(x, param_ty, body) => self.infer_abs(ctx, x, param_ty, body, &input),
            Expr::App(func, arg) => self.infer_application(ctx, func, arg, &input),
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

    /// Γ, x:A ⊢ e ⇐ B
    /// ------------------ (T-Abs)
    /// Γ ⊢ λx:A.e ⇒ A → B
    fn infer_abs(
        &mut self,
        ctx: &Context,
        x: &str,
        param_ty: &Type,
        body: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let b = self.fresh_tyvar();
        let mut new_ctx = ctx.clone();
        new_ctx.push(Entry::VarBnd(x.to_string(), param_ty.clone()));
        new_ctx.push(Entry::ETVarBnd(b.clone()));

        let (ctx1, tree) = self.check(&new_ctx, body, &Type::ETVar(b.clone()))?;
        let (left, _, right) =
            ctx1.break3(|entry| matches!(entry, Entry::VarBnd(name, _) if name == x));
        let mut final_ctx_entries = left
            .into_iter()
            .filter(|entry| matches!(entry, Entry::SETVarBnd(_, _)))
            .collect::<Vec<_>>();
        final_ctx_entries.extend(right);
        let final_ctx = Context(final_ctx_entries);
        let result_ty = Type::Arrow(Box::new(param_ty.clone()), Box::new(Type::ETVar(b)));
        let output = format!("{} ⇒ {} ⊣ {}", input, result_ty, final_ctx);
        Ok((
            result_ty,
            final_ctx,
            InferenceTree::new("InfLam", input, &output, vec![tree]),
        ))
    }

    /// Γ ⊢ e₁ ⇒ A → B  Γ ⊢ e₂ ⇐ A
    /// -------------------------- (T_App)
    ///       Γ ⊢ e₁;e₂ ⇒ B
    fn infer_application(
        &mut self,
        ctx: &Context,
        func: &Expr,
        arg: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (func_ty, ctx1, tree1) = self.infer(ctx, func)?;
        let func_ty_applied = Self::apply_ctx_type(&ctx1, &func_ty);
        let (result_ty, ctx2, tree2) = self.infer_app(&ctx1, &func_ty_applied, arg)?;
        let output = format!("{} ⇒ {} ⊣ {}", input, result_ty, ctx2);
        Ok((
            result_ty,
            ctx2,
            InferenceTree::new("InfApp", input, &output, vec![tree1, tree2]),
        ))
    }

    fn infer_app(
        &mut self,
        _ctx: &Context,
        _func_ty: &Type,
        _arg: &Expr,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        unimplemented!();
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

    fn check(
        &mut self,
        _ctx: &Context,
        _expr: &Expr,
        _ty: &Type,
    ) -> TypeResult<(Context, InferenceTree)> {
        unimplemented!()
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
