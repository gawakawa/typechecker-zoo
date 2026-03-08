use std::{collections::HashSet, fmt, vec};

use crate::{
    ast::{BinOp, Expr, Type},
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

    fn free_vars(&self, ty: &Type) -> HashSet<TyVar> {
        match ty {
            Type::Var(name) | Type::ETVar(name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                set
            }
            Type::Arrow(t1, t2) => {
                let mut set = self.free_vars(t1);
                set.extend(self.free_vars(t2));
                set
            }
            Type::Forall(var, ty) => {
                let mut set = self.free_vars(ty);
                set.remove(var);
                set
            }
            Type::Int | Type::Bool => HashSet::new(),
        }
    }

    pub fn infer(
        &mut self,
        ctx: &Context,
        expr: &Expr,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let input = format!("{} ⊢ {:?}", ctx, expr);

        match expr {
            Expr::Var(x) => Self::infer_var(ctx, x, &input),
            Expr::Ann(expr, ty) => self.infer_ann(ctx, expr, ty, &input),
            Expr::LitInt(n) => Self::infer_lit_int(ctx, *n, &input),
            Expr::LitBool(b) => Self::infer_lit_bool(ctx, *b, &input),
            Expr::Abs(x, param_ty, body) => self.infer_abs(ctx, x, param_ty, body, &input),
            Expr::App(func, arg) => self.infer_application(ctx, func, arg, &input),
            Expr::TAbs(_, _) => unimplemented!(),
            Expr::TApp(_, _) => unimplemented!(),
            Expr::Let(x, e1, e2) => self.infer_let(ctx, x, e1, e2, &input),
            Expr::IfThenElse(e1, e2, e3) => self.infer_if(ctx, e1, e2, e3, &input),
            Expr::BinOp(op, e1, e2) => self.infer_binop(ctx, op, e1, e2, &input),
        }
    }

    /// x : A ∈ Γ
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
    /// Γ ⊢ e₁;e₂ ⇒ B
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
        ctx: &Context,
        func_ty: &Type,
        arg: &Expr,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let input = format!("{} ⊢ {:?} • {}", ctx, arg, func_ty);

        match func_ty {
            Type::Arrow(param_ty, result_ty) => {
                self.infer_app_arrow(ctx, param_ty, result_ty, arg, &input)
            }
            Type::ETVar(a) => self.infer_app_etvar(ctx, a, arg, &input),
            _ => Err(TypeError::ApplicationTypeError {
                actual: func_ty.clone(),
                expr: None,
            }),
        }
    }

    /// Γ ⊢ e₂ ⇐ A
    /// ------------------ (T-AppArrow)
    /// Γ ⊢ A → B • e₂ ⇒ B
    fn infer_app_arrow(
        &mut self,
        ctx: &Context,
        param_ty: &Type,
        result_ty: &Type,
        arg: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (ctx1, tree) = self.check(ctx, arg, param_ty)?;
        let output = format!("{} ⇒⇒ {} ⊣ {}", input, result_ty, ctx1);
        Ok((
            result_ty.clone(),
            ctx1,
            InferenceTree::new("InfAppArr", input, &output, vec![tree]),
        ))
    }

    /// Γ[^α := ^α₁ → ^α₂], ^α₁, ^α₂ ⊢ e₂ ⇐ ^α₁
    /// --------------------------------------- (T-AppEVar)
    /// Γ ⊢ ^α • e₂ ⇒ ^α₂
    fn infer_app_etvar(
        &mut self,
        ctx: &Context,
        a: &TyVar,
        arg: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let a1 = self.fresh_tyvar();
        let a2 = self.fresh_tyvar();
        let (left, _, right) =
            ctx.break3(|entry| matches!(entry, Entry::ETVarBnd(name) if name == a));
        let arrow_type = Type::Arrow(
            Box::new(Type::ETVar(a1.clone())),
            Box::new(Type::ETVar(a2.clone())),
        );
        let mut new_ctx = left;
        new_ctx.push(Entry::SETVarBnd(a.clone(), arrow_type));
        new_ctx.push(Entry::ETVarBnd(a1.clone()));
        new_ctx.push(Entry::ETVarBnd(a2.clone()));
        new_ctx.extend(right);
        let ctx1 = Context(new_ctx);

        let (ctx2, tree) = self.check(&ctx1, arg, &Type::ETVar(a1))?;
        let output = format!("{} ⇒⇒ ^{} ⊣ {}", input, a2, ctx2);
        Ok((
            Type::ETVar(a2),
            ctx2,
            InferenceTree::new("InfAppETVar", input, &output, vec![tree]),
        ))
    }

    /// Γ ⊢ e₁ ⇒ A  Γ, x:A ⊢ e₂ ⇒ B
    /// --------------------------- (T-Let)
    /// Γ ⊢ let x = e₁ in e₂ ⇒ B
    fn infer_let(
        &mut self,
        ctx: &Context,
        x: &str,
        e1: &Expr,
        e2: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (ty1, ctx1, tree1) = self.infer(ctx, e1)?;
        let mut new_ctx = ctx1.clone();
        new_ctx.push(Entry::VarBnd(x.to_string(), ty1));
        let (ty2, ctx2, tree2) = self.infer(&new_ctx, e2)?;
        let (left, _, right) =
            ctx2.break3(|entry| matches!(entry, Entry::VarBnd(name, _) if name == x));
        let mut final_ctx_entries = left
            .into_iter()
            .filter(|entry| matches!(entry, Entry::SETVarBnd(_, _)))
            .collect::<Vec<_>>();
        final_ctx_entries.extend(right);
        let final_ctx = Context(final_ctx_entries);
        let output = format!("{} ⇒ {} ⊣ {}", input, ty2, final_ctx);
        Ok((
            ty2,
            final_ctx,
            InferenceTree::new("InfLet", input, &output, vec![tree1, tree2]),
        ))
    }

    /// Γ ⊢ e₁ ⇐ Bool Γ ⊢ e₂ ⇒ A Γ ⊢ e₃ ⇒ A
    /// ----------------------------------- (T-IF)
    /// Γ ⊢ if e₁ then e₂ else e₃ ⇒ A
    fn infer_if(
        &mut self,
        ctx: &Context,
        e1: &Expr,
        e2: &Expr,
        e3: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (ctx1, tree1) = self.check(ctx, e1, &Type::Bool)?;
        let (ty2, ctx2, tree2) = self.infer(&ctx1, e2)?;
        let (ty3, ctx3, tree3) = self.infer(&ctx2, e3)?;

        let (unified_ctx, tree_unify) = self.subtype(&ctx3, &ty2, &ty3)?;
        let output = format!("{} ⇒ {} ⊣ {}", input, ty2, unified_ctx);
        Ok((
            ty2,
            unified_ctx,
            InferenceTree::new(
                "InfIf",
                input,
                &output,
                vec![tree1, tree2, tree3, tree_unify],
            ),
        ))
    }

    /// Γ ⊢ e₁ ⇐ Int  Γ ⊢ e₂ ⇐ Int
    /// -------------------------- (T-Arith)
    /// Γ ⊢ e₁ ⊕ e₂ ⇒ Int
    ///
    /// Γ ⊢ e₁ ⇐ Bool  Γ ⊢ e₂ ⇐ Bool
    /// ---------------------------- (T-Bool)
    /// Γ ⊢ e₁ ∧ e₂ ⇒ Bool
    ///
    /// Γ ⊢ e₁ ⇐ Int  Γ ⊢ e₂ ⇐ Int
    /// -------------------------- (T-Cmp)
    /// Γ ⊢ e₁ < e₂ ⇒ Bool
    ///
    /// Γ ⊢ e₁ ⇒ A  Γ ⊢ e₂ ⇐ A
    /// ---------------------- (T-Eq)
    /// Γ ⊢ e₁ = e₂ ⇒ Bool
    fn infer_binop(
        &mut self,
        ctx: &Context,
        op: &BinOp,
        e1: &Expr,
        e2: &Expr,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        match op {
            // T-Arith: Int → Int → Int
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let (ctx1, tree1) = self.check(ctx, e1, &Type::Int)?;
                let (ctx2, tree2) = self.check(&ctx1, e2, &Type::Int)?;
                let output = format!("{} ⇒ {}", input, ctx2);
                Ok((
                    Type::Int,
                    ctx2,
                    InferenceTree::new("InfArith", input, &output, vec![tree1, tree2]),
                ))
            }

            // T-Bool: Bool → Bool → Bool
            BinOp::And | BinOp::Or => {
                let (ctx1, tree1) = self.check(ctx, e1, &Type::Bool)?;
                let (ctx2, tree2) = self.check(&ctx1, e2, &Type::Bool)?;
                let output = format!("{} ⇒ {}", input, ctx2);
                Ok((
                    Type::Bool,
                    ctx2,
                    InferenceTree::new("InfBool", input, &output, vec![tree1, tree2]),
                ))
            }
            // T-Cmp: Int → Int → Bool
            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                let (ctx1, tree1) = self.check(ctx, e1, &Type::Int)?;
                let (ctx2, tree2) = self.check(&ctx1, e2, &Type::Int)?;
                let output = format!("{} ⇒ {}", input, ctx2);
                Ok((
                    Type::Bool,
                    ctx2,
                    InferenceTree::new("InfCmp", input, &output, vec![tree1, tree2]),
                ))
            }

            // T-Eq: ∀α. α → α → Bool
            BinOp::Eq | BinOp::Ne => {
                let (ty1, ctx1, tree1) = self.infer(ctx, e1)?;
                let (ctx2, tree2) = self.check(&ctx1, e2, &ty1)?;
                let output = format!("{} ⇒ Bool ⊣ {}", input, ctx2);
                Ok((
                    Type::Bool,
                    ctx2,
                    InferenceTree::new("InfEq", input, &output, vec![tree1, tree2]),
                ))
            }
        }
    }

    /// Γ ⊢ e ⇐ A
    /// --------------- (T-Instr)
    /// Γ ⊢ (e : A) ⇒ A
    fn infer_ann(
        &mut self,
        ctx: &Context,
        expr: &Expr,
        ty: &Type,
        input: &str,
    ) -> TypeResult<(Type, Context, InferenceTree)> {
        let (ctx1, tree) = self.check(ctx, expr, ty)?;
        let output = format!("{} ⇒ {} ⊣ {}", input, ty, ctx1);
        Ok((
            ty.clone(),
            ctx1,
            InferenceTree::new("InfAnn", input, &output, vec![tree]),
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

    fn subtype(
        &mut self,
        ctx: &Context,
        t1: &Type,
        t2: &Type,
    ) -> TypeResult<(Context, InferenceTree)> {
        let _input = format!("{} ⊢ {} <: {}", ctx, t1, t2);

        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => unimplemented!(),
            (Type::Var(a), Type::Var(b)) if a == b => unimplemented!(),
            (Type::ETVar(a), Type::ETVar(b)) if a == b => unimplemented!(),
            (Type::Arrow(_a1, _a2), Type::Arrow(_b1, _b2)) => unimplemented!(),
            (_, Type::Forall(_b, _t2_body)) => unimplemented!(),
            (Type::Forall(_a, _t1_body), _) => unimplemented!(),
            (Type::ETVar(a), _) if !self.free_vars(t2).contains(a) => unimplemented!(),
            (_, Type::ETVar(a)) if !self.free_vars(t1).contains(a) => unimplemented!(),
            _ => unimplemented!(),
        }
    }
}
