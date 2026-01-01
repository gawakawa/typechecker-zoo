use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

use crate::{
    ast::{Expr, Lit, Scheme, Type},
    error::{InferenceError, Result},
};

#[path = "polymorphism.rs"]
mod polymorphism;
#[path = "unify.rs"]
mod unify;

pub type TyVar = String;
pub type TmVar = String;
pub type Env = BTreeMap<TmVar, Scheme>;
pub type Subst = HashMap<TyVar, Type>;

#[derive(Debug)]
pub struct InferenceTree {
    pub rules: String,
    pub input: String,
    pub output: String,
    pub children: Vec<InferenceTree>,
}

impl InferenceTree {
    fn new(rule: &str, input: &str, output: &str, children: Vec<InferenceTree>) -> Self {
        Self {
            rules: rule.to_string(),
            input: input.to_string(),
            output: output.to_string(),
            children,
        }
    }
}

impl fmt::Display for InferenceTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_with_indent(f, 0)
    }
}

impl InferenceTree {
    fn display_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        let prefix = "  ".repeat(indent);
        writeln!(
            f,
            "{}{}: {} => {}",
            prefix, self.rules, self.input, self.output
        )?;
        for child in &self.children {
            child.display_with_indent(f, indent + 1)?;
        }
        Ok(())
    }
}

pub struct TypeInference {
    counter: usize,
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInference {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    fn fresh_tyvar(&mut self) -> TyVar {
        let var = format!("t{}", self.counter);
        self.counter += 1;
        var
    }

    fn pretty_env(env: &Env) -> String {
        if env.is_empty() {
            "{}".to_string()
        } else {
            let entries: Vec<String> = env.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
            format!("{{{}}}", entries.join(", "))
        }
    }

    fn pretty_subst(subst: &Subst) -> String {
        if subst.is_empty() {
            "{}".to_string()
        } else {
            let entries: Vec<String> = subst.iter().map(|(k, v)| format!("{}/{}", v, k)).collect();
            format!("{{{}}}", entries.join(", "))
        }
    }

    fn apply_subst(subst: &Subst, ty: &Type) -> Type {
        match ty {
            Type::Var(name) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
            Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(Self::apply_subst(subst, t1)),
                Box::new(Self::apply_subst(subst, t2)),
            ),
            Type::Tuple(types) => {
                Type::Tuple(types.iter().map(|t| Self::apply_subst(subst, t)).collect())
            }
            Type::Int | Type::Bool => ty.clone(),
        }
    }

    fn compose_subst(s1: &Subst, s2: &Subst) -> Subst {
        let mut result = s1.clone();
        for (k, v) in s2 {
            result.insert(k.clone(), Self::apply_subst(s1, v));
        }
        result
    }

    fn apply_subst_scheme(subst: &Subst, scheme: &Scheme) -> Scheme {
        let mut filtered_subst = subst.clone();
        for var in &scheme.vars {
            filtered_subst.remove(var);
        }
        Scheme {
            vars: scheme.vars.clone(),
            ty: Self::apply_subst(&filtered_subst, &scheme.ty),
        }
    }

    fn apply_subst_env(subst: &Subst, env: &Env) -> Env {
        env.iter()
            .map(|(k, v)| (k.clone(), Self::apply_subst_scheme(subst, v)))
            .collect()
    }

    pub fn infer(&mut self, env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        match expr {
            Expr::Lit(Lit::Int(_)) => Self::infer_lit_int(env, expr),
            Expr::Lit(Lit::Bool(_)) => Self::infer_lit_bool(env, expr),
            Expr::Var(name) => self.infer_var(env, expr, name),
            Expr::Abs(param, body) => self.infer_abs(env, expr, param, body),
            Expr::App(func, arg) => self.infer_app(env, expr, func, arg),
            Expr::Let(var, value, body) => self.infer_let(env, expr, var, value, body),
            Expr::Tuple(exprs) => self.infer_tuple(env, expr, exprs),
        }
    }

    // x : σ ∈ Γ    τ = inst(σ)
    // ───────────────────────── (T-Var)
    //        Γ ⊢ x : τ
    fn infer_var(
        &mut self,
        env: &Env,
        expr: &Expr,
        name: &str,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {}", Self::pretty_env(env), expr);

        match env.get(name) {
            Some(scheme) => {
                let instantiated = self.instantiate(scheme);
                let output = format!("{}", instantiated);
                let tree = InferenceTree::new("T-Var", &input, &output, vec![]);
                Ok((HashMap::new(), instantiated, tree))
            }
            None => Err(InferenceError::UnboundVariable {
                name: name.to_string(),
            }),
        }
    }

    // Γ, x : α ⊢ e : τ    α fresh
    // ───────────────────────────── (T-Lam)
    //    Γ ⊢ λx. e : α → τ
    fn infer_abs(
        &mut self,
        env: &Env,
        expr: &Expr,
        param: &str,
        body: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {}", Self::pretty_env(env), expr);

        let param_type = Type::Var(self.fresh_tyvar());
        let mut new_env = env.clone();
        let param_scheme = Scheme {
            vars: vec![],
            ty: param_type.clone(),
        };
        new_env.insert(param.to_string(), param_scheme);

        let (s1, body_type, tree1) = self.infer(&new_env, body)?;
        let param_type_subst = Self::apply_subst(&s1, &param_type);
        let result_type = Type::Arrow(Box::new(param_type_subst), Box::new(body_type));

        let output = format!("{}", result_type);
        let tree = InferenceTree::new("T-Abs", &input, &output, vec![tree1]);
        Ok((s1, result_type, tree))
    }

    // Γ ⊢ e₁ : τ₁    Γ ⊢ e₂ : τ₂    α fresh    S = unify(τ₁, τ₂ → α)
    // ────────────────────────────────────────────────────────────── (T-App)
    //                     Γ ⊢ e₁ e₂ : S(α)
    fn infer_app(
        &mut self,
        env: &Env,
        expr: &Expr,
        func: &Expr,
        arg: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {}", Self::pretty_env(env), expr);
        let result_type = Type::Var(self.fresh_tyvar());

        let (s1, func_type, tree1) = self.infer(env, func)?;
        let env_subst = Self::apply_subst_env(&s1, env);
        let (s2, arg_type, tree2) = self.infer(&env_subst, arg)?;

        let func_type_subst = Self::apply_subst(&s2, &func_type);
        let expected_func_type = Type::Arrow(Box::new(arg_type), Box::new(result_type.clone()));

        let (s3, tree3) = Self::unify(&func_type_subst, &expected_func_type)?;

        let final_subst = Self::compose_subst(&s3, &Self::compose_subst(&s2, &s1));
        let final_type = Self::apply_subst(&s3, &result_type);

        let output = format!("{}", final_type);
        let tree = InferenceTree::new("T-App", &input, &output, vec![tree1, tree2, tree3]);
        Ok((final_subst, final_type, tree))
    }

    // Γ ⊢ e₁ : τ₁    σ = gen(Γ, τ₁)    Γ, x : σ ⊢ e₂ : τ₂
    // ────────────────────────────────────────────────────── (T-Let)
    //          Γ ⊢ let x = e₁ in e₂ : τ₂
    fn infer_let(
        &mut self,
        env: &Env,
        expr: &Expr,
        var: &str,
        value: &Expr,
        body: &Expr,
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {}", Self::pretty_env(env), expr);

        let (s1, value_type, tree1) = self.infer(env, value)?;
        let env_subst = Self::apply_subst_env(&s1, env);
        let generalized_type = self.generalize(&env_subst, &value_type);

        let mut new_env = env_subst;
        new_env.insert(var.to_string(), generalized_type);

        let (s2, body_type, tree2) = self.infer(&new_env, body)?;

        let final_subst = Self::compose_subst(&s2, &s1);
        let output = format!("{}", body_type);
        let tree = InferenceTree::new("T-Let", &input, &output, vec![tree1, tree2]);
        Ok((final_subst, body_type, tree))
    }

    // Γ ⊢ e₁ : τ₁    ...    Γ ⊢ eₙ : τₙ
    // ─────────────────────────────────── (T-Tuple)
    //    Γ ⊢ (e₁, ..., eₙ) : (τ₁, ..., τₙ)
    fn infer_tuple(
        &mut self,
        env: &Env,
        expr: &Expr,
        exprs: &[Expr],
    ) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {}", Self::pretty_env(env), expr);

        let mut subst = HashMap::new();
        let mut types = Vec::new();
        let mut trees = Vec::new();
        let mut current_env = env.clone();

        for expr in exprs {
            let (s, ty, tree) = self.infer(&current_env, expr)?;
            subst = Self::compose_subst(&s, &subst);
            current_env = Self::apply_subst_env(&s, &current_env);
            types.push(ty);
            trees.push(tree);
        }

        let result_type = Type::Tuple(types);
        let output = format!("{}", result_type);
        let tree = InferenceTree::new("T-Tuple", &input, &output, trees);
        Ok((subst, result_type, tree))
    }

    // ───────────────── (T-LitInt)
    //    Γ ⊢ n : Int
    fn infer_lit_int(env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {}", Self::pretty_env(env), expr);
        let tree = InferenceTree::new("T-Int", &input, "Int", vec![]);
        Ok((HashMap::new(), Type::Int, tree))
    }

    // ────────────────── (T-LitBool)
    //    Γ ⊢ b : Bool
    fn infer_lit_bool(env: &Env, expr: &Expr) -> Result<(Subst, Type, InferenceTree)> {
        let input = format!("{} ⊢ {}", Self::pretty_env(env), expr);
        let tree = InferenceTree::new("T-Bool", &input, "Bool", vec![]);
        Ok((HashMap::new(), Type::Bool, tree))
    }
}

pub fn infer_type_only(expr: &Expr) -> Result<Type> {
    let mut inference = TypeInference::new();
    let env = BTreeMap::new();
    let (_, ty, _) = inference.infer(&env, expr)?;
    Ok(ty)
}

pub fn run_inference(expr: &Expr) -> Result<InferenceTree> {
    let mut inference = TypeInference::new();
    let env = BTreeMap::new();
    let (_, _, tree) = inference.infer(&env, expr)?;
    Ok(tree)
}
