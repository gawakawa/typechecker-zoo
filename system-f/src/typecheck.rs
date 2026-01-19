use crate::ast::Type;

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

#[derive(Default)]
pub struct BiDirectional {
    counter: usize,
}

impl BiDirectional {
    fn subst_type(var: &TyVar, replacement: &Type, ty: &Type) -> Type {
        match ty {
            Type::Var(name) if name == var => replacement.clone(),
            Type::ETVar(name) if name == var => replacement.clone(),
            Type::Var(_) | Type::ETVar(_) | Type::Int | Type::Bool => ty.clone(),
            Type::Arrow(t1, t2) => Type::Arrow(
                Box::new(Self::subst_type(var, replacement, t1)),
                Box::new(Self::subst_type(var, replacement, t2)),
            ),
            Type::Forall(bound_var, body) => {
                if bound_var == var {
                    ty.clone()
                } else {
                    Type::Forall(
                        bound_var.clone(),
                        Box::new(Self::subst_type(var, replacement, body)),
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
