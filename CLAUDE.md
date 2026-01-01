# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Rust implementations of type systems from [Typechecker Zoo](https://sdiehl.github.io/typechecker-zoo/introduction.html). Uses Rust 2024 edition.

## Development Environment

Uses Nix flakes with direnv (auto-activates via `.envrc`).

## Commands

```bash
nix build                   # Build
nix flake check             # Run tests and checks
nix fmt                     # Format (treefmt: nixfmt + rustfmt)
nix run '.#algorithm-w' -- 'let id = \x -> x in id 42' # Run CLI
```

Run a specific test:
```bash
cargo test -p algorithm-w test_name
```

## Architecture

Cargo workspace with each type system as a separate crate:

- `algorithm-w/` - Hindley-Milner type inference (Algorithm W)

### Algorithm W (`algorithm-w/`)

Implements Hindley-Milner type inference with unification.

**Module structure:**
- `ast.rs` - Core types: `Expr`, `Lit`, `Type`, `Scheme`
- `infer.rs` - Type inference engine (`TypeInference` struct), includes `polymorphism.rs` and `unify.rs` as submodules
- `polymorphism.rs` - `instantiate` and `generalize` operations
- `unify.rs` - Robinson's unification algorithm with occurs check
- `parser.lalrpop` - LALRPOP grammar for expressions and types
- `error.rs` - Error types using `thiserror`

**Key types:**
- `Expr` - Lambda calculus with let-bindings: `Var`, `Abs` (λ), `App`, `Let`, `Lit`, `Tuple`
- `Type` - Types: `Var` (type variable), `Arrow` (→), `Int`, `Bool`, `Tuple`
- `Scheme` - Polymorphic type scheme (∀ vars. type)
- `Subst` - Type substitution (`HashMap<TyVar, Type>`)
- `Env` - Type environment (`BTreeMap<TmVar, Scheme>`)

**Entry points:**
- `infer_type_only(expr)` - Returns just the inferred `Type`
- `run_inference(expr)` - Returns `InferenceTree` with derivation trace

**Core algorithms in `TypeInference`:**
- `infer(env, expr)` - Main inference with inference rules (T-Var, T-Abs, T-App, T-Let, etc.)
- `unify(t1, t2)` - Robinson's unification with occurs check
- `instantiate(scheme)` - Replace quantified vars with fresh type vars
- `generalize(env, ty)` - Quantify free type vars not in environment

**Testing:**
- Unit tests in each module (`unify.rs`, `polymorphism.rs`)
- Integration tests in `tests/integration_tests.rs` using `assert_types_equal` for alpha-equivalence
