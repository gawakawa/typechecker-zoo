# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Rust implementations of type systems from [Typechecker Zoo](https://sdiehl.github.io/typechecker-zoo/introduction.html). Uses Rust 2024 edition.

## Development Environment

Uses Nix flakes. Enter dev shell with `nix develop` or use direnv (auto-activates via `.envrc`).

## Commands

```bash
cargo build                 # Build
cargo test                  # Run all tests
cargo test <test_name>      # Run single test
cargo clippy                # Lint
nix fmt                     # Format (treefmt: nixfmt + rustfmt)
nix fmt -- --ci             # Check formatting (CI mode)
nix build                   # Full build via Nix
nix flake check             # Flake checks
```

## Architecture

Cargo workspace with each type system as a separate crate:

- `algorithm-w/` - Hindley-Milner type inference (Algorithm W)

### Algorithm W (`algorithm-w/`)

Implements Hindley-Milner type inference with unification.

**Module structure:**
- `ast.rs` - Core types: `Expr`, `Lit`, `Type`, `Scheme`
- `infer.rs` - Type inference engine with unification
- `error.rs` - Error types using `thiserror`

**Key types:**
- `Expr` - Lambda calculus with let-bindings: `Var`, `Abs` (λ), `App`, `Let`, `Lit`, `Tuple`
- `Type` - Types: `Var` (type variable), `Arrow` (→), `Int`, `Bool`, `Tuple`
- `Scheme` - Polymorphic type scheme (∀ vars. type)
- `Subst` - Type substitution (`HashMap<TyVar, Type>`)
- `Env` - Type environment (`BTreeMap<TmVar, Scheme>`)

**Core algorithms in `TypeInference`:**
- `unify(t1, t2)` - Robinson's unification with occurs check
- `apply_subst` - Apply substitution to types/schemes/environments
- `compose_subst` - Compose two substitutions (s1 after s2)
