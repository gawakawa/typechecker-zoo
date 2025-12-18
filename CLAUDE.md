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
nix fmt                     # Format (treefmt: nixfmt + rustfmt)
nix fmt -- --ci             # Check formatting (CI mode)
nix build                   # Full build via Nix
nix flake check             # Flake checks
```

## Architecture

Cargo workspace with each type system as a separate crate:

- `algorithm-w/` - Hindley-Milner type inference (Algorithm W)

### Algorithm W (`algorithm-w/`)

Implements Hindley-Milner type inference. Core AST in `src/lib.rs`:

- `Expr` - Lambda calculus with let-bindings: `Var`, `Abs` (λ), `App`, `Let`, `Lit`, `Tuple`
- `Lit` - Literal values: `Int`, `Bool`
