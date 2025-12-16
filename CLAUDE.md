# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Rust implementations of type systems from [Typechecker Zoo](https://sdiehl.github.io/typechecker-zoo/introduction.html).

## Development Environment

This project uses Nix flakes for development. Enter the dev shell with:
```bash
nix develop
```

Or use direnv (`.envrc` is configured) for automatic environment activation.

## Commands

```bash
# Build
cargo build

# Run
cargo run

# Test
cargo test

# Run a single test
cargo test <test_name>

# Format (uses treefmt with nixfmt + rustfmt)
nix fmt

# Check formatting (CI mode)
nix fmt -- --ci

# Full build via Nix
nix build

# Flake checks
nix flake check
```

## Project Structure

- `src/` - Rust source code implementing various type systems
- `flake.nix` - Nix flake for development environment and builds
- Uses Rust 2024 edition with stable toolchain via rust-overlay
