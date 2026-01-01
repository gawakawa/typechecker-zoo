# Typechecker Zoo

Rust implementations of type systems from [Typechecker Zoo](https://sdiehl.github.io/typechecker-zoo/introduction.html).

## Usage

### Algorithm W

```bash
nix run .#algorithm-w -- 'let const = \x -> \y -> x in const 42 true'
```
