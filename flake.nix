{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    mcp-servers-nix.url = "github:natsukium/mcp-servers-nix";
    git-hooks-nix = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.git-hooks-nix.flakeModule
      ];

      perSystem =
        {
          config,
          pkgs,
          system,
          self',
          ...
        }:
        let
          toolchain = pkgs.rust-bin.stable.latest.default.override {
            extensions = [ "rust-src" ];
          };
          rustPlatform = pkgs.makeRustPlatform {
            cargo = toolchain;
            rustc = toolchain;
          };
          mcpConfig =
            inputs.mcp-servers-nix.lib.mkConfig
              (import inputs.mcp-servers-nix.inputs.nixpkgs {
                inherit system;
              })
              {
                programs = {
                  nixos.enable = true;
                };
              };
        in
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.rust-overlay.overlays.default
            ];
          };

          packages = {
            algorithm-w = rustPlatform.buildRustPackage {
              pname = "algorithm-w";
              version = "0.1.0";
              src = ./.;
              cargoLock.lockFile = ./Cargo.lock;
              cargoBuildFlags = [
                "-p"
                "algorithm-w"
              ];
              meta = {
                description = "Hindley-Milner type inference (Algorithm W)";
                license = pkgs.lib.licenses.mit;
              };
            };
            default = self'.packages.algorithm-w;
            mcp-config = mcpConfig;
          };

          pre-commit.settings.hooks = {
            treefmt.enable = true;
            statix.enable = true;
            deadnix.enable = true;
            actionlint.enable = true;
            cargo-test = {
              enable = true;
              name = "cargo-test";
              description = "Run cargo test";
              entry = "${toolchain}/bin/cargo test";
              pass_filenames = false;
              stages = [ "pre-push" ];
            };
          };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              toolchain
            ]
            ++ config.pre-commit.settings.enabledPackages;

            shellHook = ''
              ${config.pre-commit.shellHook}
              cat ${mcpConfig} > .mcp.json
              echo "Generated .mcp.json"
            '';
          };

          treefmt = {
            programs = {
              nixfmt = {
                enable = true;
                includes = [ "*.nix" ];
              };
              rustfmt = {
                enable = true;
                includes = [ "*.rs" ];
              };
            };
          };
        };
    };
}
