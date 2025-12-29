{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    mcp-servers-nix.url = "github:natsukium/mcp-servers-nix";
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

      imports = [ inputs.treefmt-nix.flakeModule ];

      perSystem =
        {
          pkgs,
          system,
          self',
          ...
        }:
        let
          toolchain = pkgs.rust-bin.stable.latest.default;
          rustPlatform = pkgs.makeRustPlatform {
            cargo = toolchain;
            rustc = toolchain;
          };
          mcpConfig = inputs.mcp-servers-nix.lib.mkConfig pkgs {
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

          devShells.default = pkgs.mkShell {
            buildInputs = [ toolchain ];
            shellHook = ''
              cat ${mcpConfig} > .mcp.json
              echo "Generated .mcp.json"
            '';
          };

          checks = {
            statix =
              pkgs.runCommandLocal "statix"
                {
                  src = ./.;
                  nativeBuildInputs = [ pkgs.statix ];
                }
                ''
                  cd $src
                  statix check .
                  mkdir "$out"
                '';

            deadnix =
              pkgs.runCommandLocal "deadnix"
                {
                  src = ./.;
                  nativeBuildInputs = [ pkgs.deadnix ];
                }
                ''
                  cd $src
                  deadnix --fail .
                  mkdir "$out"
                '';

            actionlint =
              pkgs.runCommandLocal "actionlint"
                {
                  src = ./.;
                  nativeBuildInputs = [ pkgs.actionlint ];
                }
                ''
                  cd $src
                  actionlint .github/workflows/*.yml
                  mkdir "$out"
                '';
          };

          treefmt = {
            programs = {
              nixfmt.enable = true;
              rustfmt.enable = true;
            };
          };
        };
    };
}
