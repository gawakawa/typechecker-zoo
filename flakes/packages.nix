_: {
  perSystem =
    { pkgs, self', ... }:
    let
      rustPlatform = pkgs.makeRustPlatform {
        cargo = pkgs.rustToolchain;
        rustc = pkgs.rustToolchain;
      };
    in
    {
      packages = {
        algorithm-w = rustPlatform.buildRustPackage {
          pname = "algorithm-w";
          version = "0.1.0";
          src = ./..;
          cargoLock.lockFile = ./../Cargo.lock;
          cargoBuildFlags = [
            "-p"
            "algorithm-w"
          ];
          meta = {
            description = "Algorithm W";
          };
        };
        system-f = rustPlatform.buildRustPackage {
          pname = "system-f";
          version = "0.1.0";
          src = ./..;
          cargoLock.lockFile = ./../Cargo.lock;
          cargoBuildFlags = [
            "-p"
            "system-f"
          ];
          meta = {
            description = "System F";
          };
        };
        default = self'.packages.algorithm-w;
      };
    };
}
