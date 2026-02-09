{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.rust-overlay.overlays.default
          (final: _prev: {
            rustToolchain = final.rust-bin.stable.latest.default.override {
              extensions = [ "rust-src" ];
            };
          })
        ];
      };
    };
}
