_: {
  perSystem =
    { pkgs, ... }:
    {
      pre-commit.settings.hooks = {
        treefmt.enable = true;
        statix.enable = true;
        deadnix.enable = true;
        actionlint.enable = true;
        cargo-test = {
          enable = true;
          name = "cargo-test";
          description = "Run cargo test";
          entry = "${pkgs.rustToolchain}/bin/cargo test";
          pass_filenames = false;
          stages = [ "pre-push" ];
        };
      };
    };
}
