{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      craneLib,
      src,
      commonArgs,
      cargoArtifacts,
      algorithm-w,
      system-f,
      ...
    }:
    {
      checks = {
        inherit algorithm-w system-f;

        typechecker-zoo-clippy = craneLib.cargoClippy (
          commonArgs
          // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets";
          }
        );

        typechecker-zoo-doc = craneLib.cargoDoc (
          commonArgs
          // {
            inherit cargoArtifacts;
            env.RUSTDOCFLAGS = "";
          }
        );

        typechecker-zoo-fmt = craneLib.cargoFmt {
          inherit src;
        };

        typechecker-zoo-toml-fmt = craneLib.taploFmt {
          src = pkgs.lib.sources.sourceFilesBySuffices src [ ".toml" ];
        };

        typechecker-zoo-audit = craneLib.cargoAudit {
          inherit src;
          inherit (inputs) advisory-db;
        };

        typechecker-zoo-deny = craneLib.cargoDeny {
          inherit src;
        };

        typechecker-zoo-nextest = craneLib.cargoNextest (
          commonArgs
          // {
            inherit cargoArtifacts;
            partitions = 1;
            partitionType = "count";
            cargoNextestPartitionsExtraArgs = "--no-tests=pass";
          }
        );
      };
    };
}
