{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      craneLib = inputs.crane.mkLib pkgs;

      lalrpopFilter = path: _type: builtins.match ".*\\.lalrpop$" path != null;
      lalrpopOrCargo = path: type: (lalrpopFilter path type) || (craneLib.filterCargoSources path type);

      src = pkgs.lib.cleanSourceWith {
        src = ./..;
        filter = lalrpopOrCargo;
        name = "source";
      };

      commonArgs = {
        inherit src;
        strictDeps = true;

        buildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin [
          pkgs.libiconv
        ];
      };

      cargoArtifacts = craneLib.buildDepsOnly commonArgs;

      individualCrateArgs = commonArgs // {
        inherit cargoArtifacts;
        inherit (craneLib.crateNameFromCargoToml { inherit src; }) version;
        doCheck = false;
      };

      fileSetForCrate =
        crate:
        pkgs.lib.fileset.toSource {
          root = ./..;
          fileset = pkgs.lib.fileset.unions [
            ./../Cargo.toml
            ./../Cargo.lock
            (craneLib.fileset.commonCargoSources crate)
          ];
        };

      fileSetForCrateWithLalrpop =
        crate:
        pkgs.lib.fileset.toSource {
          root = ./..;
          fileset = pkgs.lib.fileset.unions [
            ./../Cargo.toml
            ./../Cargo.lock
            (craneLib.fileset.commonCargoSources crate)
            (pkgs.lib.fileset.fileFilter (file: file.hasExt "lalrpop") crate)
          ];
        };

      algorithm-w = craneLib.buildPackage (
        individualCrateArgs
        // {
          pname = "algorithm-w";
          cargoExtraArgs = "-p algorithm-w";
          src = fileSetForCrateWithLalrpop ./../crates/algorithm-w;
        }
      );

      system-f = craneLib.buildPackage (
        individualCrateArgs
        // {
          pname = "system-f";
          cargoExtraArgs = "-p system-f";
          src = fileSetForCrate ./../crates/system-f;
        }
      );
    in
    {
      _module.args = {
        inherit
          craneLib
          src
          commonArgs
          cargoArtifacts
          algorithm-w
          system-f
          ;
      };
    };
}
