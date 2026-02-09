_: {
  perSystem =
    {
      config,
      self',
      craneLib,
      ...
    }:
    {
      devShells.default = craneLib.devShell {
        inherit (self') checks;

        packages = config.pre-commit.settings.enabledPackages;

        shellHook = ''
          ${config.pre-commit.shellHook}
        '';
      };
    };
}
