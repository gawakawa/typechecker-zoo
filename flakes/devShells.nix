_: {
  perSystem =
    {
      config,
      pkgs,
      self',
      ...
    }:
    {
      devShells.default = pkgs.mkShell {
        buildInputs = [
          pkgs.rustToolchain
        ]
        ++ config.pre-commit.settings.enabledPackages;

        shellHook = ''
          ${config.pre-commit.shellHook}
          cat ${self'.packages.mcp-config} > .mcp.json
          echo "Generated .mcp.json"
        '';
      };
    };
}
