_: {
  perSystem =
    { algorithm-w, ... }:
    {
      apps.default = {
        type = "app";
        program = "${algorithm-w}/bin/algorithm-w";
      };
    };
}
