_: {
  perSystem =
    { algorithm-w, system-f, ... }:
    {
      packages = {
        default = algorithm-w;
        inherit algorithm-w system-f;
      };
    };
}
