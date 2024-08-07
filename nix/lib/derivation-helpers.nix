{lib-nixpkgs}: {
  add-dev-shell = drv: {
    build-inputs ? [],
    native-build-inputs ? [],
    shell-hook ? "",
  }: let
    dev-shell = drv.overrideAttrs (attrs: {
      buildInputs = attrs.buildInputs ++ build-inputs;
      nativeBuildInputs = attrs.nativeBuildInputs ++ native-build-inputs;
      shellHook = shell-hook;
    });
  in
    lib-nixpkgs.recursiveUpdate drv
    {
      passthru.dev-shell = dev-shell;
    };
}
