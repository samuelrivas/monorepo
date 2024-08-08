{lib-nixpkgs}: {
  add-dev-shell = drv: {
    build-inputs ? [],
    native-build-inputs ? [],
    shell-hook ? "",
  }: let
    dev-shell = drv.overrideAttrs (attrs: let
      base-build-inputs = lib-nixpkgs.attrByPath ["buildInputs"] [] attrs;
      base-native-build-inputs =
        lib-nixpkgs.attrByPath ["nativeBuildInputs"] [] attrs;
      base-shell-hook = lib-nixpkgs.attrByPath ["shellHook"] "" attrs;

    in {
      buildInputs = base-build-inputs ++ build-inputs;
      nativeBuildInputs = base-native-build-inputs ++ native-build-inputs;
      shellHook = base-shell-hook + shell-hook;
    });
  in
    lib-nixpkgs.recursiveUpdate drv
    {
      passthru.dev-shell = dev-shell;
    };
}
