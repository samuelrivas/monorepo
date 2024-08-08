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
      # we are assuming that later packages in the list have precedence. For
      # example, haskell.nix overrides ghc in the dev shell using this
      # assumption
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
