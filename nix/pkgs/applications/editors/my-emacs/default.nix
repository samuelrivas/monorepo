## Generate a wrapped emacs with all required libraries and executables in the
## path
##
## "all" here means, anything required by the configuration generated by the
## emacs-config derivation in this monorepo
##
## Since some packages carry some heavy dependencies with them, you can
## blacklist them if you need to generate an emacs but don't want to spend the
## time on waiting for a plethora of things to download (check emacs-config.nix)
{
  aspell-wrapped,
  colorThemeSolarized,
  coreutils,
  emacs-config,
  emacs-config-options,
  emacsWithPackages,
  ensime,
  erlangMode,
  flycheck-haskell,
  gawk,
  ghc,
  gnused,
  groovy-mode,
  haskell-mode,
  hlint,
  merlin,
  nix-mode,
  ocpIndent,
  sbt,
  scalaMode2,
  stdenv,
  stylish-haskell,
  terraform-mode,
  tuareg,
  utop,
}:
let
  deps = {
    "haskell" = [ haskell-mode hlint ghc flycheck-haskell stylish-haskell ];
    "ocaml"   = [ merlin ocpIndent tuareg utop ];
    "erlang"  = [ erlangMode ];
    "scala"   = [ ensime sbt coreutils gnused gawk ]; #sbt doesn't run without all those extra things
  };

  mode-deps = mode:
    if   stdenv.lib.elem mode emacs-config-options.blacklisted-modes
    then []
    else builtins.getAttr mode deps;

  # Cheap or always needed, dependencies, just install them no matter what
  hardcoded-deps = [
    aspell-wrapped
    colorThemeSolarized
    emacs-config
    groovy-mode
    nix-mode
    terraform-mode
  ];

  dep-packages = stdenv.lib.concatMap mode-deps [
    "erlang"
    "haskell"
    "ocaml"
    "scala"
  ];
in
emacsWithPackages (hardcoded-deps ++ dep-packages)
