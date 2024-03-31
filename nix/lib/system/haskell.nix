{
  packages-sam,
  packages-nixpkgs,
}:
# We export Werror to fail the build by default (assuming that the package
# Makefile does not do GHC-FLAGS := ..., but GHC-FLAGS += ...), but you can
# unset that when running in a sandbox for quick iterations.
let
  haskell-template = is-lib: {
    extra-build-inputs ? [],
    extra-drv ? {},
    extra-native-build-inputs ? [],
    haskell-libs,
    name,
    src,
  }: let
    ghc = packages-nixpkgs.haskellPackages.ghcWithPackages (_: haskell-libs);
    install-bin = ''
      mkdir -p $out/bin
      cp ../build/bin/* $out/bin
    '';
    install-lib = ''
      make PREFIX="$out" install
    '';
    drv-args =
      {
        inherit name src;
        GHC-FLAGS = "-Werror";
        buildInputs =
          [
          ]
          ++ extra-build-inputs;

        nativeBuildInputs =
          [
            ghc
            (
              if is-lib
              then packages-sam.haskell-lib-mk
              else packages-sam.haskell-mk
            )
            packages-sam.haskell-test-mk
          ]
          ++ extra-native-build-inputs;

        propagatedBuildInputs =
          if is-lib
          then haskell-libs
          else [];
        installPhase =
          if is-lib
          then install-lib
          else install-bin;

        # ghcWithPackages needs this to be true for libraries, for non libraries
        # it could be omitted
        isHaskellLibrary = is-lib;
      }
      // extra-drv;
    drv = packages-nixpkgs.stdenv.mkDerivation drv-args;
    dev-shell = drv.overrideAttrs (final: previous: {
      # Emacs uses fontconfig, which needs a writable cache directory
      XDG_CACHE_HOME = "/tmp/cache";
      nativeBuildInputs =
        (builtins.filter (x: x != ghc) previous.nativeBuildInputs)
        ++ [
          packages-nixpkgs.haskell-language-server
          packages-sam.my-emacs
          packages-nixpkgs.git
          packages-nixpkgs.glibcLocales
          (packages-nixpkgs.haskellPackages.ghcWithHoogle (_: haskell-libs))
        ];
    });
  in
    drv // {inherit dev-shell;};
in {
  haskell-pkg = haskell-template false;
  haskell-lib-pkg = haskell-template true;
}
