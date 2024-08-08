{
  lib-nixpkgs,
  lib-sam,
  my-emacs,
  haskell-mk,
  haskell-lib-mk,
  haskell-test-mk,
  packages-nixpkgs,
}:
# We export Werror to fail the build by default (assuming that the package
# Makefile does not do GHC-FLAGS := ..., but GHC-FLAGS += ...), but you can
# unset that when running in a sandbox for quick iterations.
let
  haskell-template = is-lib: {
    build-doc ? is-lib,
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
    install-lib =
      ''
        make PREFIX="$out" PREFIX-DOCS="$doc" install
      ''
      + (lib-nixpkgs.optionalString build-doc ''
        make prefix="$out" PREFIX-DOCS="$doc" install-doc
      '');
    drv-args =
      {
        inherit name src;

        outputs = ["out"] ++ (lib-nixpkgs.optional build-doc "doc");
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
              then haskell-lib-mk
              else haskell-mk
            )
            haskell-test-mk
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
      // lib-nixpkgs.optionalAttrs build-doc {
        # pname and haddocDir are used by ghcWithHoogle to build the
        # documentation index
        pname = name;
        passthru.haddockDir = p: "${p.doc}/share/doc/${p.name}/html";
      }
      // extra-drv;
    drv = packages-nixpkgs.stdenv.mkDerivation drv-args;
  in
    lib-sam.derivation-helpers.add-dev-shell drv {
      native-build-inputs = [
        packages-nixpkgs.haskell-language-server
        my-emacs
        packages-nixpkgs.git
        packages-nixpkgs.glibcLocales
        (packages-nixpkgs.haskellPackages.ghcWithHoogle (_: haskell-libs))
      ];
      shell-hook = ''
        # Emacs uses fontconfig, which needs a writable cache directory
        export XDG_CACHE_HOME="/tmp/cache";

        hoogle server --local > /dev/null &
        HOOGLE_PID=$(jobs -p %1)
        echo "Hoogle server started with PID $HOOGLE_PID"

        kill_hoogle () {
          kill $HOOGLE_PID
          echo "Hoogle server terminated"
        }

        trap kill_hoogle exit
      '';
    };
in {
  haskell-pkg = haskell-template false;
  haskell-lib-pkg = haskell-template true;
}
