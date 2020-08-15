{
  pkgs,
  pkgs-sam
}:
rec {
  # A utility to instantiate a capable emacs in a haskell sandbox
  emacs-for-haskell = haskell-env: pkgs-sam.emacs.override { ghc = haskell-env; };

  # Create a haskell package with haskell-mk included and enough meta to create
  # shell environments
  haskell-pkg =
    { haskellPackages ? pkgs.haskellPackages,
      name,
      src,
      wanted-packages,
      extra-build-inputs ? [],
    }:
    let
      haskell-packages-selector = _: wanted-packages;
      ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
    in
      pkgs.stdenv.mkDerivation rec {

        inherit name src;

        buildInputs = [
          ghc
          pkgs-sam.haskell-mk
        ] ++ extra-build-inputs;

        installPhase = ''
            mkdir -p $out/bin
            cp ../build/bin/* $out/bin
          '';

        meta = {
          inherit haskellPackages ghc;
        };
      };

  # Use the package's meta to create a shell with a configured emacs and hoogle
  #
  # For this to work, haskell-drv need to expose ghc and haskellPackages in the
  # `meta` argument of the derivation
  haskell-shell = haskell-drv:
    haskell-drv.overrideAttrs (attrs:

      { buildInputs = attrs.buildInputs ++ [
          (emacs-for-haskell haskell-drv.meta.ghc)
          haskell-drv.meta.haskellPackages.hoogle
        ];
      });
}
