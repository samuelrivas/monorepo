{
  emacs-for-haskell,
  haskellPackages,
  sandbox,
  stdenv,
}:
let
  wanted-packages = with haskellPackages; [
    generic-lens
    lens
    multiset
    random-fu
    readline
  ];
  haskell-packages-selector = pkgs: wanted-packages;
  ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "onirim-helper";
  src = ./../src;

  buildInputs = [
    ghc
  ] ++ (if sandbox
        then [(emacs-for-haskell ghc) haskellPackages.hoogle]
        else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin
  '';
}
