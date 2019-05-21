{
  emacs,
  empty-builder,
  haskellPackages,
  sandbox,
  stdenv,
}:
let
  wanted-packages = with haskellPackages; [
    parallel
    abstract-par
    accelerate
    distributed-process
    distributed-process-simplelocalnet
    http-conduit
    monad-par
    normaldistribution
    repa
    utf8-string
    xml
    threadscope
  ];
  haskell-packages-selector = pkgs: wanted-packages;
  ghc = haskellPackages.ghcWithPackages haskell-packages-selector;
in
stdenv.mkDerivation rec {

  name = "concurrent-haskell";
  src = ./../src;

  buildInputs = [
    ghc
    haskellPackages.hlint
  ] ++ (if sandbox then [(emacs.override { inherit ghc; })] else []);

  installPhase = ''
    mkdir -p $out/bin
    cp ../build/bin/* $out/bin
  '';
}
