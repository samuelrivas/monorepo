{haskell-lib-pkg}:
haskell-lib-pkg {
  name = "boollib";
  src = ./../src;
  haskell-libs = [];
  # extra-drv.pname = "boollib";
  # extra-drv.passthru.haddockDir = p: "${p.doc}/share/doc/${p.name}/html";
}
