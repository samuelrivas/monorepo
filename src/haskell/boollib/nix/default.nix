{haskell-lib-pkg}:
(haskell-lib-pkg {
  name = "boollib";
  src = ./../src;
  haskell-libs = [];

  # move this out to the generic builder
  build-doc = true;
  extra-drv.pname = "boollib";
  extra-drv.passthru.haddockDir = p: "${p.doc}/share/doc/${p.name}/html";
})
// {}
