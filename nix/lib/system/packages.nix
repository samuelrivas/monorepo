{
  availableOn,
  filterAttrs,
  isDerivation,
  linkFarm,
  mapAttrsToList,
  system,
}: {
  # Create a derivation that contains other derivations as outputs excluding
  # those that are not available on `system`
  #
  # `name` is the name of the resulting derivation
  #
  # `p` is a set of packages, the resulting derivation outputs one of each as
  # `out/<name>`
  bundle = {name}: p: let
    filter = a: availableOn system a && isDerivation a;
    filtered-ps = filterAttrs (_: filter) p;
  in
    linkFarm name (
      mapAttrsToList
      (n: v: {
        name = n;
        path = v;
      })
      filtered-ps
    );
}
