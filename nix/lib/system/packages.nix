{
  availableOn,
  filterAttrs,
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
    filtered-ps = filterAttrs (_: v: availableOn system v) p;
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
