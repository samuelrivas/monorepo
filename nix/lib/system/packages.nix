{
  linkFarm,
  mapAttrsToList,
}: {
  # Create a derivation that contains other derivations as outputs
  #
  # Name is the name of the resulting derivation
  # p is a set of packages, the resulting derivation outputs one of each as `out/<name>`
  bundle = {name}: p:
    linkFarm name (
      mapAttrsToList
      (n: v: {
        name = n;
        path = v;
      })
      p
    );
}
