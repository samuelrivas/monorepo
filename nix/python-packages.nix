{
  pkgs,
  pythonPackages,
  stdenv,
}:
let
  callPackage = pkgs.lib.callPackageWith (pkgs
                                        // pythonPackages
                                        // self);
  self = {
    py4j = callPackage ./pkgs/development/libraries/py4j { };

    pyspark = callPackage ./pkgs/development/libraries/pyspark { };

    requests = callPackage ./pkgs/development/libraries/requests { };
  };
in self
