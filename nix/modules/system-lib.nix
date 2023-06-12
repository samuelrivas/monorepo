{ lib, sam-lib, ... }: {
  perSystem = { config, self', pkgs, ... }: {
    options.sys-lib = lib.mkOption {
      default = {};
    };
    config.sys-lib = {
      builders = pkgs.callPackage ./../lib/with-system/builders.nix { };
      haskell = import ./../lib/with-system/haskell.nix {
        pkgs = pkgs // self'.packages;
      };
    };
  };
}
