{
  config,
  lib,
  options,
  pkgs,
  specialArgs,
}: {
  options = {
    sams-pkgs = {
      dir = lib.mkOption {
        type = lib.types.path;
        description = "The path to sams pkgs";
      };
    };
  };

  config = {
  };
}
