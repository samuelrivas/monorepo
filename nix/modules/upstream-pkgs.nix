{
  config,
  lib,
  options,
  pkgs,
}:
{
  options = {

    upstream-pkgs = {
      dir = lib.mkOption {
        type = lib.types.path;
        description = "The path to upstream pkgs";
      };
    };
  };

  config = {
  };
}
