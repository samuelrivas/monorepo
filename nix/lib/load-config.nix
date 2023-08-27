# Load our local configuration
#
# This reads the options from config-file, and blends them with all our private
# modules
{
  config-file,
  lib,
  modules,
  pkgs,
}: let
  user-config =
    if builtins.pathExists config-file
    then import config-file
    else {};

  # Pass pkgs through using _module.args
  module-config = {config._module.args.pkgs = pkgs;};

  local-config = lib.evalModules {
    modules = [user-config module-config] ++ modules;
  };
in
  local-config.config
