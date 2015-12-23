# Load our local configuration
#
# This reads the options from config-file, and blends them with all our private
# modules
{
  config-file,
  lib,
  modules,
}:
let
  user-config =
    if   builtins.pathExists config-file
    then import config-file
    else {};
  local-config = lib.evalModules {
    modules = [user-config] ++ modules;
  };
in
local-config.config
