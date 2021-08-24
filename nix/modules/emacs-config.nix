{
  config,
  lib,
  options,
  pkgs,
  specialArgs
}:
let
  configured-modes = [
    "erlang"
    "haskell"
    "ocaml"
  ];
in
{
  options = {

    emacs-config = {
      full-user-name = lib.mkOption {
        type = lib.types.str;
        default = "Unknown Fella";
        description = "The full user name.";
      };
      extra-config = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "Config to add verbatim to emacs' configuration file.";
        example = ''
          ;; workarounds
          (require 'iso-transl) ; required for dead keys to work with ibus
        '';
      };
      denylisted-modes = lib.mkOption {
        type = lib.types.listOf (lib.types.enum configured-modes);
        default = [ ];
        description = ''
          You can prevent installing the dependencies of heavy modules by
          denylisting them here. The configuration will still be present,
          but all the needed packages will not, so those modes will not work
          if you require them.
        '';
        example = [ "erlang" "haskell" ];
      };
    };
  };

  config = {
  };
}
