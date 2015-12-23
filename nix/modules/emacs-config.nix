{
  config,
  lib,
  options,
}:
{
  options = {

    emacs-config = {

      user = lib.mkOption {
        type = lib.types.string;
        default = "unknown-fella";
        description = "The short user name (your id in the system).";
      };
      full-user-name = lib.mkOption {
        type = lib.types.string;
        default = "Unknown Fella";
        description = "The full user name.";
      };
      extra-config = lib.mkOption {
        type = lib.types.string;
        default = "";
        description = "Config to add verbatim to emacs' configuration file.";
        example = ''
          ;; workarounds
          (require 'iso-transl) ; required for dead keys to work with ibus
        '';
      };
    };
  };

  config = {
  };
}
