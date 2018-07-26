{
  config,
  lib,
  options,
  pkgs,
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
        type = lib.types.string;
        default = "Unknown Fella";
        description = "The full user name.";
      };
      org-agenda-file = lib.mkOption {
        type = lib.types.path;
        description = "File with the list of paths to add to org mode's agenda";
        example = /tmp/agendas;
      };
      org-template-dir = lib.mkOption {
        type = lib.types.path;
        description = "Directory where the capturing templates live";
        example = /tmp/templates;
      };
      org-meeting-file = lib.mkOption {
        type = lib.types.path;
        description = "File to capture meetings";
        example = /tmp/meetings.org;
      };
      org-interview-file = lib.mkOption {
        type = lib.types.path;
        description = "File to capture interviews";
        example = /tmp/interviews.org;
      };
      org-todo-file = lib.mkOption {
        type = lib.types.path;
        description = "File to capture tasks";
        example = /tmp/todo.org;
      };
      org-reflection-file = lib.mkOption {
        type = lib.types.path;
        description = "File to capture reflections";
        example = /tmp/reflection.org;
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
      blacklisted-modes = lib.mkOption {
        type = lib.types.listOf (lib.types.enum configured-modes);
        default = [ ];
        description = ''
          You can prevent installing the dependencies of heavy modules by
          blakclisting them here. The configuration will still be present,
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
