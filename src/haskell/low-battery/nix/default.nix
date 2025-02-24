{
  HSH,
  haskell-pkg,
  lib,
  libnotify,
  perlude,
  writeShellScriptBin,
}: let
  binary = haskell-pkg {
    name = "low-battery";
    src = ./../src;
    haskell-libs = [
      HSH
      perlude
    ];
    extra-native-build-inputs = [libnotify];
    extra-drv = {meta.platforms = lib.platforms.linux;};
  };
  # FIXME The DBUS address should not be hardcoded...
  script = writeShellScriptBin "low-battery-notify" ''
    export PATH=${libnotify}/bin
    export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1001/bus
    ${binary}/bin/low-battery-check
  '';
in
  ## We use the haskell binary dev shell, the script is easy to debug just by
  ## building the package
  script
  // {
    passthru.dev-shell = binary.passthru.dev-shell;
    meta.platforms = lib.platforms.linux;
  }
