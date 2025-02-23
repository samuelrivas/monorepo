{
  HSH,
  acpi,
  generic-lens,
  haskell-pkg,
  lens,
  lib,
  parsec,
  pkgs, # we get libnotify from here, if we write libnotify directly we get a haskell library
  writeShellScriptBin,
}: let
  binary = haskell-pkg {
    name = "low-battery";
    src = ./../src;
    haskell-libs = [
      HSH
      parsec
      generic-lens
      lens
    ];
    extra-native-build-inputs = [acpi];
    extra-drv = {meta.platforms = lib.platforms.linux;};
  };
  # FIXME The DBUS address should not be hardcoded...
  script = writeShellScriptBin "low-battery-notify" ''
    export PATH=${pkgs.libnotify}/bin:${acpi}/bin
    export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
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
