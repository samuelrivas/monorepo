{
  acpi,
  haskell-pkg,
  ghcWithPackages,
  pkgs, # we get libnotify from here, if we write libnotify directly we get a haskell library
  writeShellScriptBin,
  HSH,
  parsec,
  generic-lens,
  lens,
}:
let
  binary = haskell-pkg {
    name = "low-battery";
    src = ./../src;
    haskell-libs = [
      HSH
      parsec
      generic-lens
      lens
    ];
    inherit ghcWithPackages;
  };
  # FIXME The DBUS address should not be hardcoded...
  script = writeShellScriptBin "low-battery-notify" ''
    export PATH=${pkgs.libnotify}/bin:${acpi}/bin
    export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
    ${binary}/bin/low-battery-check
  '';
  # output binary instead of script if you want to sandbox this
  # TODO fix this guacamole
# in binary
in script
