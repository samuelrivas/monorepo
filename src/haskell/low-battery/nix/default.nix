{
  acpi,
  add-sandbox,
  haskell-pkg,
  haskellPackages,
  libnotify,
  writeShellScriptBin,
}:
let
  binary = haskell-pkg {
    name = "low-battery";
    src = ./../src;
    haskell-libs = with haskellPackages; [
      HSH
      parsec
      generic-lens
      lens
    ];
    inherit haskellPackages;
  };
  script = writeShellScriptBin "low-battery-notify" ''
    export PATH=${libnotify}/bin:${acpi}/bin
    export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1002/bus
    ${binary}/bin/low-battery-check
  '';
# in add-sandbox (binary.sandbox.buidInputs) script
in binary
