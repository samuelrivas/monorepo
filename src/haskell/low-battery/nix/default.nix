{
  acpi,
  haskell-pkg,
  haskellPackages,
  libnotify,
  sandbox,
  writeShellScriptBin,
}:
let low-battery = haskell-pkg {
      name = "low-battery";
      src = ./../src;
      wanted-packages = with haskellPackages; [
        HSH
        parsec
        generic-lens
        lens
      ];
      inherit haskellPackages;
    };
in (writeShellScriptBin "low-battery-notify" ''
     export PATH=${libnotify}/bin:${acpi}/bin
     export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1002/bus
     ${low-battery}/bin/low-battery-check
   '')

// { sandbox = low-battery.sandbox; }
