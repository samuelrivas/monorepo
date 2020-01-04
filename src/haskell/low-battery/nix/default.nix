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
      ];
      inherit haskellPackages sandbox;
    };
in if sandbox then
     low-battery
   else
     writeShellScriptBin "low-battery-notify" ''
       export PATH=${libnotify}/bin:${acpi}/bin
       ${low-battery}/bin/low-battery-check
     ''
