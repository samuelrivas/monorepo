{
  acpi,
  haskell-pkg,
  haskellPackages,
  libnotify,
  sandbox,
}: haskell-pkg {
  name = "low-battery";
  src = ./../src;
  wanted-packages = with haskellPackages; [
    HSH
    generic-lens
    lens
    parsec
  ];
  extra-build-inputs = [
    acpi
    libnotify
  ];
  inherit haskellPackages sandbox;
}
