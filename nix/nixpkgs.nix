let
  version = "42674051d12540d4a996504990c6ea3619505953";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz";
    sha256 = "1hz1n1hghilgzk4zlya498xm5lvhsf0r5b49yii7q86h3616fhwy";
  };
in import pkgs
