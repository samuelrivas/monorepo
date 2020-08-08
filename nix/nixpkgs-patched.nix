let
  version = "942389f161d521e8a606aead31970d089ca2a0e6";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/samuelrivas/nixpkgs/archive/${version}.tar.gz";
    sha256 = "1n48amlpc8k32i4ly4wwjm75kx5jkqrkdjpdkpadbn0qv4pa274h";
  };
in import pkgs
