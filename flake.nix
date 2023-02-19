{
  description = "Sam's monorepo derivation collection";

  inputs = {
    nixpkgs-upstream.url = github:NixOS/nixpkgs;
    nixpkgs-stable.url = github:NixOS/nixpkgs/nixos-22.11;
    nixpkgs-patched.url = github:samuelrivas/nixpkgs;
  };
  outputs = { self, nixpkgs-stable, nixpkgs-upstream, nixpkgs-patched }: {

    nixpkgs-upstream.x86_64-linux.hello = nixpkgs-stable.legacyPackages.x86_64-linux.hello;
  };
}
