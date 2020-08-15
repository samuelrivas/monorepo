with import ./../../../../nix { };

haskell-shell name-generator
# .overrideAttrs (attrs:
#   { buildInputs = attrs.buildInputs ++ [
#       (pkgs-sam.emacs-for-haskell ghc) haskellPackages.hoogle
#     ];
#   })
