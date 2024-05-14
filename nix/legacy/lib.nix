{
  #  Newer derivations use a `dev-shell` attribute instead of `sandbox`. The
  # flake.nix file understands the former and adds it automatically to the
  # `dev-shells` output

  derivation-helpers.add-sandbox = extra-deps: drv:
    drv
    // {
      sandbox = drv.overrideAttrs (attrs: {buildInputs = attrs.buildInputs ++ extra-deps;});
    };
}
