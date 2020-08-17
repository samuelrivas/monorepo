{
  # Utility to add a `sandbox` field to a derivation, adding some
  # `buildInputs`. For a derivation `X`, if you want to create a shell with
  # all X's build dependencies and some extra utilities, say `[foo bar]`,
  # you can open a nix-shell on `add-sandbox [foo bar] X`
  add-sandbox = extra-deps: drv:
    drv // {
      sandbox = drv.overrideAttrs (attrs:
        { buildInputs = attrs.buildInputs ++ extra-deps; });
    };
}
