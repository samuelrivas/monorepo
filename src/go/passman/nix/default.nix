{
  add-dev-shell,
  buildGoModule,
  my-emacs,
  ubuntu_font_family,
}: let
  drv = buildGoModule {
    pname = "passman-go";
    version = "1.0.0";

    src = ../src/src;

    vendorHash = "sha256-jwQvAIS8XCWjZtP9pKt1RRSaKBZ9dgDc2SD6q0K+sEs=";
  };

  # This could be extracted if we had more go projects in this repo
  #
  # When in the sandbox, we use go mod tools to get the dependencies, and set
  # directories in /tmp as cache. When building, nix will use vendoring and
  # check the downloaded files against vendorHash.
  extra-sandbox = {
    # my-emacs is not propagating the fonts package, so we need to add it
    # explicitly for now
    native-build-inputs = [
      my-emacs
      ubuntu_font_family
    ];
    shell-hook = ''
      # This should probably be in the sandbox script
      export XDG_CACHE_HOME="/tmp/cache"

      # We could use $TMPDIR if we wanted to make these specific to the sandbox,
      # but that requires us to download everything again for every new sandbox
      # we create
      export GOPATH=/tmp/go-sandbox/
      export GOCACHE=/tmp/go-sandbox/cache
      export GOMODCACHE=/tmp/go-sandbox/modcache

      # Disable vendoring while in the sandbox
      GOFLAGS="$GOFLAGS -mod=mod"
    '';
  };
in
  add-dev-shell drv extra-sandbox
