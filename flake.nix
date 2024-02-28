{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , iohk-nix
    , ...
    } @ inputs:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]
      (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            # patched libsodium
            iohk-nix.overlays.crypto
          ];
        };
        compiler = "ghc8107";

        haskell-language-server = pkgs.haskell.packages.${compiler}.haskell-language-server.overrideAttrs (attrs:
          { doCheck = false; }
        );
      in
      rec {
        devShells.default = pkgs.mkShell {
          name = "kupo-dev-shell";

          buildInputs = [
            # libraries
            pkgs.glibcLocales
            pkgs.libsodium-vrf # from iohk-nix overlay
            pkgs.lzma
            pkgs.secp256k1
            pkgs.zlib
            # build tools
            pkgs.git
            pkgs.pkgconfig
            pkgs.haskell.compiler.${compiler}
            pkgs.cabal-install
            # development tools
            pkgs.stylish-haskell
            haskell-language-server
          ];
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://kupo.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "kupo.cachix.org-1:RzYQ8KVjRJdPNt/Bhq/UqdyGYWFM8ShjEMNG8wzHBQ4="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
