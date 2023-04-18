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
      in
      rec {
        devShells.default = pkgs.mkShell {
          name = "kupo-dev-shell";

          buildInputs = [
            pkgs.glibcLocales
            pkgs.libsodium-vrf # from iohk-nix overlay
            pkgs.lzma
            pkgs.secp256k1
            pkgs.zlib
            pkgs.git
            pkgs.pkgconfig
            pkgs.haskell.compiler.${compiler}
            pkgs.cabal-install
          ];
        };
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
