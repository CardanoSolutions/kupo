{
  description = "Kupo - a lightweight cardano indexer";

  inputs = {
    cardano-node.url = "github:IntersectMBO/cardano-node/11.0.1";

    cardonnay-src = {
      url = "github:IntersectMBO/cardonnay?ref=v0.3.6";
      flake = false;
    };

    capkgs.url = "github:input-output-hk/capkgs";

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix?ref=2025.12.21";
      inputs.hackage.follows = "hackage";
    };

    flake-utils.url = "github:numtide/flake-utils";

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    jailed-agents.url = "github:andersonjoseph/jailed-agents";

    nixpkgs.follows = "haskell-nix/nixpkgs";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # process-compose = {
    #   url = "github:Platonic-systems/process-compose-flake";
  };

  outputs = inputs: inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      inherit (pkgs) lib;

      pkgs = import inputs.nixpkgs {
        inherit system;
        config = inputs.haskell-nix.config;
        overlays =
          builtins.attrValues inputs.iohk-nix.overlays
          ++ [
            inputs.haskell-nix.overlay
            inputs.iohk-nix.overlays.cardano-lib
            inputs.iohk-nix.overlays.crypto
            inputs.iohk-nix.overlays.haskell-nix-crypto
            inputs.iohk-nix.overlays.haskell-nix-extra
            inputs.iohk-nix.overlays.utils
          ];
          #   overlays = [
          #     haskellNix.overlay
          #     iohkNix.overlays.cardano-lib
          #     iohkNix.overlays.crypto
          #     iohkNix.overlays.haskell-nix-crypto
          #     iohkNix.overlays.haskell-nix-extra
          #     iohkNix.overlays.utils
          #     overlay
          #   ];
        # overlays = [
        # ];
      };

      project = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
        name = "kupo";
        compiler-nix-name = "ghc98";
        src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
          name = "kupo-src";
          src = ./.;
          filter = path: type:
            builtins.all (x: x) [
              (baseNameOf path != "package.yaml")
            ];
        };
        inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };

        # flake.variants = {
        #   ghc967 = {}; # Alias for the default variant
        #   # ghc967.compiler-nix-name = "ghc967";
        #   # ghc9102.compiler-nix-name = "ghc9102";
        #   # ghc9122.compiler-nix-name = "ghc9122";
        # };
      });

      utils = import ./nix/utils.nix { inherit pkgs lib; };

      mkShell = ghc: import ./nix/shell.nix {
        inherit inputs pkgs lib project ghc;
      };

      devShells = rec {
        default = ghc967;
        ghc967 = mkShell "ghc967";
        # ghc966 = mkShell "ghc966";
        # ghc9102 = mkShell "ghc9102";
        # ghc9122 = mkShell "ghc9122";
      };

      defaultHydraJobs = {
        ghc967 = projectFlake.hydraJobs.ghc967;
        # ghc966 = projectFlake.hydraJobs.ghc966;
        # ghc9102 = projectFlake.hydraJobs.ghc9102;
        # ghc9122 = projectFlake.hydraJobs.ghc9122;
        inherit packages;
        inherit devShells;
        required = utils.makeHydraRequiredJob hydraJobs;
      };

      hydraJobsPerSystem = {
        "x86_64-linux" = defaultHydraJobs;
        # "x86_64-darwin" = defaultHydraJobs;
        # "aarch64-linux" = defaultHydraJobs;
        # "aarch64-darwin" = defaultHydraJobs;
      };
      hydraJobs = utils.flattenDerivationTree "-" hydraJobsPerSystem.${system};
      packages = {
        # NOTE this is important or the static builds will fail with:
        # Error: pg_config not found
        process-compose-postgres-yaml = pkgs.callPackage ./process-compose/postgres.nix {};
      };
      projectFlake = project.flake {};
    in {
      inherit packages;
      inherit devShells;
      inherit hydraJobs;
    }
  );


  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      # "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      # "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
