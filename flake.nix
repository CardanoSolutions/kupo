{
  description = "Kupo - fast, lightweight and configurable chain-index";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohk-nix.url = "github:input-output-hk/iohk-nix";

    haskell-nix-extra-hackage.url = "github:mlabs-haskell/haskell-nix-extra-hackage/ee50d7eb739819efdb27bda9f444e007c12e9833";
    haskell-nix-extra-hackage.inputs.haskell-nix.follows = "haskellNix";
    haskell-nix-extra-hackage.inputs.nixpkgs.follows = "nixpkgs";


    cardano-node.url = "github:input-output-hk/cardano-node/9f1d7dc163ee66410d912e48509d6a2300cfa68a";
    cardano-node.flake = false;

    hedgehog-extras.url = "github:input-output-hk/hedgehog-extras/967d79533c21e33387d0227a5f6cc185203fe658";
    hedgehog-extras.flake = false;

    cardano-base.url = "github:input-output-hk/cardano-base/0f3a867493059e650cda69e20a5cbf1ace289a57";
    cardano-base.flake = false;

    cardano-crypto.url = "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
    cardano-crypto.flake = false;

    cardano-ledger.url = "github:input-output-hk/cardano-ledger/ce3057e0863304ccb3f79d78c77136219dc786c6";
    cardano-ledger.flake = false;

    cardano-prelude.url = "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
    cardano-prelude.flake = false;

    iohk-monitoring-framework.url = "github:input-output-hk/iohk-monitoring-framework/066f7002aac5a0efc20e49643fea45454f226caa";
    iohk-monitoring-framework.flake = false;

    Win32-network.url = "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
    Win32-network.flake = false;

    ouroboros-network.url = "github:input-output-hk/ouroboros-network/a65c29b6a85e90d430c7f58d362b7eb097fd4949";
    ouroboros-network.flake = false;

    typed-protocols.url = "github:input-output-hk/typed-protocols/181601bc3d9e9d21a671ce01e0b481348b3ca104";
    typed-protocols.flake = false;

    io-sim.url = "github:input-output-hk/io-sim/f4183f274d88d0ad15817c7052df3a6a8b40e6dc";
    io-sim.flake = false;

    plutus.url = "github:input-output-hk/plutus/f680ac6979e069fcc013e4389ee607ff5fa6672f";
    plutus.flake = false;

    flat.url = "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
    flat.flake = false;

    optparse-applicative.url = "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
    optparse-applicative.flake = false;

    ekg-json.url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
    ekg-json.flake = false;

    ogmios.url = "github:CardanoSolutions/ogmios/237b4ea9bc0312405af16afccda2277f9ae3f819";
    ogmios.flake = false;

    direct-sqlite.url = "github:IreneKnapp/direct-sqlite/ab62e1e0e85ca9b92a55265744821ccfb75b0633";
    direct-sqlite.flake = false;
  };

  outputs = { self, nixpkgs, haskellNix, haskell-nix-extra-hackage, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          inherit (haskellNix) config; # { allowUnsupportedSystem = true; wine = { ... }; }
          overlays = [ haskellNix.overlay iohk-nix.overlays.crypto ];
        };

      nixpkgsFor' = system: import nixpkgs { inherit system; };

      hackagesFor = system:
        let hackages = myHackages system ghcVersion;
        in {
          inherit (hackages) extra-hackages extra-hackage-tarballs;
          #modules = haskellModules ++ hackages.modules;
          modules = hackages.modules;
        };

      ghcVersion = "ghc8107";

      myHackages = system: compiler-nix-name:
        haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name (
          [
            "${inputs.cardano-node}/cardano-api"
            "${inputs.hedgehog-extras}"
            "${inputs.cardano-base}/base-deriving-via"
            "${inputs.cardano-base}/binary"
            "${inputs.cardano-base}/binary/test"
            "${inputs.cardano-base}/cardano-crypto-class"
            "${inputs.cardano-base}/cardano-crypto-praos"
            "${inputs.cardano-base}/cardano-crypto-tests"
            "${inputs.cardano-base}/measures"
            "${inputs.cardano-base}/orphans-deriving-via"
            "${inputs.cardano-base}/slotting"
            "${inputs.cardano-base}/strict-containers"
            "${inputs.cardano-crypto}"
            "${inputs.cardano-ledger}/eras/alonzo/impl"
            "${inputs.cardano-ledger}/eras/alonzo/test-suite"
            "${inputs.cardano-ledger}/eras/babbage/impl"
            "${inputs.cardano-ledger}/eras/babbage/test-suite"
            "${inputs.cardano-ledger}/eras/byron/chain/executable-spec"
            "${inputs.cardano-ledger}/eras/byron/crypto"
            "${inputs.cardano-ledger}/eras/byron/crypto/test"
            "${inputs.cardano-ledger}/eras/byron/ledger/executable-spec"
            "${inputs.cardano-ledger}/eras/byron/ledger/impl"
            "${inputs.cardano-ledger}/eras/byron/ledger/impl/test"
            "${inputs.cardano-ledger}/eras/shelley/impl"
            "${inputs.cardano-ledger}/eras/shelley/test-suite"
            "${inputs.cardano-ledger}/eras/shelley-ma/impl"
            "${inputs.cardano-ledger}/eras/shelley-ma/test-suite"
            "${inputs.cardano-ledger}/libs/cardano-data"
            "${inputs.cardano-ledger}/libs/cardano-ledger-core"
            "${inputs.cardano-ledger}/libs/cardano-ledger-pretty"
            "${inputs.cardano-ledger}/libs/cardano-protocol-tpraos"
            "${inputs.cardano-ledger}/libs/non-integral"
            "${inputs.cardano-ledger}/libs/set-algebra"
            "${inputs.cardano-ledger}/libs/small-steps"
            "${inputs.cardano-ledger}/libs/small-steps-test"
            "${inputs.cardano-ledger}/libs/vector-map"
            "${inputs.cardano-prelude}/cardano-prelude"
            "${inputs.cardano-prelude}/cardano-prelude-test"
            "${inputs.iohk-monitoring-framework}/contra-tracer"
            "${inputs.iohk-monitoring-framework}/iohk-monitoring"
            "${inputs.iohk-monitoring-framework}/plugins/backend-aggregation"
            "${inputs.iohk-monitoring-framework}/plugins/backend-ekg"
            "${inputs.iohk-monitoring-framework}/plugins/backend-monitoring"
            "${inputs.iohk-monitoring-framework}/plugins/backend-trace-forwarder"
            "${inputs.iohk-monitoring-framework}/plugins/scribe-systemd"
            "${inputs.iohk-monitoring-framework}/tracer-transformers"
            "${inputs.Win32-network}"
            "${inputs.ouroboros-network}/monoidal-synchronisation"
            "${inputs.ouroboros-network}/network-mux"
            "${inputs.ouroboros-network}/ouroboros-consensus"
            "${inputs.ouroboros-network}/ouroboros-consensus-test"
            "${inputs.ouroboros-network}/ouroboros-consensus-byron"
            "${inputs.ouroboros-network}/ouroboros-consensus-byronspec"
            "${inputs.ouroboros-network}/ouroboros-consensus-byron-test"
            "${inputs.ouroboros-network}/ouroboros-consensus-cardano"
            "${inputs.ouroboros-network}/ouroboros-consensus-protocol"
            "${inputs.ouroboros-network}/ouroboros-consensus-shelley"
            "${inputs.ouroboros-network}/ouroboros-consensus-shelley-test"
            "${inputs.ouroboros-network}/ouroboros-consensus-cardano-test"
            "${inputs.ouroboros-network}/ouroboros-network"
            "${inputs.ouroboros-network}/ouroboros-network-framework"
            "${inputs.ouroboros-network}/ouroboros-network-testing"
            "${inputs.typed-protocols}/typed-protocols"
            "${inputs.typed-protocols}/typed-protocols-cborg"
            "${inputs.typed-protocols}/typed-protocols-examples"
            "${inputs.io-sim}/io-classes"
            "${inputs.io-sim}/io-sim"
            "${inputs.io-sim}/strict-stm"
            "${inputs.plutus}/plutus-core"
            "${inputs.plutus}/plutus-ledger-api"
            "${inputs.plutus}/plutus-tx"
            "${inputs.plutus}/prettyprinter-configurable"
            "${inputs.plutus}/stubs/plutus-ghc-stub"
            "${inputs.plutus}/word-array"
            "${inputs.Win32-network}"
            "${inputs.flat}"
            "${inputs.optparse-applicative}"
            "${inputs.ekg-json}"
            "${inputs.ogmios}/server/modules/contra-tracers"
            "${inputs.ogmios}/server/modules/fast-bech32"
            "${inputs.direct-sqlite}"
          ]
        );

        haskellModules = 42;

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
            hackages = hackagesFor pkgs.system; # TODO why pkgs.?
            pkgSet = pkgs.haskell-nix.cabalProject' ({
              name = "kupo";
              src = ./.;
              compiler-nix-name = ghcVersion;
              inherit (hackages) extra-hackages extra-hackage-tarballs modules;
              index-state = "2022-02-18T00:00:00Z";
              shell = {
                withHoogle = true;
                exactDeps = true;

                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.hlint
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.nixpkgs-fmt
                ];

                tools.haskell-language-server = { };

                additional = ps: with ps; [
                  cardano-api
                  cardano-crypto-class
                  cardano-ledger-core
                  cardano-prelude
                  ouroboros-consensus
                  ouroboros-consensus-shelley
                  plutus-core
                  plutus-ledger-api
                  plutus-tx
                ];
              };

            });
          in pkgSet;

    in {
      flake = perSystem (system: (projectFor system).flake { });

      hackages = perSystem (system: hackagesFor system);

      # Built by `nix build .`
      defaultPackage = perSystem (system:
        let lib = "kupo:exe:kupo";
        in self.flake.${system}.packages.${lib});
    };
}
