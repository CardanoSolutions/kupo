{ inputs, pkgs, lib, project, ghc }:

let
  tools = {
    cabal = project.tool "cabal" "3.12.1.0";
    cabal-fmt = project.tool "cabal-fmt" "latest";
    fourmolu = project.tool "fourmolu" "latest";
    haskell-language-server = project.tool "haskell-language-server" "2.12.0.0";
    hlint = project.tool "hlint" "latest";
    implicit-hie = project.tool "implicit-hie" "latest";
    stylish-haskell = project.tool "stylish-haskell" "latest";
  };

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {
    src = lib.cleanSources ../.;

    hooks = {
      nixpkgs-fmt = {
        enable = false;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = false;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
      };
      fourmolu = {
        enable = false;
        package = tools.fourmolu;
      };
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
    };
  };

  cardano-cli = inputs.cardano-node.packages.${pkgs.system}.cardano-cli;
  cardano-node = inputs.cardano-node.packages.${pkgs.system}.cardano-node;

  commonPackages = [
    cardano-cli

    tools.cabal
    tools.cabal-fmt
    tools.fourmolu
    tools.haskell-language-server
    tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
    tools.hlint
    tools.implicit-hie
    tools.stylish-haskell

    # Inspect capkgs:
    # (builtins.trace (lib.concatStringsSep ", " (lib.attrNames inputs.capkgs.packages.${pkgs.system})) inputs.capkgs)
    inputs.capkgs.packages.${pkgs.system}.bech32-input-output-hk-cardano-node-10-7-1-045bc18
    inputs.capkgs.packages.${pkgs.system}."\"cardano-addresses:exe:cardano-address\"-IntersectMBO-cardano-addresses-4-0-2-5c00d7b"

    pkgs.act
    pkgs.bash
    pkgs.bzip2
    pkgs.cacert
    pkgs.coreutils
    pkgs.curl
    pkgs.fd
    pkgs.gawk
    pkgs.git
    pkgs.gnused
    pkgs.jq
    pkgs.nixpkgs-fmt
    pkgs.perl
    pkgs.postgresql
    pkgs.postgresql.lib
    pkgs.postgresql.dev
    pkgs.python3
    pkgs.ripgrep
    pkgs.sqitchPg
    pkgs.shellcheck
    pkgs.which
    pkgs.yarn-berry
    pkgs.yarn-berry_4.yarn-berry-fetcher
    pkgs.yarn-bash-completion
    pkgs.z3
    pkgs.zlib
  ];

  systemLevelLibDeps = project.shellFor {
    packages = p: [p.cardano-crypto-class p.ouroboros-consensus];
    withHoogle = false;
  };

  commonJail = {
    # jail.combinators.unshare-all
    #     jail.combinators.mount-cwd
    #     (jail.combinators.try-fwd-env "PKG_CONFIG_PATH")
    #   ];
    baseJailOptions =
      let
        jail = inputs.jailed-agents.lib.${pkgs.system}.internals.jail;
      in [
        jail.combinators.network
        jail.combinators.time-zone
        jail.combinators.no-new-session
        jail.combinators.mount-cwd
        (jail.combinators.try-fwd-env "PKG_CONFIG_PATH")
        (jail.combinators.try-fwd-env "LD_LIBRARY_PATH")
        (jail.combinators.try-fwd-env "CARDANO_NODE_NETWORK_ID")
        (jail.combinators.try-fwd-env "CARDANO_NODE_SOCKET_PATH")
      ];

    extraReadwriteDirs = [
      "/home/paluh/.config/cabal"   # exactly the path it complains about
      "/home/paluh/.cache/cabal"          # also include the classic cabal dir (safe)
      "/home/paluh/.cabal-devx"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/state/cabal"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/bin/cabal-plan"          # also include the classic cabal dir (safe)
      "/home/paluh/.local/bin/ghcid"          # also include the classic cabal dir (safe)
      "/home/paluh/.config/opencode"
      "/home/paluh/.local/share/opencode"
      "/home/paluh/.cache/opencode"
      "/home/paluh/programming/cardano/mainnet/state-node-preprod/node.socket"
    ];
    # (builtins.trace (lib.concatStringsSep ", " (lib.attrNames project.hsPkgs.cardano-crypto-class.components.library)) project)
    # (builtins.trace (lib.concatStringsSep ", " cryptoShell.nativeBuildInputs) cryptoShell)
    # pkgs.haskell-nix.compiler.${ghc}
    extraPkgs = systemLevelLibDeps.nativeBuildInputs ++ systemLevelLibDeps.buildInputs ++ commonPackages ++ [
      pkgs.stdenv.cc.cc.lib
    ];
  };

  ld-library-path = lib.makeLibraryPath [
    pkgs.stdenv.cc.cc.lib
    pkgs.bzip2
    # add more only if you hit further .so errors (e.g. pkgs.zlib)
  ];

  shell = project.shellFor {
    name = "kupo-${project.args.compiler-nix-name}";

    nativeBuildInputs = commonPackages ++ [
      cardano-node
      # inputs.process-compose
      pkgs.process-compose
      pkgs.dbeaver-bin

      # db-schema-info generator

      (inputs.jailed-agents.lib.${pkgs.system}.makeJailedOpencode {
        inherit (commonJail) baseJailOptions extraPkgs extraReadwriteDirs;
        env = {
          LD_LIBRARY_PATH = ld-library-path;
        };
      })
      (inputs.jailed-agents.lib.${pkgs.system}.makeJailedOpencode {
        name = "jailed-bash";
        pkg = pkgs.bashInteractive;
        # configPaths = [
        #   "~/.bashrc"
        #   "~/.inputrc"
        # ];
        inherit (commonJail) baseJailOptions extraPkgs extraReadwriteDirs;
      })
    ];

    # To make shell lightweight compile only the packages
    # which pull in external dependencies like
    # cardano-crypto-class which brings in libsodium, secp256k1, etc.
    packages = p: [p.cardano-crypto-class p.ouroboros-consensus];

    withHoogle = false;

    shellHook = ''
      ${preCommitCheck.shellHook}
      export ROOT_DIR="$(git rev-parse --show-toplevel)"
      export RUN_DIR="$ROOT_DIR/.run"

      export LD_LIBRARY_PATH="${ld-library-path}:$LD_LIBRARY_PATH"
      export PATH=$PATH:"$ROOT_DIR/marlowe-integration/node_modules/.bin"

    '';
  };
in
  shell
