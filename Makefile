OUT := dist

OS := $(shell uname -s)
ARCH := $(shell uname -m)
VERSION := $(shell cat package.yaml| grep "version:" | sed "s/.*\([0-9]\)\(.[0-9].[0-9]\)*\(-.*\)*/\1\2\3/")
STYLISH_HASKELL_VERSION := 0.13.0.0

# replace with local setup
CONFIG := /usr/local/share/cardano/network/preview

LD_LIBRARY_PATH := $(shell echo $$LD_LIBRARY_PATH | sed "s/:/ /g")
LIBSODIUM := $(shell find $(LD_LIBRARY_PATH) -type file -name "*libsodium.*.dylib" | uniq)
LIBSECP256K1 := $(shell find $(LD_LIBRARY_PATH) -type file -name "*libsecp256k1.*.dylib" | uniq)

all: $(OUT)/bin/kupo \
		 $(OUT)/lib/$(shell basename $(LIBSODIUM)) \
		 $(OUT)/lib/$(shell basename $(LIBSECP256K1)) \
		 $(OUT)/share/zsh/site-functions/_kupo \
		 $(OUT)/share/bash-completion/completions/kupo \
		 $(OUT)/share/kupo/api.yaml \
		 $(OUT)/share/kupo/LICENSE \
		 $(OUT)/share/man/man1/kupo.1
ifeq ($(OS),Darwin)
	@otool -L $(OUT)/bin/kupo
endif

kupo-$(VERSION)-$(ARCH)-$(OS).tar.gz: all
	tar -czf $@ dist/*

$(OUT)/share/man/man1/kupo.1:
	@mkdir -p $(@D)
	pandoc -s -t man docs/man/README.md > $@

$(OUT)/share/zsh/site-functions/_kupo: $(OUT)/bin/kupo
	@mkdir -p $(@D)
	$^ --zsh-completion-script kupo > $@

$(OUT)/share/bash-completion/completions/kupo: $(OUT)/bin/kupo
	@mkdir -p $(@D)
	$^ --bash-completion-script kupo > $@

$(OUT)/share/kupo/api.yaml:
	@mkdir -p $(@D)
	@cp docs/api/latest.yaml $@

$(OUT)/share/kupo/LICENSE:
	@mkdir -p $(@D)
	@cp LICENSE $@

$(OUT)/bin/kupo:
	@mkdir -p $(@D)
ifeq ($(OS),Darwin)
	cabal install --builddir=.dist-production kupo:exe:kupo --flags +production --installdir=$(@D) --install-method=copy
	@echo "Build successful."
	@echo ""
	@echo "    ╔═══ NOTE ═════════════════════════════════════════════════════════════╗"
	@echo "    ║                                                                      ║"
	@echo "    ║  The executable is dynamically linked and has limited portability,   ║"
	@echo "    ║                                                                      ║"
	@echo "    ║  Dynamic system libraries should work across same versions of MacOS. ║"
	@echo "    ║  However, other dependencies such as secpk256k1 or libsodium assume  ║"
	@echo "    ║  to be provided as standalone .dylib in a 'lib' directory next your  ║"
	@echo "    ║  installation folder.                                                ║"
	@echo "    ║                                                                      ║"
	@echo "    ║  In brief: do not change the folder structure of the archive.        ║"
	@echo "    ╚══════════════════════════════════════════════════════════════════════╝"
	@echo ""
else
	nix-build -A kupo.components.exes.kupo
	@cp result/bin/* $(@D)
	@rm -r result
	@echo "Build successful."
	@echo ""
	@echo "    ╔═══ NOTE ══════════════════════════════════════════════════════════╗"
	@echo "    ║                                                                   ║"
	@echo "    ║  The executable is statically linked and portable to amd64 arch.  ║"
	@echo "    ║                                                                   ║"
	@echo "    ╚═══════════════════════════════════════════════════════════════════╝"
	@echo ""
endif

$(OUT)/lib:
	@mkdir -p $@

$(OUT)/lib/$(shell basename $(LIBSODIUM)): $(OUT)/lib $(OUT)/bin/kupo
ifeq ($(OS),Darwin)
	@cp $(LIBSODIUM) $@
	@install_name_tool -change $(LIBSODIUM) ../lib/$(shell basename $(LIBSODIUM)) $(OUT)/bin/kupo
else
	@echo "libsodium is statically linked on Linux. Nothing to do."
endif

$(OUT)/lib/$(shell basename $(LIBSECP256K1)): $(OUT)/lib $(OUT)/bin/kupo
ifeq ($(OS),Darwin)
	@cp $(LIBSECP256K1) $@
	@install_name_tool -change $(LIBSECP256K1) ../lib/$(shell basename $(LIBSECP256K1)) $(OUT)/bin/kupo
else
	@echo "libsecp256k1 is statically linked on Linux. Nothing to do."
endif

.PHONY: archive lint man doc check clean clean-all help
.SILENT: doc clean clean-all

archive: kupo-$(VERSION)-$(ARCH)-$(OS).tar.gz # Package the application as a tarball

lint: # Format source code automatically
ifeq ($(shell stylish-haskell --version),stylish-haskell $(STYLISH_HASKELL_VERSION))
	stylish-haskell $(shell find src test app -type f -name '*.hs' ! -path '*test/vectors/*') -i -c .stylish-haskell.yaml
else
	@echo "Invalid stylish-haskell version. Require: $(STYLISH_HASKELL_VERSION)"
endif

check: # Run tests; May require a running cardano-node for end-to-end scenarios
	cabal test kupo:test:unit

man: $(OUT)/share/man/man1/kupo.1 # Build man page

doc: # Serve the rendered documentation on \033[0;33m<http://localhost:8000>\033[00m
	@cd docs && python -m SimpleHTTPServer

dev-cardano-node: # Connects Kupo to a local cardano-node on the preview network, with the provided options
	cabal run kupo:exe:kupo -- \
		--node-config $(CONFIG)/cardano-node/config.json \
		--node-socket /tmp/node.socket \
		$(filter-out $@,$(MAKECMDGOALS))

dev-ogmios: # Connects Kupo to a local ogmios bridge on the preview network, with the provided options
	cabal run kupo:exe:kupo -- \
		--ogmios-host 127.0.0.1 \
		--ogmios-port 1337 \
		$(filter-out $@,$(MAKECMDGOALS))

clean: # Remove build artifacts
	(rm -r $(OUT) 2>/dev/null && echo "Build artifacts removed.") || echo "Nothing to remove."

clean-all: clean # Remove build artifacts & build cache
	cabal clean

help:
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done
