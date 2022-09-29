OUT := dist

OS := $(shell uname -s)
ARCH := $(shell uname -m)
VERSION := $(shell cat package.yaml| grep "version:" | sed "s/.*\([0-9].[0-9].[0-9]\)\(-.*\)*/\1\2/")
STYLISH_HASKELL_VERSION := 0.13.0.0

all: $(OUT)/bin/kupo $(OUT)/share/zsh/site-functions/_kupo $(OUT)/share/bash-completion/completions/kupo $(OUT)/share/kupo/api.yaml $(OUT)/share/kupo/LICENSE # Build everything

kupo-$(VERSION)-$(ARCH)-$(OS).tar.gz: all
	tar -czf $@ dist/*

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
	cabal install kupo:exe:kupo --flags +production --installdir=$(@D) --install-method=copy
	@echo "Kupo installed successfully."
	@echo ""
	@echo "    ╔═══ NOTE ═════════════════════════════════════════════════╗"
	@echo "    ║                                                          ║"
	@echo "    ║  The executable is dynamically linked and not portable.  ║"
	@echo "    ║                                                          ║"
	@echo "    ╚══════════════════════════════════════════════════════════╝"
	@echo ""
	@otool -L $@
	@echo ""
else
	nix-build -A kupo.components.exes.kupo
	@cp result/bin/* $(@D)
	@rm -r result
	@echo "Kupo installed successfully."
	@echo ""
	@echo "    ╔═══ NOTE ══════════════════════════════════════════════════════════╗"
	@echo "    ║                                                                   ║"
	@echo "    ║  The executable is statically linked and portable to amd64 arch.  ║"
	@echo "    ║                                                                   ║"
	@echo "    ╚═══════════════════════════════════════════════════════════════════╝"
	@echo ""
endif

.PHONY: archive lint doc check clean help

archive: kupo-$(VERSION)-$(ARCH)-$(OS).tar.gz # Package the application as a tarball

lint: # Format source code automatically.
ifeq ($(shell stylish-haskell --version),stylish-haskell $(STYLISH_HASKELL_VERSION))
	stylish-haskell $(shell find src test app -type f -name '*.hs' ! -path '*test/vectors/*') -i -c .stylish-haskell.yaml
else
	@echo "Invalid stylish-haskell version. Require: $(STYLISH_HASKELL_VERSION)"
endif

check: # Run tests; May require a running cardano-node for end-to-end scenarios
	cabal test kupo:test:unit

doc: # Serve the rendered documentation on \033[0;33m<http://localhost:8000>\033[00m
	cd docs && python -m SimpleHTTPServer

clean: # Remove build artifacts
	rm -r $(OUT) || echo "Nothing to remove."
	cabal clean

help:
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done
