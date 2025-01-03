OUT := dist

GHC := 9.4.8
STYLISH_HASKELL_VERSION := 0.14.5.0

# Default network for snapshots.
NETWORK := preview

OS := $(shell uname -s | sed 's/Linux/linux/' | sed 's/Darwin/macos/')
GHC_OS := $(shell uname -s | sed 's/Linux/linux/' | sed 's/Darwin/osx/')
ARCH := $(shell uname -m | sed 's/X86/x86_64/' | sed 's/arm64/aarch64/')
NIX_GHC := $(shell echo $(GHC) | sed 's/^\([0-9]\)\.\([0-9]\)\..*/\1\2/')
NIX_SHELL := github:CardanoSolutions/devx\#ghc$(NIX_GHC)-static-minimal-iog
NIX_OPTS := --no-write-lock-file --refresh
VERSION := $(shell cat package.yaml| grep "version:" | sed "s/[^0-9]*\([0-9]\)\(.[0-9].[0-9]\)*\(-.*\)*/\1\2\3/")
TAG := $(shell echo $(VERSION) | sed "s/^0$$/nightly/")
CONFIG := $(shell pwd)/config/network/$(NETWORK)
CACHEDIR := ${HOME}/.cache/kupo/${NETWORK}
BUILD_DIR_PATH := dist-newstyle/build/$(ARCH)-$(GHC_OS)/ghc-$(GHC)/kupo-$(VERSION)
BIN_DIR_PATH := $(BUILD_DIR_PATH)/x/kupo/build/kupo

ifeq ($(postgres), true)
PG_FLAG := -f postgres
endif

all: $(OUT)/bin/kupo \
		 $(OUT)/share/zsh/site-functions/_kupo \
		 $(OUT)/share/bash-completion/completions/kupo \
		 $(OUT)/share/kupo/api.yaml \
		 $(OUT)/share/kupo/LICENSE

kupo-$(TAG)-$(ARCH)-$(OS).tar.gz: all
	tar czf $@ --cd dist .

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
	@cp docs/api/nightly.yaml $@

$(OUT)/share/kupo/LICENSE:
	@mkdir -p $(@D)
	@cp LICENSE $@

$(BIN_DIR_PATH)/kupo:
	@nix develop $(NIX_SHELL) $(NIX_OPTS) --command bash -c "cabal build kupo:exe:kupo $(PG_FLAG)"
	@chmod +x $@
ifeq ($(OS),macos)
	@scripts/patch-nix-deps.sh $@
	@chmod +x $@
	@codesign -s - -f -vvvvvv $@
endif

$(OUT)/bin/kupo: $(BIN_DIR_PATH)/kupo
	@mkdir -p $(@D)
	@echo "$^ → $(@D)/kupo"
	@cp $^ $(@D)

$(OUT)/lib:
	@mkdir -p $@

.PHONY: archive configure lint man doc check clean clean-all help
.SILENT: doc clean clean-all

configure: # Freeze projet dependencies and update package index
ifeq ($(OS),macos)
	nix develop $(NIX_SHELL) $(NIX_OPTS) --command bash -c "cat /nix/store/l0np941gvmpqcgxankbgb7zpl4mj332v-cabal.project.local >> cabal.project.local"
else
ifeq ($(ARCH),x86_64)
	nix develop $(NIX_SHELL) $(NIX_OPTS) --command bash -c "cat /nix/store/vd865r55pdbndjwh994h90m35qq77x44-cabal.project.local >> cabal.project.local"
else
	nix develop $(NIX_SHELL) $(NIX_OPTS) --command bash -c "cat /nix/store/hviyb5sciblcyr5fc3vsqcwmfh1nz69w-cabal.project.local >> cabal.project.local"
endif
endif
	nix develop $(NIX_SHELL) $(NIX_OPTS) --command bash -c "cabal update && cabal freeze -f +production $(PG_FLAG)"

archive: kupo-$(TAG)-$(ARCH)-$(OS).tar.gz # Package the application as a tarball

kupo.sqlite3-$(NETWORK).tar.gz:
	@echo "Taking snapshot of NETWORK=$(NETWORK)."
	sqlite3 $(CACHEDIR)/kupo.sqlite3 "VACUUM;"
	sqlite3 $(CACHEDIR)/kupo.sqlite3 "PRAGMA optimize;"
	GZIP=-9 tar cvzf kupo.sqlite3-$(NETWORK).tar.gz --cd $(CACHEDIR) kupo.sqlite3

snapshot: kupo.sqlite3-$(NETWORK).tar.gz # Take database snapshots. Use NETWORK=XXX to specify target network.
	md5 $<
	split -b 500m $< $<.part_

lint: # Format source code automatically
ifeq ($(shell stylish-haskell --version),stylish-haskell $(STYLISH_HASKELL_VERSION))
	stylish-haskell $(shell find src test app -type f -name '*.hs' ! -path '*test/vectors/*') -i -c .stylish-haskell.yaml
else
	@echo "Invalid stylish-haskell version. Require: $(STYLISH_HASKELL_VERSION)"
endif

check: # Run tests; May require a running cardano-node for end-to-end scenarios
	cabal test kupo:test:unit

shell: # Start a nix-shell with appropriate context
	@nix develop --no-write-lock-file --refresh "github:CardanoSolutions/devx#ghc$(NIX_GHC)-static-minimal-iog"

formula: # Re-generate Homebrew formula
ifeq ($(FORMULA_VERSION),)
	@echo "Missing \033[1;33mFORMULA_VERSION\033[00m argument."
else
ifneq ($(filter v%,$(FORMULA_VERSION)),)
	@echo "Provide \033[1;33mFORMULA_VERSION\033[00m without \033[1;33m\"v\"\033[00m prefix."
else
	@LINUX_AARCH64="$(shell sha256sum kupo-v$(FORMULA_VERSION)-aarch64-linux.zip | sed "s/^\([0-9a-f]*\).*/\1/g")"; \
	LINUX_X86_64="$(shell sha256sum kupo-v$(FORMULA_VERSION)-x86_64-linux.zip | sed "s/^\([0-9a-f]*\).*/\1/g")"; \
	MACOS_AARCH64="$(shell sha256sum kupo-v$(FORMULA_VERSION)-aarch64-macos.zip | sed "s/^\([0-9a-f]*\).*/\1/g")"; \
	MAJOR_MINOR="$(shell echo $(FORMULA_VERSION) | sed "s/..$$//g")"; \
	cat .github/formula.rb.template | sed \
	  -e "s/%VERSION%/$(FORMULA_VERSION)/g" \
	  -e "s/%MAJOR_MINOR%/$${MAJOR_MINOR}/g" \
		-e "s/%LINUX_AARCH64%/$${LINUX_AARCH64}/g" \
		-e "s/%LINUX_X86_64%/$${LINUX_X86_64}/g" \
		-e "s/%MACOS_AARCH64%/$${MACOS_AARCH64}/g"
endif
endif

man: $(OUT)/share/man/man1/kupo.1 # Build man page

doc: # Serve the rendered documentation on \033[0;33m<http://localhost:8000>\033[00m
	@cd docs && python -m SimpleHTTPServer

clean: # Remove build artifacts
	rm -r $(OUT) 2>/dev/null && echo "Removed $(OUT)" || \
		rm -r $(BUILD_DIR_PATH) 2>/dev/null && echo "Removed $(BUILD_DIR_PATH)" || \
		rm cabal.project.freeze 2>/dev/null && echo "Removed cabal.project.freeze" || true
	echo "Finished cleaning."

clean-all: clean # Remove build artifacts & build cache
	cabal clean

help:
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; do printf "\033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done
