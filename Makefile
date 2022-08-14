SHELL := env bash

.PHONY: hoogle \
	build test accept_pirs watch ghci readme_contents \
	format lint requires_nix_shell vis_blockchain \
	costing extra_suite costing_clean vis_clean\
	diagrams diagram_pngs clean_diagrams haddock clean_haddock

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  hoogle              -- Start local hoogle"
	@echo "  build               -- Run cabal v2-build"
	@echo "  dev                 -- Run cabal v2-build -f development"
	@echo "  watch [COMMAND]     -- Track files: liqwid-plutarch-extra.cabal, src/* test/* testlib/* and run 'make [COMMAND]' on change"
	@echo "  test                -- Run cabal v2-test"
	@echo "  ghci                -- Run cabal v2-repl liqwid-plutarch-extra"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  nixfmt              -- Apply nix formatting with nixfmt"
	@echo "  nixfmt_check        -- Check nix files for format errors"
	@echo "  lint                -- Apply hlint suggestions"
	@echo "  lint_check          -- Check hlint suggestions"
	@echo "  readme_contents     -- Add table of contents to README"
	@echo "  hasktags            -- Build ctags/etags files"
	@echo "  haddock             -- Build haddock docs"

hoogle: requires_nix_shell haddock
	-killall -q hoogle
	hoogle generate --local=haddock --database=liqwid-plutarch-extra.hoo
	hoogle server --local --database=liqwid-plutarch-extra.hoo -p 8070 >> /dev/null && echo $! > ./.hoogle.pid &
	hoogle server --local -p 8080 >> /dev/null && echo $! > ./.hoogle.pid &

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

build: requires_nix_shell
	cabal v2-build $(GHC_FLAGS)

dev: requires_nix_shell
	cabal v2-build $(GHC_FLAGS) -f development


watch: requires_nix_shell
	while sleep 1;																				\
	do	                                                  \
	  find src testlib test liqwid-plutarch-extra.cabal |   \
		  entr -cd make $(filter-out $@,$(MAKECMDGOALS));   \
  done


test: requires_nix_shell
	cabal v2-test --test-options="--quickcheck-tests 10_000"

ghci: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) liqwid-plutarch-extra

# Source dirs to run fourmolu on
FORMAT_SOURCES := $(shell find -name '*.hs' -not -path './dist-*/*')

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XTemplateHaskell -o -XTypeApplications -o -XPatternSynonyms

# Run fourmolu formatter
format: requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)
	nixpkgs-fmt $(NIX_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

# Check formatting (without making changes)
format_check:
	fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)
	nixpkgs-fmt --check $(NIX_SOURCES)
	cabal-fmt -c $(CABAL_SOURCES)

# Execute CI
ci: 
	nix build '.#check.x86_64-linux'

NIX_SHELL = nix develop
HLS_SHELL = $(NIX_SHELL) -c nix-shell -p bashInteractive haskell-language-server

shell:
	$(NIX_SHELL)

hls_shell:
	$(HLS_SHELL)

code:
	$(HLS_SHELL) --run "code ."

# Nix files to format
NIX_SOURCES := $(shell fd -enix)
CABAL_SOURCES := $(shell fd -ecabal)

# Apply hlint suggestions
lint: requires_nix_shell
	find -name '*.hs' -not -path './dist-*/*' -exec hlint --refactor --refactor-options="--inplace" {} \;

# Check hlint suggestions
lint_check: requires_nix_shell
	hlint $(FORMAT_SOURCES)

readme_contents:
	echo "this command is not nix-ified, you may receive an error from npx"
	npx markdown-toc ./README.md --no-firsth1

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)


PLUTUS_BRANCH = $(shell jq '.plutus.branch' ./nix/sources.json )
PLUTUS_REPO = $(shell jq '.plutus.owner + "/" + .plutus.repo' ./nix/sources.json )
PLUTUS_REV = $(shell jq '.plutus.rev' ./nix/sources.json )
PLUTUS_SHA256 = $(shell jq '.plutus.sha256' ./nix/sources.json )

update_plutus:
	@echo "Updating plutus version to latest commit at $(PLUTUS_REPO) $(PLUTUS_BRANCH)"
	niv update plutus
	@echo "Latest commit: $(PLUTUS_REV)"
	@echo "Sha256: $(PLUTUS_SHA256)"
	@echo "Make sure to update the plutus rev in stack.yaml with:"
	@echo "    commit: $(PLUTUS_REV)"
	@echo "This may require further resolution of dependency versions."

################################################################################
# Utils

build_path = dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/liqwidx-0.1
clear_build:
	@[ ! -e $(build_path) ] || rm -rf $(build_path)

################################################################################
# Docs

hasktags:
	hasktags -b ./src ./test ./testlib

haddock:
	cabal haddock --haddock-html --haddock-hoogle --builddir=haddock

clean_haddock:
	rm -rf haddock
