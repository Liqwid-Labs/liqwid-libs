SHELL := env bash

.PHONY: hoogle \
	build dev watch test ghci \
	format lint haddock clean_haddock

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  hoogle              -- Start local hoogle"
	@echo "  build               -- Run cabal v2-build"
	@echo "  dev                 -- Run cabal v2-build -f development"
	@echo "  watch [COMMAND]     -- Track files: plutarch-unit.cabal, src/* test/* testlib/* and run 'make [COMMAND]' on change"
	@echo "  test                -- Run cabal v2-test"
	@echo "  ci 		     -- Build the full CI (hydra) derivation"
	@echo "  ghci                -- Run cabal v2-repl plutarch-unit"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  nixfmt              -- Apply nix formatting with nixfmt"
	@echo "  nixfmt_check        -- Check nix files for format errors"
	@echo "  lint                -- Apply hlint suggestions"
	@echo "  lint_check          -- Check hlint suggestions"
	@echo "  hasktags            -- Build ctags/etags files"
	@echo "  haddock             -- Build haddock docs"

hoogle: requires_nix_shell haddock
	-killall -q hoogle
	hoogle generate --local=haddock --database=plutarch-unit.hoo
	hoogle server --local --database=plutarch-unit.hoo -p 8070 >> /dev/null && echo $! > ./.hoogle.pid &
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
	  find src testlib test plutarch-unit.cabal |   \
		  entr -cd make $(filter-out $@,$(MAKECMDGOALS));   \
  done


test: requires_nix_shell
	cabal v2-test

ghci: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) plutarch-unit

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

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)

hasktags:
	hasktags -b ./src ./test ./testlib

haddock:
	cabal haddock --haddock-html --haddock-hoogle --builddir=haddock

clean_haddock:
	rm -rf haddock
