SHELL := env bash

.PHONY: hoogle 	build dev watch test ghci ghci-test format format_check lint \
  refactor readme_contents hasktags haddock precommit ci

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available options:"
	@echo "  FLAGS   -- Additional options passed to --ghc-options"
	@echo
	@echo "Available commands:"
	@echo "  hoogle              -- Start local hoogle"
	@echo "  build               -- Run cabal v2-build"
	@echo "  dev                 -- Run cabal v2-build -f development"
	@echo "  ci		     -- Run the full CI derivation"
	@echo "  watch [COMMAND]     -- Track files and run 'make [COMMAND]' on change"
	@echo "  test                -- Run cabal v2-test"
	@echo "  ghci                -- Run cabal v2-repl plutarch-benchmark"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  lint                -- Check hlint suggestions"
	@echo "  refactor            -- Automatically apply hlint refactors, with prompt"
	@echo "  readme_contents     -- Add table of contents to README"
	@echo "  hasktags            -- Build ctags/etags files"
	@echo "  haddock             -- Build haddock docs"


ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

PROJECT_SOURCES := $(shell find -name '*.hs' -not -path './dist-*/*' -not -path './haddock/*')

hoogle: requires_nix_shell haddock
	-killall -q hoogle
	hoogle generate --local=haddock --database=plutarch-benchmark.hoo
	hoogle server --local --database=plutarch-benchmark.hoo -p 8070 >> /dev/null && echo $! > ./.hoogle.pid &
	hoogle server --local -p 8080 >> /dev/null && echo $! > ./.hoogle.pid &

# use this when the project has build errors. hoogle seems to currently not index the project anyway..
hoogle_deps: requires_nix_shell
	-killall -q hoogle
	hoogle generate --local --database=plutarch-benchmark.hoo
	hoogle server --local --database=plutarch-benchmark.hoo -p 8070 >> /dev/null && echo $! > ./.hoogle.pid &
	hoogle server --local -p 8080 >> /dev/null && echo $! > ./.hoogle.pid &

build: requires_nix_shell
	cabal v2-build $(GHC_FLAGS)

dev: requires_nix_shell
	cabal v2-build $(GHC_FLAGS) -f development

ci: 
	nix build '.#check.x86_64-linux'

# TODO: This should share common find/fd commands with HASKELL_SOURCES and CABAL_SOURCES
watch: requires_nix_shell
	while sleep 1;																				\
	do	                                                  \
	  find $(HASKELL_SOURCES) $(NIX_SOURCES) $(CABAL_SOURCES) |               \
		  entr -cd make $(filter-out $@,$(MAKECMDGOALS));   \
  done


test: requires_nix_shell
	cabal v2-test

ghci: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) plutarch-benchmark

# TODO: Should this use fd like the others? fd gets ignore patterns out of .gitignore.
HASKELL_SOURCES := $(shell find -name '*.hs' -not -path './dist-*/*' -not -path './haddock/*')
NIX_SOURCES := $(shell fd -enix)
CABAL_SOURCES := $(shell fd -ecabal)

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XQualifiedDo -o -XTemplateHaskell -o -XTypeApplications -o -XPatternSynonyms -o -XOverloadedRecordDot

# Run fourmolu formatter
format: requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(HASKELL_SOURCES)
	nixpkgs-fmt $(NIX_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

# Check formatting (without making changes)
format_check:
	fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(HASKELL_SOURCES)
	nixpkgs-fmt --check $(NIX_SOURCES)
	cabal-fmt -c $(CABAL_SOURCES)

NIX_SHELL = nix develop
HLS_SHELL = $(NIX_SHELL) -c nix-shell -p bashInteractive haskell-language-server

shell:
	$(NIX_SHELL)

hls_shell:
	$(HLS_SHELL)

code:
	$(HLS_SHELL) --run "code ."


# Extentions we need to tell Hlint about
HLINT_EXTS := -XQuasiQuotes

# Check with hlint
lint: requires_nix_shell
	hlint $(HLINT_EXTS) $(PROJECT_SOURCES)

# Apply automatic hlint refactors, with prompt
refactor: requires_nix_shell
	for f in $(HASKELL_SOURCES); do \
	    hlint $(HLINT_EXTS) --refactor --refactor-options="-i -s" "$$f";\
	done

readme_contents:
	echo "this command is not nix-ified, you may receive an error from npx"
	npx markdown-toc ./README.md --no-firsth1

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)


################################################################################
# Utils

build_path = dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutarch-benchmark-0.1
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
