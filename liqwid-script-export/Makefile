# This really ought to be `/usr/bin/env bash`, but nix flakes don't like that.
SHELL := /bin/sh

.PHONY: hoogle format haddock usage tag format_nix format_haskell format_check lint ps_bridge bench bench_check scripts

TARGETS := src

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available commands:"
	@echo "  hoogle -- Start local hoogle"
	@echo "  format -- Format the project"
	@echo "  haddock -- Generate Haddock docs for project"
	@echo "  tag -- Generate CTAGS and ETAGS files for project"
	@echo "  format_haskell -- Format haskell stuff, including source code and cabal files"
	@echo "  format_nix -- Format *.nix files only"
	@echo "  format_check -- Check if all haskell stuff have been formatted correctly"
	@echo "  lint -- Get hlint suggestions for project"
	@echo "  ci -- Run all the CI checks"

hoogle:
	pkill hoogle || true
	hoogle generate --local=haddock --database=hoo/local.hoo
	hoogle server --local -p 8081 >> /dev/null &
	hoogle server --local --database=hoo/local.hoo -p 8082 >> /dev/null &

format: format_haskell format_cabal format_nix

format_nix:
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix' | xargs nixpkgs-fmt

format_cabal:
	cabal-fmt -i $(CABAL_SOURCES)

CABAL_SOURCES := $(shell fd -ecabal)

FORMAT_EXTENSIONS := -o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -XOverloadedRecordDot
format_haskell:
	find -name '*.hs' -not -path './dist-*/*' | xargs fourmolu $(FORMAT_EXTENSIONS) -m inplace
	git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.cabal' | xargs cabal-fmt -i

format_check:
	find -name '*.hs' \
	     -not -path './dist*/*' \
	     -not -path './haddock/*' \
	  | xargs fourmolu $(FORMAT_EXTENSIONS) -m check

haddock:
	cabal haddock --haddock-html --haddock-hoogle --builddir=haddock

tag:
	hasktags -x $(TARGETS)

lint:
	hlint $(TARGETS)

ci:
	@ [[ "$$(uname -sm)" == "Linux x86_64" ]] || (echo "NOTE: CI only builds on Linux x86_64. Your system is $$(uname -sm), continuing...")
	nix build .#check.$(shell nix eval -f '<nixpkgs>' system)
