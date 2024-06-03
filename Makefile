.PHONY: usage

usage:
	@echo "usage: make <command>"
	@echo
	@echo "Available commands:"
	@echo ""
	@echo "    format                                                      -- Formats .hs, .cabal, .nix files"
	@echo "    format_check                                                -- Check formatting of .hs, .cabal, .nix files"

	@echo "    format_haskell                                              -- Formats .hs files"
	@echo "    format_check_haskell                                        -- Check formatting of .hs files"
	@echo "    format_nix                                                  -- Formats .nix files"
	@echo "    format_check_nix                                            -- Check formatting of .nix files"
	@echo "    format_cabal                                                -- Formats .cabal files"
	@echo "    format_check_cabal                                          -- Check formatting of .cabal files"
	@echo "    lint                                                        -- Auto-refactors code"
	@echo "    lint_check                                                  -- Run code linting"
	@echo ""
	# Build
	@echo "    build_all                                                   -- Build all"
	@echo "    build_onchain-plutarch                                      -- Build onchain-plutarch"

################################################################################
# Code

# Leave the cabal build directory and the legacy code submodule alone
FIND_EXCLUDE_PATH := -not -path '**/dist-*/*'

# NOTE: we want to apply the same style rule for both onchain-plutarch and ytxp-convex even if the
# GHC version is different)
FIND_HASKELL_SOURCES := find ./ -name '*.hs' $(FIND_EXCLUDE_PATH)
FIND_NIX_SOURCES := find ./ -name '*.nix' $(FIND_EXCLUDE_PATH)
FIND_CABAL_SOURCES := find ./ -name '*.cabal' $(FIND_EXCLUDE_PATH)

# Runs as command on all results of the `find` call at one.
# e.g.
#   foo found_file_1 found_file_2
find_exec_all_fn = $(1) -exec $(2) {} +

# Runs a command on all results of the `find` call one-by-one
# e.g.
#   foo found_file_1
#   foo found_file_2
find_exec_one_by_one_fn = $(1) | xargs -i $(2) {}


.PHONY: format
format: format_haskell format_nix format_cabal
format_check : format_check_haskell format_check_nix format_check_cabal

# Run fourmolu of .hs files

FORMAT_EXTENSIONS := -o -XImportQualifiedPost -o -XBangPatterns

.PHONY: format_haskell
format_haskell: 
	$(call find_exec_all_fn, $(FIND_HASKELL_SOURCES), fourmolu -i $(FORMAT_EXTENSIONS))

.PHONY: format_check_haskell
format_check_haskell:
	$(call find_exec_one_by_one_fn, $(FIND_HASKELL_SOURCES), fourmolu --mode check $(FORMAT_EXTENSIONS))

# Run nixpkgs-fmt of .nix files
.PHONY: format_nix
format_nix:
	$(call find_exec_all_fn, $(FIND_NIX_SOURCES), nixpkgs-fmt)

.PHONY: format_check_nix
format_check_nix:
	$(call find_exec_all_fn, $(FIND_NIX_SOURCES), nixpkgs-fmt --check)

# Run cabal-fmt of .cabal files
.PHONY: format_cabal
format_cabal:
	$(call find_exec_all_fn, $(FIND_CABAL_SOURCES), cabal-fmt -i)

.PHONY: format_check_cabal
format_check_cabal:
	$(call find_exec_all_fn, $(FIND_CABAL_SOURCES), cabal-fmt --check)


# Apply hlint suggestions
.PHONY: lint
lint:
	$(call find_exec_one_by_one_fn, $(FIND_HASKELL_SOURCES), hlint -j --refactor --refactor-options="-i")

# Check hlint suggestions
.PHONY: lint_check
lint_check:
	$(call find_exec_all_fn, $(FIND_HASKELL_SOURCES), hlint -j)

################################################################################
# Build
.PHONY: build_all
build_all:
	cabal build -j all

.PHONY: build_onchain-plutarch
build_onchain-plutarch:
	cabal build -j onchain-plutarch
