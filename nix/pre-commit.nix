# NOTE (alberto 2024-04-23): we currently can't install git hooks in the proper way
# (e.g. inside .git folder) because we have the hooks of the root project
# to run these checks, we will have to manually run `nix flake check` from the old
# pre-commit pipeline. The bash script `pre-commit-ytxp-lib.sh` can be used to achieve that.
{ inputs, ... }:
{
  perSystem = { system, lib, ... }: {
    pre-commit.check.enable = true;
    checks = {
      pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
        src = ./..;
        hooks = {
          # cabal-fmt.enable = true;
          deadnix.enable = true;
          fourmolu.enable = true;
          hlint.enable = true;
          nixpkgs-fmt.enable = true;
          typos.enable = true;
          actionlint.enable = true;
        };
      };
    };
  };
}
