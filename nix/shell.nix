{ pkgs }:
let
  inputShell = pkgs.mkShell {
    packages = with pkgs; [
      deadnix
      nixpkgs-fmt
      typos
    ];
  };
in
{
  withHoogle = true;
  tools = {
    ghcid = "latest";
    cabal = "latest";
    cabal-fmt = "latest";
    fourmolu = "latest";
    haskell-language-server = "latest";
    hlint = "latest";
    apply-refact = "latest";
  };
  inputsFrom = [ inputShell ];
  # TODO (alberto 2024-04-23): this can't work in the current repository because
  # we already have other pre-commit hooks installed
  # shellHook = ''
  #   ${config.pre-commit.installationScript}
  # '';
}
