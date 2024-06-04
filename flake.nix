{
  description = "Liqwid Libs";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    nixpkgs.follows = "haskellNix/nixpkgs";
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus";
    };
    ply = {
      url = "github:the-headless-ghost/ply?ref=edgr/0.7.0";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      imports = [
        inputs.pre-commit-hooks.flakeModule
        ./nix/pre-commit.nix
        ./nix/packages.nix
      ];
    };
}
