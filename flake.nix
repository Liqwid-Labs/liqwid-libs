{
  description = "liqwid-libs: A monorepo for Liqwid Labs maintained libraries";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" ];
    extra-substituters = [ "https://cache.iog.io" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=a2494bf2042d605ca1c4a679401bdc4971da54fb";

    liqwid-nix = {
      url = "github:liqwid-labs/liqwid-nix/v2.9.2";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    ply.url = "github:liqwid-labs/ply?ref=seungheonoh/purs";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.liqwid-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', system, ... }:
        let
          pkgs = import inputs.nixpkgs-latest { inherit system; };
        in
        {
          onchain.default = {
            src = ./.;
            ghc.version = "ghc925";
            fourmolu.package = pkgs.haskell.packages.ghc924.fourmolu_0_9_0_0;
            hlint = { };
            cabalFmt = { };
            hasktags = { };
            applyRefact = { };
            shell = { };
            enableBuildChecks = true;
            hoogleImage.enable = false;
            extraHackageDeps = [
              "${inputs.ply}/ply-core"
              "${inputs.ply}/ply-plutarch"
            ];
          };
          ci.required = [ "all_onchain" ];
        };

      flake.nixosModules.liqwid-script-export = { ... }: {
        imports = [ ./liqwid-script-export/nixos-module.nix ];
      };
      flake.hydraJobs.x86_64-linux = (
        self.checks.x86_64-linux
        // self.packages.x86_64-linux
      );
    };
}
