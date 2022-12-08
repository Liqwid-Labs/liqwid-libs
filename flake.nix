{
  description = "liqwid-script-export";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://public-plutonomicon.cachix.org" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/v2.0.0";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    liqwid-plutarch-extra.url = "github:Liqwid-Labs/liqwid-plutarch-extra";
    plutarch-quickcheck.url = "github:Liqwid-Labs/plutarch-quickcheck";
    ply.url = "github:mlabs-haskell/ply";
  };

  outputs = { self, liqwid-nix, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      imports = liqwid-nix.allModules ++ [
        ({ self, ... }:
          {
            perSystem = { config, pkgs', self', inputs, system, ... }:
              let
                pkgs = import self.inputs.nixpkgs {
                  inherit system;
                };
              in
              {
                onchain.default = {
                  src = ./.;
                  ghc.version = "ghc925";
                  shell = { };
                  enableBuildChecks = true;
                  extraHackageDeps = [
                    "${self.inputs.plutarch-quickcheck}"
                    "${self.inputs.liqwid-plutarch-extra}"
                    "${self.inputs.ply}/ply-core"
                    "${self.inputs.ply}/ply-plutarch"
                  ];
                };
                ci.required = [ "all_onchain" ];
              };
          })
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: { };
    };
}
