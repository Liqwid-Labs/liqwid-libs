{
  description = "liqwid-plutarch-extra";

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/liqwid-nix-2.0";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    plutarch-numeric.url = "github:Liqwid-Labs/plutarch-numeric/emiflake/liqwid-nix-2.0";
    plutarch-quickcheck.url = "github:Liqwid-Labs/plutarch-quickcheck/emiflake/liqwid-nix-2.0";
    plutarch-context-builder.url = "github:Liqwid-Labs/plutarch-context-builder/emiflake/liqwid-nix-2.0";

    ply.url = "github:mlabs-haskell/ply?ref=master";
  };

  outputs = { self, liqwid-nix, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      imports = liqwid-nix.allModules;
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
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
              "${self.inputs.plutarch-numeric}"
              "${self.inputs.plutarch-context-builder}"
              "${self.inputs.ply}/ply-core"
              "${self.inputs.ply}/ply-plutarch"
            ];
          };
          ci.required = [ "all_onchain" ];
        };
    };
}
