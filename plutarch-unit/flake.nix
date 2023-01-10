{
  description = "plutarch-unit";

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/v2.0.0";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    liqwid-plutarch-extra.url = "github:Liqwid-Labs/liqwid-plutarch-extra";
    plutarch-quickcheck.url = "github:Liqwid-Labs/plutarch-quickcheck";
    plutarch-context-builder.url = "github:Liqwid-Labs/plutarch-context-builder";
    liqwid-script-export.url = "github:Liqwid-Labs/liqwid-script-export";
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
              "${self.inputs.liqwid-plutarch-extra}"
              "${self.inputs.liqwid-script-export}"
              "${self.inputs.liqwid-script-export.inputs.ply}/ply-core"
              "${self.inputs.liqwid-script-export.inputs.ply}/ply-plutarch"
              "${self.inputs.plutarch-quickcheck}"
            ];
          };
          ci.required = [ "all_onchain" ];
        };
    };
}
