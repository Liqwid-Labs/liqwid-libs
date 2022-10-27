{
  description = "plutarch-unit";

  inputs = {
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";
    # temporary fix for nix versions that have the transitive follows bug
    # see https://github.com/NixOS/nix/issues/6013
    nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };
    nixpkgs-2205 = { url = "github:NixOS/nixpkgs/22.05"; };

    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
    haskell-nix.follows = "plutarch/haskell-nix";
    iohk-nix.follows = "plutarch/iohk-nix";
    haskell-language-server.follows = "plutarch/haskell-language-server";

    # Plutarch and its friends
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus";

      inputs.emanote.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
      inputs.nixpkgs.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
    };

    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix";

    # overridden here so that we can use follows and the updated fork
    plutarch-context-builder = {
      url = "github:tbidne/plutarch-context-builder?ref=liqwid-nix-update";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.liqwid-nix.follows = "liqwid-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.plutarch.follows = "plutarch";
    };
    liqwid-plutarch-extra = {
      url = "github:Liqwid-Labs/liqwid-plutarch-extra?ref=main";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.liqwid-nix.follows = "liqwid-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.nixpkgs-2205.follows = "nixpkgs-2205";
      inputs.plutarch.follows = "plutarch";
      inputs.plutarch-context-builder.follows = "plutarch-context-builder";
      inputs.plutarch-numeric.follows = "plutarch-numeric";
      inputs.plutarch-quickcheck.follows = "plutarch-quickcheck";
    };
    liqwid-script-export = {
      url = "github:Liqwid-Labs/liqwid-script-export?ref=main";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.liqwid-nix.follows = "liqwid-nix";
      inputs.liqwid-plutarch-extra.follows = "liqwid-plutarch-extra";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.plutarch.follows = "plutarch";
      inputs.plutarch-numeric.follows = "plutarch-numeric";
    };
    plutarch-numeric = {
      url = "github:tbidne/plutarch-numeric?ref=update-deps";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.liqwid-nix.follows = "liqwid-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
    };
    plutarch-quickcheck = {
      url = "github:Liqwid-Labs/plutarch-quickcheck?ref=main";
      inputs.haskell-language-server.follows = "haskell-language-server";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.haskell-nix-extra-hackage.follows = "haskell-nix-extra-hackage";
      inputs.iohk-nix.follows = "iohk-nix";
      inputs.liqwid-nix.follows = "liqwid-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
      inputs.nixpkgs-2111.follows = "nixpkgs-2111";
      inputs.nixpkgs-2205.follows = "nixpkgs-2205";
      inputs.plutarch.follows = "plutarch";
    };
  };

  outputs = inputs@{ liqwid-nix, ... }:
    (liqwid-nix.buildProject
      {
        inherit inputs;
        src = ./.;
      }
      [
        liqwid-nix.haskellProject
        liqwid-nix.plutarchProject
        (liqwid-nix.addDependencies [
          "${inputs.liqwid-plutarch-extra}"
          "${inputs.liqwid-script-export}"
          "${inputs.liqwid-script-export.inputs.ply}/ply-core"
          "${inputs.liqwid-script-export.inputs.ply}/ply-plutarch"
          "${inputs.plutarch-numeric}"
          "${inputs.plutarch-quickcheck}"
        ])
        (liqwid-nix.enableFormatCheck [
          "-XTemplateHaskell"
          "-XTypeApplications"
          "-XPatternSynonyms"
        ])
        liqwid-nix.enableCabalFormatCheck
        liqwid-nix.enableNixFormatCheck
        liqwid-nix.addBuildChecks
      ]
    ).toFlake;
}
