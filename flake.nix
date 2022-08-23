{
  description = "plutarch-benchmark";

  inputs = {
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";
    # temporary fix for nix versions that have the transitive follows bug
    # see https://github.com/NixOS/nix/issues/6013
    nixpkgs-2111.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";

    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
    haskell-nix.follows = "plutarch/haskell-nix";
    iohk-nix.follows = "plutarch/iohk-nix";
    haskell-language-server.follows = "plutarch/haskell-language-server";


    # Plutarch and its friends
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus";
      inputs.emanote.follows = "plutarch/haskell-nix/nixpkgs-unstable";
      inputs.nixpkgs.follows = "plutarch/haskell-nix/nixpkgs-unstable";
    };

    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix";



    plutarch-numeric.url = "github:Liqwid-Labs/plutarch-numeric?ref=main";
    liqwid-plutarch-extra.url = "github:Liqwid-Labs/liqwid-plutarch-extra?ref=main";
    plutarch-quickcheck.url = "github:Liqwid-Labs/plutarch-quickcheck?ref=main";
    plutarch-context-builder.url = "github:Liqwid-Labs/plutarch-context-builder?ref=main";
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
        liqwid-nix.addBuildChecks
        (liqwid-nix.addDependencies [
          "${inputs.plutarch-numeric}"
          "${inputs.liqwid-plutarch-extra}"
          "${inputs.plutarch-quickcheck}"
          "${inputs.plutarch-context-builder}"
        ])
        (liqwid-nix.enableFormatCheck [
          "-XTemplateHaskell"
          "-XOverloadedRecordDot"
          "-XTypeApplications"
          "-XPatternSynonyms"
        ])
        liqwid-nix.enableCabalFormatCheck
        liqwid-nix.enableNixFormatCheck
        liqwid-nix.enableLintCheck
      ]
    ).toFlake;
}
