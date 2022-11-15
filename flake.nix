{
  description = "liqwid-plutarch-extra";

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

    plutarch-quickcheck.url =
      "github:liqwid-labs/plutarch-quickcheck?ref=seungheonoh/constraints";
    plutarch-numeric.url =
      "github:liqwid-labs/plutarch-numeric?ref=main";
    plutarch-context-builder.url =
      "github:Liqwid-Labs/plutarch-context-builder?ref=main";
    ply.url = "github:mlabs-haskell/ply?ref=master";

    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix";
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
          "${inputs.plutarch-quickcheck}"
          "${inputs.plutarch-numeric}"
          "${inputs.plutarch-context-builder}"
          "${inputs.ply}/ply-core"
          "${inputs.ply}/ply-plutarch"
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
