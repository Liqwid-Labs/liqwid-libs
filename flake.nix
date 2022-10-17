{
  description = "liqwid-script-export";

  inputs = {
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=cf63df0364f67848083ff75bc8ac9b7ca7aa5a01";
    # temporary fix for nix versions that have the transitive follows bug
    # see https://github.com/NixOS/nix/issues/6013
    nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
    haskell-nix.follows = "plutarch/haskell-nix";
    iohk-nix.follows = "plutarch/iohk-nix";
    haskell-language-server.follows = "plutarch/haskell-language-server";

    # Plutarch and its friends
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus?ref=staging";

      inputs.emanote.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
      inputs.nixpkgs.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
    };

    ply.url = "github:mlabs-haskell/ply?ref=master";

    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix";
    liqwid-plutarch-extra.url = "github:Liqwid-Labs/liqwid-plutarch-extra";
    plutarch-numeric.url =
      "github:liqwid-labs/plutarch-numeric?ref=main";
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
          "${inputs.ply}/ply-core"
          "${inputs.ply}/ply-plutarch"
          "${inputs.liqwid-plutarch-extra}"
          "${inputs.plutarch-numeric}"
        ])
        (liqwid-nix.enableFormatCheck [
          "-XQuasiQuotes"
          "-XTemplateHaskell"
          "-XTypeApplications"
          "-XImportQualifiedPost"
          "-XPatternSynonyms"
          "-XOverloadedRecordDot"
        ])
        liqwid-nix.enableLintCheck
        liqwid-nix.enableCabalFormatCheck
        liqwid-nix.enableNixFormatCheck
        liqwid-nix.addBuildChecks
      ]
    ).toFlake;
}
