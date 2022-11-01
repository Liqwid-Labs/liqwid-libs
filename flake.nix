{
  description = "plutarch-quickcheck";

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
      url = "github:Plutonomicon/plutarch-plutus?ref=master";

      inputs.emanote.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
      inputs.nixpkgs.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
    };
    
    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };
  };

  outputs = inputs@{ liqwid-nix, ... }:
    (liqwid-nix.buildProject
      {
        inherit inputs;
        src = ./.;
      }
      [
        liqwid-nix.plutarchProject
        liqwid-nix.haskellProject
        liqwid-nix.addBuildChecks
        (liqwid-nix.enableFormatCheck [
          "-XTemplateHaskell"
          "-XTypeApplications"
          "-XPatternSynonyms"
        ])
        liqwid-nix.enableCabalFormatCheck
        liqwid-nix.enableNixFormatCheck
      ]
    ).toFlake;
}
