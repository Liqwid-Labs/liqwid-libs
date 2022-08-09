{
  description = "plutarch-benchmark";

  inputs = {
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=a0a69be4b5ee63f1b5e75887a406e9194012b492";
    # temporary fix for nix versions that have the transitive follows bug
    # see https://github.com/NixOS/nix/issues/6013
    nixpkgs-2111.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";

    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
    haskell-nix.follows = "plutarch/haskell-nix";
    iohk-nix.follows = "plutarch/iohk-nix";
    haskell-language-server.follows = "plutarch/haskell-language-server";


    # Plutarch and its friends
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus?ref=staging";
      inputs.emanote.follows = "plutarch/haskell-nix/nixpkgs-unstable";
      inputs.nixpkgs.follows = "plutarch/haskell-nix/nixpkgs-unstable";
    };

    liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix";



    plutarch-numeric.url = "github:Liqwid-Labs/plutarch-numeric?ref=main";
    # TODO: set this back to staging once `compilation-utilities` is merged
    liqwid-plutarch-extra.url = "github:Liqwid-Labs/liqwid-plutarch-extra?ref=compilation-utilities";
    plutarch-quickcheck.url = "github:Liqwid-Labs/plutarch-quickcheck?ref=main";
    plutarch-context-builder.url = "github:Liqwid-Labs/plutarch-context-builder?ref=staging";
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
          "${inputs.plutarch-numeric}"
          "${inputs.liqwid-plutarch-extra}"
          "${inputs.plutarch-quickcheck}"
          "${inputs.plutarch-context-builder}"
        ])
        (liqwid-nix.addChecks {
          plutarch-benchmark-example = "plutarch-benchmark:bench:plutarch-benchmark-example";
          plutarch-benchmark = "plutarch-benchmark:lib:plutarch-benchmark";
          plutarch-benchmark-test = "plutarch-benchmark:test:plutarch-benchmark-test";
        })
        (liqwid-nix.enableFormatCheck [
          "-XTemplateHaskell"
          "-XOverloadedRecordDot"
          "-XTypeApplications"
          "-XPatternSynonyms"
        ])
        liqwid-nix.enableCabalFormatCheck
        liqwid-nix.enableNixFormatCheck
        ## TODO: Hlint in Liqwid Nix is not currently up to date.
        ## This should be enabled and re-added to the branch protection rules once it is fixed
        ##liqwid-nix.enableLintCheck
      ]
    ).toFlake;
}
