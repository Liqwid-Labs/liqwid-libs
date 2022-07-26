{
  description = "plutarch-script-export";

  inputs.nixpkgs.follows = "plutarch/nixpkgs";
  inputs.nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=a0a69be4b5ee63f1b5e75887a406e9194012b492";
  # temporary fix for nix versions that have the transitive follows bug
  # see https://github.com/NixOS/nix/issues/6013
  inputs.nixpkgs-2111 = { url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin"; };

  # Plutarch and its friends
  inputs.plutarch.url =
    "github:Plutonomicon/plutarch-plutus?rev=67dc9cc1011044d37efc881e8f2ee491b7b8488a";
  inputs.plutarch.inputs.emanote.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";
  inputs.plutarch.inputs.nixpkgs.follows =
    "plutarch/haskell-nix/nixpkgs-unstable";

  inputs.haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
  inputs.haskell-nix.follows = "plutarch/haskell-nix";
  inputs.iohk-nix.follows = "plutarch/iohk-nix";
  inputs.haskell-language-server.follows = "plutarch/haskell-language-server";
  inputs.liqwid-nix.url = "github:Liqwid-Labs/liqwid-nix";

  outputs = inputs@{ liqwid-nix, ... }:
    (liqwid-nix.buildProject
      {
        inherit inputs;
        src = ./.;
      }
      [
        liqwid-nix.haskellProject
        liqwid-nix.plutarchProject
      ]
    ).toFlake;

}
