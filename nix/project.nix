{ pkgs, lib, inputs }:
pkgs.haskell-nix.cabalProject'
{
  compiler-nix-name = "ghc96";
  src = lib.cleanSource ./..;
  shell = import ./shell.nix { inherit pkgs; };
  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
  };
  sha256map = {
    "https://github.com/Plutonomicon/plutarch-plutus"."fcdd2209433d8b8979e820dc4fa9aad5f202216d" = "sha256-gQwaYGIds5owHivXi+ktH7CGeBqoLBykVxyHZZiDUM4=";
    "https://github.com/the-headless-ghost/ply"."7c77122e040ec34ced500946950f9041cb05277e" = "sha256-dSXC40u8taswmygFDuGDLkdRj/ebsnk/hsJM5JAmWE4=";
  };
}
