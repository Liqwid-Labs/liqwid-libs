{ pkgs, lib, inputs }:
pkgs.haskell-nix.cabalProject'
{
  compiler-nix-name = "ghc96";
  src = lib.cleanSource ./..;
  shell = import ./shell.nix { inherit pkgs; };
  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
    "https://github.com/Plutonomicon/plutarch-plutus.git" = inputs.plutarch;
    "https://github.com/the-headless-ghost/ply.git" = inputs.ply;
  };
  sha256map = {
    "https://github.com/Plutonomicon/plutarch-plutus"."a170ce4763891e24f59572209722edf2874a7d83" = "sha256-ZIKSI6zVmBd6trzx7W46sshTv+oLW97FnPERAnBlhhc=";
    "https://github.com/the-headless-ghost/ply"."7c77122e040ec34ced500946950f9041cb05277e" = "sha256-dSXC40u8taswmygFDuGDLkdRj/ebsnk/hsJM5JAmWE4=";
  };
}
