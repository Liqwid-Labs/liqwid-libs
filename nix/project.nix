{ pkgs, lib, inputs }:
pkgs.haskell-nix.cabalProject'
{
  compiler-nix-name = "ghc96";
  src = lib.cleanSource ./..;
  shell = import ./shell.nix { inherit pkgs; };
  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
    "https://github.com/Plutonomicon/plutarch-plutus.git" = inputs.plutarch;
  };
  sha256map = { "https://github.com/Plutonomicon/plutarch-plutus"."a170ce4763891e24f59572209722edf2874a7d83" = "sha256-ZIKSI6zVmBd6trzx7W46sshTv+oLW97FnPERAnBlhhc="; };
}
