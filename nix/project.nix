{ pkgs, lib, inputs }:
pkgs.haskell-nix.cabalProject'
{
  compiler-nix-name = "ghc96";
  src = lib.cleanSource ./..;
  shell = import ./shell.nix { inherit pkgs; };
  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
    "https://github.com/Plutonomicon/plutarch-plutus.git" = inputs.plutarch;
    "https://github.com/mlabs-haskell/ply.git" = inputs.ply;
  };
  sha256map = {
    "https://github.com/Plutonomicon/plutarch-plutus"."a170ce4763891e24f59572209722edf2874a7d83" = "sha256-ZIKSI6zVmBd6trzx7W46sshTv+oLW97FnPERAnBlhhc=";
    "https://github.com/mlabs-haskell/ply"."efeb0baddffccdbbf38d1e7e02cd1778422eeb78" = "sha256-om9EEGrw09gm0i+i9GYp0KYWQMXAd6ZvAZPzW6cGSLw=";
  };
}
