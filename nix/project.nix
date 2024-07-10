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
    "https://github.com/Plutonomicon/plutarch-plutus"."7913e2d883530f569b16c02878989d3394bab727" = "sha256-Vg0U0QHolNHhBH2EaMHmFxeMt+Mv+thbY58PuVpWRlQ=";
    "https://github.com/mlabs-haskell/ply"."7fb83df1397eba057d00fd03f0af04a61512d9ef" = "sha256-G9zKpDgYhcFAejwI8lZ0pQnHm+WOlQNXZ1146+FwmQQ=";
  };
}
