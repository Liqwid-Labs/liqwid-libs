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
    "https://github.com/albertodvp/plutarch-plutus"."1b2b5fb684e69e3d12319608278c1f6dea6e12af" = "sha256-xViZbZlq5Sw9rHakS9nwcQikCOfwZClSBL8UD+5eJJ4=";
  };
}
