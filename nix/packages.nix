{ inputs, ... }:
{
  perSystem = { system, lib, config, ... }:
    let
      overlays = [
        inputs.haskellNix.overlay
        inputs.iohk-nix.overlays.crypto
        inputs.iohk-nix.overlays.haskell-nix-crypto
        (_final: _prev: {
          liqwid-libs = import ./project.nix {
            inherit pkgs lib inputs;
          };
        })
      ];
      pkgs = import inputs.nixpkgs {
        inherit system overlays; inherit (inputs.haskellNix) config;
      };
      flake = pkgs.liqwid-libs.flake { };
    in
    {
      packages = {
        liqwid-libs = flake.packages."liqwid-libs:lib:liqwid-libs";
      };
      devShells = flake.devShells;
    };
}
