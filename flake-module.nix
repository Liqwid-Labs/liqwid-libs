{ self, ... }:
{
  perSystem = { config, pkgs', self', inputs, system, ... }:
    let
      pkgs = import self.inputs.nixpkgs {
        inherit system;
      };
    in
    {
      onchain.default = {
        src = ./.;
        ghc = {
          version = "ghc924";
        };
        shell = { };
        enableBuildChecks = true;
        extraHackageDeps = [ ];
      };
    };
}
