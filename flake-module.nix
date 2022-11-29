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
        extraHackageDeps = [
          "${self.inputs.plutarch-numeric}"
          "${self.inputs.plutarch-quickcheck}"
          "${self.inputs.ply}/ply-core"
          "${self.inputs.ply}/ply-plutarch"
        ];
      };
    };
}
