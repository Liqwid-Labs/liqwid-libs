{ config, lib, pkgs, ... }:
let
  cfg = config.services.liqwid-script-export;
  genName = n: "lse-${n}";
  enabledInstnaces = lib.filterAttrs (name: conf: conf.enable) cfg.instances;
in
with lib;
{
  options.services.liqwid-script-export = with types; {
    instances = mkOption {
      type = attrsOf (submodule ({ config, name, ... }@args: {
        options = {
          enable = mkEnableOption (genName name);

          package = mkOption {
            description = "Liqwid Script Export Server package.";
            type = package;
          };

          binName = mkOption {
            description = "Name of the binary to run.";
            type = str;
          };

          port = mkOption {
            description = "Port to run server on";
            type = port;
          };

          enableCorsMiddleware = mkOption {
            description = "Enable CORS middleware";
            type = bool;
            default = true;
          };

          openFirewall = mkOption {
            description = "Open firewall for the selected port";
            type = bool;
            default = false;
          };

          user = mkOption {
            description = "User to run LSE service as.";
            type = str;
            default = genName name;
          };

          group = mkOption {
            description = "Group to run LSE service as.";
            type = str;
            default = genName name;
          };
        };
      }));
    };
  };
  config = {
    users.users = mapAttrs'
      (name: conf: nameValuePair (genName name) {
        isSystemUser = true;
        group = conf.group;
      })
      enabledInstnaces;
    users.groups = mapAttrs' (name: conf: nameValuePair (genName name) { }) enabledInstnaces;

    networking.firewall.allowedTCPPorts = concatMap
      (conf:
        optional conf.openFirewall conf.port
      )
      (attrValues enabledInstnaces);

    systemd.services = mapAttrs'
      (name: conf: nameValuePair (genName name) {
        enable = true;
        description = "LSE - ${genName name}";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];

        script = escapeShellArgs (concatLists [
          [ "${conf.package}/bin/${conf.binName}" "serve" ]
          [ "-p" "${toString conf.port}" ]
          (if conf.enableCorsMiddleware then [ "--enable-cors-middleware" ] else [ ])
        ]);

        serviceConfig = {
          User = conf.user;
          Group = conf.user;
          Restart = "always";
          # TODO: Finish it
        };
      })
      enabledInstnaces;
  };
}
