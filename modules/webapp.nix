self:
{ config, lib, pkgs, ... }:

with lib;
let cfg = config.services.webapp;
in
{
  config = {
    systemd.services."webapp" = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = let webapp = self.packages.${pkgs.system}.webapp; in
        {
          Restart = "on-failure";
          ExecStart = "${webapp}/bin/webapp";
        };
    };
    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [ 3000 ];
  };
}
