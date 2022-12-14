self:
{ config, lib, pkgs, ... }:

with lib;
let cfg = config.services.webapp;
in
{
  config = {
    systemd.services.webapp = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = let webapp = self.packages.${pkgs.system}.webapp; in
        {
          Restart = "on-failure";
          ExecStart = "${webapp}/bin/webapp";
        };
    };
    # Requires separate Nginx and Acme configuration.
    services.nginx.virtualHosts."dschrempf.duckdns.org" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://localhost:3000";
      };
    };
  };
}
